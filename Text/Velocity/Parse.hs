{- |

Context-nonfree behavior complicates parsing.
The result of parsing a variable reference
depends on whether the variable is defined.

We take a different approach.
Our parser does not have side effect.

-}

module Text.Velocity.Parse where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity
import Data.Char
import System.IO.Error

import Control.Error
import Text.Parsec hiding (ParseError)
import Text.Parsec.Expr
import qualified Text.Parsec as Parsec

import Text.Velocity.Types

-- * Velocity Template Language parser

-- | Monadic version.
-- The file path is used for error reporting.
-- This only builds the syntax tree
-- and does not perform any IO such as including other files.

parseVelocityM :: (Functor m, Monad m) => FilePath -> String -> EitherT VelocityError m Node
parseVelocityM path content =
    bimapEitherT SyntaxError id . hoistEither . runIdentity $ runParserT document () path content

-- | Simple version. This is just 'parseVelocityM' with 'Identity' monad.

parseVelocity :: FilePath -> String -> Either VelocityError Node
parseVelocity path content = runIdentity . runEitherT $ parseVelocityM path content

-- | Read the entire content of the file and then parse it.

parseVelocityFile :: FilePath -> EitherT VelocityError IO Node
parseVelocityFile path = EitherT (fmapL ParseIOError <$> tryIOError (readFile path)) >>= parseVelocityM path

parseVelocityM2 :: (Functor m, Monad m) => FilePath -> String -> EitherT VelocityError m SyntaxTree
parseVelocityM2 path content = bimapEitherT SyntaxError id . EitherT $ runParserT document () path content
    where
        document = Level <$> many node
        node = lineComment
        lineComment =
            Leaf <$> (try (string "##") >> LineComment <$> manyTill anyChar newline)

type P a = ParsecT String () Identity a

-- * Character parsers

{- $

These parsers should be self-explanatory.
The result of each of these parsers is the described character.

-}

lparen :: P Char
lparen = char '('

rparen :: P Char
rparen = char ')'

lbrace :: P Char
lbrace = char '{'

rbrace :: P Char
rbrace = char '}'

quote :: P Char
quote = char '"'

apos :: P Char
apos = char '\''

equal :: P Char
equal = char '='

backslash :: P Char
backslash = char '\\'

dollar :: P Char
dollar = char '$'

exclam :: P Char
exclam = char '!'

-- | VTL specification says we must use space
-- but the Java implementation also accepts comma.

delim :: P Char
delim = space <|> comma

comma :: P Char
comma = char ','

escape :: P Char
escape = char '\\' >> (oneOf "\\\"'$#" <?> "invalid escape sequence")

-- | Horizontal space character.

hspace :: P Char
hspace = satisfy ((Space ==) . generalCategory)

-- * String parsers

hspaces :: P String
hspaces = many hspace

delims :: P String
delims = many delim

identifier :: P String
identifier = pure (:) <*> letter <*> many (alphaNum <|> oneOf "-_")

-- | The delimiters are discarded. An unparsed literal looks like @\#[[ text ]]\#@.

verbatim :: P Node
verbatim = try (string "#[[") >> Literal <$> manyTill anyChar (try $ string "]]#")

-- * Node parsers

-- | This tries to consume the entire input.
-- This tries to parse as long as possible sequence of nodes.

document :: P Node
document = Fragment . joinLiterals <$> many node

-- | This parser creates a node in 'SyntaxTree'.

node :: P Node
node =
    choice
        [
            variable
            , lineComment, blockComment
            , macroDefinition
            , setDirective, includeDirective, parseDirective, verbatim
            , escapeNode
            , literal
            , literalChar
        ]
    where
        -- If all else fails, the character is copied as is.
        literalChar = Literal . pure <$> anyChar

escapeNode :: P Node
escapeNode =
    backslash >>= \ c ->
        try (fmap Literal . choice $ map string ["#set", "#macro", "#end"])
        <|> return (Literal $ pure c)

-- ** Comment parsers

-- | Return the content of the comment.
-- The ending 'newline' is discarded.
-- If this parser fails, it does not consume any input.

lineComment :: P Node
lineComment = try (string "##") >> LineComment <$> manyTill anyChar newline

-- | Return the content of the comment.
-- Both the comment begin marker and end marker are discarded.
-- If this parser fails, it does not consume any input.

blockComment :: P Node
blockComment = try (string "#*") >> BlockComment <$> manyTill anyChar (try $ string "*#")

{- |

This parser parses a possible variable reference.
We say /possible/ because the meaning of this node
depends on whether backslash precedes the dollar sign,
whether the variable has been defined,
and whether the strict mode is enabled.

This returns a 'Variable' node.

-}

variable :: P Node
variable = try $
    success backslash >>= \ escape -> dollar >>
    success exclam >>= \ silent ->
    success lbrace >>= \ formal ->
    identifier >>= \ name ->
    when_ formal rbrace >>
    return
        (Variable
            (bool NotEscape Escape escape)
            (bool NotSilent Silent silent)
            (Name2 name)
            (concat
                [
                    if escape then "\\" else ""
                    , "$"
                    , if silent then "!" else ""
                    , if formal then "{" else ""
                    , name
                    , if formal then "}" else ""
                ]))
    where
        bool :: a -> a -> Bool -> a
        bool x0 x1 bo = if bo then x1 else x0
        when_ :: (Monad m) => Bool -> m a -> m ()
        when_ cond action = when cond $ action >> return ()

-- ** Directives

{- |

This parses a @\#set@ directive.
If this parser fails, 'node' should treat the string @\"\#set\"@
as a call without argument list to a macro named @set@.

This is more relaxed than the original VTL @\#set@ syntax?

The left parenthesis and the @\"\#set\"@ must be in the same line.

-}

setDirective :: P Node
setDirective =
    try $
        string "#set" >> hspaces >> lparen >> spaces >> variable >>= \ (Variable _ _ name _) ->
        spaces >> equal >> spaces >> expression >>= \ value ->
        spaces >> rparen >> return (Set name value)

-- | An expression that can be at the right-hand
-- side of the equal sign in a @\#set@ directive.

expression :: P (Expr Node)
expression = buildExpressionParser table term
-- choice [arithmetic, boolean, macroActualArgument, fragmentArg, fmap Literal identifier]
    where
        prefix cons op = Prefix (try (string op) >> return cons)
        binary cons op assoc = Infix (try (string op) >> return cons) assoc
        binaries assoc = map (\ (con, sym) -> binary con sym assoc)
        table :: OperatorTable [Char] () Identity (Expr Node)
        table =
            [
                [prefix Not "!"]
                , binaries AssocLeft [(Mul, "*"), (Div, "/")]
                , binaries AssocLeft [(Add, "+"), (Sub, "-")]
                , binaries AssocNone [(Equ, "=="), (Neq, "!="), (Lte, "<="), (Lth, "<"), (Gte, ">="), (Gth, ">")]
                , binaries AssocLeft [(Con, "&&"), (Dis, "||") ]
            ]
        term = between lparen rparen (between spaces spaces expression)
            <|> fmap Term (between spaces spaces (variable <|> integer))
        integer = IntegerNode . read <$> many1 digit -- XXX read can fail?
        fragmentArg = do
            _ <- quote
            s <- fragmentBody
            i <- Parsec.getInput
            p <- Parsec.getPosition
            Parsec.setInput s
            r <- fragment
            Parsec.setInput i
            Parsec.setPosition p -- XXX this screws position?
            return r
        fragmentBody = manyTill (escape <|> anyChar) quote

macroActualArgument :: P Node
macroActualArgument = choice [variable, aposLiteral]
    where
        aposLiteral = do
            _ <- apos
            Literal <$> manyTill (escape <|> anyChar) apos

{- |

In the original VTL, the input @\"\#macro\"@ by itself (without argument list) produces syntax error
whereas in this Haskell version, it is treated as a call to a macro named @macro@.

-}

macroDefinition :: P Node
macroDefinition = try $
    string "#macro" >> hspaces >> lparen >> spaces >> identifier >>= \ name ->
    spaces >> rparen >> return (MacroDef name [] (Literal ""))
    -- FIXME

includeDirective :: P Node
includeDirective = try $
    string "#include" >> hspaces >> lparen >> spaces >> return (Include [])
    -- FIXME

parseDirective :: P Node
parseDirective = try $
    string "#parse" >> hspaces >> lparen >> spaces >> return (Include [])
    -- FIXME

fragment :: P Node
fragment = Fragment . joinLiterals <$> many node

-- * Parser combinators

{- |

    *   If @p@ is successful,
        @'success' p@ discards the result of @p@ and returns 'True'.

    *   If @p@ fails, @'success' p@ returns 'False'
        and makes sure that no input is consumed,
        regardless of whether @p@ has consumed any input.

-}

success :: (Monad m) => ParsecT s u m a -> ParsecT s u m Bool
success parser = (try parser >> return True) <|> return False

parenthesized :: P a -> P a
parenthesized parser_ = between lparen rparen parser_

braced :: P a -> P a
braced parser_ = between lbrace rbrace parser_

-- | A run of text that will definitely be passed to the output as is.

literal :: P Node
literal = Literal <$> many1 (noneOf "#$\\")

toArg :: Node -> P MacroArg
toArg n =
    case n of
        Var name -> return $ MacroArg name Nothing
        _ -> parserFail "woo"

-- rename to rvalue?
delimited :: P a -> P a
delimited parser_ = between delims delims parser_ -- XXX ?

-- * Joining literal nodes

-- | Join as much possible consecutive 'Literal' nodes into one.

joinLiterals :: [Node] -> [Node]
joinLiterals nodes =
    case nodes of
        Literal a : Literal b : cs -> joinLiterals $ Literal (a ++ b) : cs
        a : bs -> a : joinLiterals bs
        [] -> []
