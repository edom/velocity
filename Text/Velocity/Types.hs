module Text.Velocity.Types where

import qualified Text.Parsec as Parsec

-- * Error type

{- |

Error that could happen while processing a template.

-}

data VelocityError =
    SyntaxError Parsec.ParseError
    | ParseIOError IOError
    | UndefinedRef Name
    deriving (Show)

-- * Lexical analysis

data Token
    = Dollar
    | DollarBang
    | Hash
    | Word String
    | White String
    deriving (Read, Show)

type Name = String
{-# DEPRECATED Name "use 'Name2'" #-}

newtype Name2 = Name2 { nameToString :: String } deriving (Read, Show, Eq, Ord)

-- | Multiway tree.

data Tree a
    -- | single node
    = Leaf a
    -- | list of nodes in the same level
    | Level [Tree a]
    deriving (Read, Show)

emptyTree :: Tree a
emptyTree = Level []

type SyntaxTree = Tree Node

-- | An inhabitant of this type describes a formal argument of a macro.

data MacroArg
    = MacroArg
    {
        maName :: Name
        , maDefault ::  (Maybe Node)
    }
    deriving (Read, Show)

data Escape = NotEscape | Escape deriving (Read, Show)
data Silent = NotSilent | Silent deriving (Read, Show)
data Formal = NotFormal | Formal deriving (Read, Show)

data Substitution
    -- | variable reference;
    -- first bool is whether the reference is silent (exlamation-marked)
    -- second bool is formal (surrounded by braces)
    = SVar Silent Formal Name
    -- | possible macro call
    | SCall Name [RValue]
    -- | like macro call but without argument list
    | SZacall
    deriving (Read, Show)

{- |

    *   right-hand side of assignment operator

    *   actual argument in macro call

-}

data RValue
    = RVar Name
    | RLit String
    | RFragment Node
    deriving (Read, Show)

-- | Right-hand side of equal sign in a @\#set@ directive.

data Expr a
    = Term a
    | Mul (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Equ (Expr a) (Expr a)
    | Neq (Expr a) (Expr a)
    | Lte (Expr a) (Expr a)
    | Lth (Expr a) (Expr a)
    | Gte (Expr a) (Expr a)
    | Gth (Expr a) (Expr a)
    | Not (Expr a)
    | Con (Expr a) (Expr a)
    | Dis (Expr a) (Expr a)
    deriving (Read, Show)

-- | An inhabitant of this type is a syntax tree.

data Node
    
    -- | interpolated string; list of nodes; also root of the syntax tree
    = Fragment [Node]
    -- FIXME get a name better than Fragment

    -- | non-interpolated string
    | Literal String
    -- TODO rename Literal to StringNode

    | IntegerNode Integer

    -- | escaped possible substitution
    | Esc Substitution

    -- | possible substitution
    | Unesc Substitution

    | Var Name
    | Call Name [Node]
    | Zacall Name2

    | Variable Escape Silent Name2 String

    -- | macro definition
    | MacroDef Name [MacroArg] Node

    -- | act as if the contents of the files were here
    | Parse [Node]

    -- | embed the contents of the files an unparsed literal
    | Include [Node]

    -- | change the content of a variable
    | Set Name2 (Expr Node)
    -- TODO change to Set Name2 RValue
    -- FIXME not yet implemented

    | LineComment String

    | BlockComment String

    deriving (Read, Show)

{-# DEPRECATED Var "use 'Variable'" #-}
