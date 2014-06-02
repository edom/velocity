-- | Lexical analysis.

module Text.Velocity.Lex where

import Control.Applicative hiding ((<|>), many)
import Control.Error
import Text.Parsec

import Text.Velocity.Types

lexVelocityM :: (Functor m, Monad m) => FilePath -> String -> EitherT VelocityError m [Token]
lexVelocityM path content =
    bimapEitherT SyntaxError id . EitherT $ runParserT lex () path content
    where
        lex = many token
        token = (try (string "$!") >> return DollarBang)
            <|> (char '$' >> return Dollar)
            <|> (char '#' >> return Hash)
            <|> Word <$> manyTill anyChar (space <|> oneOf "$#")
            <|> White <$> many space
