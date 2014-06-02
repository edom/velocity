{-# LANGUAGE GADTs #-}

{- |

    *   Parse 'String' into @['Node']@.

    *   Parse macro definitions in @['Node']@ giving a @'Context'@ and an @'Program' 'RenderI'@.
        Only macro definition in the current level is parsed.

    *   @'IO' a@.

-}

module Text.Velocity.Example where

import Control.Error

import Text.Velocity
import Text.Velocity.Context
import Text.Velocity.Parse
import Text.Velocity.Render
import Text.Velocity.Types

example1 :: Either VelocityError Node
example1 = parseVelocity "-" "Hello world."

example2 :: Either VelocityError Node
example2 = parseVelocity "-" "$abc$!abc$!{abc}$abc.def$100#100 'dojima rice $exchange'"

example3 :: IO ()
example3 = runEitherT (parseVelocityFile "test/variable.vm") >>= print

example4 :: IO ()
example4 = runEitherT (renderVelocityFile2 con "test/variable.vm") >>= either print putStr
    where
        con = emptyContext
