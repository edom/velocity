module Text.Velocity.Context where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Monoid

import Control.Error

import Text.Velocity.Parse
import Text.Velocity.Types

-- * Variable values

-- | An 'Object' is something that can be bound to a variable.

data Object
    = BooleanObject Bool
    | IntegerObject Integer
    | String String
    -- TODO rename String to StringObject
    deriving (Read, Show)

-- * Context

-- | Variable binding.

data VarBind
    = VarBind
    {
        varName :: String
        , varContent :: Object
    }
    deriving (Read, Show)

-- | Macro binding.

data Macbind
    = Macbind
    {
        macroName :: String
        , macroArgs :: [MacroArg]
        , macroBody :: Node
    }
    deriving (Read, Show)

{- |

A context is a list of bindings.

Macros and variables have different namespace.

'Context' forms a 'Monoid' in the sense that
'mempty' is 'emptyContext' and 'mappend'ing two contexts
concatenates their respective bindings.
In the result, bindings from the left side comes before
bindings from the right side.

-}

data Context
    = Context
    {
        conVariables :: [Varbind]
        , conMacros :: [Macbind]
    }
    deriving (Read, Show)

instance Monoid Context where
    mempty = emptyContext
    mappend (Context a0 b0) (Context a1 b1) = Context (mappend a0 a1) (mappend b0 b1)

-- | 'Context' with no bindings.

emptyContext :: Context
emptyContext = Context [] []

-- | Add (or (TODO) replace) a macro binding to the context.

addMacro :: Macbind -> Context -> Context
addMacro m c = c { conMacros = m : conMacros c }

-- | A context containing one macro binding.

macroContext :: Name -> [MacroArg] -> Node -> Context
macroContext name args body = Context [] [Macbind name args body]

-- | A context containing one variable binding.

varContext :: Name -> Object -> Context
varContext name val = Context [VarBind name val] []

-- | Render the object.

objectToString :: Object -> String
objectToString x = case x of
    String s -> s
    IntegerObject a -> show a
    BooleanObject a -> if a then "true" else "false"

co_get_var :: String -> Context -> [Object]
co_get_var name =
    conVariables
    >>> filter (varName >>> (name ==))
    >>> map varContent

-- FIXME
-- A variable is a macro with zero argument
-- in a different namespace.

lookupVar :: Context -> String -> Maybe Object
lookupVar con name = fmap varContent $ find ((name ==) . varName) (conVariables con)

-- | Adds or replaces a variable binding.

conSetVar :: Name -> Object -> Context -> Context
conSetVar name val con = con { conVariables = replace (conVariables con) }
    where
        replace vars =
            case vars of
                var : vs ->
                    if varName var == name then entry : vs
                    else var : replace vs
                _ -> [entry]
        entry = VarBind name val

-- * Context stack

-- | The head of the list is the top of the stack.

type Stack a = [a]

-- | This is a recursive 'lookupVar' until a binding is found.

staLookupVar :: Stack Context -> String -> Maybe Object
staLookupVar sta name = msum $ map (flip lookupVar name) sta

-- | Like 'staLookupVar' but raises an error if the variable is not found.

staLookupVarE :: (Monad m) => Stack Context -> String -> EitherT VelocityError m Object
staLookupVarE sta name = hoistEither $ note (UndefinedRef name) $ staLookupVar sta name

-- | Apply the function to the top of the stack if any;
-- otherwise does nothing.

staMapTop :: (a -> a) -> Stack a -> Stack a
staMapTop f xs =
    case xs of
        y : ys -> f y : ys
        _ -> xs

-- * Deprecated stuffs

type Varbind = VarBind
co_vars = conVariables
co_macs = conMacros
{-# DEPRECATED Varbind "use 'VarBind'" #-}
{-# DEPRECATED co_vars "use 'conVariables'" #-}
{-# DEPRECATED co_macs "use 'conMacros'" #-}
