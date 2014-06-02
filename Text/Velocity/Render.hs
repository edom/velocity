{-# LANGUAGE GADTs #-}

{- |

This module must be sufficiently general so that it can be used to
reimplement both Apache Velocity and GNU m4.

-}

module Text.Velocity.Render where

import Control.Applicative
import Control.Monad.Writer.Lazy

import Control.Error
import Control.Monad.Operational

import Text.Velocity.Context
import Text.Velocity.Parse
import Text.Velocity.Types

-- | Without macro definition.

data RenderI a where
    Emit :: String -> RenderI ()
    Raise :: VelocityError -> RenderI a
    GetVar :: String -> RenderI Object
    GetVarMaybe :: String -> RenderI (Maybe Object)
    GetVarQuiet :: String -> RenderI Object
    SetVar :: Name -> Object -> RenderI ()
    PushContext :: Context -> RenderI ()
    PopContext :: RenderI Context
    -- Embed :: RenderM a -> RenderI String
    -- DefMacro :: Name -> RenderI ()

type RenderM = Program RenderI

runRenderM :: Stack Context -> RenderM a -> EitherT VelocityError (WriterT String IO) a
runRenderM sta m_ = run m_
    where
        run m = case view m of
            Return x -> return x
            Emit x :>>= k -> lift (tell x) >>= run . k
            GetVar n :>>= k -> maybe (left $ UndefinedRef n) (run . k) (staLookupVar sta n)
            GetVarMaybe n :>>= k -> run . k $ staLookupVar sta n
            GetVarQuiet n :>>= k -> run . k $ fromMaybe (String "") (staLookupVar sta n)
            SetVar name value :>>= k -> runRenderM (staMapTop (conSetVar name value) sta) $ k ()
            Raise e :>>= _ -> left e
            -- Include p :>>= k -> run $ k p -- FIXME

eval :: Expr Node -> RenderM Object
eval e = case e of
    Term (Literal s) -> pure $ String s
    Term (IntegerNode s) -> pure $ IntegerObject s
    Term (Variable escape silent name input) -> singleton . GetVar $ nameToString name
    Add e0 e1 -> add <$> eval e0 <*> eval e1
    Sub e0 e1 -> sub <$> eval e0 <*> eval e1
    Mul e0 e1 -> mul <$> eval e0 <*> eval e1
    Equ e0 e1 -> equ <$> eval e0 <*> eval e1
    _ -> error $ "eval: " ++ show e
    where

add :: Object -> Object -> Object
add a b = case (a, b) of
    (IntegerObject x, IntegerObject y) -> IntegerObject $ x + y
    (IntegerObject x, String y) -> String $ show x ++ y
    (String x, IntegerObject y) -> String $ x ++ show y
    (String x, String y) -> String $ x ++ y
    _ -> error $ "add: " ++ show (a, b)

equ :: Object -> Object -> Object
equ a b = case (a, b) of
    (IntegerObject x, IntegerObject y) -> BooleanObject $ x == y
    (BooleanObject x, BooleanObject y) -> BooleanObject $ x == y
    (String x, String y) -> BooleanObject $ x == y
    (x, y) -> BooleanObject $ objectToString x == objectToString y

sub :: Object -> Object -> Object
sub a b = case (a, b) of
    (IntegerObject x, IntegerObject y) -> IntegerObject $ x - y
    _ -> error $ "sub: " ++ show (a, b)

mul :: Object -> Object -> Object
mul a b = case (a, b) of
    (IntegerObject x, IntegerObject y) -> IntegerObject $ x * y
    _ -> error $ "mul: " ++ show (a, b)

-- | Translate syntax tree into rendering instructions.

makeProgram :: Node -> RenderM ()
makeProgram node = case node of
    Fragment nodes -> mapM_ makeProgram nodes
    Literal s -> singleton $ Emit s
    Variable escape silent name input ->
        singleton (GetVarMaybe $ nameToString name)
        >>= singleton . Emit . maybe input objectToString
    Set name value -> eval value >>= singleton . SetVar (nameToString name)
    LineComment _ -> return ()
    BlockComment _ -> return ()
    _ -> error $ "makeProgram: " ++ show node

altRenderNode :: Stack Context -> Node -> EitherT VelocityError IO String
altRenderNode sta node =
    case node of
        Fragment nodes -> fmap concat $ mapM loop nodes
        Literal s -> return s
        -- Var n -> staLookupVarE sta n >>= loop
        _ -> return "" -- FIXME
    where
        loop x = altRenderNode sta x

renderVelocityFile :: Context -> FilePath -> EitherT VelocityError IO String
renderVelocityFile con path = parseVelocityFile path >>= altRenderNode [con]

renderVelocityFile2 :: Context -> FilePath -> EitherT VelocityError IO String
renderVelocityFile2 con path =
    parseVelocityFile path
    >>= extractWriter . runRenderM [con] . makeProgram
    where
        extractWriter :: (Monad m) => EitherT e (WriterT w m) a -> EitherT e m w
        extractWriter =
            mapEitherT $ \ writer_ -> do
                (a, w) <- runWriterT writer_
                return $ fmap (const w) a
