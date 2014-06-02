module Text.Velocity
(
    defaultVelocityState
    , Vstate(..)
    , render
    , pretty_ast

    -- * Parsing

    , parseVelocityM
    , parseVelocity
    , parseVelocityFile
)
where

import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad.State
import qualified Control.Monad.State.Class as State

import Text.Velocity.Context
import Text.Velocity.Parse
import Text.Velocity.Types

data Vstate
    = Vstate
    {
        vs_template_root :: FilePath
        , vs_stack :: Stack Context
    }

vs_add_macro :: Macbind -> Vstate -> Vstate
vs_add_macro binding state_
    = state_
    {
        vs_stack = stack_map_top (addMacro binding) (vs_stack state_)
    }

stack_map_top :: (a -> a) -> Stack a -> Stack a
stack_map_top f stack =
    case stack of
        [] -> []
        x : xs -> f x : xs

defaultVelocityState :: Vstate
defaultVelocityState = Vstate "/home/john/sender/templates/" []

v_get_var :: (Functor m, Monad m) => String -> StateT Vstate m (Maybe VarBind)
v_get_var name = State.gets $
    vs_stack >>> concatMap co_vars >>> filter (varName >>> (name ==)) >>> listToMaybe

v_get_macro :: (Functor m, Monad m) => String -> StateT Vstate m (Maybe Macbind)
v_get_macro name = State.gets $
    vs_stack >>> concatMap co_macs >>> filter (macroName >>> (name ==)) >>> listToMaybe

render :: [Node] -> StateT Vstate IO String
render = phase1 >=> phase_2

pretty_ast :: [Node] -> String
pretty_ast nodes =
    case nodes of
        [] -> ""
        node : tail_ -> pretty 0 node ++ pretty_ast tail_
    where
        pretty level node =
            let
                level' = succ level
                indent = replicate (4 * level) ' '
            in
                indent ++ case node of
                    Fragment nodes_ -> "Fragment\n" ++ concatMap (pretty level') nodes_
                    Call name args -> "Call " ++ name ++ "\n" ++ concatMap (pretty level') args
                    -- MacroDef name args body -> "MacroDef " ++ name ++ " " ++ show args ++ "\n" ++ concatMap (pretty level') body
                    _ -> show node ++ "\n"

-- | You can use a macro before its definition.

phase1 :: [Node] -> StateT Vstate IO [Node]
phase1 nodes =
    case nodes of
        [] -> return []
        node : tail_ -> do
            replacements <- case node of
                BlockComment _ -> return []
                LineComment _ -> return []
                MacroDef name args body -> do
                    State.modify $ vs_add_macro (Macbind name args body)
                    return []
                _ -> return [node]
            (replacements ++) <$> phase1 tail_

with_context :: Context -> StateT Vstate IO a -> StateT Vstate IO a
with_context context =
    withStateT (\ s -> s { vs_stack = context : vs_stack s })

-- | True if @bind@ does not occur in @macparams@.

free_var :: VarBind -> [Node] -> Bool
free_var bind macparams =
    not $ elem
        (varName bind)
        (flip mapMaybe macparams $ \ node ->
            case node of
                Var name -> Just name
                _ -> Nothing)

-- | This is supposed to be similar to lambda calculus beta-reduction.

replace_var :: VarBind -> [Node] -> [Node]
replace_var bind@(VarBind name content) nodes =
    flip concatMap nodes $ \ node ->
        case node of
            Fragment nodes_ -> [Fragment (replace_var bind nodes_)]
            -- Var name2 | name == name2 -> [content]
            -- QuietVar name2 | name == name2 -> [content]
            defn@(MacroDef mname mparams mbody) ->
                if free_var bind (map (Var . maName) mparams)
                    -- then [MacroDef mname mparams (replace_var bind mbody)]
                    then undefined -- FIXME
                    else [defn]
            Call name_ args -> [Call name_ (replace_var bind args)]
            _ -> [node]

-- what to do when undefined variable?
-- * print '$var' literally
-- * throw an error

invoke :: Node -> StateT Vstate IO String
invoke (Call name argvals) = do
    m_macbind <- v_get_macro name
    case m_macbind of
        Nothing ->
            error $ "undefined macro: #" ++ name
        Just macbind@(Macbind _ macargs mbody) -> do
            unless (length macargs == length argvals) $ do
                error $ "macro formal and actual argument count must match:\nformal: " ++ show macbind ++ "\nactual: " ++ show argvals
            -- let assignment = zipWith (\ (Var m) a -> VarBind m a) (map (Var . maName) macargs) argvals
            -- let mbody' = foldr (\ a b -> replace_var a b) mbody assignment
            let mbody' = undefined -- FIXME
            render mbody'
            {-
            with_context
                (Context
                    (zipWith
                        (\ formal actual ->
                            case formal of
                                Var name -> VarBind name actual
                                _ -> error $ "macro formal argument must be variable reference; got " ++ show formal
                        )
                        macargs
                        argvals)
                    [])
                (render mbody)
            -}


-- | Render the parsed template into string.

phase_2 :: [Node] -> StateT Vstate IO String
phase_2 nodes_ =
    action 0 nodes_
    where
        depth_limit = 8
        action :: Integer -> [Node] -> StateT Vstate IO String
        action depth nodes =
            let
                descend = action (succ depth)
            in do
                case nodes of
                    node : tail_ -> do
                        when (depth > depth_limit) $ do
                            error $ "possible cycle: phase_2 depth reached " ++ show depth ++ " while evaluating " ++ show node
                        -- liftIO $ putStrLn $ show node
                        result <- case node of
                            Literal s -> return s
                            Fragment nodes__ -> descend nodes__
                            -- Var name -> v_get_var name >>= maybe (error $ "undefined variable: $" ++ name) (descend . (: []) . varContent)
                            -- QuietVar name -> v_get_var name >>= maybe (return "") (descend . (: []) . varContent)
                            Zacall name -> do
                                m_macbind <- v_get_macro $ nameToString name
                                case m_macbind of
                                    Nothing -> do
                                        liftIO . putStrLn $ "warning: silently converting #" ++ nameToString name ++ " to literal"
                                        return ('#' : nameToString name)
                                    Just _ -> invoke (Call (nameToString name) [])
                            call@(Call _ _) -> invoke call
                            _ -> error $ "phase_2 not implemented: " ++ show node
                        (result ++) <$> render tail_
                    _ -> return ""
