module Interpreter
( evaluate
, evaluateGlobal
, Context(..)
, Scope(..)
, globalScope
) where

import Text.Read

import Parser


data Var = YlFalse
         | YlNum Double
         | YlStr [Char]
         | YlFunc ([Var] -> Scope -> Context)

instance Show Var where
    show YlFalse = "()"
    show (YlNum n) = show n
    show (YlStr str) = str
    show (YlFunc func) = "(def function (args...) ...)"

data Context = Context { var :: Var
                       , io :: IO ()
                       , scope :: Scope
                       }

data ParentScope = ScopeHasNoParent | ScopeHasParent Scope

data Scope = Scope { parent :: ParentScope
                   , vars :: [([Char], Var)]
                   }

evaluateGlobal :: Ast -> Context
evaluateGlobal ast = evaluate ast globalScope True

globalScope = Scope {
   parent=ScopeHasNoParent,
   vars=[]
}

evaluate :: Ast -> Scope -> Bool -> Context
evaluate (AstNode string) scope _ = evaluateVal string scope
evaluate (AstList list) scope evaluateFunction = evaluateList list scope evaluateFunction

evaluateVal :: [Char] -> Scope -> Context
evaluateVal string scope = let val = scopeGet scope string in
    case val of
        Nothing  -> Context { var=parseToVar string, io=return (), scope=scope }
        Just var -> Context { var=var, io=return (), scope=scope }

scopeGet :: Scope -> [Char] -> Maybe Var
scopeGet Scope {parent=p, vars=vars} identifier =
    let var = lookup identifier vars in
        case var of
            Just v -> Just v
            _      -> case p of
                        ScopeHasNoParent      -> Nothing
                        ScopeHasParent parent -> scopeGet parent identifier

parseToVar :: [Char] -> Var
parseToVar string = let result = readMaybe string :: Maybe Double in
    case result of
        Just num -> YlNum num
        _        -> YlStr string

evaluateList :: [Ast] -> Scope -> Bool -> Context
evaluateList [] scope _ = Context { var=YlFalse, io=return (), scope=scope }
evaluateList (h:[]) scope False = evaluate h scope True
evaluateList (h:next) scope False =
    let Context {var=_, io=_, scope=s} = evaluate h scope True in
        evaluateList next s False
evaluateList all@(h:next) scope True =
    case h of
        AstList list       -> evaluateList all scope False
        AstNode identifier -> let var = scopeGet scope identifier in
            case var of
                Nothing    -> evaluateList all scope False
                Just func  -> let (args, _, newScope) = getArgs scope next in
                    callVar func args newScope

getArgs :: Scope -> [Ast] -> ([Var], IO (), Scope)
getArgs scope [] = ([], return (), scope)
getArgs scope (a:next) =
    let Context{var=var, io=_, scope=newScope} = evaluate a scope True in
        let (vars, _, newNewScope) = getArgs newScope next in
            (var:vars, return (), newNewScope)

callVar :: Var -> [Var] -> Scope -> Context
callVar (YlFunc func) args scope = func args scope
callVar _ _ _ = error "Cannot call uncallable object"
