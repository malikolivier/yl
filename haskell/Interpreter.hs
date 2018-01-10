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
         | YlInt Integer
         | YlNum Double
         | YlStr [Char]
         | YlFunc ([Var] -> Scope -> Context)
ylTrue = YlInt 1

instance Show Var where
    show YlFalse = "()"
    show (YlInt n) = show n
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
   vars=[ ("print", YlFunc printFn)
        , ("!",     YlFunc notOp)
        , ("let",   YlFunc letFn)
        , ("def",   YlFunc defFn)
        ]
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

scopeSet :: Scope -> [Char] -> Var -> Scope
scopeSet Scope {parent=p, vars=vars} identifier val =
    Scope {parent=p, vars=(identifier, val):vars}

parseToVar :: [Char] -> Var
parseToVar string =
    case (readMaybe string :: Maybe Integer) of
        Just int -> YlInt int
        Nothing  -> case (readMaybe string :: Maybe Double) of
            Just num -> YlNum num
            Nothing  -> YlStr string

evaluateList :: [Ast] -> Scope -> Bool -> Context
evaluateList [] scope _ = Context { var=YlFalse, io=return (), scope=scope }
evaluateList (h:[]) scope False = evaluate h scope True
evaluateList (h:next) scope False =
    let Context {var=_, io=io, scope=s} = evaluate h scope True in
        let Context {var=v, io=nextIo, scope=nextScope} = evaluateList next s False in
            Context {var=v, io=do {io; nextIo}, scope=nextScope}
evaluateList all@(h:next) scope True =
    case h of
        AstList list       -> evaluateList all scope False
        AstNode "def"      -> callDefFn next scope
        AstNode identifier -> let var = scopeGet scope identifier in
            case var of
                Nothing    -> evaluateList all scope False
                Just func  -> let (args, _, newScope) = getArgs scope next in
                    callVar func args newScope

getArgs :: Scope -> [Ast] -> ([Var], IO (), Scope)
getArgs scope [] = ([], return (), scope)
getArgs scope (a:next) =
    let Context{var=var, io=io, scope=newScope} = evaluate a scope True in
        let (vars, nextIo, newNewScope) = getArgs newScope next in
            (var:vars, do {io; nextIo}, newNewScope)

callVar :: Var -> [Var] -> Scope -> Context
callVar (YlFunc func) args scope = func args scope
callVar _ _ _ = error "Cannot call uncallable object"


-- Built-in functions
printFn :: [Var] -> Scope -> Context
printFn [] scope = Context {var=YlFalse, io=return (), scope=scope}
printFn (var:next) scope =
    let Context {var=v, io=io, scope=s} = printFn next scope in
        Context {var=v, io=do { print var; io }, scope=s}

notOp :: [Var] -> Scope -> Context
notOp [] scope = Context {var=ylTrue, io=return (), scope=scope}
notOp (YlFalse:_) scope = Context {var=ylTrue, io=return (), scope=scope}
notOp _ scope = Context {var=YlFalse, io=return (), scope=scope}

letFn :: [Var] -> Scope -> Context
letFn (identifier:[]) scope = letFn [identifier, YlFalse] scope
letFn (identifier:val:next) scope =
    let identifierStr = show identifier in
        Context {var=val, io=return (), scope=scopeSet scope identifierStr val}
letFn _ _ = error "'let' requires at least 1 argument!"

callDefFn :: [Ast] -> Scope -> Context
callDefFn (identifier:argNames:next) scope =
    let Context {var=idVal, io=io, scope=newScope} = evaluate identifier scope True in
        let idStr = show idVal
            (argNamesStr, nextIo, newScope') = getArgNames argNames newScope in
            let func           = YlFunc (\args scope' -> callFn args argNamesStr funcOuterScope scope' next)
                funcOuterScope = scopeSet newScope' idStr func in
                    Context {var=func, io=do {io; nextIo}, scope=funcOuterScope}
callDefFn _ _ = error "'def' should be used like this: '(def name (args...) do something...)'!"

getArgNames :: Ast -> Scope -> ([[Char]], IO (), Scope)
getArgNames (AstNode string) scope =
    let Context {var=v, io=io, scope=scope'} = evaluateVal string scope in
        ([show v], io, scope')
getArgNames (AstList []) scope = ([], return (), scope)
getArgNames (AstList (h:next)) scope =
    let Context {var=v, io=io, scope=scope'} = evaluate h scope True in
        let (nextArgNames, nextIo, nextScope) = getArgNames (AstList next) scope' in
            ((show v):nextArgNames, do {io; nextIo}, nextScope)

callFn :: [Var]     -- ^ Variable given as argument
       -> [[Char]]  -- ^ List of argument names for the function
       -> Scope     -- ^ Scope where the function was defined
       -> Scope     -- ^ Scope in which the function was called (unused)
       -> [Ast]     -- ^ Body of the function
       -> Context   -- ^ Result of the call
callFn args argNames scope _ exps = dummyFn args scope -- TODO


defFn :: [Var] -> Scope -> Context
defFn = dummyFn

dummyFn :: [Var] -> Scope -> Context
dummyFn _ scope = Context {var=YlFalse, io=return (), scope=scope}
