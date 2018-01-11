module Interpreter
( evaluate
, evaluateGlobal
, Context(..)
, Scope(..)
, globalScope
) where

import Data.Fixed
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

instance Eq Var where
    YlFalse == YlFalse   = True
    YlFalse == _         = False
    _       == YlFalse   = False
    YlInt n1 == YlInt n2 = n1 == n2
    YlNum n1 == YlNum n2 = n1 == n2
    YlInt n1 == YlNum n2 = fromIntegral n1 == n2
    YlNum n1 == YlInt n2 = n1 == fromIntegral n2
    YlStr s1 == YlStr s2 = s1 == s2
    YlInt n1 == YlStr s2 = show n1 == s2
    YlStr s1 == YlInt n2 = s1 == show n2
    YlNum n1 == YlStr s2 = show n1 == s2
    YlStr s1 == YlNum n2 = s1 == show n2
    YlFunc _ == _        = False
    _        == YlFunc _ = False

instance Ord Var where
    compare YlFalse YlFalse = EQ
    compare YlFalse _       = LT
    compare _ YlFalse       = GT
    compare (YlInt n1) (YlInt n2) = compare n1 n2
    compare (YlNum n1) (YlNum n2) = compare n1 n2
    compare (YlInt n1) (YlNum n2) = compare (fromIntegral n1) n2
    compare (YlNum n1) (YlInt n2) = compare n1 (fromIntegral n2)
    compare (YlStr s1) (YlStr s2) = compare s1 s2
    compare (YlInt n1) (YlStr s2) = compare (show n1) s2
    compare (YlStr s1) (YlInt n2) = compare s1 (show n2)
    compare (YlNum n1) (YlStr s2) = compare (show n1) s2
    compare (YlStr s1) (YlNum n2) = compare s1 (show n2)
    compare (YlFunc _) _          = GT
    compare (_       ) (YlFunc _) = LT

ylPlus :: Var -> Var -> Var
ylPlus YlFalse    YlFalse    = YlFalse
ylPlus YlFalse    (YlInt n)  = YlInt n
ylPlus (YlInt n)  YlFalse    = YlInt n
ylPlus YlFalse    (YlNum n)  = YlNum n
ylPlus (YlNum n)  YlFalse    = YlNum n
ylPlus (YlInt n1) (YlInt n2) = YlInt $ n1 + n2
ylPlus (YlInt n1) (YlNum n2) = YlNum $ fromIntegral n1 + n2
ylPlus (YlNum n1) (YlInt n2) = YlNum $ n1 + fromIntegral n2
ylPlus (YlNum n1) (YlNum n2) = YlNum $ n1 + n2
ylPlus var1        var2      = YlStr $ show var1 ++ show var2

ylMinus :: Var -> Var -> Var
ylMinus (YlInt n1) (YlInt n2) = YlInt $ n1 - n2
ylMinus (YlInt n1) (YlNum n2) = YlNum $ fromIntegral n1 - n2
ylMinus (YlNum n1) (YlInt n2) = YlNum $ n1 - fromIntegral n2
ylMinus (YlNum n1) (YlNum n2) = YlNum $ n1 - n2
ylMinus _           _         = error "Cannot substract non-numbers"

ylMul :: Var -> Var -> Var
ylMul (YlInt n1)  (YlInt n2) = YlInt $ n1 * n2
ylMul (YlInt n1)  (YlNum n2) = YlNum $ fromIntegral n1 * n2
ylMul (YlNum n1)  (YlInt n2) = YlNum $ n1 * fromIntegral n2
ylMul (YlNum n1)  (YlNum n2) = YlNum $ n1 * n2
ylMul _           _          = error "Cannot multiply non-numbers"

ylDiv :: Var -> Var -> Var
ylDiv (YlInt n1) (YlInt n2) = YlNum $ fromIntegral n1 / fromIntegral n2
ylDiv (YlInt n1) (YlNum n2) = YlNum $ fromIntegral n1 / n2
ylDiv (YlNum n1) (YlInt n2) = YlNum $ n1 / fromIntegral n2
ylDiv (YlNum n1) (YlNum n2) = YlNum $ n1 / n2
ylDiv _           _         = error "Cannot divide non-numbers"

ylRem :: Var -> Var -> Var
ylRem (YlInt n1) (YlInt n2) = YlInt $ n1 `mod` n2
ylRem (YlNum n1) (YlNum n2) = YlNum $ n1 `mod'` n2
ylRem (YlInt n1) (YlNum n2) = ylRem (YlNum (fromIntegral n1)) $ YlNum n2
ylRem (YlNum n1) (YlInt n2) = ylRem (YlNum n1) $ YlNum (fromIntegral n2)

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
        , ("=",     YlFunc eqOp)
        , (">",     YlFunc gtOp)
        , (">=",    YlFunc geOp)
        , ("<",     YlFunc ltOp)
        , ("<=",    YlFunc leOp)
        , ("+",     YlFunc plusOp)
        , ("-",     YlFunc minusOp)
        , ("*",     YlFunc multiplyOp)
        , ("/",     YlFunc divideOp)
        , ("%",     YlFunc moduloOp)
        , ("if",    YlFunc ifFn)
        , ("loop",  YlFunc loopFn)
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
        AstNode "if"       -> callIfFn next scope
        AstNode "loop"     -> callLoopFn next scope
        AstNode identifier -> let var = scopeGet scope identifier in
            case var of
                Nothing    -> evaluateList all scope False
                Just func  -> let (args, io, newScope) = getArgs scope next in
                    let Context {var=v, io=io', scope=newScope'} = callVar func args newScope in
                        Context {var=v, io=do {io; io'}, scope=newScope'}

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
       -> Scope     -- ^ Scope in which the function was called
       -> [Ast]     -- ^ Body of the function
       -> Context   -- ^ Result of the call
callFn args argNames scope callScope exps =
    let fnScope = makeFuncScope (zip args argNames) scope in
        let Context {var=v, io=io, scope=_} = evaluate (AstList exps) fnScope False in
            Context {var=v, io=io, scope=callScope}

makeFuncScope :: [(Var, [Char])] -> Scope -> Scope
makeFuncScope [] scope = scope
makeFuncScope ((var, name):next) scope =
    makeFuncScope next (scopeSet scope name var)

defFn :: [Var] -> Scope -> Context
defFn = dummyFn

eqOp :: [Var] -> Scope -> Context
eqOp = dummyCtx . (cmpOp (==))
gtOp = dummyCtx . (cmpOp (>))
geOp = dummyCtx . (cmpOp (>=))
ltOp = dummyCtx . (cmpOp (<))
leOp = dummyCtx . (cmpOp (<=))

cmpOp :: (Var -> Var -> Bool) -> [Var] -> Var
cmpOp op (var1:var2:_)
    | op var1 var2 = ylTrue
    | otherwise    = YlFalse
cmpOp _ _ = error "Comparison operator requires 2 arguments!"

plusOp :: [Var] -> Scope -> Context
plusOp     = dummyCtx . (binOp ylPlus)
minusOp    = dummyCtx . (binOp ylMinus)
multiplyOp = dummyCtx . (binOp ylMul)
divideOp   = dummyCtx . (binOp ylDiv)
moduloOp   = dummyCtx . (binOp ylRem)

binOp ::  (Var -> Var -> Var) -> [Var] -> Var
binOp op (var1:[]) = var1
binOp op (var1:next) = op var1 (binOp op next)
binOp _ _ = error "Binary operator requires 2 arguments!"

callIfFn :: [Ast] -> Scope -> Context
callIfFn (cond:then':[]) scope = callIfFn [cond, then', AstList []] scope
callIfFn (cond:then':else':_) scope =
    let Context {var=result, io=io, scope=scope'} = evaluate cond scope True in
        case result of
            YlFalse -> let Context {var=ifResult, io=io', scope=scope''} = evaluate else' scope' False in
                        Context {var=ifResult, io=do{io; io'}, scope=scope''}
            _       -> let Context {var=ifResult, io=io', scope=scope''} = evaluate then' scope' False in
                        Context {var=ifResult, io=do{io; io'}, scope=scope''}
callIfFn _ _ = error "'if' should be used as follows: (if cond (then...) (else...))"

ifFn :: [Var] -> Scope -> Context
ifFn = dummyFn

callLoopFn :: [Ast] -> Scope -> Context
callLoopFn (identifier:rangeExp:loopAst) scope =
    let Context {var=idVal, io=io, scope=scope'} = evaluate identifier scope True in
        let idStr = show idVal
            (range, io', scope'') = getRange rangeExp scope' in
            runLoop (AstList loopAst) idStr range (do {io; io'}) scope'' YlFalse

getRange :: Ast -> Scope -> ([Var], IO (), Scope)
getRange (AstNode string) scope =
    let Context {var=val, io=io, scope=scope'} = evaluateVal string scope in
        ([val], io, scope')
getRange (AstList ((AstNode "range"):maxi:[])) scope =
    getRange (AstList [AstNode "range", AstNode "0", maxi]) scope
getRange (AstList ((AstNode "range"):mini:maxi:_)) scope =
    let Context{var=mini', io=io, scope=scope'} = evaluate mini scope True
        Context{var=maxi', io=io', scope=scope''} = evaluate maxi scope' True in
        case (mini', maxi') of
            (YlInt mIN, YlInt mAX) -> ( map YlInt [mIN,(mIN + 1)..(mAX - 1)]
                                      , do {io; io'}
                                      , scope'' )
getRange (AstList []) scope = ([], return (), scope)
getRange (AstList (h:next)) scope =
    let Context {var=val, io=io, scope=scope'} = evaluate h scope True in
        let (vals, io', scope'') = getRange (AstList next) scope' in
            (val:vals, do {io; io'}, scope'')

runLoop :: Ast -> [Char] -> [Var] -> IO () -> Scope -> Var -> Context
runLoop loopAst idStr [] io scope ret = Context {var=ret, io=io, scope=scope}
runLoop loopAst idStr (v:next) io scope ret =
    let scope' = scopeSet scope idStr v in
        let Context {var=ret', io=io', scope=_} = evaluate loopAst scope' True in
            runLoop loopAst idStr next (do {io; io'}) scope ret'

loopFn :: [Var] -> Scope -> Context
loopFn = dummyFn

dummyFn :: [Var] -> Scope -> Context
dummyFn _ = dummyCtx YlFalse

dummyCtx :: Var -> Scope -> Context
dummyCtx var scope = Context {var=var, io=return (), scope=scope}
