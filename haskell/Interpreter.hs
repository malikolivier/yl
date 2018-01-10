module Interpreter
( evaluate
, evaluateGlobal
, Context(..)
) where

import Parser


data Var = YlFalse
         | YlNum Double
         | YlStr [Char]
         | YlFunc ([Var] -> Scope -> Context)

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
-- TODO
evaluate ast scope evaluateFunction = Context { var=YlFalse, io=return (), scope=scope }
