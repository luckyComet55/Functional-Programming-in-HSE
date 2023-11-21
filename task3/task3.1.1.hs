type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

arity :: Type -> Int
arity (TVar _) = 0
arity (_ :-> next) = 1 + arity next

order :: Type -> Int
order (TVar _) = 0
order (a :-> b) = max (order a + 1) (order b)