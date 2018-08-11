module Common where

infix 5 <>,><

dup x = (x,x)

type Binoid a b = (a -> b -> b, b)

(<>) f g (x1,x2) (y1,y2) = (f x1 y1, g x2 y2)
(><) (o1,e1) (o2,e2) = ((o1 <> o2), (e1,e2))

addition, multiplication :: Num t => Binoid t t
addition  = ((+),0)
multiplication = ((*),1)
list = ((:),[])

lse xs = let mx = maximum xs in mx + log (sum (map (exp . subtract mx) xs))

data VTree g p = Prim p | Goal g [VTree g p] deriving Show
