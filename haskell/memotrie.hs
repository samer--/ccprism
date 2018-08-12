{-# LANGUAGE TypeOperators, TypeFamilies #-}
module MemoTrie (HasTrie, memo) where
-- See http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries

infix 4 :>
infix 6 !!!

class HasTrie a where
  data (:>) a :: * -> *
  trie   :: (a -> b) -> (a :> b)
  untrie :: (a :> b) -> (a -> b)

memo :: HasTrie a => (a -> b) -> (a -> b)
memo = untrie . trie

instance HasTrie () where
  data () :> a = UnitTrie a
  trie f = UnitTrie (f ())
  untrie (UnitTrie a) = const a

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data (Either a b) :> x = EitherTrie (a :> x) (b :> x)
  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
  untrie (EitherTrie s t) = either (untrie s) (untrie t)

instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
  data (a,b) :> x = PairTrie (a :> (b :> x))
  trie f = PairTrie (trie (trie . curry f))
  untrie (PairTrie t) = uncurry (untrie .  untrie t)

instance HasTrie a => HasTrie [a] where
  data [a] :> x = ListTrie x (a :> ([a] :> x))
  trie f = ListTrie (f []) (trie (trie . (\h t -> f (h:t))))
  untrie (ListTrie s _) [] = s
  untrie (ListTrie _ t) (x:xs) = untrie (untrie t x) xs

instance HasTrie Int where
  data Int :> x = IntTrie (NaturalTree x)
  trie f = IntTrie (fmap f $ naturals 1 0) 
  untrie (IntTrie  l) = (l !!!)

data NaturalTree a = Node a (NaturalTree a) (NaturalTree a)

instance Functor NaturalTree where
   fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)

Node a tl tr !!! 0 = a 
Node a tl tr !!! n =
   if odd n
     then tl !!! top
     else tr !!! (top-1)
        where top = n `div` 2

naturals r n =
   Node n
     ((naturals $! r2) $! (n+r))
     ((naturals $! r2) $! (n+r2))
        where r2 = 2*r
