module  Orders (bottom_lattice', maxby_chain, floats_lattice, floats, payload_chain, maxby_lb_chain, payload_lb_chain,
                payload_floats_lattice, payload_max_lb_lattice) where

import Control.Arrow (first, (>>>))
import Data.Maybe (fromMaybe)

-- These things form a category, with morphisms below. Some commute, some don't.
type Chain a      = a -> a -> Bool
type Chain' a b   = a -> b -> Maybe b
type LBChain a    = (Chain a, a)
type LBChain' a b = (Chain' a b, b)
type Lattice a    = a -> a -> a
type Lattice' a b = a -> b -> b
type LBLattice a     = (Lattice a, a)
type LBLattice' a b  = (Lattice' a b, b)

floats :: (Read t, Ord t, Floating t) => LBChain t
floats = ((>), -read "Infinity")

floats_lattice :: (Read t, Ord t, Floating t) => LBLattice t
floats_lattice = (max, -read "Infinity")

-- prime_chain (>) x y = if x > y then Just x else Nothing
prime_lattice join = join 
-- prime_lb_chain = first prime_chain
prime_lb_lattice = first prime_lattice

-- bottom_chain (>) = (greater, Nothing) where
--   greater (Just x) (Just y) = x > y
--   greater (Just x) Nothing  = True
--   greater Nothing _ = False

-- bottom_chain' (|>) = (bounce, Nothing) where
--   bounce x (Just y) = fmap Just (x |> y)
--   bounce x Nothing  = Just (Just x)

-- -- bottom_lattice join = (join_maybe, Nothing) where
-- --   join_maybe (Just x) (Just y) = Just (join x y)
-- --   join_maybe x Nothing  = x
-- --   join_maybe Nothing x  = x

-- matching proj = fromJust
bottom_lattice' :: Lattice' a a -> LBLattice' a (Maybe a)
bottom_lattice' join' = (join_maybe', Nothing) where
  join_maybe' x (Just y) = Just (join' x y)
  join_maybe' x Nothing  = Just x

maxby_chain (>) x y = if x > y then x else y
maxby_lb_chain = first maxby_chain
-- maxby_chain' (|>) x y = fromMaybe y (x |> y)
-- maxby_lb_chain' = first maxby_chain'

payload_chain :: Chain a -> Chain (a,w)
payload_chain (>) (x,_) (y,_) = x > y

payload_lb_chain :: LBChain a -> LBChain (a,w)
payload_lb_chain ((>), bottom)  = (payload_chain (>), (bottom, undefined))
-- payload_chain' (|>) (x,px) (y,_) = fmap (,px) (x |> y)
-- payload_lb_chain' ((|>), bottom)  = (payload_chain' (|>), (bottom, undefined))

payload_floats_lattice = maxby_lb_chain (payload_lb_chain floats)

payload_max_lb_lattice :: (Ord t) => LBLattice' (t,w) (Maybe (t,w))
payload_max_lb_lattice = bottom_lattice' (maxby_chain (payload_chain (>)))

-- These are all ways of getting from a Chain to an LBLattice'
-- -- p1 = payload_chain   >>> maxby_chain     >>> bottom_lattice >>> prime_lb_lattice
p2 :: Chain a -> LBLattice' (a,w) (Maybe (a,w))
p2 = payload_chain >>> maxby_chain    >>> prime_lattice >>> bottom_lattice'
-- p3 = payload_chain >>> prime_chain    >>> bottom_chain' >>> maxby_lb_chain'
-- p4 = payload_chain >>> prime_chain    >>> maxby_chain'  >>> bottom_lattice'
-- -- p5 = payload_chain >>> bottom_chain     >>> maxby_lb_chain    >>> prime_lb_lattice
-- -- p6 = payload_chain >>> bottom_chain     >>> prime_lb_chain    >>> maxby_lb_chain'
-- -- p7 = bottom_chain  >>> payload_lb_chain >>> prime_lb_chain    >>> maxby_lb_chain'
-- -- p8 = bottom_chain  >>> payload_lb_chain >>> maxby_lb_chain    >>> prime_lb_lattice
-- -- p9 = bottom_chain  >>> prime_lb_chain   >>> payload_lb_chain' >>> maxby_lb_chain'
-- pA = prime_chain   >>> payload_chain' >>> maxby_chain'  >>> bottom_lattice'
-- pB = prime_chain   >>> payload_chain' >>> bottom_chain' >>> maxby_lb_chain'
-- -- pC = prime_chain     >>> bottom_chain' >>> payload_lb_chain' >>> maxby_lb_chain'

q1 :: LBChain a -> LBLattice' (a,w) (a,w)
q1 = payload_lb_chain >>> maxby_lb_chain    >>> prime_lb_lattice
-- q2 = payload_lb_chain >>> prime_lb_chain    >>> maxby_lb_chain'
-- q3 = prime_lb_chain   >>> payload_lb_chain' >>> maxby_lb_chain'
