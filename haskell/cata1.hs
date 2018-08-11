module Cata1

import Control.Arrow
import Data.Maybe
import Graph (Graph, subg, prim)
import Common

max' x y = if x > y then (x, True) else (y, False)

mmax' x Nothing = (Just x, True)
mmax' x (Just y) = if x > y then (Just x, True) else (Just y, False)

mmax x y = fst (mmax' x y)
inf = read "Infinity" :: Float

max_by_fst max (x,e1) (y,e2) = let (z,i) = max x y in (z, if i then e1 else e2)

data A g p a b c t w = GSR (a -> b -> b) b  -- times, one
                           (b -> c -> c) c  -- plus, zero
                           ((t,p) -> a)     -- inj
                           ((g,c) -> (a,w)) -- proj

cata :: A g p a b c t w -> (p -> t) -> Graph g p -> g -> w
cata (GSR times one plus zero inj proj) theta expls = snd . phi
  where sum  = foldr plus zero
        prod = foldr times one
        phi  = proj . (id &&& sum . map (prod . map xi) . expls)
        xi   = fst . phi ||| inj . (theta &&& id)

inside :: Num t => A g p t t t t t
inside = GSR (*) 1 (+) 0 fst (dup . snd)

identity = GSR (:) [] (:) [] (prim . snd) (first subg)

viterbi :: (Num t, Ord t) => A g p t t (Maybe t) t t
viterbi  = GSR (*) 1 mmax Nothing fst (dup . fromJust . snd)
viterbi_log = GSR (+) 0.0 max (-inf) (log . fst) (dup . snd)

viterbi_tree'' times one max bottom inj proj =
  GSR (times <> (:)) (one,[]) (max_by_fst max) (bottom, undefined) (inj *** Prim) proj_vit''
  where proj_vit'' (g,s) = ((second (Goal g) &&& id) . first proj) s

viterbi_tree_lin_any :: (Num t, Ord t) => A g p (t, VTree g p) (t, [VTree g p]) (Maybe t, [VTree g p]) t (t, [VTree g p])
viterbi_tree_lin_any   = viterbi_tree'' (*) 1 mmax' Nothing id fromJust
viterbi_tree_lin_float = viterbi_tree'' (*) 1 max' (-inf) id id
viterbi_tree_log_float = viterbi_tree'' (+) 0.0 max' (-inf) log id
