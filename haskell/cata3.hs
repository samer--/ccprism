module Cata3 where
-- Like Cata2, but absorbing parameters into inj functions instead
-- of having a separate theta function to get parameters for a 
-- primitive. This makes the types overall simpler, and means that
-- algebras that don't need parameters (like identity) don't need a 
-- dummy theta function. Algebras that do need numerical probabilities 
-- for primitives (eg inside) must take the theta function as a parameter, 
-- but then turn out to be slightly simpler.
--
-- This also includes a memoised version mcata, which uses the pure
-- lazy memoisation tools provided in the MemoTrie. This converts
-- exponential time computations into sensible time!

import Control.Arrow
import Data.Maybe
import MemoTrie
import Orders (bottom_lattice', floats_lattice, floats, payload_floats_lattice, payload_max_lb_lattice)
import Common
import Graph

data A g p a b c w = GSR (Binoid a b)     -- times, one
                         (Binoid b c)     -- plus, zero
                         (p -> a)         -- inj
                         ((g,c) -> (a,w)) -- proj

cata :: A g p a b c w -> Graph g p -> g -> w
cata (GSR (times,one) (plus,zero) inj proj) expls = snd . phi
  where sum  = foldr plus zero
        prod = foldr times one
        phi  = proj . (id &&& sum . map (prod . map xi) . expls)
        xi   = fst . phi ||| inj

mcata :: HasTrie g => A g p a b c w -> Graph g p -> g -> w
mcata (GSR (times,one) (plus,zero) inj proj) expls = snd . phi
  where sum  = foldr plus zero
        prod = foldr times one
        phi  = memo $ proj . (id &&& sum . map (prod . map xi) . expls)
        xi   = fst . phi ||| inj

inside :: Num t => (p -> t) -> A g p t t t t
inside theta = GSR multiplication addition theta (dup . snd)
inside_log theta = GSR addition list (log . theta) (dup . lse . snd)

identity = GSR list list prim (first subg)

-- Viterbi -----------------------------
viterbi multiplication lattice inj proj theta =
  GSR multiplication lattice (inj . theta) (dup . proj . snd)

viterbi_tree multiplication lattice inj proj theta =
  GSR (multiplication >< list) lattice (inj . theta &&& Prim) proj_vit
  where proj_vit (g, s) = ((second (Goal g) &&& id) . proj) s

viterbi_lin_any :: (Num t, Ord t) => (p -> t) -> A g p t t (Maybe t) t
viterbi_lin_any = viterbi multiplication (bottom_lattice' max) id fromJust

viterbi_log_float :: (Floating t, Ord t, Read t) => (p -> t) -> A g p t t t t
viterbi_log_float = viterbi addition floats_lattice log id

viterbi_tree_lin_any :: (Num t, Ord t) => (p -> t) -> A g p (t, VTree g p) (t, [VTree g p]) (Maybe (t, [VTree g p])) (t, [VTree g p])
viterbi_tree_lin_any  = viterbi_tree multiplication payload_max_lb_lattice id fromJust

viterbi_tree_lin_float = viterbi_tree multiplication payload_floats_lattice id  id
viterbi_tree_log_float = viterbi_tree addition       payload_floats_lattice log id
