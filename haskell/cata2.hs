module Cata2 where
-- Graph catamorphism, like Cata1, but now using Orders for modular building
-- of lattice for Viterbi algorithms, and a product type Binoid a b to represent
-- a function a -> b -> b and an initial element of type b.

import Control.Arrow
import Data.Maybe
import Orders (bottom_lattice', floats_lattice, floats, payload_floats_lattice, payload_max_lb_lattice)
import Common
import Graph

data A g p a b c t w = GSR (Binoid a b)     -- times, one
                           (Binoid b c)     -- plus, zero
                           ((t,p) -> a)     -- inj
                           ((g,c) -> (a,w)) -- proj

cata :: A g p a b c t w -> (p -> t) -> Graph g p -> g -> w
cata (GSR (times,one) (plus,zero) inj proj) theta expls = snd . phi
  where sum  = foldr plus zero
        prod = foldr times one
        phi  = proj . (id &&& sum . map (prod . map xi) . expls)
        xi   = fst . phi ||| inj . (theta &&& id)

inside :: Num t => A g p t t t t t
inside = GSR multiplication addition fst (dup . snd)
inside_log = GSR addition list (log . fst) (dup . lse . snd)

identity = GSR list list (prim . snd) (first subg)

-- Viterbi -----------------------------
viterbi multiplication lattice inj proj = 
  GSR multiplication lattice (inj . fst) (dup . proj . snd)

viterbi_tree multiplication lattice inj proj =
  GSR (multiplication >< list) lattice (inj *** Prim) proj_vit
  where proj_vit (g, s) = ((second (Goal g) &&& id) . proj) s

viterbi_lin_any :: (Num t, Ord t) => A g p t t (Maybe t) t t
viterbi_lin_any = viterbi multiplication (bottom_lattice' max) id fromJust

viterbi_log_float :: (Floating t, Ord t, Read t) => A g p t t t t t
viterbi_log_float = viterbi addition floats_lattice log id

viterbi_tree_lin_any :: (Num t, Ord t) => A g p (t, VTree g p) (t, [VTree g p]) (Maybe (t, [VTree g p])) t (t, [VTree g p])
viterbi_tree_lin_any  = viterbi_tree multiplication payload_max_lb_lattice id fromJust

viterbi_tree_lin_float = viterbi_tree multiplication payload_floats_lattice id  id
viterbi_tree_log_float = viterbi_tree addition       payload_floats_lattice log id
