-- @File test_Term.hs
-- @Author Eloi PERDEREAU
-- @Date 10-10-2013

import Term
import ArbitraryTerm
import Data.Maybe

testApplicationId :: (Eq v, Eq f) => Term v f -> Bool
testApplicationId t = t *! identity == t

testComposId
  :: (Eq v, Eq f) => TSubstitution v f -> Bool
testComposId (TSubstitution s) = s @@ identity == s &&
                                 s == identity @@ s

testComposAssoc
  :: (Eq v, Eq f) =>
     TSubstitution v f -> TSubstitution v f -> TSubstitution v f -> Bool
testComposAssoc (TSubstitution rho) (TSubstitution sigma) (TSubstitution tau) =
                (rho @@  sigma) @@ tau == rho @@ (sigma  @@ tau)

testComposDef
  :: (Eq v, Eq f) =>
     (Term v f) -> (TSubstitution v f) -> (TSubstitution v f) -> Bool
testComposDef t (TSubstitution sigma) (TSubstitution tau) =
                (t *! sigma) *! tau == t *! (tau @@ sigma)

testUnif :: (Eq a, Eq b) => UnifProblem a b -> Bool
testUnif ps = isNothing maybeU ||
              null (filter (\(var,term) -> var *! u /= term *! u) ps)
                where
                  maybeU = unif ps
                  u = fromJust maybeU

