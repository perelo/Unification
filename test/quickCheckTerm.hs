-- @File test_Term.hs
-- @Author Eloi PERDEREAU
-- @Date 10-10-2013

module Main (main) where

import Term
import ArbitraryTerm

import Data.Maybe
import Test.QuickCheck

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

testUnifIdempotence :: (Eq a, Eq b) => UnifProblem a b -> Bool
testUnifIdempotence ps = isNothing maybeU ||
                     u @@ u == u
                       where
                         maybeU = unify ps
                         u = fromJust maybeU

testUnif :: (Eq a, Eq b) => UnifProblem a b -> Bool
testUnif ps = isNothing maybeU ||
              null (filter (\(var,term) -> var *! u /= term *! u) ps)
                where
                  maybeU = unify ps
                  u = fromJust maybeU

testFailUnif :: (Eq a, Eq b) => UnifProblem a b -> Bool
testFailUnif ps = not (hasNoUnifier ps) || isNothing maybeU
                    where
                      maybeU = unify ps
                      hasNoUnifier ps = or (map (\x -> isNothing (unify [x])) ps)


main = do
        putStrLn $ "Testing (term *! identity == identity)"
        quickCheck (testApplicationId   :: Term Char Char -> Bool)

        putStrLn $ "Testing (sigma @@ identity == sigma == identity @@ sigma)"
        quickCheck (testComposId        :: TSubstitution Char Char -> Bool)

        putStrLn $ "Testing ((rho @@ sigma) @@ tau == rho @@ (sigma @@ tau))"
        quickCheck (testComposAssoc     :: TSubstitution Char Char ->
                                           TSubstitution Char Char ->
                                           TSubstitution Char Char -> Bool)

        putStrLn $ "Testing ((t *! sigma) *! tau == t *! (sigma @@ tau))"
        quickCheck (testComposDef       :: Term Char Char ->
                                           TSubstitution Char Char ->
                                           TSubstitution Char Char -> Bool)

        putStrLn $ "Testing (sigma @@ sigma == sigma) for sigma an unifier"
        quickCheck (testUnifIdempotence :: UnifProblem Char Char -> Bool)

        putStr   $ "Testing (si *! sigma == ti *! sigma) "
        putStrLn $ "for sigma = unify ((s1,t1),...,(sn,tn)) and 1 <= i <= n"
        quickCheck (testUnif            :: UnifProblem Char Char -> Bool)

        putStr   $ "Testing unify ((s1,t1),...,(sn,tn)) = ø "
        putStrLn $ "for an i such as unif ((si,ti)) = ø"
        quickCheck (testFailUnif        :: UnifProblem Char Char -> Bool)
