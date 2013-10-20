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

args1 = stdArgs { maxSuccess = 2000, maxSize = 100 }
args2 = stdArgs { maxSuccess = 200,  maxSize = 100 }

main = do
        putStrLn $ "Testing (term *! identity == identity)"
        quickCheckWith args1 (testApplicationId   :: Term Var FuncSymb -> Bool)

        putStrLn $ "Testing (sigma @@ identity == sigma == identity @@ sigma)"
        quickCheckWith args2 (testComposId        :: TSubstitution Var FuncSymb -> Bool)

        putStrLn $ "Testing ((rho @@ sigma) @@ tau == rho @@ (sigma @@ tau))"
        quickCheckWith args2 (testComposAssoc     :: TSubstitution Var FuncSymb ->
                                           TSubstitution Var FuncSymb ->
                                           TSubstitution Var FuncSymb -> Bool)

        putStrLn $ "Testing ((t *! sigma) *! tau == t *! (sigma @@ tau))"
        quickCheckWith args1 (testComposDef       :: Term Var FuncSymb ->
                                           TSubstitution Var FuncSymb ->
                                           TSubstitution Var FuncSymb -> Bool)

        putStrLn $ "Testing (sigma @@ sigma == sigma) for sigma an unifier"
        quickCheckWith args1 (testUnifIdempotence :: UnifProblem Var FuncSymb -> Bool)

        putStr   $ "Testing (si *! sigma == ti *! sigma) "
        putStrLn $ "for sigma = unify ((s1,t1),...,(sn,tn)) and 1 <= i <= n"
        quickCheckWith args1 (testUnif            :: UnifProblem Var FuncSymb -> Bool)

        putStr   $ "Testing unify ((s1,t1),...,(sn,tn)) = ø "
        putStrLn $ "for an i such as unif ((si,ti)) = ø"
        quickCheckWith args1 (testFailUnif        :: UnifProblem Var FuncSymb -> Bool)

