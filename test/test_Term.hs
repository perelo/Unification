-- @File test_Term.hs
-- @Author Eloi PERDEREAU
-- @Date 10-10-2013

import Term
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Data.Maybe

instance (Arbitrary v, Arbitrary f) => Arbitrary (Term v f) where
    arbitrary = arbitraryTerm

arbitraryTerm :: (Arbitrary v, Arbitrary f) => Gen (Term v f)
arbitraryTerm = oneof [arbitraryTermVar, arbitraryTermFunc]

arbitraryTermVar :: Arbitrary v => Gen (Term v f)
arbitraryTermVar = do
                   x <- arbitrary
                   return $ varToTerm x

arbitraryTermFunc :: (Arbitrary v, Arbitrary f) => Gen (Term v f)
arbitraryTermFunc = do
                    x <- arbitrary
                    xs <- arbitraryTerms (elements [0..2])
                    return $ constructTerm x xs

arbitraryTerms :: (Arbitrary v, Arbitrary f) => Gen Int -> Gen [Term v f]
arbitraryTerms gn = gn >>= (\x -> arbitraryTerms' x)
    where
        arbitraryTerms' :: (Arbitrary v, Arbitrary f) => Int -> Gen [Term v f]
        arbitraryTerms' 0 = return []
        arbitraryTerms' n = do
                            x <- arbitrary
                            xs <- arbitraryTerms' (n-1)
                            return (x:xs)

-- TSubstitution just encapsulates Substitution in a new type so we can
-- define how they are arbitrary generated
data TSubstitution v f = TSubstitution (Substitution v f) deriving (Show)

instance (Arbitrary v, Arbitrary f, Eq v) => Arbitrary (TSubstitution v f) where
    arbitrary = do
                s <- arbitrary
                return $ TSubstitution (cleanSubstitution s)

-- cleanSubstitution removes from the substitition couples like (x, TermVar x)
-- it also removes fst duplicates
--      ie remove the second elem of [(x, TermVar y), (x, TermVar z)]
cleanSubstitution :: Eq v => Substitution v f -> Substitution v f
cleanSubstitution ss = let s0 = filter (not.(uncurry compareVar)) ss
                        in map (\(v,t) -> (v, fromJust (lookup v s0))) s0

-- printArbitrary :: Gen (Term Char Char) -> IO()
-- printArbitrary :: Gen (TSubstitution Char Char) -> IO()
-- printArbitrary a = do
--                     s <- newStdGen
--                     print $ unGen a s 100

testApplicationId :: (Eq v, Eq f) => Term v f -> Bool
testApplicationId t = t *! identity == t

testComposId
  :: (Eq v, Eq f) => TSubstitution v f -> Bool
testComposId (TSubstitution s) = s @@ identity == s &&
                                 s == identity @@ s

testComposAssoc
  :: (Ord v, Ord f) =>
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

