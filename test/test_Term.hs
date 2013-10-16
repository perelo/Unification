-- @File test_Term.hs
-- @Author Eloi PERDEREAU
-- @Date 10-10-2013

import Term
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

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
data TSubstitution v f = TSubstitution (Substitution v f) deriving (Eq, Show)

instance (Arbitrary v, Arbitrary f, Eq v) => Arbitrary (TSubstitution v f) where
    arbitrary = do
                s <- arbitrary
                return $ TSubstitution (cleanSubstitution s)

-- cleanSubstitution removes from the substitition couples like (x, TermVar )
-- TODO it must also removes fst duplicates
--      ie remove one of theses [(x, TermVar y), (x, TermVar z)]
cleanSubstitution :: Eq v => Substitution v f -> Substitution v f
cleanSubstitution ss = filter (not.(uncurry compareVar)) ss
