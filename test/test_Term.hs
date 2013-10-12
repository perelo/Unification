-- @File test_Term.hs
-- @Author Eloi PERDEREAU
-- @Date 10-10-2013

import Term
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

instance (Arbitrary v, Arbitrary f) => Arbitrary (Term v f) where
    arbitrary = oneof [arbitraryTermVar, arbitraryTermFunc]

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
