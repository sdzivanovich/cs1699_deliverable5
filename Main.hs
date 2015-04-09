module Main where 

import qualified MyList as My
import Test.QuickCheck
import Test.QuickCheck.All

data NamedProp a = NamedProp String a 

-- Helper function to output a labeled property
labeledCheck :: Testable prop => NamedProp prop -> IO ()
labeledCheck (NamedProp s p) = do
    putStrLn s
    quickCheckWith stdArgs { maxSuccess = 200 } p 

-- Haskell doesn't have factorial built in :(
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n 
    | n < 0     = error "n must be nonnegative"
    | otherwise = n * factorial (n - 1)

---- (++) properties ----

-- Length of a concatenation is the sum of the length of both pieces.
prop_ConcatLength :: [Int] -> [Int] -> Bool 
prop_ConcatLength xs ys = My.length xs + My.length ys == My.length (xs My.++ ys)

-- Empty list concatenated with another list is the other list.
prop_ConcatRhsEmpty :: [Int] -> Bool
prop_ConcatRhsEmpty ys = ([] My.++ ys) == ys

-- A list concatenated with the empty list is itself.
prop_ConcatLhsEmpty :: [Int] -> Bool 
prop_ConcatLhsEmpty xs = (xs My.++ []) == xs

-- First list follows the second in a concatenation.
prop_ConcatStructure :: [Int] -> [Int] -> Bool 
prop_ConcatStructure xs ys =    let n       = My.length xs 
                                    (a, b)  = My.splitAt n (xs My.++ ys)
                                in  a == xs && b == ys

---- head properties ----

-- head of a singleton list is the single element.
prop_HeadSingleElement :: Int -> Bool
prop_HeadSingleElement x = My.head [x] == x 

-- head of a list is the first element.
prop_HeadIsFirstElement :: Int -> [Int] -> Bool 
prop_HeadIsFirstElement x xs = head (x:xs) == x

---- tail properties ----

-- tail of a singleton list is the single element.
prop_TailSingleElement :: Int -> Bool 
prop_TailSingleElement x = My.tail [x] == []

-- length of the tail of a list is length of the list minus 1.
prop_TailMultipleElements :: [Int] -> Property 
prop_TailMultipleElements xs = not (My.null xs) ==> My.length (My.tail xs) == My.length xs - 1

-- tail of a list is everything except for the first element.
prop_TailRemovesFirstElement :: Int -> [Int] -> Bool 
prop_TailRemovesFirstElement x xs = My.tail (x:xs) == xs 

---- init properties ----

-- init of a singleton list is the empty list.
prop_InitSingleElement :: Int -> Bool 
prop_InitSingleElement x = My.init [x] == []

-- init of a list is everything except for the last element.
prop_InitIsFirstElements :: [Int] -> Int -> Bool 
prop_InitIsFirstElements xs x = My.init (xs My.++ [x]) == xs 

-- length of the init of a list is equal to the length of the list minus 1.
prop_LengthInitList :: [Int] -> Property 
prop_LengthInitList xs = not (My.null xs) ==> My.length (My.init xs) == My.length xs - 1

---- uncons properties ----

-- uncons of a an element prepended to a list is a pair containing
-- that element, and the list.
prop_UnconsStructure :: Int -> [Int] -> Bool 
prop_UnconsStructure x xs = My.uncons (x:xs) == Just (x, xs)

-- no need for multple generated tests here.
prop_UnconsEmptyList :: Bool
prop_UnconsEmptyList = My.uncons ([] :: [Int]) == Nothing

---- null properties ----

-- if a list has length > 0, it is not null.
prop_NonEmptyListsNotNull :: [Int] -> Property 
prop_NonEmptyListsNotNull xs = My.length xs > 0 ==> not . My.null $ xs

-- if a list is not null, it has a length greater than 0.
prop_NonNullListsNotEmpty :: [Int] -> Property
prop_NonNullListsNotEmpty xs = not (My.null xs) ==> My.length xs > 0

---- length properties ----

-- length of the empty list is 0.
prop_LengthEmptyList :: Bool
prop_LengthEmptyList = My.length [] == 0

-- length of the singleton list is 1.
prop_LengthSingletonList :: Int -> Bool
prop_LengthSingletonList x = My.length [x] == 1

-- length of a list formed by prepending an element to another list.
-- is the length of the original list + 1. 
prop_AddingElementIncreasesLength :: Int -> [Int] -> Bool 
prop_AddingElementIncreasesLength x xs = My.length (x:xs) == My.length xs + 1

---- map properties ----

-- length of the list produced by a map is equal to length of the original list.
prop_mappedListLength :: [Int] -> Bool 
prop_mappedListLength xs = (My.length . My.map show $ xs) == My.length xs

-- map over a singleton list is the same as a list created from applying the
-- function to the single element.
prop_mapSingletonList :: Int -> Bool 
prop_mapSingletonList x = My.map show [x] == [show x]

---- reverse properties ----

-- reverse of a reverse of a list is the original list.
prop_reverseReverseIsOriginal :: [Int] -> Bool 
prop_reverseReverseIsOriginal xs = My.reverse (My.reverse xs) == xs

-- length of a reverse of a list is equal to the length of the original list.
prop_reverseLength :: [Int] -> Bool 
prop_reverseLength xs = My.length (My.reverse xs) == My.length xs

-- elements appear in reverse order for a list.
prop_reverseIsReverse :: [Int] -> Bool 
prop_reverseIsReverse xs =  let revXs   = My.reverse xs 
                                len     = My.length xs 
                            in  and . My.map (\i -> xs My.!! i == revXs My.!! (len - i - 1)) $ [0..len - 1]

---- intersperse properties ----

-- if result of an intersperse is the same as the original list, original list was empty or singleton.
prop_intersperseLengthSingletonOrNull :: Int -> [Int] -> Property
prop_intersperseLengthSingletonOrNull x xs = (My.intersperse x xs == xs) ==> My.null xs || My.length xs == 1

-- if a list is non-null and non-singleton, then the length of an intersperse 
-- is twice the length of the original list, plus one.
prop_intersperseLengthGeneral :: Int -> [Int] -> Property
prop_intersperseLengthGeneral x xs = not (My.null xs || My.length xs == 1) ==> My.length (My.intersperse x xs) == My.length xs + My.length xs - 1

-- interspersing y between [x, z] yields [x, y, z].
prop_intersperseStructure :: Int -> Int -> Int -> Bool 
prop_intersperseStructure x y z = My.intersperse y [x, z] == [x, y, z]

---- intercalate properties ----

-- intercalate definition from documentation.
prop_intercalateIdentity :: [Int] -> [[Int]] -> Bool
prop_intercalateIdentity x xs = My.intercalate x xs == My.concat (My.intersperse x xs)


---- transpose properties ----

-- TODO: this fails for [[]]
prop_transposeTransposeIsOriginal :: [[Int]] -> Bool 
prop_transposeTransposeIsOriginal xs = (My.transpose . My.transpose $ xs) == xs


---- subsequences properties ----

-- size of the subsequences (power set) of a list with length n is 2 ^ n.
prop_subsequencesLength :: [Int] -> Bool 
prop_subsequencesLength xs = My.length (My.subsequences xs) == 2 ^ (My.length xs)

-- all lists of subsequences contain the empty list.
prop_subsequencesContainsEmptyList :: [Int] -> Bool 
prop_subsequencesContainsEmptyList xs = [] `My.elem` My.subsequences xs

-- all lists of subsequences contain the original list.
prop_subsequencesContainsOriginal:: [Int] -> Bool 
prop_subsequencesContainsOriginal xs = xs `My.elem` My.subsequences xs


---- permutations properties ----

-- size of the set of permutations of a list is the factorial of the length of the list.
prop_permutationsLength :: [Int] -> Bool 
prop_permutationsLength xs = My.length (My.permutations xs) == factorial (My.length xs)

-- all sets of permutations contain the original list.
prop_permutationsContainsOriginal :: [Int] -> Bool 
prop_permutationsContainsOriginal xs = xs `My.elem` My.permutations xs

--TODO
--prop_permutationsHaveSamePermutations :: [Int] -> Bool 
--prop_permutationsHaveSamePermutations xs =  let perms = My.permutations xs
--                                                permsOfPerms = My.map My.permutations perms
--                                            in My.all ( == perms) permsOfPerms

---- foldl properties ----

-- foldl can reproduce the original list.
prop_foldlListConstruction :: [Int] -> Bool
prop_foldlListConstruction xs = My.foldl (\acc x -> acc ++ [x]) [] xs == xs

---- foldl1 properties ----

-- foldl1 is equivalent to foldl with a starting identity value. 
prop_foldl1Equivalence :: [Int] -> Property
prop_foldl1Equivalence xs = (not . My.null) xs ==> My.foldl1 (+) xs == My.foldl (+) 0 xs

---- foldr properties ----

-- foldr can reproduce the original list.
prop_foldrListConstruction :: [Int] -> Bool 
prop_foldrListConstruction xs = My.foldr (:) [] xs == xs

---- foldr1 properties ----

-- foldr1 is equivalent to foldr with a starting identity value.
prop_foldr1Equivalence :: [Int] -> Property 
prop_foldr1Equivalence xs = (not . My.null) xs ==> My.foldr1 (+) xs == My.foldr (+) 0 xs

---- concat properties ----

-- concat of three lists is equivalent to appending the three lists together.
prop_concatStructure :: [Int] -> [Int] -> [Int] -> Bool
prop_concatStructure xs ys zs = My.concat [xs, ys, zs] == (xs ++ ys ++ zs)

-- length of a concat is equal to the sum of the lengths of all the lists.
prop_concatLength :: [[Int]] -> Bool 
prop_concatLength xss = (My.length . My.concat) xss == (My.sum . My.map (My.length)) xss 

---- concatMap properties ----

-- concatMap can reproduce the original list.
prop_concatMapIdentity :: [Int] -> Bool 
prop_concatMapIdentity xs = My.concatMap (\x -> [x]) xs == xs

-- length of a concatMap replicating each element n times is the length
-- of the original list multiplied by n. 
prop_concatMapLength :: Int -> [Int] -> Property
prop_concatMapLength n xs = n > 0 ==> (My.length . My.concatMap (My.replicate n)) xs == (My.length xs * n)

---- and properties ----

-- result of an and is False whenever a False is in the list.
prop_andWithAFalseIsFalse :: [Bool] -> Bool 
prop_andWithAFalseIsFalse bs = My.and (False:bs) == False 

-- and works on infinite lists if a False is finitely far from the beginning of the list.
prop_andInfiniteListWithFalse :: Int -> Bool 
prop_andInfiniteListWithFalse n = My.and (My.replicate n True ++ [False] ++ My.repeat True) == False

-- and of any number of True's is True.
prop_anyNumberOfTrueIsTrue :: Int -> Bool
prop_anyNumberOfTrueIsTrue n = My.and (My.replicate n True) == True 

-- and of a singleton [Bool] is the Bool itself.
prop_andOfSingleValueIsItself :: Bool -> Bool 
prop_andOfSingleValueIsItself b = My.and [b] == b

---- or properties ----

-- result of an or with a True in the list is True.
prop_orWithATrueIsTrue :: [Bool] -> Bool 
prop_orWithATrueIsTrue bs = My.or (True:bs) == True 

-- or works on infinite lists if True is finitely far from the beginning.
prop_orInfiniteListWithTrue :: Int -> Bool 
prop_orInfiniteListWithTrue n = My.or (My.replicate n False ++ [True] ++ My.repeat False) == True

-- or of any number of Falses is False.
prop_anyNumberOfFalseIsFalse :: Int -> Bool
prop_anyNumberOfFalseIsFalse n = My.or (My.replicate n False) == False

-- or of a singleton [Bool] is the Bool itself.
prop_orOfSingleValueIsItself :: Bool -> Bool 
prop_orOfSingleValueIsItself b = My.or [b] == b

---- sum properties ----

-- summing a number x repeated n times is n * x.
prop_sumOfRepeatedNumber :: Int -> Int -> Bool 
prop_sumOfRepeatedNumber n x = My.sum (My.replicate n x) == n * x

-- summing the numbers 1..n = n * (n + 1) / 2.
prop_sumOfOneToN :: Int -> Property
prop_sumOfOneToN n = n > 0 ==> 2 * My.sum ([1..n]) == n * (n + 1)

---- product properties ----

-- multiplying a number x for n times is x ^ n.
prop_productOfRepeatedNumber :: Int -> Int -> Bool 
prop_productOfRepeatedNumber n x = My.product (My.replicate n x) == x ^ n

-- product of 1..n is n!. 
prop_productOfOneToN :: Int -> Property
prop_productOfOneToN n = n > 0 ==> My.product [1..n] == factorial n 



main = do
    labeledCheck (NamedProp "prop_ConcatLength" prop_ConcatLength)
    labeledCheck (NamedProp "prop_ConcatRhsEmpty" prop_ConcatRhsEmpty)
    labeledCheck (NamedProp "prop_ConcatLhsEmpty" prop_ConcatLhsEmpty)
    labeledCheck (NamedProp "prop_ConcatStructure" prop_ConcatStructure)
    labeledCheck (NamedProp "prop_HeadSingleElement" prop_HeadSingleElement)
    labeledCheck (NamedProp "prop_HeadIsFirstElement" prop_HeadIsFirstElement)
    labeledCheck (NamedProp "prop_TailSingleElement" prop_TailSingleElement)
    labeledCheck (NamedProp "prop_TailMultipleElements" prop_TailMultipleElements)
    labeledCheck (NamedProp "prop_TailRemovesFirstElement" prop_TailRemovesFirstElement)
    labeledCheck (NamedProp "prop_InitSingleElement" prop_InitSingleElement)
    labeledCheck (NamedProp "prop_InitIsFirstElements" prop_InitIsFirstElements)
    labeledCheck (NamedProp "prop_LengthInitList" prop_LengthInitList)
    labeledCheck (NamedProp "prop_UnconsStructure" prop_UnconsStructure)
    labeledCheck (NamedProp "prop_UnconsEmptyList" prop_UnconsEmptyList)
    labeledCheck (NamedProp "prop_NonEmptyListsNotNull" prop_NonEmptyListsNotNull)
    labeledCheck (NamedProp "prop_NonNullListsNotEmpty" prop_NonNullListsNotEmpty)
    labeledCheck (NamedProp "prop_LengthEmptyList" prop_LengthEmptyList)
    labeledCheck (NamedProp "prop_LengthSingletonList" prop_LengthSingletonList)
    labeledCheck (NamedProp "prop_AddingElementIncreasesLength" prop_AddingElementIncreasesLength)
    labeledCheck (NamedProp "prop_mappedListLength" prop_mappedListLength)
    labeledCheck (NamedProp "prop_mapSingletonList" prop_mapSingletonList)
    labeledCheck (NamedProp "prop_reverseReverseIsOriginal" prop_reverseReverseIsOriginal)
    labeledCheck (NamedProp "prop_reverseLength" prop_reverseLength)
    labeledCheck (NamedProp "prop_reverseIsReverse" prop_reverseIsReverse)
    labeledCheck (NamedProp "prop_intersperseLengthSingletonOrNull" prop_intersperseLengthSingletonOrNull)
    labeledCheck (NamedProp "prop_intersperseLengthGeneral" prop_intersperseLengthGeneral)
    labeledCheck (NamedProp "prop_intersperseStructure" prop_intersperseStructure)
    labeledCheck (NamedProp "prop_intercalateIdentity" prop_intercalateIdentity)
    labeledCheck (NamedProp "prop_transposeTransposeIsOriginal" prop_transposeTransposeIsOriginal)
    -- TODO: constrain length of input somehow
    --labeledCheck (NamedProp "prop_subsequencesLength" prop_subsequencesLength)
    labeledCheck (NamedProp "prop_subsequencesContainsEmptyList" prop_subsequencesContainsEmptyList)
    --labeledCheck (NamedProp "prop_subsequencesContainsOriginal" prop_subsequencesContainsOriginal)
    --labeledCheck (NamedProp "prop_permutationsLength" prop_permutationsLength)
    --labeledCheck (NamedProp "prop_permutationsContainsOriginal" prop_permutationsContainsOriginal)
    --labeledCheck (NamedProp "prop_permutationsHaveSamePermutations" prop_permutationsHaveSamePermutations)
    labeledCheck (NamedProp "prop_foldlListConstruction" prop_foldlListConstruction)
    labeledCheck (NamedProp "prop_foldl1Equivalence" prop_foldl1Equivalence)
    labeledCheck (NamedProp "prop_foldrListConstruction" prop_foldrListConstruction)
    labeledCheck (NamedProp "prop_foldr1Equivalence" prop_foldr1Equivalence)
    labeledCheck (NamedProp "prop_concatStructure" prop_concatStructure)
    labeledCheck (NamedProp "prop_concatLength" prop_concatLength)
    labeledCheck (NamedProp "prop_concatMapIdentity" prop_concatMapIdentity)
    labeledCheck (NamedProp "prop_concatMapLength" prop_concatMapLength)
    labeledCheck (NamedProp "prop_andWithAFalseIsFalse" prop_andWithAFalseIsFalse)
    labeledCheck (NamedProp "prop_andInfiniteListWithFalse" prop_andInfiniteListWithFalse)
    labeledCheck (NamedProp "prop_anyNumberOfTrueIsTrue" prop_anyNumberOfTrueIsTrue)
    labeledCheck (NamedProp "prop_andOfSingleValueIsItself" prop_andOfSingleValueIsItself)
    labeledCheck (NamedProp "prop_orWithATrueIsTrue" prop_orWithATrueIsTrue)
    labeledCheck (NamedProp "prop_orInfiniteListWithTrue" prop_orInfiniteListWithTrue)
    labeledCheck (NamedProp "prop_anyNumberOfFalseIsFalse" prop_anyNumberOfFalseIsFalse)
    labeledCheck (NamedProp "prop_orOfSingleValueIsItself" prop_orOfSingleValueIsItself)
    labeledCheck (NamedProp "prop_sumOfRepeatedNumber" prop_sumOfRepeatedNumber)
    labeledCheck (NamedProp "prop_sumOfOneToN" prop_sumOfOneToN)
    labeledCheck (NamedProp "prop_productOfRepeatedNumber" prop_productOfRepeatedNumber)
    labeledCheck (NamedProp "prop_productOfOneToN" prop_productOfOneToN)