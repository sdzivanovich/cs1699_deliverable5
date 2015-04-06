module Main where 

import qualified MyList as My
import Test.QuickCheck
import Test.QuickCheck.All

data NamedProp a = NamedProp String a 

-- Helper function to output a labeled property
labeledCheck :: Testable prop => NamedProp prop -> IO ()
labeledCheck (NamedProp s p) = do
    putStrLn s
    quickCheckWith stdArgs { maxSuccess = 500 } p 


---- (++) properties ----

prop_ConcatLength :: [Int] -> [Int] -> Bool 
prop_ConcatLength xs ys = My.length xs + My.length ys == My.length (xs My.++ ys)

prop_ConcatRhsEmpty :: [Int] -> Bool
prop_ConcatRhsEmpty ys = ([] My.++ ys) == ys

prop_ConcatLhsEmpty :: [Int] -> Bool 
prop_ConcatLhsEmpty xs = (xs My.++ []) == xs

prop_ConcatStructure :: [Int] -> [Int] -> Bool 
prop_ConcatStructure xs ys =    let n       = My.length xs 
                                    (a, b)  = My.splitAt n (xs My.++ ys)
                                in  a == xs && b == ys

---- head properties ----

-- head of a singleton list is the single element
prop_HeadSingleElement :: Int -> Bool
prop_HeadSingleElement x = My.head [x] == x 

-- head of a list is the first element
prop_HeadIsFirstElement :: Int -> [Int] -> Bool 
prop_HeadIsFirstElement x xs = head (x:xs) == x

---- tail properties ----

-- tail of a singleton list is the single element
prop_TailSingleElement :: Int -> Bool 
prop_TailSingleElement x = My.tail [x] == []

-- length of the tail of a list is length of the list minus 1
prop_TailMultipleElements :: [Int] -> Property 
prop_TailMultipleElements xs = not (My.null xs) ==> My.length (My.tail xs) == My.length xs - 1

-- tail of a list is everything except for the first element
prop_TailRemovesFirstElement :: Int -> [Int] -> Bool 
prop_TailRemovesFirstElement x xs = My.tail (x:xs) == xs 

---- init properties ----

-- init of a singleton list is the empty list
prop_InitSingleElement :: Int -> Bool 
prop_InitSingleElement x = My.init [x] == []

-- init of a list is everything except for the last element
prop_InitIsFirstElements :: [Int] -> Int -> Bool 
prop_InitIsFirstElements xs x = My.init (xs My.++ [x]) == xs 

prop_LengthInitList :: [Int] -> Property 
prop_LengthInitList xs = not (My.null xs) ==> My.length (My.init xs) == My.length xs - 1

---- uncons properties ----

prop_UnconsStructure :: Int -> [Int] -> Bool 
prop_UnconsStructure x xs = My.uncons (x:xs) == Just (x, xs)

-- no need for multple generated tests here.
prop_UnconsEmptyList :: Bool
prop_UnconsEmptyList = My.uncons ([] :: [Int]) == Nothing

---- null properties ----
prop_NonEmptyListsNotNull :: [Int] -> Property 
prop_NonEmptyListsNotNull xs = My.length xs > 0 ==> not . My.null $ xs

prop_NonNullListsNotEmpty :: [Int] -> Property
prop_NonNullListsNotEmpty xs = not (My.null xs) ==> My.length xs > 0

---- length properties ----

prop_LengthEmptyList :: Bool
prop_LengthEmptyList = My.length [] == 0

prop_LengthSingletonList :: Int -> Bool
prop_LengthSingletonList x = My.length [x] == 1

prop_AddingElementIncreasesLength :: Int -> [Int] -> Bool 
prop_AddingElementIncreasesLength x xs = My.length (x:xs) == My.length xs + 1

---- map properties ----

prop_mappedListLength :: [Int] -> Bool 
prop_mappedListLength xs = (My.length . My.map show $ xs) == My.length xs

prop_mapSingletonList :: Int -> Bool 
prop_mapSingletonList x = My.map show [x] == [show x]

---- reverse properties ----

prop_reverseReverseIsOriginal :: [Int] -> Bool 
prop_reverseReverseIsOriginal xs = My.reverse (My.reverse xs) == xs

prop_reverseLength :: [Int] -> Bool 
prop_reverseLength xs = My.length (My.reverse xs) == My.length xs

prop_reverseIsReverse :: [Int] -> Bool 
prop_reverseIsReverse xs =  let revXs   = My.reverse xs 
                                len     = My.length xs 
                            in  and . My.map (\i -> xs My.!! i == revXs My.!! (len - i - 1)) $ [0..len - 1]


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
