module Main where 

import qualified MyList as My
import Test.QuickCheck
import Test.QuickCheck.All

data NamedProp a = NamedProp String a 

---------------- utility functions ----------------

-- Helper function to output a labeled property.
labeledCheck :: Testable prop => NamedProp prop -> IO ()
labeledCheck (NamedProp s p) = do
    putStrLn s
    quickCheckWith stdArgs { maxSuccess = 200 } p 

-- Haskell doesn't have factorial built in. :(
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n 
    | n < 0     = error "n must be nonnegative"
    | otherwise = n * factorial (n - 1)

-- dummy predicate for *While functions.
dummyPredicate :: Int -> Int -> Bool 
dummyPredicate n = not . (== 0) . (`div` n)

-- function to filter keys from association list.
filterKeys :: Eq a => a -> [(a, b)] -> [(a, b)]
filterKeys x xs =   let keyEquiv a (b, c) = a == b 
                    in  My.filter (not . keyEquiv x) xs

-- helper function to compare the i'th element in a zipped list 
-- against a pair formed from the i'th elements in two lists. 
zippedElemEqual :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> [c] -> Int -> Bool 
zippedElemEqual f xs ys zs i = f (xs My.!! i) (ys My.!! i) == (zs My.!! i)

-- helper function to generate a pair.
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- HACK: maximum length allowable for generating subsequences of lists.
-- ideally, we would modify the random generator to only generate lists
-- of a certain length or lower.
maxLenForSubsequences :: Int 
maxLenForSubsequences = 10

-- HACK: maximum length allowable for generating permutations of lists.
-- ideally, we would modify the random generator to only generate lists
-- of a certain length or lower.
maxLenForPermutations :: Int 
maxLenForPermutations = 6

---------------- (++) properties ----------------

-- Length of a concatenation is the sum of the length of both pieces.
prop_appendLength :: [Int] -> [Int] -> Bool 
prop_appendLength xs ys = My.length xs + My.length ys == My.length (xs My.++ ys)

-- Empty list appended with another list is the other list.
prop_appendRhsEmpty :: [Int] -> Bool
prop_appendRhsEmpty ys = ([] My.++ ys) == ys

-- A list appended with the empty list is itself.
prop_appendLhsEmpty :: [Int] -> Bool 
prop_appendLhsEmpty xs = (xs My.++ []) == xs

-- First list follows the second in an append.
prop_appendStructure :: [Int] -> [Int] -> Bool 
prop_appendStructure xs ys =    let n       = My.length xs 
                                    (a, b)  = My.splitAt n (xs My.++ ys)
                                in  a == xs && b == ys

---------------- head properties ----------------

-- head of a list is the first element.
prop_headIsFirstElement :: Int -> [Int] -> Bool 
prop_headIsFirstElement x xs = My.head (x:xs) == x

---------------- last properties ----------------

-- last of a list is the last element.
prop_lastIsLastElement :: Int -> [Int] -> Bool 
prop_lastIsLastElement x xs = My.last (xs ++ [x]) == x

---------------- tail properties ----------------

-- tail of a singleton list is the single element.
prop_tailSingleElement :: Int -> Bool 
prop_tailSingleElement x = My.tail [x] == []

-- length of the tail of a list is length of the list minus 1.
prop_tailMultipleElements :: [Int] -> Property 
prop_tailMultipleElements xs = not (My.null xs) ==> My.length (My.tail xs) == My.length xs - 1

-- tail of a list is everything except for the first element.
prop_tailRemovesFirstElement :: Int -> [Int] -> Bool 
prop_tailRemovesFirstElement x xs = My.tail (x:xs) == xs 

---------------- init properties ----------------

-- init of a singleton list is the empty list.
prop_initSingleElement :: Int -> Bool 
prop_initSingleElement x = My.init [x] == []

-- init of a list is everything except for the last element.
prop_initIsFirstElements :: [Int] -> Int -> Bool 
prop_initIsFirstElements xs x = My.init (xs My.++ [x]) == xs 

-- length of the init of a list is equal to the length of the list minus 1.
prop_lengthInitList :: [Int] -> Property 
prop_lengthInitList xs = not (My.null xs) ==> My.length (My.init xs) == My.length xs - 1

---------------- uncons properties ----------------

-- uncons of a an element prepended to a list is a pair containing
-- that element, and the list.
prop_unconsStructure :: Int -> [Int] -> Bool 
prop_unconsStructure x xs = My.uncons (x:xs) == Just (x, xs)

-- no need for multple generated tests here.
prop_unconsEmptyList :: Bool
prop_unconsEmptyList = My.uncons ([] :: [Int]) == Nothing

---------------- null properties ----------------

-- if a list has length > 0, it is not null.
prop_nonEmptyListsNotNull :: [Int] -> Property 
prop_nonEmptyListsNotNull xs = My.length xs > 0 ==> not . My.null $ xs

-- if a list is not null, it has a length greater than 0.
prop_nonNullListsNotEmpty :: [Int] -> Property
prop_nonNullListsNotEmpty xs = not (My.null xs) ==> My.length xs > 0

---------------- length properties ----------------

-- length of the empty list is 0.
prop_lengthEmptyList :: Bool
prop_lengthEmptyList = My.length [] == 0

-- length of the singleton list is 1.
prop_lengthSingletonList :: Int -> Bool
prop_lengthSingletonList x = My.length [x] == 1

-- length of a list formed by prepending an element to another list.
-- is the length of the original list + 1. 
prop_addingElementIncreasesLength :: Int -> [Int] -> Bool 
prop_addingElementIncreasesLength x xs = My.length (x:xs) == My.length xs + 1

---------------- map properties ----------------

-- length of the list produced by a map is equal to length of the original list.
prop_mappedListLength :: [Int] -> Bool 
prop_mappedListLength xs = (My.length . My.map show $ xs) == My.length xs

-- map over a singleton list is the same as a list created from applying the
-- function to the single element.
prop_mapSingletonList :: Int -> Bool 
prop_mapSingletonList x = My.map show [x] == [show x]

---------------- reverse properties ----------------

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

---------------- intersperse properties ----------------

-- result of an intersperse on a singleton list is the singleton list.
prop_intersperseSingleton :: Int -> Int -> Bool
prop_intersperseSingleton x y = My.intersperse x [y] == [y]

-- interspersing on an empty list is the empty list.
prop_intersperseEmptyList :: Int -> Bool 
prop_intersperseEmptyList x = My.intersperse x [] == []

-- if a list is non-null and non-singleton, then the length of an intersperse 
-- is twice the length of the original list, plus one.
prop_intersperseLengthGeneral :: Int -> [Int] -> Property
prop_intersperseLengthGeneral x xs = not (My.null xs || My.length xs == 1) ==> My.length (My.intersperse x xs) == My.length xs + My.length xs - 1

-- interspersing y between [x, z] yields [x, y, z].
prop_intersperseStructure :: Int -> Int -> Int -> Bool 
prop_intersperseStructure x y z = My.intersperse y [x, z] == [x, y, z]

---------------- intercalate properties ----------------

-- intercalate definition from documentation.
prop_intercalateIdentity :: [Int] -> [[Int]] -> Bool
prop_intercalateIdentity x xs = My.intercalate x xs == My.concat (My.intersperse x xs)

---------------- subsequences properties ----------------

-- size of the subsequences (power set) of a list with length n is 2 ^ n.
prop_subsequencesLength :: [Int] -> Property 
prop_subsequencesLength xs = 
    My.length xs < maxLenForSubsequences ==> My.length (My.subsequences xs) == 2 ^ (My.length xs)

-- all lists of subsequences contain the empty list.
prop_subsequencesContainsEmptyList :: [Int] -> Bool 
prop_subsequencesContainsEmptyList xs = [] `My.elem` My.subsequences xs

-- all lists of subsequences contain the original list.
prop_subsequencesContainsOriginal:: [Int] -> Property 
prop_subsequencesContainsOriginal xs = 
    My.length xs < maxLenForSubsequences ==> xs `My.elem` My.subsequences xs


---------------- permutations properties ----------------

-- size of the set of permutations of a list is the factorial of the length of the list.
prop_permutationsLength :: [Int] -> Property 
prop_permutationsLength xs = 
    My.length xs < maxLenForPermutations ==> My.length (My.permutations xs) == factorial (My.length xs)

-- all sets of permutations contain the original list.
prop_permutationsContainsOriginal :: [Int] -> Property 
prop_permutationsContainsOriginal xs = 
    My.length xs < maxLenForPermutations ==> xs `My.elem` My.permutations xs

-- permutations of the permutations of a list all have the same permutations.
prop_permutationsHaveSamePermutations :: [Int] -> Property 
prop_permutationsHaveSamePermutations xs =  
    My.length xs < maxLenForPermutations ==>  
        let permAndSort     = My.sort . My.permutations
            perms           = permAndSort xs
            permsOfPerms    = My.map permAndSort perms
        in  all ( == perms) permsOfPerms

---------------- foldl properties ----------------

-- foldl can reproduce the original list.
prop_foldlListConstruction :: [Int] -> Bool
prop_foldlListConstruction xs = My.foldl (\acc x -> acc ++ [x]) [] xs == xs

---------------- foldl1 properties ----------------

-- foldl1 is equivalent to foldl with a starting identity value. 
prop_foldl1Equivalence :: [Int] -> Property
prop_foldl1Equivalence xs = (not . My.null) xs ==> My.foldl1 (+) xs == My.foldl (+) 0 xs

---------------- foldr properties ----------------

-- foldr can reproduce the original list.
prop_foldrListConstruction :: [Int] -> Bool 
prop_foldrListConstruction xs = My.foldr (:) [] xs == xs

---------------- foldr1 properties ----------------

-- foldr1 is equivalent to foldr with a starting identity value.
prop_foldr1Equivalence :: [Int] -> Property 
prop_foldr1Equivalence xs = (not . My.null) xs ==> My.foldr1 (+) xs == My.foldr (+) 0 xs

---------------- concat properties ----------------

-- concat of three lists is equivalent to appending the three lists together.
prop_concatStructure :: [Int] -> [Int] -> [Int] -> Bool
prop_concatStructure xs ys zs = My.concat [xs, ys, zs] == (xs ++ ys ++ zs)

-- length of a concat is equal to the sum of the lengths of all the lists.
prop_concatLength :: [[Int]] -> Bool 
prop_concatLength xss = (My.length . My.concat) xss == (My.sum . My.map (My.length)) xss 

---------------- concatMap properties ----------------

-- concatMap can reproduce the original list.
prop_concatMapIdentity :: [Int] -> Bool 
prop_concatMapIdentity xs = My.concatMap (\x -> [x]) xs == xs

-- length of a concatMap replicating each element n times is the length
-- of the original list multiplied by n. 
prop_concatMapLength :: Int -> [Int] -> Property
prop_concatMapLength n xs = n > 0 ==> (My.length . My.concatMap (My.replicate n)) xs == (My.length xs * n)

---------------- and properties ----------------

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

---------------- or properties ----------------

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

---------------- sum properties ----------------

-- summing a number x repeated n times is n * x.
prop_sumOfRepeatedNumber :: Int -> Int -> Property 
prop_sumOfRepeatedNumber n x = n >= 0 ==> My.sum (My.replicate n x) == n * x

-- summing the numbers 1..n = n * (n + 1) / 2.
prop_sumOfOneToN :: Int -> Property
prop_sumOfOneToN n = n > 0 ==> 2 * My.sum ([1..n]) == n * (n + 1)

---------------- product properties ----------------

-- multiplying a number x for n times is x ^ n.
prop_productOfRepeatedNumber :: Int -> Int -> Property 
prop_productOfRepeatedNumber n x = n >= 0 ==> My.product (My.replicate n x) == x ^ n

-- product of 1..n is n!. 
prop_productOfOneToN :: Int -> Property
prop_productOfOneToN n = n > 0 ==> My.product [1..n] == factorial n 

---------------- maximum properties ----------------

-- maximum of a singleton list is the single element.
prop_maximumSingleton :: Int -> Bool 
prop_maximumSingleton x = My.maximum [x] == x

-- maximum of a two element list is the larger one.
prop_maximumOfTwoElements :: Int -> Int -> Bool 
prop_maximumOfTwoElements x y = My.maximum [x, y] == max x y 

-- adding a new maximum to the list changes the maximum element.
prop_addingNewMaximum :: [Int] -> Property
prop_addingNewMaximum xs =  
    (not . null) xs ==> let oldMax = My.maximum xs
                            newMax = oldMax + 1
                        in  My.maximum (newMax : xs) == newMax

---------------- minimum properties ----------------

-- minimum of a singleton list is the single element.
prop_minimumSingleton :: Int -> Bool 
prop_minimumSingleton x = My.minimum [x] == x

-- minimum of a two element list is the larger one.
prop_minimumOfTwoElements :: Int -> Int -> Bool 
prop_minimumOfTwoElements x y = My.minimum [x, y] == min x y 

-- adding a new minimum to the list changes the minimum element.
prop_addingNewMinimum :: [Int] -> Property
prop_addingNewMinimum xs =  
    (not . null) xs ==> let oldMin = My.minimum xs
                            newMin = oldMin - 1
                        in  My.minimum (newMin : xs) == newMin

----------------- iterate properties -----------------

-- head of an iterate list is the original element.
prop_headOfIterateIsOriginal :: Int -> Bool
prop_headOfIterateIsOriginal x = (My.head . My.iterate negate) x == x

-- test that iterate repeatedly applies the given function.
prop_iterateRepeatedApplications :: Int -> Property
prop_iterateRepeatedApplications n = n > 0 ==> (My.takeWhile ( <= n) . My.iterate ( + 1)) 0 == [0..n]


----------------- repeat properties -----------------

-- any element of the repeat list is the repeated element.
prop_nthElementOfRepeatIsTheOriginalElement :: Int -> Int -> Property 
prop_nthElementOfRepeatIsTheOriginalElement n x = n >= 0 ==> (My.repeat x My.!! n) == x

----------------- replicate properties -----------------

-- length of a replicate list is the argument given to replicate.
prop_replicateLength :: Int -> Int -> Property 
prop_replicateLength n x = n >= 0 ==> (My.length . My.replicate n) x == n 

-- an element within the length of the replicated list is the replicated element.
prop_elementOfReplicatedList :: Int -> Int -> Int -> Property 
prop_elementOfReplicatedList n i x = n >= 0 && i >= 0 && i < n ==> (My.replicate n x !! i) == x

----------------- cycle properties -----------------

-- first n elements of the cycled list is the original list. 
prop_firstElementsOfCycledListIsOriginalList :: [Int] -> Bool
prop_firstElementsOfCycledListIsOriginalList xs = My.take (My.length xs) (My.cycle xs) == xs

-- cycle produces multiple copies of the given list.
prop_cycleOfListContainsOriginalListMultipleTimes :: Int -> [Int] -> Property 
prop_cycleOfListContainsOriginalListMultipleTimes n xs = 
    n >= 0 ==>  let cycledList = My.cycle xs
                    target = My.concat . (My.replicate n) $ xs 
                in  My.take (n * My.length xs) cycledList == target

----------------- take properties -----------------

-- if n is greater than or equal to the length of the list, the result
-- of a take is the original list.
prop_takeGivesOriginalListIfNIsAtLeastLengthOfList :: Int -> [Int] -> Property
prop_takeGivesOriginalListIfNIsAtLeastLengthOfList n xs = 
    n >= My.length xs ==> My.take n xs == xs

-- if n <= length of the list and n >= 0, length of the result is equal to n.
prop_lengthOfTakeIsGivenArgument :: Int -> [Int] -> Bool
prop_lengthOfTakeIsGivenArgument n xs = 
    let len = My.length xs
    in  (My.length . My.take n) xs == (min len . max 0) n

-- if n < 0, then result of a take is the empty list.
prop_takeWithNegativeNGivesEmptyList :: Int -> [Int] -> Property 
prop_takeWithNegativeNGivesEmptyList n xs = n < 0 ==> My.take n xs == []

-- take gives elements from the front of the list.
prop_takeGivesFirstElements :: Int -> Int -> Property 
prop_takeGivesFirstElements i n = i < n && i > 0 && n >= 0 ==> My.take i [0..n] == [0..(i - 1)]

----------------- drop properties -----------------

-- if n >= 0, drop should return the empty list.
prop_dropGivesEmptyListIfNIsBiggerThanListLength :: Int -> [Int] -> Property 
prop_dropGivesEmptyListIfNIsBiggerThanListLength n xs =
    n >= My.length xs ==> My.drop n xs == [] 

-- if n < 0, drop should return the original list.
prop_dropGivesOriginalListIfNLessThanZero :: Int -> [Int] -> Property 
prop_dropGivesOriginalListIfNLessThanZero n xs =
    n < 0 ==> My.drop n xs == xs

-- if n >= 0 and n < length of the list, then drop should return the suffix.
prop_dropGivesSuffixOfList :: Int -> Int -> Property
prop_dropGivesSuffixOfList i n =
    i < n && n >= 0 && i >= 0 ==> My.drop i [0..n] == [i..n]

-- length of dropping n elements from a list is length of list minus n.
-- includes cases when n >= length and n < 0.
prop_lengthOfDropIsLengthLessArgument :: Int -> [Int] -> Bool
prop_lengthOfDropIsLengthLessArgument n xs = 
    let len = My.length xs
    in  (My.length . My.drop n) xs == (min len . max 0) (len - n)

---------------- splitAt properties ----------------

-- splitAt is equivalent to using take and drop.
prop_splitAtEquivalentToTakeAndDrop :: Int -> [Int] -> Bool 
prop_splitAtEquivalentToTakeAndDrop n xs = My.splitAt n xs == (My.take n xs, My.drop n xs)

---------------- dropWhile properties ----------------

-- first element of a list from dropWhile does not satisfy the predicate.
prop_firstElementOfDropWhileDoesNotSatisfyPredicate :: [Int] -> Property 
prop_firstElementOfDropWhileDoesNotSatisfyPredicate xs = 
    let p = dummyPredicate 3
        dropped = My.dropWhile p xs 
    in  (not . null) dropped ==> (not . p . My.head) dropped 

-- dropWhile is what remains after a takeWhile with the same predicate.
prop_dropWhileIsSuffixAfterTakeWhile :: [Int] -> Bool 
prop_dropWhileIsSuffixAfterTakeWhile xs = 
    let p = dummyPredicate 4
        n = My.length . My.takeWhile p $ xs
    in  My.dropWhile p xs == My.drop n xs 

---------------- takeWhile properties ----------------

-- everything in a takeWhile satisfies the given predicate.
prop_allElementsOfTakeWhileSatisfyPredicate :: [Int] -> Bool 
prop_allElementsOfTakeWhileSatisfyPredicate xs =
    let p = dummyPredicate 5
        taken = My.takeWhile p xs 
    in  all p taken

-- the next element after a takeWhile should not satisfy the predicate.
prop_nextElementAfterTakeWhileDoesNotSatisfyPredicate :: [Int] -> Property
prop_nextElementAfterTakeWhileDoesNotSatisfyPredicate xs =
    let p = dummyPredicate 6
        taken = My.takeWhile p  xs
        takenLen = My.length taken
        leftover = My.drop takenLen xs 
    in  (not . My.null) leftover ==> (not . p . My.head) leftover  

---------------- span properties ----------------

-- span p xs is equivalent to (takeWhile p xs, dropWhile p xs). 
prop_spanIsEquivalentToTakeWhileAndDropWhile :: [Int] -> Bool
prop_spanIsEquivalentToTakeWhileAndDropWhile xs = 
    let p = dummyPredicate 6
    in  My.span p xs == (My.takeWhile p xs, My.dropWhile p xs)

---------------- break properties ----------------

-- break p xs is equivalent to span (not . p) xs. 
prop_breakIsEquivalentToSpanWithNegatedPredicate :: [Int] -> Bool 
prop_breakIsEquivalentToSpanWithNegatedPredicate xs =
    let p = dummyPredicate 7
    in  My.break p xs == My.span (not . p) xs


---------------- elem properties ----------------

-- elem returns true when the element is contained in the list.
prop_elemReturnsTrueWhenElementIsInList :: Int -> [Int] -> Bool 
prop_elemReturnsTrueWhenElementIsInList x xs = x `My.elem` (xs ++ [x])

-- elem returns false when the element is not in the list.
prop_elemReturnsFalseWhenElementIsNotInList :: Int -> [Int] -> Bool 
prop_elemReturnsFalseWhenElementIsNotInList x xs = 
    let noElems = My.filter ( /= x) xs 
    in  not (x `My.elem` noElems)

---------------- notElem properties ----------------

prop_notElemIsNegationOfElem :: Int -> [Int] -> Bool 
prop_notElemIsNegationOfElem x xs = (x `My.notElem` xs) == not (x `My.elem` xs)

---------------- lookup properties ----------------

-- lookup returns Nothing if the key is not found.
prop_lookupGivesNothingWhenKeyIsNotInList :: Int -> [(Int, Int)] -> Bool 
prop_lookupGivesNothingWhenKeyIsNotInList x xs = 
    let noKeys = filterKeys x xs 
    in  My.lookup x noKeys == Nothing

-- lookup of x returns Just y if list contains (x, y)
prop_lookupGivesJustWhenKeyIsInList :: Int -> Int -> [(Int, Int)] -> Bool 
prop_lookupGivesJustWhenKeyIsInList x y xs = 
    let noKeys = filterKeys x xs 
        withKey = noKeys ++ [(x, y)]
    in  My.lookup x withKey == Just y 

---------------- filter properties ----------------

-- applying a filter to a filtered list is the same as filtering it once
prop_filterFilterIsFirstFilter :: [Int] -> Bool
prop_filterFilterIsFirstFilter xs = 
    let p = dummyPredicate 8
        filtered = My.filter p xs 
    in My.filter p filtered == filtered 

-- if all elements satisfy the predicate, the resulting list is the same as
-- the original.
prop_filterDoesNotChangeListIfAllElementsSatisfyPredicate :: [Int] -> Bool 
prop_filterDoesNotChangeListIfAllElementsSatisfyPredicate xs =
    let p x = True 
    in  My.filter p xs == xs

-- if no elements satisfy the predicate, the resulting list is empty.
prop_filterReturnsEmptyListIfNoElementsSatisfyPredicate :: [Int] -> Bool 
prop_filterReturnsEmptyListIfNoElementsSatisfyPredicate xs = 
    let p x = False 
    in  My.filter p xs == []

-- filter will remove elements that do not satisfy the predicate. 
prop_filterRemovesElementsThatDoNotSatisfyPredicate :: Int -> Property 
prop_filterRemovesElementsThatDoNotSatisfyPredicate n = 
    n > 0   ==> let p = ( > 0)
                    positive = [1..n]
                    withNegatives = (0:positive) ++ [0]
                in  My.filter p withNegatives == positive

---------------- !! properties ----------------

-- indexing into a list with i is the same as dropping the first i elements,
-- then taking the head of the remaining elements. 
prop_indexGivesNthElement :: Int -> [Int] -> Property
prop_indexGivesNthElement i xs = 
    i >= 0 && i < My.length xs ==> xs My.!! i == (My.head . My.drop i) xs

---------------- zip properties ----------------

-- zipWith should be equivalent to zip with the function pairs two elements.
prop_zipEquivalentWithZipWith :: [Int] -> [Int] -> Bool 
prop_zipEquivalentWithZipWith xs ys = My.zipWith pair xs ys == My.zip xs ys 

---------------- unzip properties ----------------

-- unzipping should give back the original two lists, minus any excess elements
-- (i.e., elements that were discarded from the longer list). 
prop_unzipGivesOriginalsLessExcessElements :: [Int] -> [Int] -> Bool
prop_unzipGivesOriginalsLessExcessElements xs ys =
    let zipped      = My.zip xs ys
        zippedLen   = My.length zipped 
        xs'         = My.take zippedLen xs 
        ys'         = My.take zippedLen ys 
    in  My.unzip zipped == (xs', ys')

-- verifies general unzip structure. 
prop_unzipStructure :: [(Int, Int)] -> Bool 
prop_unzipStructure pairs = 
    let len         = My.length pairs 
        (xs, ys)    = My.unzip pairs 
        zipEquiv    = zippedElemEqual pair xs ys pairs
    in  My.and . My.map zipEquiv $ [0..len - 1]

---------------- zipWith properties ----------------

-- zip will ignore excess elements in the longer list.
prop_zipWithDiscardsElementsFromLongerList :: [Int] -> [Int] -> Bool
prop_zipWithDiscardsElementsFromLongerList xs ys = 
    My.length (My.zipWith (+) xs ys) == min (My.length xs) (My.length ys)

-- verifies general zipWith structure.
prop_zipWithStructure :: [Int] -> [Int] -> Bool 
prop_zipWithStructure xs ys = 
    let f           = (+)
        zipped      = My.zipWith f xs ys
        zippedLen   = My.length zipped 
        zipEquiv    = zippedElemEqual f xs ys zipped 
    in  My.and . My.map zipEquiv $ [0..zippedLen - 1]

----------------- property execution -----------------

-- enumerating each of the properties here is gross and somewhat unmaintainable.
-- however, an alternate solution would require using Template Haskell or an
-- external script, since we cannot store lists of these properties due to the
-- fact that they have differing types.
main = do
    labeledCheck (NamedProp "prop_appendLength" prop_appendLength)
    labeledCheck (NamedProp "prop_appendRhsEmpty" prop_appendRhsEmpty)
    labeledCheck (NamedProp "prop_appendLhsEmpty" prop_appendLhsEmpty)
    labeledCheck (NamedProp "prop_appendStructure" prop_appendStructure)
    labeledCheck (NamedProp "prop_headIsFirstElement" prop_headIsFirstElement)
    labeledCheck (NamedProp "prop_lastIsLastElement" prop_lastIsLastElement)
    labeledCheck (NamedProp "prop_tailSingleElement" prop_tailSingleElement)
    labeledCheck (NamedProp "prop_tailMultipleElements" prop_tailMultipleElements)
    labeledCheck (NamedProp "prop_tailRemovesFirstElement" prop_tailRemovesFirstElement)
    labeledCheck (NamedProp "prop_initSingleElement" prop_initSingleElement)
    labeledCheck (NamedProp "prop_initIsFirstElements" prop_initIsFirstElements)
    labeledCheck (NamedProp "prop_lengthInitList" prop_lengthInitList)
    labeledCheck (NamedProp "prop_unconsStructure" prop_unconsStructure)
    labeledCheck (NamedProp "prop_unconsEmptyList" prop_unconsEmptyList)
    labeledCheck (NamedProp "prop_nonEmptyListsNotNull" prop_nonEmptyListsNotNull)
    labeledCheck (NamedProp "prop_nonNullListsNotEmpty" prop_nonNullListsNotEmpty)
    labeledCheck (NamedProp "prop_lengthEmptyList" prop_lengthEmptyList)
    labeledCheck (NamedProp "prop_lengthSingletonList" prop_lengthSingletonList)
    labeledCheck (NamedProp "prop_addingElementIncreasesLength" prop_addingElementIncreasesLength)
    labeledCheck (NamedProp "prop_mappedListLength" prop_mappedListLength)
    labeledCheck (NamedProp "prop_mapSingletonList" prop_mapSingletonList)
    labeledCheck (NamedProp "prop_reverseReverseIsOriginal" prop_reverseReverseIsOriginal)
    labeledCheck (NamedProp "prop_reverseLength" prop_reverseLength)
    labeledCheck (NamedProp "prop_reverseIsReverse" prop_reverseIsReverse)
    labeledCheck (NamedProp "prop_intersperseSingleton" prop_intersperseSingleton)
    labeledCheck (NamedProp "prop_intersperseEmptyList" prop_intersperseEmptyList)
    labeledCheck (NamedProp "prop_intersperseLengthGeneral" prop_intersperseLengthGeneral)
    labeledCheck (NamedProp "prop_intersperseStructure" prop_intersperseStructure)
    labeledCheck (NamedProp "prop_intercalateIdentity" prop_intercalateIdentity)  
    labeledCheck (NamedProp "prop_subsequencesLength" prop_subsequencesLength)
    labeledCheck (NamedProp "prop_subsequencesContainsEmptyList" prop_subsequencesContainsEmptyList)
    labeledCheck (NamedProp "prop_subsequencesContainsOriginal" prop_subsequencesContainsOriginal)
    labeledCheck (NamedProp "prop_permutationsLength" prop_permutationsLength)
    labeledCheck (NamedProp "prop_permutationsContainsOriginal" prop_permutationsContainsOriginal)
    labeledCheck (NamedProp "prop_permutationsHaveSamePermutations" prop_permutationsHaveSamePermutations)
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
    labeledCheck (NamedProp "prop_maximumSingleton" prop_maximumSingleton)
    labeledCheck (NamedProp "prop_maximumOfTwoElements" prop_maximumOfTwoElements)
    labeledCheck (NamedProp "prop_addingNewMaximum" prop_addingNewMaximum)
    labeledCheck (NamedProp "prop_minimumSingleton" prop_minimumSingleton)
    labeledCheck (NamedProp "prop_minimumOfTwoElements" prop_minimumOfTwoElements)
    labeledCheck (NamedProp "prop_addingNewMinimum" prop_addingNewMinimum)
    labeledCheck (NamedProp "prop_headOfIterateIsOriginal" prop_headOfIterateIsOriginal)
    labeledCheck (NamedProp "prop_iterateRepeatedApplications" prop_iterateRepeatedApplications)
    labeledCheck (NamedProp "prop_nthElementOfRepeatIsTheOriginalElement" prop_nthElementOfRepeatIsTheOriginalElement)
    labeledCheck (NamedProp "prop_replicateLength" prop_replicateLength)
    labeledCheck (NamedProp "prop_elementOfReplicatedList" prop_elementOfReplicatedList)
    labeledCheck (NamedProp "prop_firstElementsOfCycledListIsOriginalList" prop_firstElementsOfCycledListIsOriginalList)
    labeledCheck (NamedProp "prop_cycleOfListContainsOriginalListMultipleTimes" prop_cycleOfListContainsOriginalListMultipleTimes)
    labeledCheck (NamedProp "prop_takeGivesOriginalListIfNIsAtLeastLengthOfList" prop_takeGivesOriginalListIfNIsAtLeastLengthOfList)
    labeledCheck (NamedProp "prop_lengthOfTakeIsGivenArgument" prop_lengthOfTakeIsGivenArgument)
    labeledCheck (NamedProp "prop_takeWithNegativeNGivesEmptyList" prop_takeWithNegativeNGivesEmptyList)
    labeledCheck (NamedProp "prop_takeGivesFirstElements" prop_takeGivesFirstElements)
    labeledCheck (NamedProp "prop_dropGivesEmptyListIfNIsBiggerThanListLength" prop_dropGivesEmptyListIfNIsBiggerThanListLength)
    labeledCheck (NamedProp "prop_dropGivesOriginalListIfNLessThanZero" prop_dropGivesOriginalListIfNLessThanZero)
    labeledCheck (NamedProp "prop_dropGivesSuffixOfList" prop_dropGivesSuffixOfList)
    labeledCheck (NamedProp "prop_lengthOfDropIsLengthLessArgument" prop_lengthOfDropIsLengthLessArgument)
    labeledCheck (NamedProp "prop_splitAtEquivalentToTakeAndDrop" prop_splitAtEquivalentToTakeAndDrop)
    labeledCheck (NamedProp "prop_firstElementOfDropWhileDoesNotSatisfyPredicate" prop_firstElementOfDropWhileDoesNotSatisfyPredicate)
    labeledCheck (NamedProp "prop_dropWhileIsSuffixAfterTakeWhile" prop_dropWhileIsSuffixAfterTakeWhile)
    labeledCheck (NamedProp "prop_allElementsOfTakeWhileSatisfyPredicate" prop_allElementsOfTakeWhileSatisfyPredicate)
    labeledCheck (NamedProp "prop_nextElementAfterTakeWhileDoesNotSatisfyPredicate" prop_nextElementAfterTakeWhileDoesNotSatisfyPredicate)
    labeledCheck (NamedProp "prop_spanIsEquivalentToTakeWhileAndDropWhile" prop_spanIsEquivalentToTakeWhileAndDropWhile)
    labeledCheck (NamedProp "prop_breakIsEquivalentToSpanWithNegatedPredicate" prop_breakIsEquivalentToSpanWithNegatedPredicate)
    labeledCheck (NamedProp "prop_elemReturnsTrueWhenElementIsInList" prop_elemReturnsTrueWhenElementIsInList)
    labeledCheck (NamedProp "prop_elemReturnsFalseWhenElementIsNotInList" prop_elemReturnsFalseWhenElementIsNotInList)
    labeledCheck (NamedProp "prop_notElemIsNegationOfElem" prop_notElemIsNegationOfElem)
    labeledCheck (NamedProp "prop_lookupGivesNothingWhenKeyIsNotInList" prop_lookupGivesNothingWhenKeyIsNotInList)
    labeledCheck (NamedProp "prop_lookupGivesJustWhenKeyIsInList" prop_lookupGivesJustWhenKeyIsInList)
    labeledCheck (NamedProp "prop_filterFilterIsFirstFilter" prop_filterFilterIsFirstFilter)
    labeledCheck (NamedProp "prop_filterDoesNotChangeListIfAllElementsSatisfyPredicate" prop_filterDoesNotChangeListIfAllElementsSatisfyPredicate)
    labeledCheck (NamedProp "prop_filterReturnsEmptyListIfNoElementsSatisfyPredicate" prop_filterReturnsEmptyListIfNoElementsSatisfyPredicate)
    labeledCheck (NamedProp "prop_filterRemovesElementsThatDoNotSatisfyPredicate" prop_filterRemovesElementsThatDoNotSatisfyPredicate)
    labeledCheck (NamedProp "prop_indexGivesNthElement" prop_indexGivesNthElement)
    labeledCheck (NamedProp "prop_zipEquivalentWithZipWith" prop_zipEquivalentWithZipWith)    
    labeledCheck (NamedProp "prop_unzipGivesOriginalsLessExcessElements" prop_unzipGivesOriginalsLessExcessElements)
    labeledCheck (NamedProp "prop_unzipStructure" prop_unzipStructure)
    labeledCheck (NamedProp "prop_zipWithDiscardsElementsFromLongerList" prop_zipWithDiscardsElementsFromLongerList)
    labeledCheck (NamedProp "prop_zipWithStructure" prop_zipWithStructure)
