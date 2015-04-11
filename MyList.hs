{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.List
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The List data type and its operations
--
-----------------------------------------------------------------------------

-- NOTE: This code was adapted from the GHC.List module specified above.
-- All licensing remains as specified above.

module MyList (
   -- [] (..),          -- built-in syntax; can't be used in export list

   map, (++), filter, concat,
   head, last, tail, init, uncons, null, length, (!!),
   intersperse, intercalate, subsequences, permutations,
   foldl, foldl1, foldr, foldr1,
   iterate, repeat, replicate, cycle,
   take, drop, sum, product, maximum, minimum, splitAt, takeWhile, dropWhile,
   span, break, reverse, and, or,
   elem, notElem, lookup,
   concatMap,
   zip, zipWith, unzip, sort

 ) where

import Data.Maybe
import GHC.Base
import GHC.Num (Num(..))
import GHC.Integer (Integer)

infixl 9  !!
infix  4 `elem`, `notElem`


sort :: (Ord a) => [a] -> [a]
sort = foldr (insertBy compare) []

-- | The non-overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys

--------------------------------------------------------------
-- List-manipulation functions
--------------------------------------------------------------

-- | Extract the first element of a list, which must be non-empty.
head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}

badHead :: a
badHead = errorEmptyList "head"

-- This rule is useful in cases like
--      head [y | (x,y) <- ps, x==t]
{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b->b) .
                head (build g) = g (\x _ -> x) badHead
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

-- | Decompose a list into its head and tail. If the list is empty,
-- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
--
-- @since 4.8.0.0
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

-- | Extract the elements after the head of a list, which must be non-empty.
tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

-- | Extract the last element of a list, which must be finite and non-empty.
last                    :: [a] -> a
#ifdef USE_REPORT_PRELUDE
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  errorEmptyList "last"
#else
-- use foldl to allow fusion
last = foldl (\_ x -> x) (errorEmptyList "last")
#endif

-- | Return all the elements of a list except the last one.
-- The list must be non-empty.
init                    :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  errorEmptyList "init"
#else
-- eliminate repeated cases
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs
#endif

-- | Test whether a list is empty.
null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- | /O(n)/. 'length' returns the length of a finite list as an 'Int'.
-- It is an instance of the more general 'Data.List.genericLength',
-- the result type of which may be any kind of number.
{-# NOINLINE [1] length #-}
length                  :: [a] -> Int
length xs               = lenAcc xs 0

lenAcc          :: [a] -> Int -> Int
lenAcc []     n = n
lenAcc (_:ys) n = lenAcc ys (n+1)

-- | 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]

{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.


-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a list, reduces the list
-- using the binary operator, from left to right:
--
-- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
--
-- The list must be finite.

-- We write foldl as a non-recursive thing, so that it
-- can be inlined, and then (often) strictness-analysed,
-- and hence the classic space leak on foldl (+) 0 xs

foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl #-}
foldl k z0 xs =
  foldr (\(v::a) (fn::b->b) -> (\(z::b) -> fn (k z v))) (id :: b -> b) xs z0
  -- See Note [Left folds via right fold]

{-
Note [Left folds via right fold]

Implementing foldl et. al. via foldr is only a good idea if the compiler can
optimize the resulting code (eta-expand the recursive "go"). See #7994.
We hope that one of the two measure kick in:

   * Call Arity (-fcall-arity, enabled by default) eta-expands it if it can see
     all calls and determine that the arity is large.
   * The oneShot annotation gives a hint to the regular arity analysis that
     it may assume that the lambda is called at most once.
     See [One-shot lambdas] in CoreArity and especially [Eta expanding thunks]
     in CoreArity.

The oneShot annotations used in this module are correct, as we only use them in
argumets to foldr, where we know how the arguments are called.
-}

-- ----------------------------------------------------------------------------

-- | 'foldl1' is a variant of 'foldl' that has no starting value argument,
-- and thus must be applied to non-empty lists.
foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

-- -----------------------------------------------------------------------------
-- List sum and product

-- | The 'sum' function computes the sum of a finite list of numbers.
sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl (+) 0

-- | The 'product' function computes the product of a finite list of numbers.
product                 :: (Num a) => [a] -> a
{-# INLINE product #-}
product                 =  foldl (*) 1

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty lists.

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

-- | 'maximum' returns the maximum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'Data.List.maximumBy', which allows the
-- programmer to supply their own comparison function.
maximum                 :: (Ord a) => [a] -> a
{-# INLINE [1] maximum #-}
maximum []              =  errorEmptyList "maximum"
maximum xs              =  foldl1 max xs

-- | 'minimum' returns the minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'Data.List.minimumBy', which allows the
-- programmer to supply their own comparison function.
minimum                 :: (Ord a) => [a] -> a
{-# INLINE [1] minimum #-}
minimum []              =  errorEmptyList "minimum"
minimum xs              =  foldl1 min xs


-- | 'iterate' @f x@ returns an infinite list of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]

{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

-- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
-- It is an instance of the more general 'Data.List.genericReplicate',
-- in which @n@ may be of any integral type.
{-# INLINE replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

-- | 'cycle' ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle                   :: [a] -> [a]
cycle []                = errorEmptyList "cycle"
cycle xs                = xs' where xs' = xs ++ xs'

-- | 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
-- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@:
--
-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- > takeWhile (< 0) [1,2,3] == []
--

{-# NOINLINE [1] takeWhile #-}
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@:
--
-- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
-- > dropWhile (< 9) [1,2,3] == []
-- > dropWhile (< 0) [1,2,3] == [1,2,3]
--

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@:
--
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
--
-- It is an instance of the more general 'Data.List.genericTake',
-- in which @n@ may be of any integral type.
take                   :: Int -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs
#else

{- We always want to inline this to take advantage of a known length argument
sign. Note, however, that it's important for the RULES to grab take, rather
than trying to INLINE take immediately and then letting the RULES grab
unsafeTake. Presumably the latter approach doesn't grab it early enough; it led
to an allocation regression in nofib/fft2. -}
{-# INLINE [1] take #-}
take n xs | 0 < n     = unsafeTake n xs
          | otherwise = []

-- A version of take that takes the whole list if it's given an argument less
-- than 1.
{-# NOINLINE [1] unsafeTake #-}
unsafeTake :: Int -> [a] -> [a]
unsafeTake !_  []     = []
unsafeTake 1   (x: _) = [x]
unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs

#endif

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
--
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
--
-- It is an instance of the more general 'Data.List.genericDrop',
-- in which @n@ may be of any integral type.
drop                   :: Int -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs
#else /* hack away */
{-# INLINE drop #-}
drop n ls
  | n <= 0     = ls
  | otherwise  = unsafeDrop n ls
  where
    -- A version of drop that drops the whole list if given an argument
    -- less than 1
    unsafeDrop :: Int -> [a] -> [a]
    unsafeDrop !_ []     = []
    unsafeDrop 1  (_:xs) = xs
    unsafeDrop m  (_:xs) = unsafeDrop (m - 1) xs
#endif

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
splitAt                :: Int -> [a] -> ([a],[a])

#ifdef USE_REPORT_PRELUDE
splitAt n xs           =  (take n xs, drop n xs)
#else
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs
#endif /* USE_REPORT_PRELUDE */

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- > span (< 9) [1,2,3] == ([1,2,3],[])
-- > span (< 0) [1,2,3] == ([],[1,2,3])
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.

break                   :: (a -> Bool) -> [a] -> ([a],[a])
#ifdef USE_REPORT_PRELUDE
break p                 =  span (not . p)
#else
-- HBC version (stolen)
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)
#endif

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse                 :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
reverse                 =  foldl (flip (:)) []
#else
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
#endif

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r


-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)


-- | The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
--
-- > intersperse ',' "abcde" == "a,b,c,d,e"

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs


-- Not exported:
-- We want to make every element in the 'intersperse'd list available
-- as soon as possible to avoid space leaks. Experiments suggested that
-- a separate top-level helper is more efficient than a local worker.
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
-- result.
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- | 'and' returns the conjunction of a Boolean list.  For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value at a finite index of a finite or infinite list.
and                     :: [Bool] -> Bool
#ifdef USE_REPORT_PRELUDE
and                     =  foldr (&&) True
#else
and []          =  True
and (x:xs)      =  x && and xs
{-# NOINLINE [1] and #-}

{-# RULES
"and/build"     forall (g::forall b.(Bool->b->b)->b->b) .
                and (build g) = g (&&) True
 #-}
#endif

-- | 'or' returns the disjunction of a Boolean list.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value at a finite index of a finite or infinite list.
or                      :: [Bool] -> Bool
#ifdef USE_REPORT_PRELUDE
or                      =  foldr (||) False
#else
or []           =  False
or (x:xs)       =  x || or xs
{-# NOINLINE [1] or #-}

{-# RULES
"or/build"      forall (g::forall b.(Bool->b->b)->b->b) .
                or (build g) = g (||) False
 #-}
#endif

-- | 'elem' is the list membership predicate, usually written in infix form,
-- e.g., @x \`elem\` xs@.  For the result to be
-- 'False', the list must be finite; 'True', however, results from an element
-- equal to @x@ found at a finite index of a finite or infinite list.
elem                    :: (Eq a) => a -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
elem x                  =  any (== x)
#else
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys
{-# NOINLINE [1] elem #-}
{-# RULES
"elem/build"    forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b)
   . elem x (build g) = g (\ y r -> (x == y) || r) False
 #-}
#endif

-- | 'notElem' is the negation of 'elem'.
notElem                 :: (Eq a) => a -> [a] -> Bool
#ifdef USE_REPORT_PRELUDE
notElem x               =  all (/= x)
#else
notElem _ []    =  True
notElem x (y:ys)=  x /= y && notElem x ys
{-# NOINLINE [1] notElem #-}
{-# RULES
"notElem/build" forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b)
   . notElem x (build g) = g (\ y r -> (x /= y) && r) True
 #-}
#endif

-- | 'lookup' @key assocs@ looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- | Map a function over a list and concatenate the results.
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

{-# NOINLINE [1] concatMap #-}

{-# RULES
"concatMap" forall f xs . concatMap f xs =
    build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
 #-}


-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = foldr (++) []

{-# NOINLINE [1] concat #-}

{-# RULES
  "concat" forall xs. concat xs =
     build (\c n -> foldr (\x y -> foldr c y x) n xs)
-- We don't bother to turn non-fusible applications of concat back into concat
 #-}

-- | List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'Data.List.genericIndex',
-- which takes an index of any integral type.
(!!)                    :: [a] -> Int -> a
#ifdef USE_REPORT_PRELUDE
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)
#else

-- We don't really want the errors to inline with (!!).
-- We may want to fuss around a bit with NOINLINE, and
-- if so we should be careful not to trip up known-bottom
-- optimizations.
tooLarge :: Int -> a
tooLarge _ = error (prel_list_str ++ "!!: index too large")

negIndex :: a
negIndex = error $ prel_list_str ++ "!!: negative index"

{-# INLINABLE (!!) #-}
xs !! n
  | n < 0     = negIndex
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge xs n
#endif

--------------------------------------------------------------
-- The zip family
--------------------------------------------------------------

----------------------------------------------
-- | 'zip' takes two lists and returns a list of corresponding pairs.
-- If one input list is short, excess elements of the longer list are
-- discarded.
--
-- 'zip' is right-lazy:
--
-- > zip [] _|_ = []
{-# NOINLINE [1] zip #-}
zip :: [a] -> [b] -> [(a,b)]
zip []     _bs    = []
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.

----------------------------------------------
-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- list of corresponding sums.
--
-- 'zipWith' is right-lazy:
--
-- > zipWith f [] _|_ = []
{-# NOINLINE [1] zipWith #-}
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith _f []     _bs    = []
zipWith _f _as    []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

--------------------------------------------------------------
-- Error code
--------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:

errorEmptyList :: String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "Prelude."
