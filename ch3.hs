import Data.List (sortOn)

-- 1. Write a function that computes the number of elements in a list. To test it, ensure
--    that it gives the same answers as the standard length function.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 3. Write a function that computes the mean of a list, i.e., the sum of all elements in
--    the list divided by its length. (You may need to use the fromIntegral function to
--    convert the length of the list from an integer into a floating-point number.)
myMean :: (Real a, Fractional b) => [a] -> b
myMean list = let (sum, count) = sumCount list 0 0 in (realToFrac sum) / (fromIntegral count) 
    where sumCount :: (Real a) => [a] -> a -> Int -> (a, Int)
          sumCount [] s c     = (s, c)
          sumCount (x:xs) s c = sumCount xs (x + s) (c + 1)

-- 4. Turn a list into a palindrome; i.e., it should read the same both backward and
--    forward. For example, given the list [1,2,3], your function should return
--    [1,2,3,3,2,1].
toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ (rev xs [])
    where rev :: [a] -> [a] -> [a]
          rev []     xs = xs
          rev (y:ys) xs = rev ys (y:xs)

-- 5. Write a function that determines whether its input list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You
--    may want to look at the sortBy function from the Data.List module.)
--sortByLength :: [[a]] -> [[a]]
--sortByLength

-- 7. Define a function that joins a list of lists together using a separator value:
--    -- file: ch03/Intersperse.hs
--    intersperse :: a -> [[a]] -> [a]
intersperse :: a -> [[a]] -> [a]
intersperse _ []     = []
intersperse _ (x:[]) = x
intersperse a (x:xs) = x ++ a:(intersperse a xs)

-- 8. The separator should appear between elements of the list, but it should not follow
--    the last element. Your function should behave as follows:
--    ghci> :load Intersperse
--    [1 of 1] Compiling Main ( Intersperse.hs, interpreted )
--    Ok, modules loaded: Main.
--    ghci> intersperse ',' []
--    ""
--    ghci> intersperse ',' ["foo"]
--    "foo"
--    ghci> intersperse ',' ["foo","bar","baz","quux"]
--    "foo,bar,baz,quux"


-- 9. Using the binary tree type that we defined earlier in this chapter, write a function
--    that will determine the height of the tree. The height is the largest number of hops
--    from the root to an Empty. For example, the tree Empty has height zero; Node "x"
--    Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height
--    two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Nil
            deriving (Show)

lengthTree :: Tree a -> Int
lengthTree Nil = 0
lengthTree (Node _ treeLeft treeRight) = 1 + max (lengthTree treeLeft) (lengthTree treeRight)

-- 10. Consider three two-dimensional points, a, b, and c. If we look at the angle formed
--     by the line segment from a to b and the line segment from b to c, it turns left, turns
--     right, or forms a straight line. Define a Direction data type that lets you represent
--     these possibilities.
data Point = Point (Double, Double)
             deriving (Eq)

mkPoint :: (Real a) => (a, a) -> Point
mkPoint (a, b) = Point (realToFrac a, realToFrac b)

ptX :: Point -> Double
ptX (Point (x, _)) = x

ptY :: Point -> Double
ptY (Point (_, y)) = y

instance Show Point where
    show (Point (x, y)) = "<" ++ show x ++ "," ++ show y ++ ">"

instance Num Point where
    (+) pt1 pt2 = mkPoint (ptX pt1 + ptX pt2, ptY pt1 + ptX pt2)
    (-) pt1 pt2 = mkPoint (ptX pt1 - ptX pt2, ptY pt1 - ptY pt2)
    (*) pt1 pt2 = mkPoint (ptX pt1 * ptX pt2, ptY pt1 * ptY pt2)

    abs         pt = mkPoint (abs (ptX pt)   , abs    (ptY pt) )
    signum      pt = mkPoint (signum (ptX pt), signum (ptY pt) )
    fromInteger n  = mkPoint (n, n)

data Turn = L -- Left
          | S -- Stright
          | R -- Right
          deriving (Show, Eq)

a = mkPoint (2,2)
b = mkPoint (4,2)
c = mkPoint (1,4)

d = mkPoint (5,4)
e = mkPoint (0,0)
f = mkPoint (2,2)

allPoints = [a, b, c, d, e, f]

sample2 = map mkPoint [ (9,5)
                      , (9,8)
                      , (8,7)
                      , (8,10)
                      , (7,2)
                      , (7,4)
                      , (7,11)
                      , (6,6)
                      , (6,7)
                      , (5,4)
                      , (5,12)
                      , (4,6)
                      , (4,9)
                      , (3,5)
                      , (2,2)
                      , (2,9) ]

-- 11. Write a function that calculates the turn made by three two-dimensional points
--     and returns a Direction.
-- First attempt. Fixed above according to wikipedia algorithm below.
-- getTurn a b c 
--        | bOffset <  aOffset = L
--        | bOffset == aOffset = S
--        | bOffset >  aOffset = R
--     where dv = a - c -- Difference Vector between a and c
--           equation :: Point -> Double
--           equation pt
--               | (ptX a == ptX c) = ptY pt * (ptY dv) 
--               | otherwise        = ((ptY dv) / (ptX dv) * (ptX pt) - (ptY pt)) * (ptX dv)
--           aOffset = equation a
--           bOffset = equation b
getTurn :: Point -> Point -> Point -> Turn
getTurn a b c
    | det <  0 = R
    | det == 0 = S
    | det >  0 = L
    where det = (ptX b - ptX a) * (ptY c - ptY a) - (ptY b - ptY a) * (ptX c - ptX a)

-- 12. Define a function that takes a list of two-dimensional points and computes the
--     direction of each successive triple. Given a list of points [a,b,c,d,e], it should
--     begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
--     then [c,d,e]. Your function should return a list of Direction.
turnList :: [Point] -> [Turn]
turnList (x:xs@(y:z:_)) = (getTurn x y z) : turnList xs
turnList _              = []

-- 13. Using the code from the preceding three exercises, implement Graham’s scan algorithm
--     for the convex hull of a set of 2D points. You can find good description
--     of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the
--     Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work,
--     on Wikipedia (http://en.wikipedia.org/).

dropWhile2 :: (a -> a -> Bool) -> [a] -> [a]
dropWhile2 f (x:y:zs) = if f x y
                        then dropWhile2 f (y:zs)
                        else x:y:zs
dropWhile2 _ list = list

gScanAlg :: [Point] -> [Point]
gScanAlg points@(a:b:c:d:xs) = loop (drop 2 sortedPoints) (sortedPoints !! 1 : sortedPoints !! 0 : [lowestPoint]) 
    where heightList = (sortOn (\pt -> ptY pt) points)
          lowestPoint = head heightList 
          sortedPoints = sortOn angle (tail heightList)
              where angle pt = let adj   = ptX pt - ptX lowestPoint
                                   adjSq = adj ** 2
                                   oppSq = (ptY pt - ptY lowestPoint) ** 2
                               in adjSq * signum adj / (adjSq + oppSq) 
          loop (nxtItem:remainingItems) stack = loop remainingItems (nxtItem : dropWhile2 (\x y -> getTurn nxtItem x y /= L) stack)
          loop _ stack = stack

gScanAlg list = list
