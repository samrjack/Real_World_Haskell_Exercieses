import System.Environment (getArgs)

-- EXERCISES
-- 1. Write your own “safe” definitions of the standard partial list functions, but make
--    sure they never fail. As a hint, you might want to consider using the following types:
--    -- file: ch04/ch04.exercises.hs
--    safeHead :: [a] -> Maybe a
--    safeTail :: [a] -> Maybe [a]
--    safeLast :: [a] -> Maybe a
--    safeInit :: [a] -> Maybe [a]
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- 2. Write a function splitWith that acts similarly to words but takes a predicate and a
--    list of any type, and then splits its input list on every element for which the predicate
--    returns False:
--    -- file: ch04/ch04.exercises.hs
--    splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = split xs
    where split [] = []
          split tl = let (front, back) = span f tl
                     in front : split (if (null back) then [] else tail back)


-- 3. Using the command framework from the earlier section “A Simple Command-Line
--    Framework” on page 71, write a program that prints the first word of each line of
--    its input.
main = do
    args <- getArgs
    case args of
        [input, output] -> firstWords input output
        _ -> putStrLn "error: exactly two arguemtns needed"
    where firstWords inputFile outputFile = do
              input <- readFile inputFile
              writeFile outputFile (func input)
          func = unlines . map head . map words . lines 
        

-- 4. Write a program that transposes the text in a file. For instance, it should convert
--    "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
transpose :: String -> String
transpose = unlines . filter (not . null) . tran . lines
    where tran :: [[a]] -> [[a]]
          tran [] = []
          tran xs = map head clean : tran (map tail clean)
              where clean = filter (not . null) xs


-- 1. Use a fold (choosing the appropriate fold will make your code much simpler) to
-- rewrite and improve upon the asInt function from the earlier section“Explicit Recursion”
-- on page 85.
-- How to Think About Loops | 97
-- -- file: ch04/ch04.exercises.hs
-- asInt_fold :: String -> Int


-- 2. Your function should behave as follows:
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798


-- 3. Extend your function to handle the following kinds of exceptional conditions by
-- calling error:
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374


-- 4. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite
-- the function to fix this problem:
-- -- file: ch04/ch04.exercises.hs
-- type ErrorMessage = String
-- asInt_either :: String -> Ei
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'o'"


-- 5. The Prelude function concat concatenates a list of lists into a single list and has the
-- following type:
-- -- file: ch04/ch04.exercises.hs
-- concat :: [[a]] -> [a]


-- 6. Write your own definition of concat using foldr.

-- 7. Write your own definition of the standard takeWhile function, first using explicit
-- recursion, and then foldr.


-- 8. The Data.List module defines a function, groupBy, which has the following type:
-- -- file: ch04/ch04.exercises.hs
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]


-- 9. Use ghci to load the Data.List module and figure out what groupBy does, then
-- write your own implementation using a fold.


-- 10. How many of the following Prelude functions can you rewrite using list folds?
-- • any
-- • cycle
-- • words
-- 98 | Chapter 4: Functional Programming
-- • unlines
-- For those functions where you can use either foldl' or foldr, which is more appropriate
-- in each case?



