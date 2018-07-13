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


-- 3. Using the command framework from the earlier section “A Simple Command-Line
--    Framework” on page 71, write a program that prints the first word of each line of
--    its input.


-- 4. Write a program that transposes the text in a file. For instance, it should convert
--    "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
