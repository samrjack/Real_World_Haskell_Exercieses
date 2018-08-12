
splitLines [] = []
splitLines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                    ('\r':'\n':rest) -> splitLines rest
                    ('\r':rest)      -> splitLines rest
                    ('\n':rest)      -> splitLines rest
                    otherwise        -> []
isLineTerminator c = c == '\r' || c == '\n'

-- Functions discussed:
-- length
-- null
-- head
-- tail
-- last
-- init
-- (++)
-- concat
-- reverse
-- and
-- or
-- all
-- any
-- take
-- drop
-- splitAt
-- takeWhile
-- dropWhile
-- span
-- break
-- elem
-- notElem
-- filter
-- isPrefixOf
-- isInfixOf
-- isSuffixOf
-- zip
-- zipWith
-- zip3 ... 7
-- zipWith3 ... 7
-- lines
-- unlines
-- words
-- unwords
