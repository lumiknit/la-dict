module Main where

revcat [] b = b
revcat (a:as) b = revcat as (a:b)

splitOn c s = f c [] s
  where f c [] [] = []
        f c x [] = [reverse x]
        f c x (y:ys) | y == c = reverse x : f c [] ys
                     | otherwise = f c (y:x) ys

lstrip [] = []
lstrip t@(x:xs) | elem x [' ', '\t', '\r', '\n'] = lstrip xs
                | otherwise = t

strip = reverse . lstrip . reverse . lstrip

pass' c x [] = (reverse x, [])
pass' c x (y:ys) | y == c = (reverse x, ys)
                 | otherwise = c (y:x) ys
pass c x = pass' c [] x

isVowel c = elem c ['a', 'e', 'i', 'o', 'u']


suffix' a b@(h:t) | isVowel h = revcat (f isVowel ra) b
                  | otherwise = revcat (f (not . isVowel) ra) b
  where ra = reverse a
        f c [] = []
        f c t@(x:xs) | c x = xs
                     | otherwise = t
suffix a ('-':b) = suffix' a b
suffix a b = suffix' a b

---------
data WordSex = Male | Female | Neutral | MF
data Word = Noun String String WordSex
          | Verb String String String String
          | Adj String String String
          | Adv String
          | Prep String
type Meaning = String
type Comment = String
data WordSet = WordSet Word Meaning Comment
type WordList = [WordSet]


parseSex x = case x of
  "m" -> Male
  "f" -> Female
  "n" -> Neutral
  "mf" -> MF

parseNoun x =
  where l = split ',' x
        l1 = l !! 0
        l2 = l !! 1
        l3 = l !! 2

parseLine x = WordSet w (strip m) (strip r3)
  where (ty, r1) = pass ':' x
        (w, r2) = pass '[' r1
        (m, r3) = pass ']' r2
        w' = strip w
        w'' = case strip ty of
          "n" -> parseNoun w'
          "v" -> parseVerb w'
          "adj" -> parseAdj w'
          "adv" -> parseAdv w'
          "prev" -> parsePrev w'

parse src = map parseLine l
  where l = map strip $ splitOn '\n' src


main :: IO ()
main = do
  putStrLn "hello world"
