module Main where

import Data.List
import System.Process

revcat [] b = b
revcat (a:as) b = revcat as (a:b)

splitOn c s = f c [] s
  where f c x [] = [reverse x]
        f c x (y:ys) | y == c = reverse x : f c [] ys
                     | otherwise = f c (y:x) ys

lstrip [] = []
lstrip t@(x:xs) | elem x [' ', '\t', '\r', '\n'] = lstrip xs
                | otherwise = t

strip = reverse . lstrip . reverse . lstrip

replacePPP to [] = []
replacePPP to ('%':'%':'%':xs) = to ++ replacePPP to xs
replacePPP to (x:xs) = x : replacePPP to xs

pass :: Char -> String -> (String, String)
pass' c x [] = (reverse x, [])
pass' c x (y:ys) | y == c = (reverse x, ys)
                 | otherwise = pass' c (y:x) ys
pass c x = pass' c [] x

isVowel c = elem c ['a', 'e', 'i', 'o', 'u']


suffix' a b@(h:t) | isVowel h = revcat (f isVowel ra) b
                  | otherwise = revcat (f (not . isVowel) ra) b
  where ra = reverse a
        f c [] = []
        f c t@(x:xs) | c x = f c xs
                     | otherwise = t
suffix a ('-':b) = suffix' a b
suffix a b = suffix' a b

---------
data WordSex = Masculine | Feminine | Neuter | MF
  deriving Eq
data WordT = Noun String String WordSex
           | Verb String String String String
           | Adj String String String
           | Adv String
           | Prep String
           | Other String
  deriving Eq
type Meaning = String
type Comment = String
data WordSet = WordSet WordT Meaning Comment
type WordList = [WordSet]

instance Show WordSex where
  show Masculine = "m"
  show Feminine = "f"
  show Neuter = "n"
  show MF = "mf"
instance Show WordT where
  show (Noun a b s) = "n: " ++ a ++ "," ++ b ++ "," ++ show s
  show (Verb a b c d) = "v: " ++ a ++ "," ++ b ++ "," ++ c ++ "," ++ d
  show (Adj a b c) = "adj: " ++ a ++ "," ++ b ++ "," ++ c
  show (Adv s) = "adv: " ++ s
  show (Prep s) = "prep: " ++ s
  show (Other s) = "o: " ++ s
instance Show WordSet where
  show (WordSet t m c) = show t ++ " [" ++ m ++ "] " ++ c

instance Ord WordT where
  (Noun a _ _) <= (Noun b _ _) = a <= b
  (Verb a _ _ _) <= (Verb b _ _ _) = a <= b
  (Adj a _ _) <= (Adj b _ _) = a <= b
  (Adv a) <= (Adv b) = a <= b
  (Prep a) <= (Prep b) = a <= b
  (Other a) <= (Other b) = a <= b
  (Noun _ _ _) <= _ = True
  _ <= (Noun _ _ _) = False
  (Verb _ _ _ _) <= _ = True
  _ <= (Verb _ _ _ _) = False
  (Adj _ _ _) <= _ = True
  _ <= (Adj _ _ _) = False
  (Adv _) <= _ = True
  _ <= (Adv _) = False
  (Prep _) <= _ = True
  _ <= (Prep _) = False

instance Eq WordSet where
  (WordSet a _ _) == (WordSet b _ _) = a <= b && a >= b

instance Ord WordSet where
  (WordSet a _ _) <= (WordSet b _ _) = a <= b


cmpLe (WordSet a1 a2 _) (WordSet b1 b2 _) = case (a1, b1) of
  (Noun a _ _, Noun b _ _) -> compare a2 b2
  (Verb a _ _ _, Verb b _ _ _) -> compare a2 b2
  (Adj a _ _, Adj b _ _) -> compare a2 b2
  (Adv a, Adv b) -> compare a2 b2
  (Prep a, Prep b) -> compare a2 b2
  (Other a, Other b) -> compare a2 b2
  (Noun _ _ _, _) -> LT
  (_, Noun _ _ _) -> GT
  (Verb _ _ _ _, _) -> LT
  (_, Verb _ _ _ _) -> GT
  (Adj _ _ _, _) -> LT
  (_, Adj _ _ _) -> GT
  (Adv _, _) -> LT
  (_, Adv _) -> GT
  (Prep _, _) -> LT
  (_, Prep _) -> GT

showMean (WordSet t m c) = "[" ++ m ++ "] " ++ show t ++ " " ++ c

parseSex x = case x of
  "m" -> Masculine
  "f" -> Feminine
  "n" -> Neuter
  "mf" -> MF

parseNoun x = Noun l1 l2 l3
  where l = splitOn ',' x
        l1 = strip $ l !! 0
        l2 = case strip $ l !! 1 of
          '-':x -> suffix l1 x
          x -> x
        l3 = parseSex $ l !! 2
parseVerb x = Verb l1 l2 l3 l4
  where l = splitOn ',' x
        l1 = strip $ l !! 0
        l2 = case strip $ l !! 1 of
          '-':x -> suffix l1 x
          x -> x
        l3 = case strip $ l !! 2 of
          '-':x -> suffix l1 x
          x -> x
        l4 = case strip $ l !! 3 of
          '-':x -> suffix l1 x
          x -> x
parseAdj x = Adj l1 l2 l3
  where l = splitOn ',' x
        l1 = strip $ l !! 0
        l2 = case strip $ l !! 1 of
          '-':x -> suffix l1 x
          x -> x
        l3 = case strip $ l !! 2 of
          '-':x -> suffix l1 x
          x -> x
parseAdv x = Adv x
parsePrep x = Prep x
parseOther x = Other x

parseLine :: String -> [WordSet]
parseLine x = w''
  where (ty, r1) = pass ':' x
        (w, r2) = pass '[' r1
        (m, r3) = pass ']' r2
        m' = strip m
        r' = strip r3
        w' = strip w
        w'' = case strip ty of
          "n" -> [WordSet (parseNoun w') m' r']
          "v" -> [WordSet (parseVerb w') m' r']
          "adj" -> [WordSet (parseAdj w') m' r']
          "adv" -> [WordSet (parseAdv w') m' r']
          "prep" -> [WordSet (parsePrep w') m' r']
          "o" -> [WordSet (parseOther w') m' r']
          _ -> []

parse src = l >>= parseLine
  where l = map strip $ splitOn '\n' src

texfy s1 s2 = s1' ++ "\\newline\\newline " ++ s2'
  where s1' = concat $ map ((++ "\\newline ") . show) s1
        s2' = concat $ map ((++ "\\newline ") . showMean) s2

main :: IO ()
main = do
  s <- readFile "DICT"
  let r = parse s
  let r1 = sort r
  let r2 = sortBy cmpLe r
  putStrLn $ show r1
  writeFile "DICT_" s
  writeFile "DICT" $ concat $ map ((++ "\n") . show) r1
  t <- readFile "TEMPLATE.tex"
  let t' = replacePPP (texfy r1 r2) t
  writeFile "OUT.tex" t'
  system "pdflatex OUT.tex > LOG"
  system "rm OUT.aux OUT.log"
  return ()
