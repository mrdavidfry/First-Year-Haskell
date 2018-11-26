module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp word list
  = [ a | (strng, a) <- list, strng == word ]


lookUpDef :: String -> KeywordDefs -> String
lookUpDef word ((x1, x2) : xs)
  | word == x1 = x2
  | otherwise  = lookUpDef word xs


split :: [Char] -> String -> (String, [String])
split separs sentence = split' (sentence)
    where
      split' [] = ("", [""])
      split' (x : xs)
        | elem x separs = (x : separs', "" : wrd : sentence)
        | otherwise     = (separs', ((x : wrd) : sentence) )
        where
          (separs', (wrd : sentence)) = split' xs


combine :: String -> [String] -> [String]
combine [] wordList       = wordList
combine separs []         = [separs]
combine (x : xs) (y : ys) = y : [x] : (combine xs ys)


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (x : xs)
  |  xs == [] = [elemPair]
  | otherwise = elemPair : getKeywordDefs xs
  where
    elemPair = (w1, concat (combine separs remWords) )
    (fstSepar : separs, (w1: remWords) ) = split " " x 


expand :: FileContents -> FileContents -> FileContents
expand sentence definString
  = concat (combine separs [ replaceWord word definList | word <- wordList ] )
  where
    definList          = getKeywordDefs definList'
    (_, definList')    = split "\n" definString
    (separs, wordList) = split separators sentence

    replaceWord :: String -> KeywordDefs -> String
    replaceWord "" _definitions = ""
    replaceWord word@(fstLet : remWord) definitions
      | fstLet == '$' = lookUpDef word definitions
      | otherwise     = word

{-
enhancedExpand :: FileContents -> FileContents -> String
enhancedExpand paragraph [] = []
enhancedExpand paragraph definString
  = concat( "-----" : [ expand paragraph y | y <- list])
  where
    (_separs, list@(x : xs)) = split "#" definString
-}

enhancedExpand :: FileContents -> FileContents -> String
enhancedExpand paragraph [] = []
enhancedExpand paragraph definString
  = concat( "-----" : [ expand paragraph y | y <- defList])
  where
    (_separs, defList) = split "#" definString


main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
