module Parser (fullParse, Parser) where 

import Data.Char
import Markov
import qualified Text.Parsec as P

type Parser = P.Parsec String ()

{- parseTweet :: String -> [State]
parseTweet tweet = concatMap (processString) $ tweetToStrings tweet

isOpen :: Char -> Bool
isOpen c
  | c == ')' = True
  | c == '(' = True
  | c == '\"' = True
  | otherwise = False

  
tweetToStrings :: String -> [String]
tweetToStrings s = words s



processString :: String -> [State]
processString (c:[])
  | isOpen c = [toOpen c]
  | otherwise = [toState [c]]
processString s
  | length opens > 0 = concatMap (processString) $ parseAt s opens
  | otherwise = [toState s]
  where opens = findIndices (isOpen) s
        

parseAt :: String -> [Int] -> [String]
parseAt s (i:[]) = (take i s) : [s !! i] :  [drop (i+1) s] 
parseAt s (i:is) = (take i s) : [s !! i] : parseAt (drop (i+1) s) is

toState :: String -> State
toState s = Word s

toOpen :: Char -> State
toOpen c
  | c == ')' = Open RParen
  | c == '(' = Open LParen
  | c == '\"' = Open DQuote


-}
fullParse :: String -> Either P.ParseError [State]
fullParse text = P.runParser parseStates () "Error" text

parseStates :: Parser [State]
parseStates = P.many $ P.spaces *> parseState <* P.spaces

parseState :: Parser State
parseState =
  parseNumber P.<|> parseWord P.<|> parsePunk

parseNumber :: Parser State
parseNumber = Word <$>  P.many1 (P.satisfy (\x -> isDigit x || x == ','))


parseWord :: Parser State
parseWord = Word  . map toLower <$> P.many1 (P.satisfy (\x -> isAlphaNum x || x == '\''))
                                                   

parsePunk :: Parser State
parsePunk = do
  c <- P.anyChar
  case c of
    '(' -> return $ Open LParen
    ')' -> return $ Open RParen
    '\"' -> return $ Open DQuote
    _ | isPunctuation c || isSymbol c -> return $ Word [c]
      | otherwise -> fail $ "got: " ++ show c
  
  
