module Parser (fullParse, Parser) where 

import Data.Char
import Markov
import qualified Text.Parsec as P

type Parser = P.Parsec String ()

fullParse :: String -> Either P.ParseError [State]
fullParse text = P.runParser parseStates () "Error" text

parseStates :: Parser [State]
parseStates = P.many $ parseState

parseState :: Parser State
parseState =
  P.spaces *> parseWord <* P.spaces P.<|> parsePunk <* P.spaces

--parseNumber :: Parser State
--parseNumber = Word <$>  P.many1 (P.satisfy (\x -> isDigit x || x == ','))


parseWord :: Parser State
parseWord =  Word . map toLower <$> P.many1 (P.satisfy (\x -> isAscii x && not (isSpace x))) 
  {-where
    go :: String -> Parser String
    go acc = do
      letters <- parseLetters
      puncs <- parsePunctuation
      if puncs == []
        then return $ acc ++ letters
        else peek <- P.optionMaybe $ P.try $ P.lookAhead $ P.anyChar
                     case peek of
                       Nothing -> return $ acc ++ letters    
                       Just c
                         | not (isSpace c) -> P.anyChar *> go (acc ++ letters ++ puncs) 
                       _ -> return $ acc ++ letters
                                                   
parseLetters :: Parser String
parseLetters = P.many1 (P.satisfy (\x -> isAscii x && not (isSpace x || isPunctuation x)))

parsePunctuation :: Parser String
parsePunctuation = P.many (P.satisfy (\x -> isPunctuation x))
-}

parsePunk :: Parser State
parsePunk = do
  c <- P.anyChar
  case c of 
    '(' -> return $ Open LParen
    ')' -> return $ Open RParen
    '\"' -> return $ Open DQuote
    _ | isPunctuation c || isSymbol c -> return $ Word [c]
      | otherwise -> fail $ "got: " ++ show c
  
  
