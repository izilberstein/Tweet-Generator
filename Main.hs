module Main where

import Markov
import Parser
import Control.Monad.Random
import Jason

main :: IO ()
main = do
  file <- parseFile "trump3.json"
  case file of
    Left msg -> error $ show msg
    Right n -> do  
      let chain = chainIt n
      let chainKeys = getKeys chain
      putStrLn "Tweet (y/n)?"
      cont <- getLine
      if cont == "n"
        then putStrLn "No tweets today."
        else go cont chain chainKeys

          
chainIt :: [Tweet] -> MChain
chainIt (Tweet s :[]) = do
  let states = fullParse s
  case states of
    Left msg -> error $ show msg
    Right n -> createMChain n newMChain
chainIt (Tweet s :ts) = do
  let states = fullParse s
  case states of
    Left msg -> error $ show msg
    Right n -> createMChain n $ chainIt ts


  
go :: String -> MChain -> [State] -> IO ()
go c chain chainKeys
  | c == "n" = putStrLn "No tweets today."
  | otherwise = do
      seed <- evalRandIO $ getWord chainKeys
      let tweet = textify <$> generateStates seed chain
      result <- evalRandIO $ tweet
      putStrLn result
      putStrLn "Tweet again (y/n)?"
      cont <- getLine
      go cont chain chainKeys

getWord :: [State] -> Rnd State
getWord s = do
  i <- getRandomR (0, length s-1)
  return $ s !! i
