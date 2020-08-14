module Main where

import Markov
import Parser
import Control.Monad.Random


main :: IO ()
main = do
  txt <- readFile "trump.txt"
  let tweet = fullParse txt
  case tweet of
    Left msg -> error $ show msg
    Right n -> do
      result <- evalRandIO $ doStuff n
      putStrLn result
      
     
                
doStuff :: [State] -> Rnd String
doStuff oldStates = do
  let chain = createMChain oldStates newMChain
  textify <$> generateStates (Word "a") chain

