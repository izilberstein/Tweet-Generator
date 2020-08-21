module Markov (MChain, State(..), Open(..), newMChain, createMChain, generateStates, textify, Rnd, getKeys) where

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Hashable
import Data.Char
import System.Random
import Control.Monad.Random

type Rnd a = Rand StdGen a

data State = Word String | Stop | Open Open | Duo String String
  deriving (Eq, Show)

data Open = LParen | RParen | DQuote | SQuote
  deriving (Eq, Show)

opChar :: Open -> Char
opChar LParen = '('
opChar RParen = ')'
opChar DQuote = '\"'

transform :: State -> State -> State
transform (Word w1) (Word w2) = Duo w1 w2
transform _ s = s   
  
instance Hashable State where
  hashWithSalt salt (Open c) = hashWithSalt 2 $ hashWithSalt salt (opChar c)
  hashWithSalt salt Stop = hashWithSalt 0 salt
  hashWithSalt salt (Word w) = hashWithSalt 1 $ hashWithSalt salt w
  hashWithSalt salt (Duo w1 w2) = hashWithSalt 3 $ hashWithSalt salt (w1++" "++w2)
  
type MChain = HashMap State (HashMap State Int)

newMChain = HM.empty

createMChain :: [State] -> MChain -> MChain  --creates new Markov-Chain given a list of states and an old chain
createMChain [] m = m
createMChain (s:[]) m = HM.insertWith (HM.unionWith (+)) s (HM.singleton Stop 1) m  
createMChain (s1:s2:[]) m = createMChain (s2:[]) $ HM.insertWith (HM.unionWith (+)) (transform s1 s2) (HM.singleton Stop 1) $ HM.insertWith (HM.unionWith (+)) s1 (HM.singleton s2 1) m
createMChain (s1:s2:s3:ss) m = createMChain (s2:s3:ss) $ HM.insertWith (HM.unionWith (+)) (transform s1 s2) (HM.singleton s3 1) $ HM.insertWith (HM.unionWith (+)) s1 (HM.singleton s2 1) m



data STree = Leaf { state :: State, weight ::  Int }
           | Node { left :: STree, state ::  State, freq :: Int, weight :: Int, right :: STree }
  deriving (Show)

constructSTree :: [(State,Int)] -> STree
constructSTree [] = Leaf Stop 0
constructSTree ((s, f):ss) = Node lTree s f (f + (weight lTree) + (weight rTree)) rTree
  where half = div (length ss) 2
        lTree = constructSTree $ take half ss
        rTree = constructSTree $ drop half ss

getState :: STree -> Int -> State
getState tree rank
  | rank <= wl = getState (left tree) rank
  | rank > wl + freq tree = getState (right tree) (rank - wl - (freq tree))
  | otherwise = state tree
    where wl = weight $ left tree
    
generateStates :: State -> MChain -> Rnd [State]
generateStates s@(Duo s1 s2) m = do
  let check = (HM.lookup s m)
  case check of
    Nothing -> generateStates (Word s1) m
    Just a -> do
      let stateList = toList $ HM.unionWith (\x y -> x+2*y) (m ! (Word s2)) a
      let stateTree = constructSTree stateList
      target <- getRandomR (1, weight stateTree)
      let newState = getState stateTree target
      states <- generateStates (transform (Word s2) newState) m
      return $ (Word s2) : states
generateStates oldState m
  | oldState == Stop = return []
  | otherwise = do
      let stateList = toList (m ! oldState)
      let stateTree = constructSTree stateList
      target <- getRandomR (1, weight stateTree)
      let newState = getState stateTree target
      states <- generateStates (transform oldState newState) m
      return $ oldState : states

         
textify :: [State] -> String
textify (Open s : []) = [opChar s]
textify (Word [c] : [])
  | c == 'i' = "I"
  | otherwise = [c]
textify (Word s : []) = s
textify (state : Open s : ss) = case s of
                          LParen -> space state (Open s : ss)
                          RParen -> noSpace state (Open s : ss)
                          DQuote -> noSpace state (Open s : ss) 
textify (state : Word [c] : ss)
  | c `elem` [',', ':', ';'] = noSpace state (Word [c] : ss)
  | c `elem` ['.', '?', '!'] = noSpace state (Word [c] : toCap ss)
  | otherwise = space state (Word [c] : ss)
textify (state : ss) = space state ss 

noSpace :: State -> [State] -> String
noSpace state ss = textify [state] ++ textify ss
space :: State -> [State] -> String
space s@(Open a) ss
  | a == RParen = noSpace s ss
  | otherwise = textify [s] ++ " " ++ textify ss
space s@(Word [c]) ss
  |  c `elem` ['$', '#', '@', '%'] = noSpace s ss
  | otherwise = textify [s] ++ " " ++ textify ss
space state ss = textify [state] ++ " " ++ textify ss
        

toCap :: [State] -> [State]
toCap s@(Open o : _ ) = s
toCap (Word (w:ws) : ss) = (Word (toUpper w : ws) : ss)


getKeys :: MChain -> [State]
getKeys chain = keys chain 
  
