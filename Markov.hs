module Markov where

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM
import Data.List
import GHC.Generics (Generic)
import Data.Hashable
import System.Random
import Control.Monad.Random

type Rnd a = Rand StdGen a

data State = Word String | Stop | Open Open
  deriving (Eq, Show)

data Open = LParen | RParen | DQuote | SQuote
  deriving (Eq, Show)

opChar :: Open -> Char
opChar LParen = '('
opChar RParen = ')'
opChar DQuote = '\"'
opChar SQuote = '\''
  
instance Hashable State where
  hashWithSalt salt (Open c) = hashWithSalt 2 $ hashWithSalt salt (opChar c)
  hashWithSalt salt Stop = hashWithSalt 0 salt
  hashWithSalt salt (Word w) = hashWithSalt 1 $ hashWithSalt salt w
  
type MChain = HashMap State (HashMap State Int)

createMChain :: [State] -> MChain -> MChain  --creates new Markov-Chain given a list of states and an old chain
createMChain [] m = m
createMChain (s:[]) m = HM.insertWith (HM.unionWith (+)) s (HM.singleton Stop 1) m  
createMChain (s1:s2:ss) m = createMChain (s2:ss) $ HM.insertWith (HM.unionWith (+)) s1 (HM.singleton s2 1) m



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
--getState l@Leaf{} _ = state l
getState tree rank
  | rank <= wl = getState (left tree) rank
  | rank > wl + freq tree = getState (right tree) (rank - wl - (freq tree))
  | otherwise = state tree
    where wl = weight $ left tree
    
generateStates :: State -> MChain -> Rnd [State]
generateStates oldState m
  | oldState == Stop = return []
  | otherwise = do
      let stateList = toList (m ! oldState)
      let stateTree = constructSTree stateList
      target <- getRandomR (1, weight stateTree)
      let newState = getState stateTree target
      states <- generateStates newState m
      return $ oldState : states
    
          
