{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Jason (parseFile, Tweet(..))  where

import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Aeson


import qualified Data.ByteString.Lazy as BS



data Tweet = Tweet { text :: String }
  deriving (Show)

instance FromJSON Tweet where
  parseJSON (Object v) =
    Tweet <$> v .: "text"
  parseJSON _ = mempty


instance ToJSON Tweet where
 toJSON Tweet{..} =
    object [ "text" .= text ]


parseFile :: FromJSON a => FilePath -> IO (Either String a)
parseFile f = do
  f1 <- BS.readFile f
  return $ eitherDecode f1
