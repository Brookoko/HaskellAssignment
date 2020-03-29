module Table where

import Data.List;

newtype Table = Table [Column] deriving (Show)
data Column = Column {
  header :: String,
  content :: [Maybe String]
} deriving (Show)

fromArray = unpack . uncons
 where
  unpack (Just (name, d)) = Column name (map pack d)
  pack x = if null x then Nothing else Just x
