module Bible.Data.Bible where

import Data.Map (Map)

type Bible = Array Book

type Book
  = { name :: String
    , abbrev :: String
    , chapters :: Map Int Chapter
    }

type Chapter = Map Int String
