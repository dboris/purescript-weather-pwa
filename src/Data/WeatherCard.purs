module Data.WeatherCard where

import Prelude


type CardKey = String
type ForcastData =
  { code :: Int
  , high :: Int
  , low :: Int
  }

type WeatherChannel =
  { astronomy :: { sunrise :: String
                 , sunset :: String
                 }
  , item :: { condition :: { text :: String
                           , date :: String
                           , temp :: Int
                           , code :: Int
                           }
            , forecast :: Array ForcastData
            }
  , atmosphere :: { humidity :: Int }
  , wind :: { speed :: Int
            , direction :: Int
            }
  }

type WeatherCard =
  { key :: CardKey
  , label :: String
  , created :: String
  , channel :: WeatherChannel
  }
