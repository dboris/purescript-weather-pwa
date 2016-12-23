module Data.AppState where

import Prelude

import Control.Monad.Eff.JQuery (JQuery)

import Data.Map as M
import Data.WeatherCard (CardKey)


newtype AppState = AppState
  { isLoading :: Boolean
  , visibleCards :: M.Map CardKey JQuery
  , spinner :: JQuery
  , cardTemplate :: JQuery
  , container :: JQuery
  , addDialog :: JQuery
  }
