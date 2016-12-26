module Data.AppState where

import Control.Monad.Eff.JQuery (JQuery)

import Data.Generic (class Generic)
import Data.Map as M


type CardKey = String

newtype AppState = AppState
  { isLoading :: Boolean
  , visibleCards :: M.Map CardKey JQuery
  , selectedCities :: Array SelectedCity
  , spinner :: JQuery
  , cardTemplate :: JQuery
  , container :: JQuery
  , addDialog :: JQuery
  }

newtype SelectedCity = SelectedCity
  { key :: CardKey
  , label :: String
  }

derive instance genericSelectedCity :: Generic SelectedCity

data StorageKey a = SelectedCitiesKey

derive instance genericStorageKey :: Generic (StorageKey a)

-- Make "smart constructor" for each key
selectedCitiesKey :: StorageKey (Array SelectedCity)
selectedCitiesKey = SelectedCitiesKey
