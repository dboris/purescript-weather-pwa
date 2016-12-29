module Data.Foreign.WeatherService where

import Prelude

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


newtype Response = Response
  { query :: Query }

newtype Query = Query
  { created :: String
  , results :: Results }

newtype Results = Results
  { channel :: Channel }

newtype Channel = Channel
  { astronomy :: Astronomy
  , item :: Item
  , atmosphere :: Atmosphere
  , wind :: Wind }

newtype Astronomy = Astronomy
  { sunrise :: String
  , sunset :: String }

newtype Item = Item
  { condition :: Condition
  , forecast :: Array DailyForecast }

newtype Atmosphere = Atmosphere
  { humidity :: Int }

newtype Wind = Wind
  { speed :: Int
  , direction :: Int }

newtype Condition = Condition
  { text :: String
  , date :: String
  , temp :: Int
  , code :: Int }

newtype DailyForecast = DailyForecast
  { code :: Int
  , high :: Int
  , low :: Int }

derive instance genericResponse :: Generic Response _
derive instance genericQuery :: Generic Query _
derive instance genericResults :: Generic Results _
derive instance genericChannel :: Generic Channel _
derive instance genericAstronomy :: Generic Astronomy _
derive instance genericItem :: Generic Item _
derive instance genericAtmosphere :: Generic Atmosphere _
derive instance genericWind :: Generic Wind _
derive instance genericCondition :: Generic Condition _
derive instance genericDailyForecast :: Generic DailyForecast _

instance isForeignResponse :: IsForeign Response where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignQuery :: IsForeign Query where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignResults :: IsForeign Results where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignChannel :: IsForeign Channel where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignAstronomy :: IsForeign Astronomy where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignItem :: IsForeign Item where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignAtmosphere :: IsForeign Atmosphere where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignWind :: IsForeign Wind where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignCondition :: IsForeign Condition where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance isForeignDailyForecast :: IsForeign DailyForecast where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance showResponse :: Show Response where
  show = genericShow

instance showQuery :: Show Query where
  show = genericShow

instance showResults :: Show Results where
  show = genericShow

instance showChannel :: Show Channel where
  show = genericShow

instance showAstronomy :: Show Astronomy where
  show = genericShow

instance showItem :: Show Item where
  show = genericShow

instance showAtmosphere :: Show Atmosphere where
  show = genericShow

instance showWind :: Show Wind where
  show = genericShow

instance showCondition :: Show Condition where
  show = genericShow

instance showDailyForecast :: Show DailyForecast where
  show = genericShow
