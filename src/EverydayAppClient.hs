module EverydayAppClient ( mark, skip, unmark ) where

import qualified Data.Configurator as DC
import System.FilePath ((<.>), (</>))

import Data.Maybe (fromJust)
import Data.List (nubBy)
import Data.Text ( Text )

import Control.Lens ((.~), (&), (^.))
import Data.Aeson.Lens ( AsValue(_Array) )

import Network.Wreq ( Options
                    , getWith, postWith, defaults, header
                    , statusCode, responseBody, responseStatus)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson ( Result (..), FromJSON, ToJSON
                  , fromJSON, toJSON, parseJSON
                  , withObject, object, (.=), (.:))

import qualified Data.Vector as V

type YYMMDD = Text
type HabitId = Int

data Habit =
    Habit { _id :: Int, _name :: Text } deriving (Show)

instance FromJSON Habit where
    parseJSON = withObject "selector" $ \o -> Habit <$> o .: "id" <*> o .: "name"

data HabitPayload =
    HabitPayload { _date :: Text, _habitId :: Int }

data Status = OK | NotOK deriving (Show)

instance ToJSON HabitPayload where
    toJSON (HabitPayload d hid) = object ["date" .= d, "habit_id" .= hid]

mark   :: YYMMDD -> HabitId -> IO Status
skip   :: YYMMDD -> HabitId -> IO Status
unmark :: YYMMDD -> HabitId -> IO Status

_getAuthToken :: FilePath -> IO (Maybe Text)
_getAuthToken path = do
  config <- DC.load [DC.Required path]
  DC.lookup config "token"

_fromResult :: Result a -> a
_fromResult (Success r) = r
_fromResult (Error e) = error e

_getOpts :: IO Options
_getOpts = do
    token <- _getAuthToken ("conf" </> "auth" <.> "config")
    let authToken = if token == Nothing
                      then error "Authorization token not configured."
                      else fromJust token
    let opts = defaults & header "Authorization" .~ [encodeUtf8 authToken]
    return opts

_habits :: IO [Habit]
_habits = do
    opts <- _getOpts
    resp <- getWith opts "https://api.everyday.app/habits/"
    return $ nubBy _comparingIds $ V.toList $ V.map _fromResult $ V.map fromJSON $ resp ^. responseBody . _Array

_comparingIds :: Habit -> Habit -> Bool
_comparingIds (Habit id1 _) (Habit id2 _) = id1 == id2

mark d h = do
    opts <- _getOpts
    let payload = HabitPayload d h
    resp <- postWith opts "https://api.everyday.app/mark" (toJSON payload)
    let status = resp ^. responseStatus . statusCode
    return $ if status == 200 then OK else NotOK

skip d h = do
    opts <- _getOpts
    let payload = HabitPayload d h
    resp <- postWith opts "https://api.everyday.app/skip" (toJSON payload)
    let status = resp ^. responseStatus . statusCode
    return $ if status == 200 then OK else NotOK

unmark d h = do
    opts <- _getOpts
    let payload = HabitPayload d h
    resp <- postWith opts "https://api.everyday.app/unmark" (toJSON payload)
    let status = resp ^. responseStatus . statusCode
    return $ if status == 200 then OK else NotOK