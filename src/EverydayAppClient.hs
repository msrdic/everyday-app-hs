module EverydayAppClient ( mark, skip, unmark ) where

import Data.Text ( Text )
import System.FilePath ((<.>), (</>))
import Control.Lens ((.~), (&), (^.))

import qualified Data.Configurator as DC
import Network.Wreq (responseBody, getWith, statusCode, postWith, defaults, header, responseStatus)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Data.Aeson (Result (..), fromJSON, (.:), withObject, parseJSON, FromJSON, object, ToJSON, toJSON, (.=))
import Data.Aeson.Lens ( AsValue(_Array) )
import Data.List (nub, sort)

import qualified Data.Vector as V

type YYMMDD = Text
type HabitId = Int

newtype Habit = Habit { _id :: Int } deriving (Show)
instance FromJSON Habit where
    parseJSON = withObject "selector" $ \o -> Habit <$> o .: "id"

data HabitPayload = HabitPayload { _date :: Text
                                 , _habitId :: Int
                                 }

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

_habitIds :: IO [Int]
_habitIds = do
    token <- _getAuthToken ("conf" </> "auth" <.> "config")
    let authToken = if token == Nothing
                      then error "Authorization token not configured."
                      else fromJust token
    let opts = defaults & header "Authorization" .~ [encodeUtf8 authToken]
    resp <- getWith opts "https://api.everyday.app/habits/"
    return $ nub $ sort $ map _id $ V.toList $ V.map _fromResult $ V.map fromJSON $ resp ^. responseBody . _Array

mark d h = do
    token <- _getAuthToken ("conf" </> "auth" <.> "config")
    let authToken = if token == Nothing
                      then error "Authorization token not configured."
                      else fromJust token
    let opts = defaults & header "Authorization" .~ [encodeUtf8 authToken]
        payload = HabitPayload d h
    resp <- postWith opts "https://api.everyday.app/mark" (toJSON payload)
    let status = resp ^. responseStatus . statusCode
    return $ if status == 200 then OK else NotOK

skip   = undefined
unmark = undefined