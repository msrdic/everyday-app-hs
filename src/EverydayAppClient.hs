module EverydayAppClient ( mark, skip, unmark ) where

import Data.Text ( Text )
import System.FilePath ((<.>), (</>))
import Control.Lens ((.~), (&), (^.))

import qualified Data.Configurator as DC
import Network.Wreq (statusCode, postWith, defaults, header, responseStatus)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Data.Aeson (object, ToJSON, toJSON, (.=))

type YYMMDD = Text
type HabitId = Int

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