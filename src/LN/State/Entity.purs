module LN.State.Entity (
  Entity,
  defaultEntity
) where



import Data.Maybe (Maybe(..))
import Data.Date.Helpers (Date, defaultDate)
import LN.Router.Types (Routes(..))



type Entity =
  { name        :: String
  , displayName :: String
  , createdAt   :: Maybe Date
  , logo        :: String
  , route       :: Routes
  }



defaultEntity :: Partial => Entity
defaultEntity =
  { name:        "unknown"
  , displayName: "unknown"
  , createdAt:   Just defaultDate
  , logo:        ""
  , route:       NotFound
}
