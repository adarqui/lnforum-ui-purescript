module LN.Router.Class.Routes (
  Routes (..),
  class HasCrumb,
  crumb
) where



import Data.Generic                (class Generic, gEq)
import Data.Map                    as M
import Data.Maybe                  (maybe)
import Data.Tuple                  (Tuple(..), fst)
import Optic.Core                  ((^.), (..))
import Prelude                     (class Eq, class Show, show, (<>), ($), (==))

import LN.T
import LN.Router.Util              (slash)
import LN.Router.Class.CRUD
import LN.Router.Class.Params      (Params, emptyParams, fixParams)
import LN.Router.Class.Link
import LN.Router.Class.OrderBy
import LN.State.Internal.Types     (InternalState)
-- import LN.State.Types              (DriverCh)



data Routes
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Users CRUD Params
  | UsersProfile String Params
  | UsersSettings String Params
  | UsersResources String Params
  | UsersLeurons String Params
  | Resources CRUD Params
  | ResourcesLeurons Int CRUD Params
  | ResourcesSiftLeurons Int Params
  | ResourcesSiftLeuronsLinear Int CRUD Params
  | ResourcesSiftLeuronsRandom Int Params
  | Leurons CRUD Params
  | Login
  | Logout
  | NotFound
  -- Other
  | ViewExamples



-- derive instance genericRoutes :: Generic Routes



-- instance eqRoute :: Eq Routes where eq = gEq



class HasCrumb a where
  crumb :: a -> InternalState Routes -> Array (Tuple Routes String)



instance routesHasLink :: HasLink Routes where

  link Home   = Tuple "#/"       emptyParams

  link About  = Tuple "#/about"  emptyParams

  link Me     = Tuple "#/me"     emptyParams

  link Errors = Tuple "#/errors" emptyParams

  link Portal = Tuple "#/portal" emptyParams

  link (Users Index params)           = Tuple "#/u" (fixParams params)
  link (Users crud params)            = Tuple ("#/u" <> (fst $ link crud)) (fixParams params)
  link (UsersProfile user params)     = Tuple ("#/u/" <> user <> "/profile") (fixParams params)
  link (UsersSettings user params)    = Tuple ("#/u/" <> user <> "/settings") (fixParams params)
  link (UsersResources user params)   = Tuple ("#/u/" <> user <> "/resources") (fixParams params)
  link (UsersLeurons user params)     = Tuple ("#/u/" <> user <> "/leurons") (fixParams params)

  link (Resources crud params)                              = Tuple ("#/resources" <> (fst $ link crud)) (fixParams params)

  link (ResourcesLeurons resource_id crud params)           = Tuple ("#/resources/" <> show resource_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeurons resource_id params) = Tuple ("#/resources/" <> show resource_id <> "/sift") (fixParams params)
  link (ResourcesSiftLeuronsLinear resource_id crud params) = Tuple ("#/resources/" <> show resource_id <> "/sift/linear" <> (fst $ link crud)) (fixParams params)
  link (ResourcesSiftLeuronsRandom resource_id params)      = Tuple ("#/resources/" <> show resource_id <> "/sift/random") (fixParams params)

  link (Leurons crud params) = Tuple ("#/leurons" <> (fst $ link crud)) (fixParams params)

  link Login    = Tuple "/auth/login" emptyParams
  link Logout   = Tuple "/auth/logout" emptyParams

  link NotFound = Tuple "#/404" emptyParams
  link ViewExamples = Tuple "#/view_examples" emptyParams




instance routesHasCrumb :: HasCrumb Routes where

  crumb route st =

    case route of



      Home   -> [Tuple Home "Home"]



      About  -> [Tuple About "About"]



      Me     -> [Tuple Me "Me"]



      Errors -> [Tuple Errors "Errors"]



      Portal -> [Tuple Portal "Portal"]



      Users Index params ->
        [
          Tuple (Users Index params) "Users"
        ]

      Users (Show user) params ->
        [
          Tuple (Users Index emptyParams) "Users",
          Tuple (Users (Show user) params) user
        ]



      UsersProfile user params ->
        [
          Tuple (Users Index emptyParams) "Users",
          Tuple (Users (Show user) emptyParams) user,
          Tuple (UsersProfile (slash user) params) "Profile"
        ]

      UsersSettings user params ->
        [
          Tuple (Users Index emptyParams) "Users",
          Tuple (Users (Show user) emptyParams) user,
          Tuple (UsersSettings (slash user) params) "Settings"
        ]

      UsersResources user params ->
        [
          Tuple (Users Index emptyParams) "Users",
          Tuple (Users (Show user) emptyParams) user,
          Tuple (UsersResources (slash user) params) "Resources"
        ]

      UsersLeurons user params ->
        [
          Tuple (Users Index emptyParams) "Users",
          Tuple (Users (Show user) emptyParams) user,
          Tuple (UsersLeurons (slash user) params) "Leurons"
        ]



      Resources Index params ->
        [Tuple (Resources Index params) "Resources"]

      Resources New params ->
        [Tuple (Resources Index params) "Resources"]

      Resources (EditI resource_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]

      Resources (DeleteI resource_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]

      Resources (ShowI resource_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id params
        ]



      ResourcesLeurons resource_id Index params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id New params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesLeurons resource_id Index params) "Leurons"
        ]

      ResourcesLeurons resource_id (EditI leuron_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      ResourcesLeurons resource_id (DeleteI leuron_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      ResourcesLeurons resource_id (ShowI leuron_id) params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesLeurons resource_id Index emptyParams) "Leurons",
          Tuple (ResourcesLeurons resource_id (ShowI leuron_id) params) (show leuron_id)
        ]



      ResourcesSiftLeurons resource_id params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsRandom resource_id params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesSiftLeurons resource_id params) "Sift"
        ]

      ResourcesSiftLeuronsLinear resource_id _ params ->
        [
          Tuple (Resources Index emptyParams) "Resources",
          resource_pretty resource_id emptyParams,
          Tuple (ResourcesSiftLeurons resource_id emptyParams) "Sift",
          Tuple (ResourcesSiftLeuronsLinear resource_id Index params) "Linear"
        ]


      Leurons Index params ->
        [Tuple (Leurons Index params) "Leurons"]

      Leurons New params ->
        [Tuple (Leurons Index params) "Leurons"]

      Leurons (EditI leuron_id) params ->
        [
          Tuple (Leurons Index emptyParams) "Leurons",
          leuron_pretty leuron_id params
        ]

      Leurons (DeleteI leuron_id) params ->
        [
          Tuple (Leurons Index emptyParams) "Leurons",
          leuron_pretty leuron_id params
        ]

      Leurons (ShowI leuron_id) params ->
        [
          Tuple (Leurons Index emptyParams) "Leurons",
          leuron_pretty leuron_id params
        ]



      ViewExamples -> [Tuple ViewExamples "ViewExamples"]



      _ -> [Tuple NotFound "Error"]



    where
    resource_pretty resource_id params =
      Tuple (Resources (ShowI resource_id) params)
        $ maybe (show resource_id) (\pack -> pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse .. displayName_) st.currentResource

    leuron_pretty leuron_id params =
      Tuple (Leurons (ShowI leuron_id) params)
        $ maybe (show leuron_id) (\pack -> show $ pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse .. id_) st.currentLeuron



instance routesHasOrderBy :: HasOrderBy Routes where
  orderBy _                   = []



instance routesShow :: Show Routes where
  show Home   = "Home"
  show About  = "About"
  show Me     = "Me"
  show Errors = "Errors"
  show Portal = "Portal"
  show (Users crud params)            = "Users " <> show crud
  show (UsersProfile user params)     = "UsersProfile " <> user
  show (UsersSettings user params)    = "UsersSettings " <> user
  show (UsersResources user params)   = "UsersResources " <> user
  show (UsersLeurons user params)     = "UsersLeurons " <> user
  show (Resources crud params)        = "Resources " <> show crud
  show (ResourcesLeurons resource_id crud params)           = "ResourcesLeurons " <> show resource_id <> sp <> show crud
  show (ResourcesSiftLeurons resource_id params)            = "ResourcesSiftLeurons " <> show resource_id
  show (ResourcesSiftLeuronsLinear resource_id crud params) = "ResourcesSiftLeuronsLinear " <> show resource_id <> sp <> show crud
  show (ResourcesSiftLeuronsRandom resource_id params)      = "ResourcesSiftLeuronsRandom " <> show resource_id
  show (Leurons crud params)          = "Leurons " <> show crud
  show Login    = "Login"
  show Logout   = "Logout"
  show NotFound = "NotFound"
  show ViewExamples = "ViewExamples"
  show _ = "make sure Show covers all Routes"



sp :: String
sp = " "
