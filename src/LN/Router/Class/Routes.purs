module LN.Router.Class.Routes (
  Routes (..),
  class HasCrumb,
  crumb
) where



import Data.Maybe                  (maybe)
import Data.Tuple                  (Tuple(..), fst)
import Optic.Core                  ((^.), (..))
import Prelude (class Show, show, ($), (<>))

import LN.T
import LN.Router.Util              (slash)
import LN.Router.Class.CRUD (CRUD(..))
import LN.Router.Class.Params      (Params, emptyParams, fixParams)
import LN.Router.Class.Link (class HasLink, link)
import LN.Router.Class.OrderBy (class HasOrderBy)
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
  | Buckets CRUD Params
  | BucketsLeurons Int CRUD Params
  | BucketsResources Int CRUD Params
  | BucketsRounds Int CRUD Params
  | Login
  | Logout
  | NotFound
  -- | Other
  | ViewExamples



{-
instance routersEq :: Eq Routes where
  eq Home Home = true
  eq About About = true
  eq Me Me  = true
  eq Errors Errors = true
  eq Portal Portal = true
  eq (Users c1 p1) (Users c2 p2) = c1 == c2 && p1 == p2
  eq (Buckets c1 p1) (Buckets c2 p2) = c1 == c2 && p1 == p2

  eq _ _ = false
  -}

  {-
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
  | Buckets CRUD Params
  | BucketsLeurons Int CRUD Params
  | BucketsResources Int CRUD Params
  | BucketsRounds Int CRUD Params
  | Login
  | Logout
  | NotFound
  -- | Other
  | ViewExamples
  -}
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

  link (Buckets crud params) = Tuple ("#/buckets" <> (fst $ link crud)) (fixParams params)
  link (BucketsResources bucket_id crud params) = Tuple ("#/buckets/" <> show bucket_id <> "/resources" <> (fst $ link crud)) (fixParams params)
  link (BucketsLeurons bucket_id crud params)   = Tuple ("#/buckets/" <> show bucket_id <> "/leurons" <> (fst $ link crud)) (fixParams params)
  link (BucketsRounds bucket_id crud params)  = Tuple ("#/buckets/" <> show bucket_id <> "/rounds" <> (fst $ link crud)) (fixParams params)

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



      Buckets Index params ->
        [Tuple (Buckets Index params) "Buckets"]

      Buckets New params ->
        [Tuple (Buckets Index params) "Buckets"]

      Buckets (EditI bucket_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id params
        ]

      Buckets (DeleteI bucket_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id params
        ]

      Buckets (ShowI bucket_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id params
        ]



      BucketsResources bucket_id Index params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsResources bucket_id Index params) "Resources"
        ]

      BucketsResources bucket_id New params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsResources bucket_id Index params) "Resources"
        ]

      BucketsResources bucket_id (EditI resource_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsResources bucket_id Index emptyParams) "Resources",
          Tuple (BucketsResources bucket_id (ShowI resource_id) emptyParams) (show resource_id)
        ]

      BucketsResources bucket_id (DeleteI resource_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsResources bucket_id Index emptyParams) "Resources",
          Tuple (BucketsResources bucket_id (ShowI resource_id) emptyParams) (show resource_id)
        ]

      BucketsResources bucket_id (ShowI resource_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsResources bucket_id Index emptyParams) "Resources",
          Tuple (BucketsResources bucket_id (ShowI resource_id) params) (show resource_id)
        ]





      BucketsLeurons bucket_id Index params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsLeurons bucket_id Index params) "Leurons"
        ]

      BucketsLeurons bucket_id New params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsLeurons bucket_id Index params) "Leurons"
        ]

      BucketsLeurons bucket_id (EditI leuron_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsLeurons bucket_id Index emptyParams) "Leurons",
          Tuple (BucketsLeurons bucket_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      BucketsLeurons bucket_id (DeleteI leuron_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsLeurons bucket_id Index emptyParams) "Leurons",
          Tuple (BucketsLeurons bucket_id (ShowI leuron_id) emptyParams) (show leuron_id)
        ]

      BucketsLeurons bucket_id (ShowI leuron_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsLeurons bucket_id Index emptyParams) "Leurons",
          Tuple (BucketsLeurons bucket_id (ShowI leuron_id) params) (show leuron_id)
        ]





      BucketsRounds bucket_id Index params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsRounds bucket_id Index params) "Rounds"
        ]

      BucketsRounds bucket_id New params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsRounds bucket_id Index params) "Rounds"
        ]

      BucketsRounds bucket_id (EditI rounds_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsRounds bucket_id Index emptyParams) "Rounds",
          Tuple (BucketsRounds bucket_id (ShowI rounds_id) emptyParams) (show rounds_id)
        ]

      BucketsRounds bucket_id (DeleteI rounds_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsRounds bucket_id Index emptyParams) "Rounds",
          Tuple (BucketsRounds bucket_id (ShowI rounds_id) emptyParams) (show rounds_id)
        ]

      BucketsRounds bucket_id (ShowI rounds_id) params ->
        [
          Tuple (Buckets Index emptyParams) "Buckets",
          bucket_pretty bucket_id emptyParams,
          Tuple (BucketsRounds bucket_id Index emptyParams) "Rounds",
          Tuple (BucketsRounds bucket_id (ShowI rounds_id) params) (show rounds_id)
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

    bucket_pretty bucket_id params =
      Tuple (Buckets (ShowI bucket_id) params)
        $ maybe (show bucket_id) (\pack -> show $ pack ^. _BucketPackResponse .. bucket_ ^. _BucketResponse .. id_) st.currentBucket



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
  show (Buckets crud params)          = "Buckets" <> show crud
  show (BucketsResources bucket_id crud params) = "BucketsResources " <> show bucket_id <> sp <> show crud
  show (BucketsLeurons bucket_id crud params)   = "BucketsLeurons " <> show bucket_id <> sp <> show crud
  show (BucketsRounds bucket_id crud params)  = "BucketsRounds " <> show bucket_id <> sp <> show crud
  show Login    = "Login"
  show Logout   = "Logout"
  show NotFound = "NotFound"
  show ViewExamples = "ViewExamples"
  show _ = "make sure Show covers all Routes"



sp :: String
sp = " "
