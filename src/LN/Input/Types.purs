module LN.Input.Types (
  Input (..),
  -- helpers
  cResource,
  cResourceMod,
  cLeuron,
  cLeuronMod,
  cBucket,
  cBucketMod,
  cArrayString
) where



import Data.Foreign            (ForeignError)
import Data.Maybe              (Maybe())
import Purescript.Api.Helpers  (ApiError, class QueryParam)

import LN.Input.ArrayString    (InputArrayString)
import LN.Input.Leuron         (InputLeuron(..), Leuron_Mod)
import LN.Input.Bucket         (InputBucket(..), Bucket_Mod)
import LN.Input.OrderBy        (InputOrderBy)
import LN.Input.Profile        (InputProfile)
import LN.Input.Resource       (InputResource(..), Resource_Mod)
import LN.Router.Class.Routes  (Routes)
import LN.T



data Input a
  = Goto Routes a
  | AddError String String a
  | AddErrorF String ForeignError a
  | AddErrorApi String (ApiError ApplicationError) a
  | DelError Int a
  | ClearErrors a
  | GetUser String a
  | GetMe a
  | GetUsers a
  | GetUsers_MergeMap_ByUser (Array UserSanitizedResponse) a
  | GetUsers_MergeMap_ByUserId (Array Int) a
  | GetResources a
  | GetResourceId Int a
  | GetResourcesLeurons Int a
  | GetResourceLeuronLinear Int Int a
  | GetResourceLeuronRandom Int a
  | GetResourcesSiftLeurons Int a
  | GetLeurons (Maybe Int) (Array Param) a
  | GetLeuronId Int a
  | GetLeuronRandom a
  | GetBuckets a
  | GetBucketId Int a
  | ConnectSocket a
  | CompArrayString    InputArrayString  a
  | CompOrderBy        InputOrderBy      a
  | CompProfile        InputProfile      a
  | CompResource       InputResource     a
  | CompLeuron         InputLeuron       a
  | CompBucket         InputBucket       a
  | Nop a



-- | Helpers for "components" and "subcomponents"
--

cResource :: forall a. InputResource -> a -> Input a
cResource ir next = CompResource ir next

cResourceMod :: forall a. Resource_Mod -> a -> Input a
cResourceMod rm next = CompResource (InputResource_Mod rm) next



cLeuron :: forall a. InputLeuron -> a -> Input a
cLeuron sub next = CompLeuron sub next

cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
cLeuronMod mod next = CompLeuron (InputLeuron_Mod mod) next



cBucket :: forall a. InputBucket -> a -> Input a
cBucket sub next = CompBucket sub next

cBucketMod :: forall a. Bucket_Mod -> a -> Input a
cBucketMod mod next = CompBucket (InputBucket_Mod mod) next



cArrayString :: forall a. InputArrayString -> a -> Input a
cArrayString sub next = CompArrayString sub next
