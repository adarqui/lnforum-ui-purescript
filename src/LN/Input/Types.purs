module LN.Input.Types (
  Input (..),
  -- helpers
  cMembership,
  cMembershipMod,
  cMembershipAct,
  cResource,
  cResourceMod,
  cLeuron,
  cLeuronMod,
  cArrayString
) where



import Data.Foreign            (ForeignError)
import Purescript.Api.Helpers  (ApiError)

import LN.Input.ArrayString    (InputArrayString)
import LN.Input.Leuron         (InputLeuron(..), Leuron_Mod)
import LN.Input.Membership     (InputMembership(..), Membership_Act, Membership_Mod)
import LN.Input.OrderBy        (InputOrderBy)
import LN.Input.Profile        (InputProfile)
import LN.Input.Resource       (InputResource(..), Resource_Mod)
import LN.Router.Class.Routes  (Routes)
import LN.T



data Input a
  = Goto Routes a

  | AddError String String a
  | AddErrorF String ForeignError a
  | AddErrorApi String ApiError a
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

  | GetLeurons a
  | GetLeuronId Int a
  | GetLeuronRandom a

  | ConnectSocket a

  | CompArrayString    InputArrayString  a
  | CompOrderBy        InputOrderBy      a
  | CompMembership     InputMembership   a
  | CompProfile        InputProfile      a
  | CompResource       InputResource     a
  | CompLeuron         InputLeuron       a

  | Nop a



-- | Helpers for "components" and "subcomponents"
--

cMembership :: forall a. InputMembership -> a -> Input a
cMembership sub next = CompMembership sub next

cMembershipMod :: forall a. Membership_Mod -> a -> Input a
cMembershipMod mod next = CompMembership (InputMembership_Mod mod) next

cMembershipAct :: forall a. Membership_Act -> a -> Input a
cMembershipAct act next = CompMembership (InputMembership_Act act) next



cResource :: forall a. InputResource -> a -> Input a
cResource ir next = CompResource ir next

cResourceMod :: forall a. Resource_Mod -> a -> Input a
cResourceMod rm next = CompResource (InputResource_Mod rm) next



cLeuron :: forall a. InputLeuron -> a -> Input a
cLeuron sub next = CompLeuron sub next

cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
cLeuronMod mod next = CompLeuron (InputLeuron_Mod mod) next



cArrayString :: forall a. InputArrayString -> a -> Input a
cArrayString sub next = CompArrayString sub next
