module LN.View.Users (
  renderView_Users,
  usersLayout,
  showUser
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (($), (<>))

import LN.View.Util                    (showIfSelf_UserName)
import LN.Input.Types                  (Input)
import LN.State.Types                  (State)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.T



usersLayout :: String -> State -> Array (HTML _ _) -> HTML _ _
usersLayout user_name st page =
  H.div [P.class_ B.containerFluid] [
    H.div [P.classes [B.colSm2]] [
      H.ul [P.class_ B.listUnstyled] [
        H.li_ [linkToP [] (Users (Show user_name) emptyParams)        "Users"],
        H.li_ [linkToP [] (UsersProfile user_name emptyParams)     "Profile"],
        showIfSelf_UserName st user_name [H.li_ [linkToP [] (UsersSettings user_name emptyParams)    "Settings"]],
--        showIfSelf_UserName st user_name [H.li_ [linkToP [] (UsersPMs user_name emptyParams)         "Personal Messages"]],
        H.li_ [linkToP [] (UsersResources user_name emptyParams)   "Resources"],
        H.li_ [linkToP [] (UsersLeurons user_name emptyParams)     "Leurons"]
      ]
    ],
    H.div [P.class_ B.colSm10] page
  ]
--    H.div [P.class_ B.colLg12] page



renderView_Users :: String -> State -> ComponentHTML Input
renderView_Users user_name st =
  usersLayout user_name st [
    H.div_ [
      H.h1_ [ H.text "me." ],
      H.div_ (showUser st.me)
    ]
  ]


showUser :: forall a b. Maybe UserPackResponse -> Array (HTML a b)
showUser Nothing = [ H.p_ [ H.text "login.." ] ]
showUser (Just (UserPackResponse user)) =
  [
      H.p_ [ H.text ("hello, " <> user ^. user_ ^. _UserResponse .. name_) ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. displayName_ ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. name_ ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. email_ ]
--    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. plugin_ ]
--    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. ident_ ]
{-
    , H.p_ [ H.text $ show user.isActive ]
    , H.p_ [ H.text $ show user.createdAt ]
    , H.p_ [ H.text $ show user.createdAt ]
    , H.p_ [ H.text $ show user.modifiedAt ]
    , H.p_ [ H.text $ show user.deactivatedAt ]
    -}
  ]
