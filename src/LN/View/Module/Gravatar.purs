module LN.View.Module.Gravatar (
  renderGravatarForUser,
  gravatarSize,
  gravatarSizeParam,
  gravatarUrlFromUser
) where



import Data.Maybe                         (Maybe(..))
import Halogen                            (ComponentHTML)
import Halogen.HTML               as H
import Halogen.HTML.Properties    as P
import Prelude                            (show, ($), (<>))

import LN.Input.Types                     (Input)
import LN.Router.Link (linkTo')
import LN.Router.Types                    (Routes(..), CRUD(..))
import LN.Router.Class.Params             (emptyParams)
import LN.T



{-
renderGravatar :: String -> ComponentHTML Input
renderGravatar md5 =
  H.img [P.src $ "//www.gravatar.com/avatar/" <> md5 <> "?r=pg", linkToHref (Users (Show]
  -}


renderGravatarForUser :: Size -> Maybe UserSanitizedResponse -> ComponentHTML Input
renderGravatarForUser sz Nothing =
  H.img [P.src $ "//www.gravatar.com/avatar/none?d=identicon&" <> (gravatarSizeParam sz)]
renderGravatarForUser sz (Just (UserSanitizedResponse user)) =
  linkTo'
    (Users (Show user.name) emptyParams)
    [H.img [P.src $ "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz), P.alt user.name]]


gravatarSize :: Size -> Int
gravatarSize sz =
  case sz of
    XSmall -> 20
    Small  -> 40
    Medium -> 60
    Large  -> 80
    XLarge -> 100



gravatarSizeParam :: Size -> String
gravatarSizeParam sz = "s=" <> show (gravatarSize sz)


-- TODO CLEANUP


gravatarUrlFromUser :: Size -> UserSanitizedResponse -> String
gravatarUrlFromUser sz (UserSanitizedResponse user) =
  "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?d=identicon&r=pg" <> "&" <> (gravatarSizeParam sz)



{-
gravatarFromUserId :: State -> Int -> ComponentHTML Input
gravatarFromUserId st user_id =
  H.div_ [
    linkTo' (Users $ Show user.name) [H.img [P.src $ "//www.gravatar.com/avatar/" <> user.emailMD5 <> "?r=pg", P.alt user.name]]
  ]
  where
    -}
