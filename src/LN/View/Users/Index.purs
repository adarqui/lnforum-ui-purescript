module LN.View.Users.Index (
  renderView_Users_Index
) where



import LN.ArrayList        (listToArray)
import Data.Map                     as M
import Data.Maybe                   (Maybe(..))
import Halogen                      (ComponentHTML)
import Optic.Core                   ((^.), (..))
import Prelude                      (map, ($))

import LN.Input.Types               (Input)
import LN.Router.Types              (Routes(..), CRUD(..))
import LN.Router.Class.Params       (emptyParams)
import LN.State.Types               (State)
import LN.T                         (Size(XLarge), user_, _UserSanitizedPackResponse, _UserSanitizedResponse)
import LN.View.Module.Gravatar      (gravatarUrlFromUser)
import LN.View.Module.PageNumbers   (renderPageNumbers)
import LN.View.Module.EntityListing (renderEntityListing)



renderView_Users_Index :: State -> ComponentHTML Input
renderView_Users_Index st =
  renderEntityListing "Users" Nothing (
    map (\pack ->
      let user = pack ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse
      in
      { name:        user.name
      , displayName: user.displayName
      , createdAt:   user.createdAt
      , logo:        gravatarUrlFromUser XLarge (pack ^. _UserSanitizedPackResponse .. user_)
      , route: Users (Show $ user.name) emptyParams
      }
    ) $ listToArray $ M.values st.users) pNum
  where
  pNum = renderPageNumbers st.usersPageInfo st.currentPage
