module LN.State.Lens (
  stMe,
  stErrors,
  stUsers,
  stUsersMap,
  stCurrentUser,
  stCurrentPage,
  stCurrentPageInfo,
  stUsersPageInfo
) where



import Data.Map                  as M
import Data.Maybe                (Maybe)
import Data.Tuple                (Tuple)
import Optic.Core                (Lens', lens)

import LN.Router.Types           (Routes)
import LN.State.PageInfo         (PageInfo)
import LN.State.Types            (State)
import LN.T



stMe :: Lens' State (Maybe UserPackResponse)
stMe =
  lens
    _.me
    (_ { me = _ })



stErrors :: Lens' State (Array (Tuple String String))
stErrors = lens _.errors _ { errors = _ }



stUsers :: Lens' State (M.Map Int UserSanitizedPackResponse)
stUsers =
  lens
    _.users
    (_ { users = _ })



stUsersMap :: Lens' State (M.Map Int UserSanitizedPackResponse)
stUsersMap =
  lens
    _.usersMap
    (_ { usersMap = _ })



stCurrentUser :: Lens' State (Maybe UserSanitizedPackResponse)
stCurrentUser =

  lens
    (_.currentUser)
    (_ { currentUser = _ })



stCurrentPage :: Lens' State Routes
stCurrentPage =
  lens
    _.currentPage
    (_ { currentPage = _ })



stCurrentPageInfo :: Lens' State PageInfo
stCurrentPageInfo =
  lens
    _.currentPageInfo
    (_ { currentPageInfo = _ })



stUsersPageInfo :: Lens' State PageInfo
stUsersPageInfo =
  lens
    _.usersPageInfo
    (_ { usersPageInfo = _ })
