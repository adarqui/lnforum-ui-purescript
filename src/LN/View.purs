module LN.View (
  renderView
) where



import Data.Maybe                (Maybe(..))
import Halogen                   (HTML, ComponentHTML)
import Halogen.HTML      as H
import Prelude                   (map, const, ($))



import LN.Input.Types            (Input)
import LN.Router.Types           (Routes(..), CRUD(..))
import LN.State.Types            (State)
import LN.View.Home              (renderView_Home)
import LN.View.About             (renderView_About)
import LN.View.Errors            (renderView_Errors)
import LN.View.Portal            (renderView_Portal)
import LN.View.Users.Index       (renderView_Users_Index)
import LN.View.Users.Show        (renderView_Users_Show)
import LN.View.Users.Profile     (renderView_Users_Profile)
import LN.View.Users.Settings    (renderView_Users_Settings)
import LN.View.Users.Resources   (renderView_Users_Resources)
import LN.View.Users.Leurons     (renderView_Users_Leurons)
import LN.View.Four04            (renderView_404)



import LN.View.Resources.Index                   (renderView_Resources_Index)
import LN.View.Resources.Mod                     ( renderView_Resources_New, renderView_Resources_Edit
                                                 , renderView_Resources_Delete)
import LN.View.Resources.Show                    (renderView_Resources_Show)
import LN.View.Resources.Leurons.Index           (renderView_Resources_Leurons_Index)
import LN.View.Resources.SiftLeurons             (renderView_Resources_SiftLeurons)
import LN.View.Resources.SiftLeuronsLinear.Index (renderView_Resources_SiftLeuronsLinear_Index)
import LN.View.Resources.SiftLeuronsLinear.Show  (renderView_Resources_SiftLeuronsLinear_Show)
import LN.View.Resources.SiftLeuronsRandom       (renderView_Resources_SiftLeuronsRandom)

import LN.View.Leurons.Index               (renderView_Leurons_Index)
import LN.View.Leurons.Show                (renderView_Leurons_Show)
import LN.View.Leurons.Mod                 (renderView_Leurons_New, renderView_Leurons_Edit, renderView_Leurons_Delete)



renderView :: Routes -> State -> ComponentHTML Input



renderView Home   = const $ renderView_Home



renderView About  = const $ renderView_About



renderView Errors = renderView_Errors



renderView Portal = const $ renderView_Portal



renderView (Users Index params)            = renderView_Users_Index
renderView (Users (Show user_name) params) = renderView_Users_Show user_name



renderView (UsersProfile user_name params)     = renderView_Users_Profile user_name
renderView (UsersSettings user_name params)    = renderView_Users_Settings user_name
renderView (UsersResources user_name params)   = renderView_Users_Resources user_name
renderView (UsersLeurons user_name params)     = renderView_Users_Leurons user_name



renderView (Resources Index params )                = renderView_Resources_Index
renderView (Resources New params)                   = renderView_Resources_New
renderView (Resources (EditI resource_id) params)   = renderView_Resources_Edit resource_id
renderView (Resources (ShowI resource_id) params)   = renderView_Resources_Show resource_id
renderView (Resources (DeleteI resource_id) params) = renderView_Resources_Delete resource_id



renderView (ResourcesLeurons resource_id Index params)                    = renderView_Resources_Leurons_Index resource_id
renderView (ResourcesLeurons resource_id New params)                      = renderView_Leurons_New resource_id
renderView (ResourcesLeurons resource_id (EditI leuron_id) params)        = renderView_Leurons_Edit resource_id leuron_id
renderView (ResourcesLeurons resource_id (ShowI leuron_id) params)        = renderView_Leurons_Show resource_id leuron_id
renderView (ResourcesLeurons resource_id (DeleteI leuron_id) params)      = renderView_Leurons_Delete resource_id leuron_id



renderView (ResourcesSiftLeurons resource_id params)                      = renderView_Resources_SiftLeurons resource_id
renderView (ResourcesSiftLeuronsLinear resource_id Index params)          = renderView_Resources_SiftLeuronsLinear_Index resource_id
renderView (ResourcesSiftLeuronsLinear resource_id (ShowI offset) params) = renderView_Resources_SiftLeuronsLinear_Show resource_id offset
renderView (ResourcesSiftLeuronsRandom resource_id params)                = renderView_Resources_SiftLeuronsRandom resource_id



-- renderView (Leurons Index params)              = renderView_Leurons_Index
-- renderView (Leurons (ShowI resource_id) params) = renderView_Leurons_Show resource_id



renderView _ = const $ renderView_404



errs :: forall a b. Maybe (Array String) -> HTML a b
errs Nothing = H.div_ []
errs (Just errors) = H.div_ (map (\e -> H.p_ [H.text e]) errors)
