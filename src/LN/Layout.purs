module LN.Layout (
  defaultLayout
) where



import Data.Array                      ((:), length, concat)
import Data.Maybe                      (Maybe(..))
import Halogen.HTML            (HTML(), ClassName())
import Halogen.HTML            as H
import Halogen.HTML.Core       as C
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (map, show, ($), (<>))

import LN.Debug                        (ifDebug_ByUser)
import LN.Helpers.Halogen.Util
import LN.Router.Link                  (linkToHref, linkTo)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Module.Breadcrumbs      (renderBreadcrumbs)
import LN.T



row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]



col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]



col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]



defaultLayout :: State -> Array (HTML _ _) -> HTML _ _
defaultLayout st page =
  H.div [ P.class_ B.containerFluid ] [
    header st.me (length st.errors),

    ifDebug_ByUser
      st.me
      (\_ -> H.p_ [H.text $ "DEBUG(currentPage): " <> show st.currentPage])
      (\_ -> H.div_ []),

    renderBreadcrumbs st.currentPage st,
    row page
  ]



-- container :: Array IProp -> HTML _ _
container attrs = H.div (P.class_ B.container : attrs)



container_ :: Array (HTML _ _) -> HTML _ _
container_ = container []



header :: Maybe UserPackResponse -> Int -> HTML _ _
header muser n_errors =
  H.div [P.class_ B.containerFluid] [

    H.nav [P.classes [B.navbar, B.navbarDefault]] [
      container_ [
        H.div [P.class_ B.navbarHeader] [
          H.button [P.classes [B.navbarToggle, C.ClassName "collapsed"], dataToggle "collapse", dataHelper "target" "lnotes-navbar"] [
            H.span [P.class_ B.iconBar] [],
            H.span [P.class_ B.iconBar] [],
            H.span [P.class_ B.iconBar] []
          ],
          H.a [P.classes [B.navbarBrand], linkToHref Home] [H.text "LNotes"]
        ],

        H.div [P.classes [B.navbarCollapse, B.collapse], P.id_ "lnotes-navbar"] [

          -- TODO FIXME: ugly, but need to make sure the debug check doesn't wreck the navbar
          H.ul [P.classes [B.navbarNav, B.nav, B.navTabs]]
            (concat [
              [
                H.li_ [linkTo About "About"],
                H.li_ [linkTo (Resources Index emptyParams) "Resources"],
                H.li_ [linkTo Portal "Portal"],
                H.li_ [me]
              ],
              ifDebug_ByUser
                muser
                (\_ -> [H.li_ [errors]])
                (\_ -> [])
            ]),

          case muser of
            Nothing ->
              H.ul [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight ]] [
                H.li_ [linkTo Login "Log in"]
              ]
            Just u ->
              H.ul [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight]] [
                H.li_ [linkTo Logout $ "Log out: " <> u ^. _UserPackResponse .. user_ ^. _UserResponse .. name_]
              ]
        ]
      ]
    ]
  ]
  where
  me = case muser of
            Nothing   -> linkTo NotFound "Me"
            Just user -> linkTo (Users (Show (user ^. _UserPackResponse .. user_ ^. _UserResponse .. name_)) emptyParams) "Me"
  errors =
    -- TODO FIXME: use proper pill number
    linkTo Errors $ "Errors [" <> show n_errors <> "]"




footer :: forall a b. HTML a b
footer =
  H.footer [P.class_ (H.ClassName "footer")] [
    H.text "LN",
    H.ul [] (map (\s -> H.li [] [H.text s]) ["About", "Contact"])
  ]
