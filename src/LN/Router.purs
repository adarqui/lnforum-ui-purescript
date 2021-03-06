module Router (
  routing,
  hashChangeProducer,
  hashChangeConsumer
) where



import Control.Alt             ((<|>))
import Control.Apply           ((*>), (<*))
import Control.Plus            (empty)
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Either (Either(..))
import Data.NaturalTransformation
import Data.Functor            ((<$))
import Data.Int                (fromString)
import Data.List               (List(..))
import Data.Maybe              (Maybe(..))
import Data.String             (length)
import Data.Tuple (Tuple(Tuple), snd)
import DOM (DOM)
-- import Halogen                 hiding (set)
import Halogen as H
import Halogen.Aff as HA
import Prelude (Unit, bind, discard, pure, ($), (<$>), (<*>), (>))
import Routing                 (matchesAff)
import Routing.Match           (Match(..))
import Routing.Match.Class (class MatchClass, lit, params)
import Routing.Types           (RoutePart(..))

import LN.Input.Types          (Input(..))
import LN.Router.Types (CRUD(ShowI, DeleteI, EditI, New, Index, Show), Routes(ViewExamples, Logout, Login, BucketsRounds, BucketsLeurons, BucketsResources, Buckets, Leurons, Resources, ResourcesSiftLeuronsRandom, ResourcesSiftLeuronsLinear, ResourcesSiftLeurons, ResourcesLeurons, Users, UsersLeurons, UsersResources, UsersSettings, UsersProfile, Portal, Home, Errors, Me, About))
import LN.Router.Class.Params  (Params, emptyParams, psRoutingParamsToParams)



-- params' :: forall f. (Bind f, MatchClass f) => f (Array (Tuple String String))
-- params' :: forall f. MatchClass f => f (Array (Tuple String String))
params' :: forall f. MatchClass f => f Params
params' = psRoutingParamsToParams <$> params
--  pure emptyParams
--  (listToArray <<< M.toList) <$> params




-- | Matches a non-empty string
--
str1 :: Match String
str1 = Match \route ->
    case route of
      Cons (Path input) rs ->
        if length input > 0
          then pure $ Tuple rs input
          else empty
      _ -> empty



int :: Match Int
int = Match \route ->
  case route of
    Cons (Path input) rs ->
      case fromString input of
        Nothing -> empty
        Just n  -> pure $ Tuple rs n
    _ -> empty



routing :: Match Routes
routing =

      about <|>

      portal <|>

      users_profile <|>
      users_settings <|>
      users_resources <|>
      users_leurons <|>

      users_new <|>
      users_show <|>
      users_index <|>

      me <|>

      errors <|>

      resources_sift_linear_show_int <|>
      resources_sift_linear_index <|>
      resources_sift_random <|>

      resources_sift <|>

      resources_leurons_new <|>
      resources_leurons_edit_int <|>
      resources_leurons_delete_int <|>
      resources_leurons_show_int <|>
      resources_leurons_index <|>

      resources_new <|>
      resources_show_int <|>
      resources_edit_int <|>
      resources_delete_int <|>
      resources_index <|>

      leurons_new <|>
      leurons_show_int <|>
      leurons_edit_int <|>
      leurons_delete_int <|>
      leurons_index <|>

      buckets_resources_new <|>
      buckets_resources_edit_int <|>
      buckets_resources_delete_int <|>
      buckets_resources_show_int <|>
      buckets_resources_index <|>

      buckets_leurons_new <|>
      buckets_leurons_edit_int <|>
      buckets_leurons_delete_int <|>
      buckets_leurons_show_int <|>
      buckets_leurons_index <|>

      buckets_rounds_new <|>
      buckets_rounds_edit_int <|>
      buckets_rounds_delete_int <|>
      buckets_rounds_show_int <|>
      buckets_rounds_index <|>

      buckets_new <|>
      buckets_show_int <|>
      buckets_edit_int <|>
      buckets_delete_int <|>
      buckets_index <|>

      login <|>
      logout <|>

      view_examples <|>

      home <|>
      home2
  where

    about = About <$ route "about"



    me = Me <$ route "me"



    errors = Errors <$ route "errors"



    home = Home <$ lit ""
    home2 = pure Home



    portal = Portal <$ route "portal"



    users_profile =
      UsersProfile <$> (lit "" *> lit "u" *> str1) <*> (lit "profile" *> (params' <|> pure emptyParams))

    users_settings =
      UsersSettings <$> (lit "" *> lit "u" *> str1) <*> (lit "settings" *> (params' <|> pure emptyParams))

    users_resources =
      UsersResources <$> (lit "" *> lit "u" *> str1) <*> (lit "resources" *> (params' <|> pure emptyParams))

    users_leurons =
      UsersLeurons <$> (lit "" *> lit "u" *> str1) <*> (lit "leurons" *> (params' <|> pure emptyParams))




    users_index =
      Users
      <$> (lit "" *> lit "u" *> pure Index)
      <*> (params' <|> pure emptyParams)

    users_new =
      Users
      <$> (lit "" *> lit "u" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    users_show =
      Users
      <$> (lit "" *> lit "u" *> (Show <$> str1))
      <*> (params' <|> pure emptyParams)



    resources_leurons_index =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_leurons_new =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    resources_leurons_edit_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_leurons_delete_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_leurons_show_int =
      ResourcesLeurons
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "leurons" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)



    resources_sift =
      ResourcesSiftLeurons
      <$> (lit "" *> lit "resources" *> int <* lit "sift")
      <*> (params' <|> pure emptyParams)

    resources_sift_linear_index =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_sift_linear_show_int =
      ResourcesSiftLeuronsLinear
      <$> (lit "" *> lit "resources" *> int)
      <*> (lit "sift" *> lit "linear" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_sift_random =
      ResourcesSiftLeuronsRandom
      <$> (lit "" *> lit "resources" *> int <* lit "sift" <* lit "random")
      <*> (params' <|> pure emptyParams)



    resources_index =
      Resources
      <$> (lit "" *> lit "resources" *> pure Index)
      <*> (params' <|> pure emptyParams)

    resources_new =
      Resources
      <$> (lit "" *> lit "resources" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    resources_show_int =
      Resources
      <$> (lit "" *> lit "resources" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_edit_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    resources_delete_int =
      Resources
      <$> (lit "" *> lit "resources" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)



    leurons_index =
      Leurons
      <$> (lit "" *> lit "leurons" *> pure Index)
      <*> (params' <|> pure emptyParams)

    leurons_new =
      Leurons
      <$> (lit "" *> lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    leurons_show_int =
      Leurons
      <$> (lit "" *> lit "leurons" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    leurons_edit_int =
      Leurons
      <$> (lit "" *> lit "leurons" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    leurons_delete_int =
      Leurons
      <$> (lit "" *> lit "leurons" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)



    buckets_index =
      Buckets
      <$> (lit "" *> lit "buckets" *> pure Index)
      <*> (params' <|> pure emptyParams)

    buckets_new =
      Buckets
      <$> (lit "" *> lit "buckets" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    buckets_show_int =
      Buckets
      <$> (lit "" *> lit "buckets" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_edit_int =
      Buckets
      <$> (lit "" *> lit "buckets" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_delete_int =
      Buckets
      <$> (lit "" *> lit "buckets" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)



    buckets_resources_index =
      BucketsResources
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "resources" *> pure Index)
      <*> (params' <|> pure emptyParams)

    buckets_resources_new =
      BucketsResources
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "resources" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    buckets_resources_edit_int =
      BucketsResources
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "resources" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_resources_delete_int =
      BucketsResources
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "resources" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_resources_show_int =
      BucketsResources
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "resources" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)



    buckets_leurons_index =
      BucketsLeurons
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "leurons" *> pure Index)
      <*> (params' <|> pure emptyParams)

    buckets_leurons_new =
      BucketsLeurons
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "leurons" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    buckets_leurons_edit_int =
      BucketsLeurons
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "leurons" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_leurons_delete_int =
      BucketsLeurons
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "leurons" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_leurons_show_int =
      BucketsLeurons
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "leurons" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)



    buckets_rounds_index =
      BucketsRounds
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "rounds" *> pure Index)
      <*> (params' <|> pure emptyParams)

    buckets_rounds_new =
      BucketsRounds
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "rounds" *> lit "new" *> pure New)
      <*> (params' <|> pure emptyParams)

    buckets_rounds_edit_int =
      BucketsRounds
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "rounds" *> lit "_edit" *> (EditI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_rounds_delete_int =
      BucketsRounds
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "rounds" *> lit "_delete" *> (DeleteI <$> int))
      <*> (params' <|> pure emptyParams)

    buckets_rounds_show_int =
      BucketsRounds
      <$> (lit "" *> lit "buckets" *> int)
      <*> (lit "rounds" *> (ShowI <$> int))
      <*> (params' <|> pure emptyParams)



    login = Login <$ route "login"
    logout = Logout <$ route "logout"



    view_examples = ViewExamples <$ route "view_examples"



    route str = lit "" *> lit str




-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: forall eff. CR.Producer Routes (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
hashChangeProducer = CRA.produceAff \emit -> do
  v <- matchesAff routing
  emit $ Left (snd v)



-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer :: forall eff. (Input ~> Aff (HA.HalogenEffects eff)) -> CR.Consumer Routes (Aff (HA.HalogenEffects eff)) Unit
hashChangeConsumer query = CR.consumer \route -> do
  query $ H.action $ GotoH route
  pure Nothing
