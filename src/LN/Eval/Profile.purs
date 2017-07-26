module LN.Eval.Profile (
  eval_Profile,
  eval_Profile_Setter
) where


import Data.Date.Helpers  (dateFromString)
import Data.Maybe         (Maybe(..), fromJust)
import Halogen            (gets, modify)
import Optic.Core         ((^.),(..), (.~))
import Prelude            (bind, pure, discard, ($))

import LN.Component.Types (EvalEff)
import LN.Input.Profile   (InputProfile(..))
import LN.Input.Types     (Input(..))
import LN.Api             (putUserProfile')
import LN.Helpers.Api     (rd)
import LN.T (ProfileGender(GenderMale), _ProfileResponse, _UserPackResponse, birthdate_, debug_, gender_, id_, location_, profile_, signature_, website_)
import LN.T.Convert



eval_Profile :: Partial => EvalEff


eval_Profile eval (CompProfile InputProfile_Nop next) = pure next



eval_Profile eval (CompProfile InputProfile_Nop next) = pure next



eval_Profile eval (CompProfile InputProfile_Post next) = do

  m_me <- gets _.me

  case m_me of
       Nothing -> eval (AddError "eval_Profile" "st.me doesn't exist" next)
       Just me -> do
         let
           profile_id  = me ^. _UserPackResponse .. profile_ ^. _ProfileResponse .. id_
           profile_req = (profileResponseToProfileRequest [] Nothing $ me ^. _UserPackResponse .. profile_)

         _ <- rd $ putUserProfile' profile_id profile_req
         pure next




eval_Profile eval (CompProfile (InputProfile_Gender gender) next) = do

  eval_Profile_Setter gender_ GenderMale next



eval_Profile eval (CompProfile (InputProfile_Birthdate birthdate) next) = do

  eval_Profile_Setter birthdate_ (fromJust $ dateFromString birthdate) next



eval_Profile eval (CompProfile (InputProfile_Website mwebsite) next) = do

  eval_Profile_Setter website_ mwebsite next



eval_Profile eval (CompProfile (InputProfile_Location mlocation) next) = do

  eval_Profile_Setter location_ mlocation next



eval_Profile eval (CompProfile (InputProfile_Signature msignature) next) = do

  eval_Profile_Setter signature_ msignature next



eval_Profile eval (CompProfile (InputProfile_SetDebug b) next) = do

  eval_Profile_Setter debug_ b next



{-
eval_Profile_Setter :: forall t11 t19 t20 t4 t56.
  Bind t4 => MonadState
               { me :: Maybe UserPackResponse
               | t11
               }
               t4
              => ((t20 -> Identity t19)
                  -> { id :: Int
                     , ent :: Ent
                     , entId :: Int
                     , gender :: ProfileGender
                     , birthdate :: Date
                     , website :: Maybe String
                     , location :: Maybe String
                     , signature :: Maybe String
                     , debug :: Boolean
                     , karmaGood :: Int
                     , karmaBad :: Int
                     , guard :: Int
                     , createdAt :: Maybe Date
                     , modifiedAt :: Maybe Date
                     }
                     -> Identity
                          { id :: Int
                          , ent :: Ent
                          , entId :: Int
                          , gender :: ProfileGender
                          , birthdate :: Date
                          , website :: Maybe String
                          , location :: Maybe String
                          , signature :: Maybe String
                          , debug :: Boolean
                          , karmaGood :: Int
                          , karmaBad :: Int
                          , guard :: Int
                          , createdAt :: Maybe Date
                          , modifiedAt :: Maybe Date
                          }
                 )
                 -> t19 -> t56 -> t4 t56
                 -}
eval_Profile_Setter accessor value next = do
  m_me <- gets _.me

  case m_me of
       Nothing -> pure next
       Just me -> do
          let
            profile = (_ProfileResponse .. accessor .~ value) $ (me ^. _UserPackResponse .. profile_)
            me' = (_UserPackResponse .. profile_ .~ profile) $ me
          modify (_ { me = Just me' })
          pure next
