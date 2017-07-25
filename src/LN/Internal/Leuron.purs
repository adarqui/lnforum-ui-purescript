module LN.Internal.Leuron (
  LeuronSift (..),
  leuronToTyLeuron,
  defaultLeuronResponse,
  defaultLeuronRequest,
  defaultFact,
  defaultFactList,
  defaultCard,
  defaultDCard,
  defaultDCardX,
  defaultAcronym,
  defaultSynonym,
  defaultAntonym,
  defaultTemplate,
  defaultImageAssociation,
  defaultLinearDemo,
  defaultTable,
  defaultScript,
  defaultQA
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(..))
import Prelude (class Eq, class Show)

import LN.T                        ( LeuronResponse (..)
                                   , LeuronRequest, mkLeuronRequest, LeuronData(..), TyLeuron(..)
                                   , mkFact, Fact
                                   , mkFactList, FactList
                                   , mkCard, Card
                                   , mkDCard, DCard
                                   , mkDCardX, DCardX
                                   , mkAcronym, Acronym
                                   , mkSynonym, Synonym
                                   , mkAntonym, Antonym
                                   , mkTemplate, Template
                                   , mkImageAssociation, ImageAssociation
                                   , mkLinearDemo, LinearDemo
                                   , mkTable, Table
                                   , mkScript, Script
                                   , mkQA, QA)



data LeuronSift
  = LeuronSift_Linear
  | LeuronSift_Random



derive instance genericLeuronSift :: Generic LeuronSift



instance leuronSiftEq :: Eq LeuronSift where
  eq LeuronSift_Linear LeuronSift_Linear = true
  eq LeuronSift_Random LeuronSift_Random = true
  eq _                 _                 = false



instance leuronSiftShow :: Show LeuronSift where
  show LeuronSift_Linear = "Linear"
  show LeuronSift_Random = "Random"



leuronToTyLeuron :: LeuronData -> TyLeuron
leuronToTyLeuron ld =
  case ld of
       LnEmpty              -> TyLnEmpty
       LnFact _             -> TyLnFact
       LnFactList _         -> TyLnFactList
       LnCard _             -> TyLnCard
       LnDCard _            -> TyLnDCard
       LnDCardX _           -> TyLnDCardX
       LnAcronym _          -> TyLnAcronym
       LnSynonym _          -> TyLnSynonym
       LnAntonym _          -> TyLnAntonym
       LnTemplate _         -> TyLnTemplate
       LnImageAssociation _ -> TyLnImageAssociation
       LnLinearDemo _       -> TyLnLinearDemo
       LnTable _            -> TyLnTable
       LnScript _           -> TyLnScript
       LnQA _               -> TyLnQA
       _                    -> TyLnEmpty



defaultLeuronResponse :: LeuronResponse
defaultLeuronResponse = LeuronResponse {
  id: 0,
  userId: 0,
  resourceId: 0,
  dataP: LnEmpty,
  title: Nothing,
  description: Nothing,
  section: Nothing,
  page: Nothing,
  examples: Nothing,
  strengths: Nothing,
  categories: [],
  splits: Nothing,
  substitutions: Nothing,
  tags: [],
  style: Nothing,
  checksum: "",
  active: true,
  guard: 0,
  createdAt: Nothing,
  modifiedAt: Nothing,
  activityAt: Nothing
}



defaultLeuronRequest :: LeuronRequest
defaultLeuronRequest = mkLeuronRequest LnEmpty Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing [] Nothing 0



defaultFact :: Fact
defaultFact = mkFact ""



defaultFactList :: FactList
defaultFactList = mkFactList "" []



defaultCard :: Card
defaultCard = mkCard "" ""



defaultDCard :: DCard
defaultDCard = mkDCard "" ""



defaultDCardX :: DCardX
defaultDCardX = mkDCardX [] []



defaultAcronym :: Acronym
defaultAcronym = mkAcronym "" ""



defaultSynonym :: Synonym
defaultSynonym = mkSynonym "" ""



defaultAntonym :: Antonym
defaultAntonym = mkAntonym "" ""



defaultTemplate :: Template
defaultTemplate = mkTemplate "" []



defaultImageAssociation :: ImageAssociation
defaultImageAssociation = mkImageAssociation [] [] []



defaultLinearDemo :: LinearDemo
defaultLinearDemo = mkLinearDemo "" []



defaultTable :: Table
defaultTable = mkTable "" [] []



defaultScript :: Script
defaultScript = mkScript "" "" ""



defaultQA :: QA
defaultQA = mkQA "" ""
