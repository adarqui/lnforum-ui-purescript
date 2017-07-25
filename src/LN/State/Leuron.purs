module LN.State.Leuron (
  LeuronRequestState,
  defaultLeuronRequestState,
  leuronRequestStateFromLeuronData
) where


import LN.Internal.Leuron (defaultAcronym, defaultAntonym, defaultCard, defaultDCard, defaultDCardX, defaultFact, defaultFactList, defaultImageAssociation, defaultLinearDemo, defaultQA, defaultSynonym, defaultTable, defaultTemplate)

import LN.T (Acronym, Antonym, Card, DCard, DCardX, Fact, FactList, ImageAssociation, LeuronData(LnQA, LnTable, LnLinearDemo, LnImageAssociation, LnTemplate, LnAntonym, LnSynonym, LnAcronym, LnDCardX, LnDCard, LnCard, LnFactList, LnFact), LinearDemo, QA, Synonym, Table, Template, TyLeuron(TyLnQA, TyLnTable, TyLnLinearDemo, TyLnImageAssociation, TyLnTemplate, TyLnAntonym, TyLnSynonym, TyLnAcronym, TyLnDCardX, TyLnDCard, TyLnCard, TyLnFactList, TyLnFact))



type LeuronRequestState = {
  ty                :: TyLeuron,
  exampleItem       :: String,
  fact              :: Fact,
  factList          :: FactList,
  factList_listItem :: String,
  card              :: Card,
  dcard             :: DCard,
  dcardx            :: DCardX,
  acronym           :: Acronym,
  synonym           :: Synonym,
  antonym           :: Antonym,
  template          :: Template,
  imageAssociation  :: ImageAssociation,
  linearDemo        :: LinearDemo,
  table             :: Table,
  qa                :: QA,
  ids               :: Array Int,
  scrubTrim         :: Boolean,
  scrubTrimLeft     :: Boolean,
  scrubConcat       :: Boolean,
  preview           :: Boolean
}



defaultLeuronRequestState :: LeuronRequestState
defaultLeuronRequestState = {
  ty:                TyLnFact,
  exampleItem:       "",
  fact:              defaultFact,
  factList:          defaultFactList,
  factList_listItem: "",
  card:              defaultCard,
  dcard:             defaultDCard,
  dcardx:            defaultDCardX,
  acronym:           defaultAcronym,
  synonym:           defaultSynonym,
  antonym:           defaultAntonym,
  template:          defaultTemplate,
  imageAssociation:  defaultImageAssociation,
  linearDemo:        defaultLinearDemo,
  table:             defaultTable,
  qa:                defaultQA,
  ids:               [],
  scrubTrim:         false,
  scrubTrimLeft:     false,
  scrubConcat:       false,
  preview:           false
}



leuronRequestStateFromLeuronData :: LeuronData -> LeuronRequestState -> LeuronRequestState
leuronRequestStateFromLeuronData d st =
  case d of
       LnFact v             -> st{fact = v,             ty=TyLnFact}
       LnFactList v         -> st{factList = v,         ty=TyLnFactList}
       LnCard v             -> st{card = v,             ty=TyLnCard}
       LnDCard v            -> st{dcard = v,            ty=TyLnDCard}
       LnDCardX v           -> st{dcardx = v,           ty=TyLnDCardX}
       LnAcronym v          -> st{acronym = v,          ty=TyLnAcronym}
       LnSynonym v          -> st{synonym = v,          ty=TyLnSynonym}
       LnAntonym v          -> st{antonym = v,          ty=TyLnAntonym}
       LnTemplate v         -> st{template = v,         ty=TyLnTemplate}
       LnImageAssociation v -> st{imageAssociation = v, ty=TyLnImageAssociation}
       LnLinearDemo v       -> st{linearDemo = v,       ty=TyLnLinearDemo}
       LnTable v            -> st{table = v,            ty=TyLnTable}
       LnQA v               -> st{qa = v,               ty=TyLnQA}
       _                    -> st
