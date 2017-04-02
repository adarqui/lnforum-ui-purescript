module LN.View.Leurons.Mod (
  renderView_Leurons_Delete,
  renderView_Leurons_New,
  renderView_Leurons_Edit,
  renderView_Leurons_Mod
) where



import Data.Array                      (modifyAt, deleteAt, nub)
import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties as P
-- import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<), (<>))

{-
import LN.Halogen.Util                 (simpleInfoButton, input_DeleteEdit, input_Label
                                       , textArea_DeleteEdit, input_maybeField_DeleteEdit, radioMenu
                                       , textArea_Label, textArea_LabelWithButton, textArea_Label_WithClear)
                                       -}
import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
--import LN.Helpers.JSON                 (decodeString)
-- import LN.Internal.Leuron
--import LN.Input.Leuron
import LN.Internal.Leuron
import LN.Input.Leuron                 (Leuron_Mod(..))
import LN.Input.Types                  (Input, cLeuronMod)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentLeuron)
import LN.State.Leuron                 (LeuronRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Leurons_Delete :: Int -> Int -> State -> ComponentHTML Input
renderView_Leurons_Delete resource_id leuron_id st =

  case st.currentLeuron, getLoading l_currentLeuron st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "leuron unavailable."]]
       Just pack, false -> renderView_Leurons_Delete' pack st



renderView_Leurons_Delete' :: LeuronPackResponse -> State -> ComponentHTML Input
renderView_Leurons_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 leuron = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse



renderView_Leurons_New :: Int -> State -> ComponentHTML Input
renderView_Leurons_New resource_id = renderView_Leurons_Mod resource_id Nothing



renderView_Leurons_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_Leurons_Edit resource_id leuron_id = renderView_Leurons_Mod resource_id (Just leuron_id)



renderView_Leurons_Mod :: Int -> Maybe Int -> State -> ComponentHTML Input
renderView_Leurons_Mod resource_id m_leuron_id st =
  case st.currentLeuronRequest, st.currentLeuronRequestSt, getLoading l_currentLeuron st.loading of
    _, _, true                         -> renderLoading
    Just leuron_req, Just lst, false   -> renderView_Leurons_Mod' resource_id m_leuron_id leuron_req lst st
    _, _, false                        -> H.div_ [H.p_ [H.text "Leurons_Mod: unexpected error."]]



renderView_Leurons_Mod' :: Int -> Maybe Int -> LeuronRequest -> LeuronRequestState -> State -> ComponentHTML Input
renderView_Leurons_Mod' resource_id m_leuron_id leuron_req lst st =
  H.div_ [

      H.h2_ [ H.text "Add Leuron" ]

--    , H.h2_ [ H.text $ "For resource: " <> resource.resourceTitle ]

   , create_or_save

  -- LeuronData

   , radioMenu
      "Leuron Type"
      "leuron-type"
      [ TyLnEmpty, TyLnFact, TyLnFactList, TyLnCard, TyLnDCard, TyLnDCardX, TyLnAcronym
      , TyLnSynonym, TyLnAntonym, TyLnTemplate, TyLnImageAssociation, TyLnLinearDemo
      , TyLnTable, TyLnScript, TyLnQA, TyLnExamples
      ]
      (cLeuronMod <<< SetType)
      lst.ty

   , case lst.ty of
          TyLnEmpty    -> empty
          TyLnFact     -> clearLeuronData (LnFact defaultFact)
          TyLnFactList -> clearLeuronData (LnFactList defaultFactList)
          TyLnCard     -> clearLeuronData (LnCard defaultCard)
          TyLnDCard    -> clearLeuronData (LnDCard defaultDCard)
          TyLnDCardX   -> clearLeuronData (LnDCardX defaultDCardX)
          TyLnAcronym  -> clearLeuronData (LnAcronym defaultAcronym)
          TyLnSynonym  -> clearLeuronData (LnSynonym defaultSynonym)
          TyLnAntonym  -> clearLeuronData (LnAntonym defaultAntonym)
          TyLnQA       -> clearLeuronData (LnQA defaultQA)
          _            -> H.div_ []

   , case lst.ty of
          TyLnEmpty    -> empty
          TyLnFact     -> fact lst.fact
          TyLnFactList -> factList lst.factList
          TyLnCard     -> card lst.card
          TyLnDCard    -> dcard lst.dcard
          TyLnDCardX   -> dcardx lst.dcardx
          TyLnAcronym  -> acronym lst.acronym
          TyLnSynonym  -> synonym lst.synonym
          TyLnAntonym  -> antonym lst.antonym
          TyLnQA       -> qa lst.qa
          _            -> H.p_ [H.text "not implemented."]



  -- Title

--  , input_maybeField_DeleteEdit
--      P.InputText
--      "Title"
--      leuron.title
--      (E.input_ (cLeuronMod $ SetTitle ""))
--      (E.input (\new -> cLeuronMod $ SetTitle new))
--      (E.input_ (cLeuronMod $ RemoveTitle))



  -- Description

--  , input_maybeField_DeleteEdit
--      P.InputText
--      "Description"
--      leuron.description
--      (E.input_ (cLeuronMod $ SetDescription ""))
--      (E.input (\new -> cLeuronMod $ SetDescription new))
--      (E.input_ (cLeuronMod $ RemoveDescription))



  -- Section

  , input_maybeField_DeleteEdit
      P.InputText
      "Section"
      leuron.section
      (E.input_ (cLeuronMod $ SetSection ""))
      (E.input (\new -> cLeuronMod $ SetSection new))
      (E.input_ (cLeuronMod $ RemoveSection))



  -- Examples
  , textArea_LabelWithButton "Examples" "Example" "" "Add"
      (E.input (cLeuronMod <<< SetExample))
      (E.input_ (cLeuronMod $ AddExample lst.exampleItem))

  , H.div_ $
      map (\(Tuple idx example) ->
        textArea_DeleteEdit
          example
          (E.input (\new -> cLeuronMod $ EditExample idx new))
          (E.input_ (cLeuronMod $ DeleteExample idx))
      ) $ seqArrayFrom $ maybe [] id leuron.examples




  -- Strengths

{-
TODO: add this back, removing for now
  , input_Label "Strengths" "Strength" "" P.InputText  (E.input AddLeuronStrengths)

  , H.div_ $
      map (\strength ->
        input_DeleteEdit
          P.InputText
          strength
          (E.input (\new -> EditLeuronStrengths strength new))
          (E.input_ (RemoveLeuronStrengths strength))
      ) leuron.leuronStrengths
-}



  -- Categories

  , input_Label "Categories" "Category" "" P.InputText  (E.input_ (cLeuronMod $ AddCategory [])) -- <<< decode))

  , H.div_ $
      map (\category ->
        input_DeleteEdit
          P.InputText
          (show category)
          (E.input (\new -> cLeuronMod $ EditCategory 0 [])) -- TODO FIXME (decode new)))
          (E.input_ (cLeuronMod $ DeleteCategory 0))
      ) leuron.categories



  -- Splits

--  , input_Label "Splits" "Splits" "" P.InputText  (E.input (AddSplits <<< decode))

{-
TODO FIXME
  , H.div_ $
      map (\split ->
        input_DeleteEdit
          P.InputText
          (show split)
          (E.input (\new -> EditLeuronSplits split (decode new)))
          (E.input_ (RemoveLeuronSplits split))
      ) [] -- leuron.leuronSplits
-}



  -- Substitutions

--  , input_Label "Substitutions" "Substitutions" "" P.InputText  (E.input (AddLeuronSubstitutions <<< decode))

{-
TODO FIXME
  , H.div_ $
      map (\split ->
        input_DeleteEdit
          P.InputText
          (show split)
          (E.input (\new -> EditLeuronSubstitutions split (decode new)))
          (E.input_ (RemoveLeuronSubstitutions split))
      ) [] -- leuron.leuronSubstitutions



  -- Tags

TODO: add this back, removing for now
  , input_Label "Tags" "Tags" "" P.InputText  (E.input AddLeuronTags)

  , case leuron.leuronTags of
         Nothing -> H.div_ []
         (Just tagss) -> H.div_ $
            map (\tags ->
              input_DeleteEdit
                P.InputText
                tags
                (E.input (\new -> EditLeuronTags tags new))
                (E.input_ (RemoveLeuronTags tags))
              ) tagss



  -- Style

TODO FIXME
  , input_Label "Style" "Style" "" P.InputText  (E.input (cLeuronMod <<< AddStyle))

  , case leuron.leuronStyle of
         Nothing -> H.div_ []
         (Just styles) -> H.div_ $
            map (\style ->
              input_DeleteEdit
                P.InputText
                style
                (E.input (\new -> (cLeuronMod <<< EditStyle style new))
                (E.input_ (cLeuronMod <<< RemoveStyle style))
              ) styles



  -- SpecificTo

  , input_maybeField_DeleteEdit
      P.InputText
      "SpecificTo"
      leuron.leuronSpecificTo
      (E.input_ (SetLeuronSpecificTo ""))
      (E.input (\new -> SetLeuronSpecificTo new))
      (E.input_ RemoveLeuronSpecificTo)
-}


  , create_or_save

  -- show a list of recently added leurons
  , H.ul_ $ map (\id_ -> H.li_ [linkToP [] (ResourcesLeurons resource_id (ShowI id_) emptyParams) (show id_)]) lst.ids
  ]
  where

  clearLeuronData type_ = H.p_ [H.button [buttonInfoClasses, P.title "Clear", E.onClick (E.input (\s -> cLeuronMod $ SetData type_))] [H.text "Clear"]]

  create_or_save = case m_leuron_id of
         Nothing         -> simpleInfoButton "Create" (cLeuronMod $ Save resource_id)
         Just leuron_id  -> simpleInfoButton "Save" (cLeuronMod $ EditP leuron_id)
         _               -> H.p_ [H.text "unexpected error."]

  empty = H.h2_ [H.text "NONE"]

  fact (Fact v) =
    H.p_ [
      H.h2_ [H.text "Fact"],
      textArea_Label_WithClear "Fact" "fact" v.text
        (E.input (\s -> cLeuronMod $ SetData $ LnFact $ mkFact s))
        (E.input (\s -> cLeuronMod $ SetData $ LnFact $ mkFact ""))
    ]

  factList (FactList v) =
    H.p_ [
      H.h2_ [H.text "FactList"],
      textArea_Label_WithClear "Fact" "fact" v.fact
        (E.input (\s -> cLeuronMod $ SetData $ LnFactList $ FactList v{fact=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnFactList $ FactList v{fact=""})),
      textArea_LabelWithButton "List" "fact" lst.factList_listItem "Add"
        (E.input (\s -> cLeuronMod $ SetSt $ lst{factList_listItem=s}))
        (E.input_ (cLeuronMod $ SetData $ LnFactList $ FactList v{list=nub $ v.list<>[lst.factList_listItem]})),
      H.div_ $
        map (\(Tuple idx fact) ->
          textArea_DeleteEdit
            fact
            (E.input (\new -> cLeuronMod $ SetData $ LnFactList $ FactList v{list=maybe v.list id (modifyAt idx (const new) v.list)}))
            (E.input_ (cLeuronMod $ SetData $ LnFactList $ FactList v{list=maybe v.list id (deleteAt idx v.list)}))
        ) $ seqArrayFrom v.list
    ]

  card (Card v) =
    H.p_ [
      H.h2_ [H.text "Card"],
      textArea_Label_WithClear "Front" "front" v.front
        (E.input (\s -> cLeuronMod $ SetData $ LnCard $ Card v{front=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnCard $ Card v{front=""})),
      textArea_Label_WithClear "Back" "back" v.back
        (E.input (\s -> cLeuronMod $ SetData $ LnCard $ Card v{back=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnCard $ Card v{back=""}))
    ]

  dcard (DCard v) =
    H.p_ [
      H.h2_ [H.text "DCard"],
      textArea_Label_WithClear "Front" "front" v.front
        (E.input (\s -> cLeuronMod $ SetData $ LnDCard $ DCard v{front=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnDCard $ DCard v{front=""})),
      textArea_Label_WithClear "Back" "back" v.back
        (E.input (\s -> cLeuronMod $ SetData $ LnDCard $ DCard v{back=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnDCard $ DCard v{back=""}))
    ]

  dcardx (DCardX v) = H.p_ [H.text "dcardx"]

  acronym (Acronym v) =
    H.p_ [
      H.h2_ [H.text "Acronym"],
      textArea_Label_WithClear "Abbreviation" "abbreviation" v.abbreviation
        (E.input (\s -> cLeuronMod $ SetData $ LnAcronym $ Acronym v{abbreviation=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnAcronym $ Acronym v{abbreviation=""})),
      textArea_Label_WithClear "Meaning" "meaning" v.meaning
        (E.input (\s -> cLeuronMod $ SetData $ LnAcronym $ Acronym v{meaning=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnAcronym $ Acronym v{meaning=""}))
    ]

  synonym (Synonym v) =
    H.p_ [
      H.h2_ [H.text "Synonym"],
      textArea_Label_WithClear "A" "a" v.a
        (E.input (\s -> cLeuronMod $ SetData $ LnSynonym $ Synonym v{a=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnSynonym $ Synonym v{a=""})),
      textArea_Label_WithClear "B" "b" v.b
        (E.input (\s -> cLeuronMod $ SetData $ LnSynonym $ Synonym v{b=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnSynonym $ Synonym v{b=""}))
    ]

  antonym (Antonym v) =
    H.p_ [
      H.h2_ [H.text "Antonym"],
      textArea_Label_WithClear "A" "a" v.a
        (E.input (\s -> cLeuronMod $ SetData $ LnAntonym $ Antonym v{a=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnAntonym $ Antonym v{a=""})),
      textArea_Label_WithClear "B" "b" v.b
        (E.input (\s -> cLeuronMod $ SetData $ LnAntonym $ Antonym v{b=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnAntonym $ Antonym v{b=""}))
    ]

  qa (QA v) =
    H.p_ [
      H.h2_ [H.text "QA"],
      textArea_Label_WithClear "Question" "question" v.question
        (E.input (\s -> cLeuronMod $ SetData $ LnQA $ QA v{question=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnQA $ QA v{question=""})),
      textArea_Label_WithClear "Answer" "answer" v.answer
        (E.input (\s -> cLeuronMod $ SetData $ LnQA $ QA v{answer=s}))
        (E.input (\s -> cLeuronMod $ SetData $ LnQA $ QA v{answer=""}))
    ]

  leuron   = unwrapLeuronRequest leuron_req
