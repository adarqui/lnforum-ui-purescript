module LN.Helpers.JSON (
  decodeString
) where



import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Parser
import Data.Maybe
import Data.Either


{-
decodeString :: forall a. DecodeJson a => String -> Maybe a
decodeString s =
  case jsonParser s of
       Left err   -> Nothing
       Right json -> decodeMaybe json
       -}


-- TODO FIXME: MAYBE BROKEN
decodeString :: forall a. DecodeJson a => String -> Maybe a
decodeString s =
  case jsonParser s of
       Left err   -> Nothing
       Right json -> case decodeJson json of
                          Left _  -> Nothing
                          Right v -> Just v
