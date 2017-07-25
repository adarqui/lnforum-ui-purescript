module LN.Helpers.JSON (
  decodeString
) where



import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe
import Data.Either (Either(..))


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
