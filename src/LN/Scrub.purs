module LN.Scrub (
  scrubTrim,
  scrubTrimLeft,
  scrubConcat
) where



import Data.Maybe                  (Maybe(..))
import Data.String
import Data.String.Utils
import Data.String.Utils as SU
import Data.Array as Array
import Data.List as List
import Prelude



scrubTrim :: String -> String
scrubTrim s = joinWith "\n" $ map (\s_ -> reverse $ dropWhile (\c -> c == ' ') $ reverse s_) l
  where
  l = lines s



scrubTrimLeft :: String -> String
scrubTrimLeft s = joinWith "\n" $ map (\s_ -> dropWhile (\c -> c == ' ') s_) l
  where
  l = lines s



scrubConcat :: String -> String
scrubConcat s =
  removeTrailingNewlines $ Array.foldl (\acc s -> if stripChars " " s == "" then acc <> "\n\n" else joinWith " " [acc, s]) "" l
  where
  l = lines s



removeTrailingNewlines :: String -> String
removeTrailingNewlines s = reverse $ dropWhile (\s_ -> s_ == '\n') $ reverse s



-- | Reverse a `String`, may give funky results with unicode
reverse :: String -> String
reverse = fromCharArray <<< Array.reverse <<< SU.toCharArray
