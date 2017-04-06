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



--
-- Very ugly, but does the job for now. Accounts for consecutive newlines, newlines before chars, etc
--
scrubConcat :: String -> String
scrubConcat s =
  removeTrailingNewlines $ Array.foldl go "__initial__" l
  where
  l = Array.concat $ map (\a -> Array.nubBy (\x y -> x == y && x == "") a) $ map Array.fromFoldable $ Array.groupBy (\a b -> a == b && a == "") $ lines s
  go "__initial__" s = s
  go acc s =
    if stripChars " " s == ""
       then acc <> "\n\n"
       else case (uncons $ reverse acc) of
                 Nothing       -> acc <> " " <> s
                 Just { head: h, tail: t } -> if h == '\n'
                                                  then acc <> s
                                                  else acc <> " " <> s



removeTrailingNewlines :: String -> String
removeTrailingNewlines s = reverse $ dropWhile (\s_ -> s_ == '\n') $ reverse s



-- | Reverse a `String`, may give funky results with unicode
reverse :: String -> String
reverse = fromCharArray <<< Array.reverse <<< SU.toCharArray
