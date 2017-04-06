module LN.Scrub (
  scrubTrim,
  scrubTrimLeft,
  scrubConcat
) where



import Data.Maybe                  (Maybe(..))
import Prelude                     (class Eq, class Show, eq, show)



scrubTrim :: String -> String
scrubTrim s = s



scrubTrimLeft :: String -> String
scrubTrimLeft s = s



scrubConcat :: String -> String
scrubConcat s = s
