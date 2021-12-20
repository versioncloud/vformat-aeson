{-| Extend vformat to 'Aeson'

The instance will try the following conditions one by one
when formatting 'Value':

(1) the arg is a 'Number', use 'formatInteger' or 'formatRealFloat' to format
its value.

(2) the arg is a 'String', use 'formatString' to format its value.

(3) the key is an empty key (i.e. 'mempty'), encode the whole 'Value' into
string, then use 'formatString' to format the string.

(4) the arg is 'Array' and the topmost key is 'Index', get the element by the
index, then format the element.

(5) the arg is 'Object' and the topmost key is 'Name', get the value by the
name, then format the value.

(6) raise an 'ArgKeyError'.


If you have a 'ToJSON' datatype, you can extend vformat to it directly,
or just convert it into 'Value' (use 'toJSON') and then format the 'Value'.

=== Example
>>> :set -XDeriveGeneric
>>> import GHC.Generics
>>> data Color = Red | Yellow | Blue deriving Generic
>>> instance ToJSON Color
>>> data Flag = Flag Color Int Int deriving Generic
>>> instance ToJSON Flag
>>> data Country = Country { name :: String, flag :: Flag } deriving Generic
>>> instance ToJSON Country
>>> let country = toJSON $ Country "X" $ Flag Blue 100 50
>>> format1 "{name} {flag!0} {flag!1} {flag!2}" country
"X Blue 100 50"
>>> format "{}" country
"{\"flag\":[\"Blue\",100,50],\"name\":\"X\"}"
-}
module Text.Format.Aeson ( ) where


import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson.KeyMap          as M
import qualified Data.Aeson.Key             as K
import           Data.Scientific            hiding (formatScientific)
import qualified Data.Text                  as T
import           Data.Vector
import           Text.Format


instance FormatArg Value where
  formatArg (Number x) k   = formatScientific (floatingOrInteger x) k
  formatArg (String x) k   = formatString (T.unpack x) k
  formatArg x k            | k == mempty = formatJSON x k
  formatArg (Array xs) k   = formatArray (topKey k) xs (popKey k)
  formatArg (Object obj) k = formatObject (topKey k) obj (popKey k)
  formatArg _ _            = const $ Left $ toException ArgKeyError


formatJSON :: ToJSON a => a -> Formatter
formatJSON = formatString . B.unpack . encode


formatScientific :: (RealFloat r, Integral i) => Either r i -> Formatter
formatScientific (Left x)  = formatRealFloat x
formatScientific (Right x) = formatInteger $ toInteger x


formatArray :: ArgKey -> Array -> Formatter
formatArray (Index i) xs = case (xs !? i) of (Just x) -> formatArg x

formatObject :: ArgKey -> Object -> Formatter
formatObject (Name k) obj =
  case (M.lookup (K.fromText (T.pack k)) obj) of (Just x) -> formatArg x
