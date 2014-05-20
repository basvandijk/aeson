{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types #-}

-- |
-- Module:      Data.Aeson.Types.Internal
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Internal
    (
      -- * Core JSON types
      JsonBuilder(..)
    , IStream(..)

    , jsonNull

      -- ** Booleans
    , jsonTrue
    , jsonFalse

      -- ** Strings
    , jsonChar
    , jsonString
    , jsonText

      -- ** Numbers
    , jsonInt
    , jsonInt8
    , jsonInt16
    , jsonInt32
    , jsonInt64

    , jsonWord
    , jsonWord8
    , jsonWord16
    , jsonWord32
    , jsonWord64

    , jsonInteger

    , jsonScientific

      -- ** Arrays
    , emptyArray
    , singletonArray
    , many1Array
    , CommaPrefixedElements
    , element

      -- ** Objects
    , emptyObject
    , singletonObject
    , JsonFirstProperty
    , jsonFirstProperty
    , many1Object
    , CommaPrefixedProperties
    , commaPrefixedProperty
    , Pair
    , object

    , jsonValue

    -- * JSON Value
    , Value(..)
    , Array
    , isEmptyArray
    , Object
    -- * Type conversion
    , Parser
    , Result(..)
    , parse
    , parseEither
    , parseMaybe
    , modifyFailure

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

    -- * Used for changing CamelCase names into something else.
    , camelTo

    -- * Other types
    , DotNetTime(..)
    ) where


import Control.Applicative
import Control.Monad
import Control.DeepSeq (NFData(..))
import Data.Char (toLower, isUpper)
import Data.Scientific (Scientific)
import Data.Hashable (Hashable(..))
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Foldable (Foldable, foldMap)
import Data.Int
import Data.Word
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Format (FormatTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

---------------------------------------------------------------------------
-- Instruction stream
---------------------------------------------------------------------------

data IStream = INull                              IStream

             | ITrue                              IStream
             | IFalse                             IStream

             | IDoubleQuote                       IStream

             | IChar   {-# UNPACK #-} !Char       IStream
             | IString                !String     IStream
             | IText                  !Text       IStream

             | IInt    {-# UNPACK #-} !Int        IStream
             | IInt8   {-# UNPACK #-} !Int8       IStream
             | IInt16  {-# UNPACK #-} !Int16      IStream
             | IInt32  {-# UNPACK #-} !Int32      IStream
             | IInt64  {-# UNPACK #-} !Int64      IStream

             | IWord   {-# UNPACK #-} !Word       IStream
             | IWord8  {-# UNPACK #-} !Word8      IStream
             | IWord16 {-# UNPACK #-} !Word16     IStream
             | IWord32 {-# UNPACK #-} !Word32     IStream
             | IWord64 {-# UNPACK #-} !Word64     IStream

             | IFloat  {-# UNPACK #-} !Float      IStream
             | IDouble {-# UNPACK #-} !Double     IStream

             | IInteger               !Integer    IStream

             | IScientific            !Scientific IStream

             | IComma                             IStream

             | IBeginArray                        IStream
             | IEndArray                          IStream

             | IBeginObject                       IStream
             | IEndObject                         IStream

             | IColon                             IStream

             | IValue                 !Value      IStream

             -- Fused:
             | IBeginObject_IDoubleQuote          IStream
             | IComma_IDoubleQuote                IStream

             | IEnd

{-# RULES
  "IBeginObject_IDoubleQuote" forall is.
   IBeginObject (IDoubleQuote is) =
   IBeginObject_IDoubleQuote is #-}

{-# RULES
  "IComma_IDoubleQuote" forall is.
   IComma (IDoubleQuote is) =
   IComma_IDoubleQuote  is #-}


---------------------------------------------------------------------------
-- Json builders
---------------------------------------------------------------------------

newtype JsonBuilder = JsonBuilder {unJsonBuilder :: IStreamDiff}

-- | A difference list
type IStreamDiff = IStream -> IStream

---------------------------------------------------------------------------
-- JsonBuilder constructors
---------------------------------------------------------------------------

-- | Encoded to: @null@.
jsonNull :: JsonBuilder
jsonNull = JsonBuilder INull
{-# INLINE jsonNull #-}


---------------------------------------------------------------------------
-- Booleans

-- | Encoded to: @true@.
jsonTrue :: JsonBuilder
jsonTrue = JsonBuilder ITrue
{-# INLINE jsonTrue #-}

-- | Encoded to: @false@.
jsonFalse :: JsonBuilder
jsonFalse = JsonBuilder IFalse
{-# INLINE jsonFalse #-}


---------------------------------------------------------------------------
-- Strings

jsonChar :: Char -> JsonBuilder
jsonChar = doubleQuotes . IChar
{-# INLINE jsonChar #-}

jsonString :: String -> JsonBuilder
jsonString = doubleQuotes . IString
{-# INLINE jsonString #-}

jsonText :: Text -> JsonBuilder
jsonText = doubleQuotes . IText
{-# INLINE jsonText #-}

doubleQuotes :: IStreamDiff -> JsonBuilder
doubleQuotes = \f -> JsonBuilder $ IDoubleQuote . f . IDoubleQuote
{-# INLINE doubleQuotes #-}


---------------------------------------------------------------------------
-- Numbers

jsonInt :: Int -> JsonBuilder
jsonInt = JsonBuilder . IInt
{-# INLINE jsonInt #-}

jsonInt8 :: Int8 -> JsonBuilder
jsonInt8 = JsonBuilder . IInt8
{-# INLINE jsonInt8 #-}

jsonInt16 :: Int16 -> JsonBuilder
jsonInt16 = JsonBuilder . IInt16
{-# INLINE jsonInt16 #-}

jsonInt32 :: Int32 -> JsonBuilder
jsonInt32 = JsonBuilder . IInt32
{-# INLINE jsonInt32 #-}

jsonInt64 :: Int64 -> JsonBuilder
jsonInt64 = JsonBuilder . IInt64
{-# INLINE jsonInt64 #-}


jsonWord :: Word -> JsonBuilder
jsonWord = JsonBuilder . IWord
{-# INLINE jsonWord #-}

jsonWord8 :: Word8 -> JsonBuilder
jsonWord8 = JsonBuilder . IWord8
{-# INLINE jsonWord8 #-}

jsonWord16 :: Word16 -> JsonBuilder
jsonWord16 = JsonBuilder . IWord16
{-# INLINE jsonWord16 #-}

jsonWord32 :: Word32 -> JsonBuilder
jsonWord32 = JsonBuilder . IWord32
{-# INLINE jsonWord32 #-}

jsonWord64 :: Word64 -> JsonBuilder
jsonWord64 = JsonBuilder . IWord64
{-# INLINE jsonWord64 #-}


jsonInteger :: Integer -> JsonBuilder
jsonInteger = JsonBuilder . IInteger
{-# INLINE jsonInteger #-}

jsonScientific :: Scientific -> JsonBuilder
jsonScientific = JsonBuilder . IScientific
{-# INLINE jsonScientific #-}

---------------------------------------------------------------------------
-- Arrays

-- | The empty array.
--
-- Encoded to: @[]@
emptyArray :: JsonBuilder
emptyArray = JsonBuilder $ IBeginArray . IEndArray
{-# INLINE emptyArray #-}

-- | An array with a single element @e@:
--
-- Encoded to: @[e]@.
singletonArray :: JsonBuilder -> JsonBuilder
singletonArray = \(JsonBuilder f) -> JsonBuilder $
                 IBeginArray . f . IEndArray
{-# INLINE singletonArray #-}

-- | An array with a first element @e@ and other comma prefixed
-- elements @es@.
--
-- Encoded to: @[e,es_1...,es_n]@.
--
-- @,es_1...,es_n@ can be null when @es = 'mempty'@.
many1Array :: JsonBuilder -> CommaPrefixedElements -> JsonBuilder
many1Array = \(JsonBuilder frst) (CommaPrefixedElements rest) ->
             JsonBuilder $ IBeginArray . frst . rest . IEndArray
{-# INLINE many1Array #-}

-- | Constructs an array element @e@ other than the first.
--
-- Encoded to: @,e@.
element :: JsonBuilder -> CommaPrefixedElements
element = \(JsonBuilder f) -> CommaPrefixedElements $ IComma . f
{-# INLINE element #-}

-- | An array element other than the first element of the array.
--
-- Gets prefixed with a comma when encoded.
newtype CommaPrefixedElements = CommaPrefixedElements IStreamDiff

instance Monoid CommaPrefixedElements where
    mempty = CommaPrefixedElements $ \is -> is
    {-# INLINE mempty #-}

    CommaPrefixedElements f `mappend` CommaPrefixedElements g =
      CommaPrefixedElements $ \is -> f (g is)
    {-# INLINE mappend #-}


---------------------------------------------------------------------------
-- Objects

-- | The empty object.
--
-- Encoded to: @{}@.
emptyObject :: JsonBuilder
emptyObject = JsonBuilder $ IBeginObject . IEndObject
{-# INLINE emptyObject #-}

-- | An object with a single property @p@.
--
-- Encoded to: @{p}@.
singletonObject :: JsonFirstProperty -> JsonBuilder
singletonObject = \(JsonFirstProperty frst) -> JsonBuilder $
  IBeginObject . frst . IEndObject
{-# INLINE singletonObject #-}

-- | The first property of an object.
--
-- Unlike 'CommaPrefixedProperties' this does not get prefixed with a
-- comma when encoded.
newtype JsonFirstProperty = JsonFirstProperty IStreamDiff

-- | Prefix version of '.='. Constructs the first property of an
-- object with key @k@ and value @v@.
--
-- Encoded to: @k:v@.
jsonFirstProperty :: Text        -- ^ key
                  -> JsonBuilder -- ^ value
                  -> JsonFirstProperty
jsonFirstProperty = \k b -> JsonFirstProperty $ jsonProperty k b
{-# INLINE jsonFirstProperty #-}

jsonProperty :: Text -> JsonBuilder -> IStreamDiff
jsonProperty = \k (JsonBuilder f) -> unJsonBuilder (jsonText k) . IColon . f
{-# INLINE jsonProperty #-}

-- | An object with a first property @p@ and other comma prefixed
-- properties @es@.
--
-- Encoded to: @{p,ps_1...,ps_n}@.
--
-- @,ps_1...,ps_n@ can be nul when @ps = 'mempty'@.
many1Object :: JsonFirstProperty -> CommaPrefixedProperties -> JsonBuilder
many1Object = \(JsonFirstProperty frst) (CommaPrefixedProperties rest) ->
  JsonBuilder $ IBeginObject . frst . rest . IEndObject
{-# INLINE many1Object #-}

-- | Prefix version of '.='. Constructs an object property other than
-- the first with key @k@ and value @v@.
--
-- Encoded to: @,k:v@.
commaPrefixedProperty :: Text        -- ^ key
                      -> JsonBuilder -- ^ value
                      -> CommaPrefixedProperties
commaPrefixedProperty = \k b -> CommaPrefixedProperties $
                                  IComma . jsonProperty k b
{-# INLINE commaPrefixedProperty #-}

-- | An object property other than the first property of the object.
--
-- Gets prefixed with a comma when encoded.
newtype CommaPrefixedProperties = CommaPrefixedProperties IStreamDiff

instance Monoid CommaPrefixedProperties where
    mempty = CommaPrefixedProperties $ \is -> is
    {-# INLINE mempty #-}

    CommaPrefixedProperties f `mappend` CommaPrefixedProperties g =
      CommaPrefixedProperties $ \is -> f (g is)
    {-# INLINE mappend #-}

type Pair = (Text, JsonBuilder)

-- | Create a 'JsonBuilder' from a list of name\/value 'Pair's:
--
-- @[(k_1, v_1), (k_2, v_2)..., (k_n, v_n)]@.
--
-- Encoded to: @{k_1:v_1,k_2:v_2...,k_n:v_n}@.
object :: [Pair] -> JsonBuilder
object = \pairs -> case pairs of
  []         -> emptyObject
  ((k,v):[]) -> singletonObject $ jsonFirstProperty k v
  ((k,v):ps) -> many1Object (jsonFirstProperty k v) $
                 foldMap (uncurry commaPrefixedProperty) ps
{-# INLINE object #-}

---------------------------------------------------------------------------
-- Miscellaneous

jsonValue :: Value -> JsonBuilder
jsonValue = JsonBuilder . IValue
{-# INLINE jsonValue #-}


---------------------------------------------------------------------------
-- JSON Value
---------------------------------------------------------------------------

-- | A JSON \"object\" (key\/value map).
type Object = HashMap Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable, Data)

-- | A newtype wrapper for 'UTCTime' that uses the same non-standard
-- serialization format as Microsoft .NET, whose @System.DateTime@
-- type is by default serialized to JSON as in the following example:
--
-- > /Date(1302547608878)/
--
-- The number represents milliseconds since the Unix epoch.
newtype DotNetTime = DotNetTime {
      fromDotNetTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = rnf n
    rnf (Bool b)   = rnf b
    rnf Null       = ()

instance IsString Value where
    fromString = String . pack
    {-# INLINE fromString #-}

hashValue :: Int -> Value -> Int
hashValue s (Object o)   = H.foldl' hashWithSalt
                              (s `hashWithSalt` (0::Int)) o
hashValue s (Array a)    = V.foldl' hashWithSalt
                              (s `hashWithSalt` (1::Int)) a
hashValue s (String str) = s `hashWithSalt` (2::Int) `hashWithSalt` str
hashValue s (Number n)   = s `hashWithSalt` (3::Int) `hashWithSalt` n
hashValue s (Bool b)     = s `hashWithSalt` (4::Int) `hashWithSalt` b
hashValue s Null         = s `hashWithSalt` (5::Int)

instance Hashable Value where
    hashWithSalt = hashValue

-- | Determines if the 'Value' is an empty 'Array'.
-- Note that: @isEmptyArray 'emptyArray'@.
isEmptyArray :: Value -> Bool
isEmptyArray (Array arr) = V.null arr
isEmptyArray _ = False

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right
{-# INLINE parseEither #-}

-- | If the inner @Parser@ failed, modify the failure message using the
-- provided function. This allows you to create more descriptive error messages.
-- For example:
--
-- > parseJSON (Object o) = modifyFailure
-- >     ("Parsing of the Foo value failed: " ++)
-- >     (Foo <$> o .: "someField")
--
-- Since 0.6.2.0
modifyFailure :: (String -> String) -> Parser a -> Parser a
modifyFailure f (Parser p) = Parser $ \kf -> p (kf . f)

--------------------------------------------------------------------------------
-- Generic and TH encoding configuration
--------------------------------------------------------------------------------

-- | Options that specify how to encode\/decode your datatype to\/from JSON.
data Options = Options
    { fieldLabelModifier :: String -> String
      -- ^ Function applied to field labels.
      -- Handy for removing common record prefixes for example.
    , constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags which could be handy
      -- for lower-casing them for example.
    , allNullaryToStringTag :: Bool
      -- ^ If 'True' the constructors of a datatype, with /all/
      -- nullary constructors, will be encoded to just a string with
      -- the constructor tag. If 'False' the encoding will always
      -- follow the `sumEncoding`.
    , omitNothingFields :: Bool
      -- ^ If 'True' record fields with a 'Nothing' value will be
      -- omitted from the resulting object. If 'False' the resulting
      -- object will include those fields mapping to @null@.
    , sumEncoding :: SumEncoding
      -- ^ Specifies how to encode constructors of a sum datatype.
    }

-- | Specifies how to encode constructors of a sum datatype.
data SumEncoding =
    TaggedObject { tagFieldName      :: String
                 , contentsFieldName :: String
                 }
    -- ^ A constructor will be encoded to an object with a field
    -- 'tagFieldName' which specifies the constructor tag (modified by
    -- the 'constructorTagModifier'). If the constructor is a record
    -- the encoded record fields will be unpacked into this object. So
    -- make sure that your record doesn't have a field with the same
    -- label as the 'tagFieldName'. Otherwise the tag gets overwritten
    -- by the encoded value of that field! If the constructor is not a
    -- record the encoded constructor contents will be stored under
    -- the 'contentsFieldName' field.
  | ObjectWithSingleField
    -- ^ A constructor will be encoded to an object with a single
    -- field named after the constructor tag (modified by the
    -- 'constructorTagModifier') which maps to the encoded contents of
    -- the constructor.
  | TwoElemArray
    -- ^ A constructor will be encoded to a 2-element array where the
    -- first element is the tag of the constructor (modified by the
    -- 'constructorTagModifier') and the second element the encoded
    -- contents of the constructor.

-- | Default encoding 'Options':
--
-- @
-- 'Options'
-- { 'fieldLabelModifier'      = id
-- , 'constructorTagModifier'  = id
-- , 'allNullaryToStringTag'   = True
-- , 'omitNothingFields'       = False
-- , 'sumEncoding'             = 'defaultTaggedObject'
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
                 { fieldLabelModifier      = id
                 , constructorTagModifier  = id
                 , allNullaryToStringTag   = True
                 , omitNothingFields       = False
                 , sumEncoding             = defaultTaggedObject
                 }

-- | Default 'TaggedObject' 'SumEncoding' options:
--
-- @
-- defaultTaggedObject = 'TaggedObject'
--                       { 'tagFieldName'      = \"tag\"
--                       , 'contentsFieldName' = \"contents\"
--                       }
-- @
defaultTaggedObject :: SumEncoding
defaultTaggedObject = TaggedObject
                      { tagFieldName      = "tag"
                      , contentsFieldName = "contents"
                      }

-- | Converts from CamelCase to another lower case, interspersing
--   the character between all capital letters and their previous
--   entries, except those capital letters that appear together,
--   like 'API'.
--
--   For use by Aeson template haskell calls.
--
--   > camelTo '_' 'CamelCaseAPI' == "camel_case_api"
camelTo :: Char -> String -> String
camelTo c = lastWasCap True
  where
    lastWasCap :: Bool    -- ^ Previous was a capital letter
              -> String  -- ^ The remaining string
              -> String
    lastWasCap _    []           = []
    lastWasCap prev (x : xs)     = if isUpper x
                                      then if prev
                                             then toLower x : lastWasCap True xs
                                             else c : toLower x : lastWasCap True xs
                                      else x : lastWasCap False xs
