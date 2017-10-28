-- |
-- A DSL for declaration of query parameter encoders.
module Hasql.Encoders
(
  -- * Params
  Params,
  unit,
  value,
  nullableValue,
  -- * Value
  Value,
  bool,
  int2,
  int4,
  int8,
  float4,
  float8,
  numeric,
  char,
  text,
  bytea,
  date,
  timestamp,
  timestamptz,
  time,
  timetz,
  interval,
  uuid,
  inet,
  json,
  jsonBytes,
  jsonb,
  jsonbBytes,
  array,
  enum,
  unknown,
  -- * Array
  Array,
  arrayValue,
  arrayNullableValue,
  arrayDimension,
  -- ** Insert Many
  -- $insertMany
)
where

import Hasql.Private.Prelude hiding (bool)
import qualified PostgreSQL.Binary.Encoding as A
import qualified PostgreSQL.Binary.Data as B
import qualified Hasql.Private.Encoders.Params as Params
import qualified Hasql.Private.Encoders.Value as Value
import qualified Hasql.Private.Encoders.Array as Array
import qualified Hasql.Private.PTI as PTI
import qualified Hasql.Private.Prelude as Prelude

-- * Parameters Product Encoder
-------------------------

-- |
-- Encoder of some representation of the parameters product.
-- 
-- Has instances of 'Contravariant', 'Divisible' and 'Monoid',
-- which you can use to compose multiple parameters together.
-- E.g.,
-- 
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contramap' 'fst' ('value' 'int8') '<>'
--   'contramap' 'snd' ('nullableValue' 'text')
-- @
-- 
-- As a general solution for tuples of any arity, instead of 'fst' and 'snd',
-- consider the functions of the @contrazip@ family
-- from the \"contravariant-extras\" package.
-- E.g., here's how you can achieve the same as the above:
-- 
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contrazip2' ('value' 'int8') ('nullableValue' 'text')
-- @
-- 
-- Here's how you can implement encoders for custom composite types:
-- 
-- @
-- data Person =
--   Person { name :: Text, gender :: Gender, age :: Int }
-- 
-- data Gender =
--   Male | Female
-- 
-- personParams :: 'Params' Person
-- personParams =
--   'contramap' name ('value' 'text') '<>'
--   'contramap' gender ('value' genderValue) '<>'
--   'contramap' (fromIntegral . age) ('value' 'int8')
-- 
-- genderValue :: 'Value' Gender
-- genderValue =
--   'contramap' genderText 'text'
--   where
--     genderText gender =
--       case gender of
--         Male -> "male"
--         Female -> "female"
-- @
-- 
newtype Params a =
  Params (Params.Params a)
  deriving (Contravariant, Divisible, Decidable, Monoid, Semigroup)

-- |
-- Encode no parameters.
-- 
{-# INLINABLE unit #-}
unit :: Params ()
unit =
  Params mempty

-- |
-- Lift an individual value encoder to a parameters encoder.
-- 
{-# INLINABLE value #-}
value :: Value a -> Params a
value (Value x) =
  Params (Params.value x)

-- |
-- Lift an individual nullable value encoder to a parameters encoder.
-- 
{-# INLINABLE nullableValue #-}
nullableValue :: Value a -> Params (Maybe a)
nullableValue (Value x) =
  Params (Params.nullableValue x)


-- ** Instances
-------------------------

-- |
-- Maps to 'unit'.
instance Default (Params ()) where
  {-# INLINE def #-}
  def =
    unit

instance Default (Value a) => Default (Params (Identity a)) where
  {-# INLINE def #-}
  def =
    contramap runIdentity (value def)

instance (Default (Value a1), Default (Value a2)) => Default (Params (a1, a2)) where
  {-# INLINE def #-}
  def =
    contrazip2 (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3)) => Default (Params (a1, a2, a3)) where
  {-# INLINE def #-}
  def =
    contrazip3 (value def) (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3), Default (Value a4)) => Default (Params (a1, a2, a3, a4)) where
  {-# INLINE def #-}
  def =
    contrazip4 (value def) (value def) (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3), Default (Value a4), Default (Value a5)) => Default (Params (a1, a2, a3, a4, a5)) where
  {-# INLINE def #-}
  def =
    contrazip5 (value def) (value def) (value def) (value def) (value def)


-- * Value Encoder
-------------------------

-- |
-- An individual value encoder.
-- Will be mapped to a single placeholder in the query.
-- 
newtype Value a =
  Value (Value.Value a)
  deriving (Contravariant)

-- |
-- Encoder of @BOOL@ values.
{-# INLINABLE bool #-}
bool :: Value Bool
bool =
  Value (Value.unsafePTI PTI.bool (const A.bool))

-- |
-- Encoder of @INT2@ values.
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.unsafePTI PTI.int2 (const A.int2_int16))

-- |
-- Encoder of @INT4@ values.
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.unsafePTI PTI.int4 (const A.int4_int32))

-- |
-- Encoder of @INT8@ values.
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  Value (Value.unsafePTI PTI.int8 (const A.int8_int64))

-- |
-- Encoder of @FLOAT4@ values.
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.unsafePTI PTI.float4 (const A.float4))

-- |
-- Encoder of @FLOAT8@ values.
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.unsafePTI PTI.float8 (const A.float8))

-- |
-- Encoder of @NUMERIC@ values.
{-# INLINABLE numeric #-}
numeric :: Value B.Scientific
numeric =
  Value (Value.unsafePTI PTI.numeric (const A.numeric))

-- |
-- Encoder of @CHAR@ values.
-- Note that it supports UTF-8 values and
-- identifies itself under the @TEXT@ OID because of that.
{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.unsafePTI PTI.text (const A.char_utf8))

-- |
-- Encoder of @TEXT@ values.
{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.unsafePTI PTI.text (const A.text_strict))

-- |
-- Encoder of @BYTEA@ values.
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.unsafePTI PTI.bytea (const A.bytea_strict))

-- |
-- Encoder of @DATE@ values.
{-# INLINABLE date #-}
date :: Value B.Day
date =
  Value (Value.unsafePTI PTI.date (const A.date))

-- |
-- Encoder of @TIMESTAMP@ values.
{-# INLINABLE timestamp #-}
timestamp :: Value B.LocalTime
timestamp =
  Value (Value.unsafePTI PTI.timestamp (Prelude.bool A.timestamp_float A.timestamp_int))

-- |
-- Encoder of @TIMESTAMPTZ@ values.
{-# INLINABLE timestamptz #-}
timestamptz :: Value B.UTCTime
timestamptz =
  Value (Value.unsafePTI PTI.timestamptz (Prelude.bool A.timestamptz_float A.timestamptz_int))

-- |
-- Encoder of @TIME@ values.
{-# INLINABLE time #-}
time :: Value B.TimeOfDay
time =
  Value (Value.unsafePTI PTI.time (Prelude.bool A.time_float A.time_int))

-- |
-- Encoder of @TIMETZ@ values.
{-# INLINABLE timetz #-}
timetz :: Value (B.TimeOfDay, B.TimeZone)
timetz =
  Value (Value.unsafePTI PTI.timetz (Prelude.bool A.timetz_float A.timetz_int))

-- |
-- Encoder of @INTERVAL@ values.
{-# INLINABLE interval #-}
interval :: Value B.DiffTime
interval =
  Value (Value.unsafePTI PTI.interval (Prelude.bool A.interval_float A.interval_int))

-- |
-- Encoder of @UUID@ values.
{-# INLINABLE uuid #-}
uuid :: Value B.UUID
uuid =
  Value (Value.unsafePTI PTI.uuid (const A.uuid))

-- |
-- Encoder of @INET@ values.
{-# INLINABLE inet #-}
inet :: Value (B.NetAddr B.IP)
inet =
  Value (Value.unsafePTI PTI.inet (const A.inet))

-- |
-- Encoder of @JSON@ values from JSON AST.
{-# INLINABLE json #-}
json :: Value B.Value
json =
  Value (Value.unsafePTI PTI.json (const A.json_ast))

-- |
-- Encoder of @JSON@ values from raw JSON.
{-# INLINABLE jsonBytes #-}
jsonBytes :: Value ByteString
jsonBytes =
  Value (Value.unsafePTI PTI.json (const A.json_bytes))

-- |
-- Encoder of @JSONB@ values from JSON AST.
{-# INLINABLE jsonb #-}
jsonb :: Value B.Value
jsonb =
  Value (Value.unsafePTI PTI.jsonb (const A.jsonb_ast))

-- |
-- Encoder of @JSONB@ values from raw JSON.
{-# INLINABLE jsonbBytes #-}
jsonbBytes :: Value ByteString
jsonbBytes =
  Value (Value.unsafePTI PTI.jsonb (const A.jsonb_bytes))

-- |
-- Unlifts the 'Array' encoder to the plain 'Value' encoder.
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) =
  Array.run imp & \(arrayOID, encoder') ->
    Value (Value.Value arrayOID arrayOID encoder')

-- |
-- Given a function,
-- which maps the value into the textual enum label from the DB side,
-- produces a encoder of that value.
{-# INLINABLE enum #-}
enum :: (a -> Text) -> Value a
enum mapping =
  Value (Value.unsafePTI PTI.text (const (A.text_strict . mapping)))

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
{-# INLINABLE unknown #-}
unknown :: Value ByteString
unknown =
  Value (Value.unsafePTI PTI.unknown (const A.bytea_strict))


-- ** Instances
-------------------------

-- | Maps to 'bool'.
instance Default (Value Bool) where
  {-# INLINE def #-}
  def =
    bool

-- | Maps to 'int2'.
instance Default (Value Int16) where
  {-# INLINE def #-}
  def =
    int2

-- | Maps to 'int4'.
instance Default (Value Int32) where
  {-# INLINE def #-}
  def =
    int4

-- | Maps to 'int8'.
instance Default (Value Int64) where
  {-# INLINE def #-}
  def =
    int8

-- | Maps to 'float4'.
instance Default (Value Float) where
  {-# INLINE def #-}
  def =
    float4

-- | Maps to 'float8'.
instance Default (Value Double) where
  {-# INLINE def #-}
  def =
    float8

-- | Maps to 'numeric'.
instance Default (Value B.Scientific) where
  {-# INLINE def #-}
  def =
    numeric

-- | Maps to 'char'.
instance Default (Value Char) where
  {-# INLINE def #-}
  def =
    char

-- | Maps to 'text'.
instance Default (Value Text) where
  {-# INLINE def #-}
  def =
    text

-- | Maps to 'bytea'.
instance Default (Value ByteString) where
  {-# INLINE def #-}
  def =
    bytea

-- | Maps to 'date'.
instance Default (Value B.Day) where
  {-# INLINE def #-}
  def =
    date

-- | Maps to 'timestamp'.
instance Default (Value B.LocalTime) where
  {-# INLINE def #-}
  def =
    timestamp

-- | Maps to 'timestamptz'.
instance Default (Value B.UTCTime) where
  {-# INLINE def #-}
  def =
    timestamptz

-- | Maps to 'time'.
instance Default (Value B.TimeOfDay) where
  {-# INLINE def #-}
  def =
    time

-- | Maps to 'timetz'.
instance Default (Value (B.TimeOfDay, B.TimeZone)) where
  {-# INLINE def #-}
  def =
    timetz

-- | Maps to 'interval'.
instance Default (Value B.DiffTime) where
  {-# INLINE def #-}
  def =
    interval

-- | Maps to 'uuid'.
instance Default (Value B.UUID) where
  {-# INLINE def #-}
  def =
    uuid

-- | Maps to 'json'.
instance Default (Value B.Value) where
  {-# INLINE def #-}
  def =
    json


-- * Array
-------------------------

-- |
-- A generic array encoder.
-- 
-- Here's an example of its usage:
-- 
-- >x :: Value [[Int64]]
-- >x =
-- >  array (arrayDimension foldl' (arrayDimension foldl' (arrayValue int8)))
-- 
-- Please note that the PostgreSQL __IN__ keyword does not "accept" an array, but rather a syntactical list of
-- values, thus this encoder is not suited for that. Use a **field** = ANY($1) query instead.
--
newtype Array a =
  Array (Array.Array a)

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a non-nullable value.
{-# INLINABLE arrayValue #-}
arrayValue :: Value a -> Array a
arrayValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.value elementOID arrayOID encoder')

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a nullable value.
{-# INLINABLE arrayNullableValue #-}
arrayNullableValue :: Value a -> Array (Maybe a)
arrayNullableValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.nullableValue elementOID arrayOID encoder')

-- |
-- An encoder of an array dimension,
-- which thus provides support for multidimensional arrays.
-- 
-- Accepts:
-- 
-- * An implementation of the left-fold operation,
-- such as @Data.Foldable.'foldl''@,
-- which determines the input value.
-- 
-- * A component encoder, which can be either another 'arrayDimension',
-- 'arrayValue' or 'arrayNullableValue'.
-- 
{-# INLINABLE arrayDimension #-}
arrayDimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
arrayDimension foldl (Array imp) =
  Array (Array.dimension foldl imp)

-- $insertMany
-- It is not currently possible to pass in an array of encodable values
-- to use in an 'insert many' query using Hasql. Instead, PostgreSQL's
--  (9.4 or later) `unnest` function can be used to in an analogous way
-- to haskell's `zip` function by passing in multiple arrays of values
-- to be zipped into the rows we want to insert:
--
-- @
--   insertMultipleLocations :: Query (Vector (UUID, Double, Double)) ()
--   insertMultipleLocations =
--     statement sql encoder decoder True
--     where
--       sql =
--         "insert into location (id, x, y) select * from unnest ($1, $2, $3)"
--       encoder =
--         contramap Vector.unzip3 $
--         contrazip3 (vector Encoders.uuid) (vector Encoders.float8) (vector Encoders.float8)
--         where
--           vector value =
--             Encoders.value (Encoders.array (Encoders.arrayDimension foldl' (Encoders.arrayValue value)))
--       decoder =
--         Decoders.unit
-- @