-- |
-- A DSL for declaration of result decoders.
module Hasql.Decoders
(
  -- * Result
  Result,
  unit,
  rowsAffected,
  singleRow,
  -- ** Specialized multi-row results
  rowMaybe,
  rowVector,
  rowList,
  -- ** Multi-row traversers
  foldlRows,
  foldrRows,
  -- * Columnar types
  Columnar(..),
  -- * Positioned types
  Positioned(..),
  -- * Named types
  Named(..),
  -- * Row
  Row,
  -- ** Row metadata
  columnNames,
  columnPosition,
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
  composite,
  hstore,
  enum,
  custom,
  refine,
  -- * Array
  Array,
  dimension,
  -- * Composite
  Composite,
  -- * NullableGroup
  NullableGroup,
  group
)
where

import Hasql.Private.Prelude hiding (maybe, bool, group)
import qualified Data.Vector as Vector
import qualified PostgreSQL.Binary.Decoding as A
import qualified PostgreSQL.Binary.Data as B
import qualified Hasql.Private.Decoders.Results as Results
import qualified Hasql.Private.Decoders.Result as Result
import qualified Hasql.Private.Decoders.Row as Row
import qualified Hasql.Private.Decoders.Value as Value
import qualified Hasql.Private.Decoders.Array as Array
import qualified Hasql.Private.Decoders.Composite as Composite
import qualified Hasql.Private.Decoders.NullableGroup as Group
import qualified Hasql.Private.Prelude as Prelude

-- * Result
-------------------------

-- |
-- Decoder of a query result.
--
newtype Result a =
  Result (Results.Results a)
  deriving (Functor)

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
--
{-# INLINABLE unit #-}
unit :: Result ()
unit =
  Result (Results.single Result.unit)

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
--
{-# INLINABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  Result (Results.single Result.rowsAffected)

-- |
-- Exactly one row.
-- Will raise the 'Hasql.Errors.UnexpectedAmountOfRows' error if it's any other.
--
{-# INLINABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) =
  Result (Results.single (Result.single row))

-- ** Multi-row traversers
-------------------------

-- |
-- Foldl multiple rows.
--
{-# INLINABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row row) =
  Result (Results.single (Result.foldl step init row))

-- |
-- Foldr multiple rows.
--
{-# INLINABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) =
  Result (Results.single (Result.foldr step init row))

-- ** Specialized multi-row results
-------------------------

-- |
-- Maybe one row or none.
--
{-# INLINABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row row) =
  Result (Results.single (Result.maybe row))

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
--
{-# INLINABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector (Row row) =
  Result (Results.single (Result.vector row))

-- |
-- Zero or more rows packed into the list.
--
{-# INLINABLE rowList #-}
rowList :: Row a -> Result [a]
rowList =
  foldrRows strictCons []


-- ** Instances
-------------------------

-- | Maps to 'unit'.
instance Default (Result ()) where
  {-# INLINE def #-}
  def =
    unit

-- | Maps to 'rowsAffected'.
instance Default (Result Int64) where
  {-# INLINE def #-}
  def =
    rowsAffected

-- | Maps to @('rowMaybe' def)@.
instance Default (Row a) => Default (Result (Maybe a)) where
  {-# INLINE def #-}
  def =
    rowMaybe def

-- | Maps to @('rowVector' def)@.
instance Default (Row a) => Default (Result (Vector a)) where
  {-# INLINE def #-}
  def =
    rowVector def

-- | Maps to @('rowList' def)@.
instance Default (Row a) => Default (Result ([] a)) where
  {-# INLINE def #-}
  def =
    rowList def

-- | Maps to @(fmap Identity ('singleRow' def)@.
instance Default (Row a) => Default (Result (Identity a)) where
  {-# INLINE def #-}
  def =
    fmap Identity (singleRow def)


-- * Row
-------------------------

-- |
-- Decoder of an individual row,
-- which gets composed of column value decoders.
--
-- E.g.:
--
-- >x :: Row (Maybe Int64, Text, TimeOfDay)
-- >x =
-- >  (,,) <$> nullableColumn int8 <*> column text <*> column time
--
newtype Row a =
  Row (Row.Row a)
  deriving (Functor, Applicative, Monad)

{-
-- |
-- Lift one or more nullable value decodings into a group
--
nullableColumns :: Row a -> Row (Maybe a)
nullableColumns (Row (Row.Row oneOrMoreColumns)) =
  Row (Row.Row $ local (\(env, _) -> env)) oneOrMoreColumns)
-}

-- ** Instances
-------------------------

instance Default (Value a) => Default (Row (Identity a)) where
  {-# INLINE def #-}
  def =
    fmap Identity (field def)

instance Default (Value a) => Default (Row (Maybe a)) where
  {-# INLINE def #-}
  def =
    nullableField def

instance (Default (Value a1), Default (Value a2)) => Default (Row (a1, a2)) where
  {-# INLINE def #-}
  def =
    ap (fmap (,) (field def)) (field def)

-- ** Row metadata

-- |
-- The index of the next unconsumed column. Not recommended
-- for general use, but may be useful in tandem with 'columnNames'.
--
{-# INLINE columnPosition #-}
columnPosition :: Row Int
columnPosition = Row Row.columnPosition

-- |
-- A vector containing all of the column names within the Row.
-- Identical for all 'Row's within a 'Result'
--
{-# INLINE columnNames #-}
columnNames :: Row (Vector ByteString)
columnNames = Row Row.columnNames

-- * Value
-------------------------

-- |
-- Decoder of an individual value.
--
newtype Value a =
  Value (Value.Value a)
  deriving (Functor)


-- ** Plain value decoders
-------------------------

-- |
-- Decoder of the @BOOL@ values.
--
{-# INLINABLE bool #-}
bool :: Value Bool
bool =
  Value (Value.decoder (const A.bool))

-- |
-- Decoder of the @INT2@ values.
--
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.decoder (const A.int))

-- |
-- Decoder of the @INT4@ values.
--
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.decoder (const A.int))

-- |
-- Decoder of the @INT8@ values.
--
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-}
  Value (Value.decoder (const ({-# SCC "int8.int" #-} A.int)))

-- |
-- Decoder of the @FLOAT4@ values.
--
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.decoder (const A.float4))

-- |
-- Decoder of the @FLOAT8@ values.
--
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.decoder (const A.float8))

-- |
-- Decoder of the @NUMERIC@ values.
--
{-# INLINABLE numeric #-}
numeric :: Value B.Scientific
numeric =
  Value (Value.decoder (const A.numeric))

-- |
-- Decoder of the @CHAR@ values.
-- Note that it supports UTF-8 values.
{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.decoder (const A.char))

-- |
-- Decoder of the @TEXT@ values.
--
{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.decoder (const A.text_strict))

-- |
-- Decoder of the @BYTEA@ values.
--
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.decoder (const A.bytea_strict))

-- |
-- Decoder of the @DATE@ values.
--
{-# INLINABLE date #-}
date :: Value B.Day
date =
  Value (Value.decoder (const A.date))

-- |
-- Decoder of the @TIMESTAMP@ values.
--
{-# INLINABLE timestamp #-}
timestamp :: Value B.LocalTime
timestamp =
  Value (Value.decoder (Prelude.bool A.timestamp_float A.timestamp_int))

-- |
-- Decoder of the @TIMESTAMPTZ@ values.
--
-- /NOTICE/
--
-- Postgres does not store the timezone information of @TIMESTAMPTZ@.
-- Instead it stores a UTC value and performs silent conversions
-- to the currently set timezone, when dealt with in the text format.
-- However this library bypasses the silent conversions
-- and communicates with Postgres using the UTC values directly.
{-# INLINABLE timestamptz #-}
timestamptz :: Value B.UTCTime
timestamptz =
  Value (Value.decoder (Prelude.bool A.timestamptz_float A.timestamptz_int))

-- |
-- Decoder of the @TIME@ values.
--
{-# INLINABLE time #-}
time :: Value B.TimeOfDay
time =
  Value (Value.decoder (Prelude.bool A.time_float A.time_int))

-- |
-- Decoder of the @TIMETZ@ values.
--
-- Unlike in case of @TIMESTAMPTZ@,
-- Postgres does store the timezone information for @TIMETZ@.
-- However the Haskell's \"time\" library does not contain any composite type,
-- that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
-- to represent a value on the Haskell's side.
{-# INLINABLE timetz #-}
timetz :: Value (B.TimeOfDay, B.TimeZone)
timetz =
  Value (Value.decoder (Prelude.bool A.timetz_float A.timetz_int))

-- |
-- Decoder of the @INTERVAL@ values.
--
{-# INLINABLE interval #-}
interval :: Value B.DiffTime
interval =
  Value (Value.decoder (Prelude.bool A.interval_float A.interval_int))

-- |
-- Decoder of the @UUID@ values.
--
{-# INLINABLE uuid #-}
uuid :: Value B.UUID
uuid =
  Value (Value.decoder (const A.uuid))

-- |
-- Decoder of the @INET@ values.
--
{-# INLINABLE inet #-}
inet :: Value (B.NetAddr B.IP)
inet =
  Value (Value.decoder (const A.inet))

-- |
-- Decoder of the @JSON@ values into a JSON AST.
--
{-# INLINABLE json #-}
json :: Value B.Value
json =
  Value (Value.decoder (const A.json_ast))

-- |
-- Decoder of the @JSON@ values into a raw JSON 'ByteString'.
--
{-# INLINABLE jsonBytes #-}
jsonBytes :: (ByteString -> Either Text a) -> Value a
jsonBytes fn =
  Value (Value.decoder (const (A.json_bytes fn)))

-- |
-- Decoder of the @JSONB@ values into a JSON AST.
--
{-# INLINABLE jsonb #-}
jsonb :: Value B.Value
jsonb =
  Value (Value.decoder (const A.jsonb_ast))

-- |
-- Decoder of the @JSONB@ values into a raw JSON 'ByteString'.
--
{-# INLINABLE jsonbBytes #-}
jsonbBytes :: (ByteString -> Either Text a) -> Value a
jsonbBytes fn =
  Value (Value.decoder (const (A.jsonb_bytes fn)))

-- |
-- Lifts a custom value decoder function to a 'Value' decoder.
--
{-# INLINABLE custom #-}
custom :: (Bool -> ByteString -> Either Text a) -> Value a
custom fn =
  Value (Value.decoderFn fn)

-- |
-- Maps a custom decoding over an existing 'Value' decoder.
--
{-# INLINABLE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value v) =
  Value (Value.Value (ask >>= \b -> lift (A.refine fn (Value.run v b))))


-- ** Composite value decoders
-------------------------

-- |
-- Lifts the 'Array' decoder to the 'Value' decoder.
--
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) =
  Value (Value.decoder (Array.run imp))

-- |
-- Lifts the 'Composite' decoder to the 'Value' decoder.
--
{-# INLINABLE composite #-}
composite :: Composite a -> Value a
composite (Composite imp) =
  Value (Value.decoder (Composite.run imp))

-- |
-- A generic decoder of @HSTORE@ values.
--
-- Here's how you can use it to construct a specific value:
--
-- @
-- x :: Value [(Text, Maybe Text)]
-- x =
--   hstore 'replicateM'
-- @
--
{-# INLINABLE hstore #-}
hstore :: (forall m. Monad m => Int -> m (Text, Maybe Text) -> m a) -> Value a
hstore replicateM =
  Value (Value.decoder (const (A.hstore replicateM A.text_strict A.text_strict)))

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
enum :: (Text -> Maybe a) -> Value a
enum mapping =
  Value (Value.decoder (const (A.enum mapping)))


-- ** Instances
-------------------------

-- |
-- Maps to 'bool'.
instance Default (Value Bool) where
  {-# INLINE def #-}
  def =
    bool

-- |
-- Maps to 'int2'.
instance Default (Value Int16) where
  {-# INLINE def #-}
  def =
    int2

-- |
-- Maps to 'int4'.
instance Default (Value Int32) where
  {-# INLINE def #-}
  def =
    int4

-- |
-- Maps to 'int8'.
instance Default (Value Int64) where
  {-# INLINE def #-}
  def =
    int8

-- |
-- Maps to 'float4'.
instance Default (Value Float) where
  {-# INLINE def #-}
  def =
    float4

-- |
-- Maps to 'float8'.
instance Default (Value Double) where
  {-# INLINE def #-}
  def =
    float8

-- |
-- Maps to 'numeric'.
instance Default (Value B.Scientific) where
  {-# INLINE def #-}
  def =
    numeric

-- |
-- Maps to 'char'.
instance Default (Value Char) where
  {-# INLINE def #-}
  def =
    char

-- |
-- Maps to 'text'.
instance Default (Value Text) where
  {-# INLINE def #-}
  def =
    text

-- |
-- Maps to 'bytea'.
instance Default (Value ByteString) where
  {-# INLINE def #-}
  def =
    bytea

-- |
-- Maps to 'date'.
instance Default (Value B.Day) where
  {-# INLINE def #-}
  def =
    date

-- |
-- Maps to 'timestamp'.
instance Default (Value B.LocalTime) where
  {-# INLINE def #-}
  def =
    timestamp

-- |
-- Maps to 'timestamptz'.
instance Default (Value B.UTCTime) where
  {-# INLINE def #-}
  def =
    timestamptz

-- |
-- Maps to 'time'.
instance Default (Value B.TimeOfDay) where
  {-# INLINE def #-}
  def =
    time

-- |
-- Maps to 'timetz'.
instance Default (Value (B.TimeOfDay, B.TimeZone)) where
  {-# INLINE def #-}
  def =
    timetz

-- |
-- Maps to 'interval'.
instance Default (Value B.DiffTime) where
  {-# INLINE def #-}
  def =
    interval

-- |
-- Maps to 'uuid'.
instance Default (Value B.UUID) where
  {-# INLINE def #-}
  def =
    uuid

-- |
-- Maps to 'json'.
instance Default (Value B.Value) where
  {-# INLINE def #-}
  def =
    json


-- * Array decoders
-------------------------

-- |
-- A generic array decoder.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: Value [[Text]]
-- x =
--   array (dimension 'replicateM' (dimension 'replicateM' (element text)))
-- @
--
newtype Array a =
  Array (Array.Array a)
  deriving (Functor)

-- |
-- A function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
--
-- Accepts:
--
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
--
-- * A decoder of its components, which can be either another 'dimension',
-- 'element' or 'nullableElement'.
--
{-# INLINABLE dimension #-}
dimension :: (forall m. Monad m => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) =
  Array (Array.dimension replicateM imp)


-- * Composite decoders
-------------------------

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a =
  Composite (Composite.Composite a)
  deriving (Functor, Applicative, Monad)

-- * Nullable group
-------------------------

-- |
-- A generic nullable group decoder. Useful for defining a 'Columnar' decoder and reusing it in joins.
--
-- @
-- -- Text is email, Maybe Text is name
-- userRow :: (Applicative c, Columnar c) => c (Text, Maybe Text)
-- userRow = (,) <$> field text <*> nullableField text
-- @
--
-- > rowVector (group userRow) :: Result (Vector (Maybe (Text, Maybe Text)))
--
-- > rowVector userRow :: Result (Vector (Text, Maybe Text))
--
newtype NullableGroup a =
  NullableGroup (Group.NullableGroup a)
  deriving (Functor, Applicative)


-- |
-- Decode a group of values which may all be nullable.
-- If any field decoded via 'field' returns 'Nothing', the group will return 'Nothing'.
--
-- Fields decoded via 'nullableField' may be @NULL@ while allowing the group to return a 'Just' value.
group :: NullableGroup a -> Row (Maybe a)
group (NullableGroup g) = Row $ Group.run g

class Columnar f where
  field :: Value a -> f a
  nullableField :: Value a -> f (Maybe a)

instance Columnar Row where
  -- |
  -- Lift an individual non-nullable value decoder to a composable row decoder.
  --
  {-# INLINABLE field #-}
  field (Value imp) =
    Row (Row.nonNullValue imp)
  -- |
  -- Lift an individual nullable value decoder to a composable row decoder.
  --
  {-# INLINABLE nullableField #-}
  nullableField (Value imp) =
    Row (Row.value imp)

instance Columnar Array where
  -- |
  -- Lift a 'Value' decoder into an 'Array' decoder for parsing of non-nullable leaf values.
  {-# INLINABLE field #-}
  field (Value imp) =
    Array (Array.nonNullValue (Value.run imp))

  -- |
  -- Lift a 'Value' decoder into an 'Array' decoder for parsing of nullable leaf values.
  {-# INLINABLE nullableField #-}
  nullableField (Value imp) =
    Array (Array.value (Value.run imp))


instance Columnar Composite where
  -- |
  -- Lift a 'Value' decoder into a 'Composite' decoder for parsing of non-nullable leaf values.
  {-# INLINABLE field #-}
  field (Value imp) =
    Composite (Composite.nonNullValue (Value.run imp))
  -- |
  -- Lift a 'Value' decoder into a 'Composite' decoder for parsing of nullable leaf values.
  {-# INLINABLE nullableField #-}
  nullableField (Value imp) =
    Composite (Composite.value (Value.run imp))

instance Columnar NullableGroup where
  {-# INLINABLE field #-}
  field (Value imp) =
    NullableGroup (Group.nonNullValue imp)

  {-# INLINABLE nullableField #-}
  nullableField (Value imp) =
    NullableGroup (Group.value imp)

class Positioned f where
  fieldPosition :: f Int

instance Positioned Row where
  fieldPosition = Row Row.columnPosition
instance Positioned NullableGroup where
  fieldPosition = NullableGroup Group.columnPosition

class Named f where
  allFieldNames :: f (Vector ByteString)
  nextFieldName :: f ByteString

instance Named Row where
  allFieldNames = Row Row.columnNames
  nextFieldName = (Vector.!) <$> allFieldNames <*> fieldPosition
instance Named NullableGroup where
  allFieldNames = NullableGroup Group.columnNames
  nextFieldName = (Vector.!) <$> allFieldNames <*> fieldPosition
