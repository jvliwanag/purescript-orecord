module ORecord
  ( ORecord
  , orecord
  , class ORecordMapping
  , toORecord
  , fromORecord
  , class MapMaybe
  , class MapMaybeRL
  , allRequired
  , allOptional
  -- Accessors
  , getRequired
  , getOptional
  , setRequired
  , setOptional
  -- Utils
  , unrequire
  -- Helpers
  , class RowKeys
  , _rowKeys
  , class RowKeysRL
  , _rowKeysRL
  ) where

import Prelude
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Nub, class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

--| A data type for records with a separate rows for required and optional members. For ex.
--| `ORecord (foo :: Int) (bar :: String)` represents a record that is guaranteed
--| to have a value for label `foo` of type `Int`, but may or may note have a value for `bar`.
--|
--| Like Purescript records, an `ORecord` is represented by a JavaScript object. The object
--| will have all the properties from the `required` row, and zero or more properties
--| from the `optional` row. Values for labels in the `optional` row are stored directly without
--| any wrappers.
foreign import data ORecord :: #Type -> #Type -> Type

--| Creates an `ORecord` from a record having all labels specified in `required` and zero or more
--| labels from the `optional` row.
--|
--| Example:
--| ```purescript
--| sample :: ORecord ( i :: Int, s :: String ) ( b :: Boolean, n :: Number )
--| sample = orecord { i: 10, s: "foo", b: true }
--| ```
orecord :: forall g required optional o o'. Union required o g => Union o o' optional => Nub g g => { | g } -> ORecord required optional
orecord = unsafeCoerce

instance orecordEq ::
  ( ORecordMapping required optional r
  , Eq { | r }
  ) =>
  Eq (ORecord required optional) where
  eq a a' = (fromORecord a :: { | r }) == fromORecord a'

instance orecordOrd ::
  ( ORecordMapping required optional r
  , Ord { | r }
  , Eq (ORecord required optional)
  ) =>
  Ord (ORecord required optional) where
  compare a a' = compare (fromORecord a :: { | r }) (fromORecord a')

instance orecordShow ::
  ( ORecordMapping required optional r
  , Show { | r }
  ) =>
  Show (ORecord required optional) where
  show a = "(ORecord " <> show (fromORecord a :: { | r }) <> ")"

--| A two way mapping for conversion between an `ORecord required optional` and `{|r}` where
--| `r` has all of the labels of `required` and `optional`. The values in `r` for `optional`
--| are wrapped in `Maybe`.
class ORecordMapping (required :: #Type) (optional :: #Type) (r :: #Type) | r -> required optional, required optional -> r where
  toORecord :: { | r } -> ORecord required optional
  fromORecord :: ORecord required optional -> { | r }

instance orecordMappingInstance ::
  ( RowKeys optional
  , MapMaybe optional maybes
  , Union required maybes r
  ) =>
  ORecordMapping required optional r where
  toORecord = toORecordImpl (fromMaybe undefined) (rowKeys (RProxy :: _ optional))
  fromORecord = fromORecordImpl Nothing Just (rowKeys (RProxy :: _ optional))

foreign import toORecordImpl :: forall a a' r required optional. (Maybe a -> a') -> Array String -> { | r } -> ORecord required optional

class MapMaybe (inr :: #Type) (outr :: #Type) | inr -> outr

instance mapMaybeInstance ::
  ( RowToList inr inRl
  , MapMaybeRL inRl outRl
  , ListToRow outRl outr
  ) =>
  MapMaybe inr outr

class MapMaybeRL (inRl :: RowList) (outRl :: RowList) | inRl -> outRl

instance mapMaybeRLNil :: MapMaybeRL Nil Nil

instance mapMaybeRLCons :: MapMaybeRL tl tl' => MapMaybeRL (Cons k t tl) (Cons k (Maybe t) tl')

foreign import fromORecordImpl :: forall required optional r. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array String -> ORecord required optional -> { | r }

--| Create a record from an `ORecord` containing only required values.
allRequired :: forall required optional. RowKeys required => ORecord required optional -> { | required }
allRequired = allRequiredImpl (rowKeys (RProxy :: _ required))

foreign import allRequiredImpl :: forall r o. Array String -> ORecord r o -> { | r }

--| Create a record from an `ORecord` containing only optional values. All values are wrapped in `Maybe`.
allOptional :: forall required optional maybes. RowKeys optional => MapMaybe optional maybes => ORecord required optional -> { | maybes }
allOptional = allOptionalImpl Nothing Just (rowKeys (RProxy :: _ optional))

foreign import allOptionalImpl :: forall required optional r. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array String -> ORecord required optional -> { | r }

-- Accessors

--| Get a required property for a label which is specified using a value-level proxy for a type-level string.
getRequired :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' required => SProxy l -> ORecord required optional -> a
getRequired sp r = getRequiredImpl (reflectSymbol sp) r

foreign import getRequiredImpl :: forall a required optional. String -> ORecord required optional -> a

--| Get an optional property for a label which is specified using a value-level proxy for a type-level string.
getOptional :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' optional => SProxy l -> ORecord required optional -> Maybe a
getOptional sp r = getOptionalImpl Nothing Just (reflectSymbol sp) r

foreign import getOptionalImpl :: forall a required optional. Maybe a -> (a -> Maybe a) -> String -> ORecord required optional -> Maybe a

--| Set a required property for a label which is specified using a value-level proxy for a type-level string.
setRequired :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' required => SProxy l -> a -> ORecord required optional -> ORecord required optional
setRequired sp a r = setImpl (reflectSymbol sp) a r

--| Set an optional property for a label which is specified using a value-level proxy for a type-level string.
--|
--| If a value is specified as Nothing, the property is deleted from the underlying JavaScript Object.
setOptional :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' optional => SProxy l -> Maybe a -> ORecord required optional -> ORecord required optional
setOptional sp (Just a) r = setImpl (reflectSymbol sp) a r

setOptional sp Nothing r = deleteImpl (reflectSymbol sp) r

foreign import setImpl :: forall a required optional. String -> a -> ORecord required optional -> ORecord required optional

foreign import deleteImpl :: forall required optional. String -> ORecord required optional -> ORecord required optional

-- Utils

--| Mark zero or more required properties as optional at the type level.
unrequire ::
  forall required optional required' optional' unrequired.
  Union optional unrequired optional' =>
  Union required' unrequired required =>
  ORecord required optional ->
  ORecord required' optional'
unrequire = unsafeCoerce

-- Helpers
rowKeys :: forall r. RowKeys r => RProxy r -> Array String
rowKeys = List.toUnfoldable <<< _rowKeys

class RowKeys (r :: #Type) where
  _rowKeys :: RProxy r -> List String

instance _rowKeysInstance ::
  ( RowToList r rl
  , RowKeysRL rl
  ) =>
  RowKeys r where
  _rowKeys _ = _rowKeysRL (RLProxy :: _ rl)

class RowKeysRL (rl :: RowList) where
  _rowKeysRL :: RLProxy rl -> List String

instance _rowKeysRLNil :: RowKeysRL Nil where
  _rowKeysRL _ = mempty

instance _rowKeysRLCons ::
  ( IsSymbol name
  , RowKeysRL tl
  ) =>
  RowKeysRL (Cons name typ tl) where
  _rowKeysRL _ = List.Cons (reflectSymbol (SProxy :: _ name)) (_rowKeysRL (RLProxy :: _ tl))

foreign import undefined :: forall a. a
