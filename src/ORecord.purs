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

foreign import data ORecord :: # Type -> # Type -> Type

orecord :: forall g required optional o o'. Union required o g => Union o o' optional => Nub g g => { |g } -> ORecord required optional
orecord = unsafeCoerce

instance orecordEq ::
  ( ORecordMapping required optional r
  , Eq {|r}
  ) => Eq (ORecord required optional) where
  eq a a' = (fromORecord a :: {|r}) == fromORecord a'

instance orecordOrd ::
  ( ORecordMapping required optional r
  , Ord {|r}
  , Eq (ORecord required optional)
  ) => Ord (ORecord required optional) where
  compare a a' = compare (fromORecord a :: {|r}) (fromORecord a')

class ORecordMapping
      (required :: # Type)
      (optional :: # Type)
      (r :: # Type)
      | r -> required optional, required optional -> r where
  toORecord :: { |r } -> ORecord required optional
  fromORecord :: ORecord required optional -> { |r }

instance orecordMappingInstance ::
  ( RowKeys optional
  , MapMaybe optional maybes
  , Union required maybes r
  ) => ORecordMapping required optional r where
  toORecord = toORecordImpl (fromMaybe undefined) (rowKeys (RProxy :: _ optional))
  fromORecord = fromORecordImpl Nothing Just (rowKeys (RProxy :: _ optional))

foreign import toORecordImpl :: forall a a' r required optional. (Maybe a -> a') -> Array String -> { |r } -> ORecord required optional

class MapMaybe (inr :: # Type) (outr :: # Type) | inr -> outr

instance mapMaybeInstance ::
  ( RowToList inr inRl
  , MapMaybeRL inRl outRl
  , ListToRow outRl outr
  ) => MapMaybe inr outr

class MapMaybeRL
      (inRl :: RowList)
      (outRl :: RowList)
      | inRl -> outRl

instance mapMaybeRLNil :: MapMaybeRL Nil Nil
instance mapMaybeRLCons :: MapMaybeRL tl tl' => MapMaybeRL (Cons k t tl) (Cons k (Maybe t) tl')

foreign import fromORecordImpl :: forall required optional r. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array String -> ORecord required optional -> { |r }

allRequired :: forall required optional. RowKeys required => ORecord required optional -> { |required }
allRequired = allRequiredImpl (rowKeys (RProxy :: _ required))

foreign import allRequiredImpl :: forall r o. Array String -> ORecord r o -> { |r }

allOptional :: forall required optional maybes. RowKeys optional => MapMaybe optional maybes => ORecord required optional -> { |maybes }
allOptional = allOptionalImpl Nothing Just (rowKeys (RProxy :: _ optional))

foreign import allOptionalImpl :: forall required optional r. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array String -> ORecord required optional -> { |r }

-- Accessors

getRequired :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' required => SProxy l -> ORecord required optional -> a
getRequired sp r = getRequiredImpl (reflectSymbol sp) r

foreign import getRequiredImpl :: forall a required optional. String -> ORecord required optional -> a

getOptional :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' optional => SProxy l -> ORecord required optional -> Maybe a
getOptional sp r = getOptionalImpl Nothing Just (reflectSymbol sp) r

foreign import getOptionalImpl :: forall a required optional. Maybe a -> (a -> Maybe a) -> String -> ORecord required optional -> Maybe a

setRequired :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' required => SProxy l -> a -> ORecord required optional -> ORecord required optional
setRequired sp a r = setImpl (reflectSymbol sp) a r

setOptional :: forall l a r' required optional. IsSymbol l => Row.Cons l a r' optional => SProxy l -> Maybe a -> ORecord required optional -> ORecord required optional
setOptional sp (Just a) r = setImpl (reflectSymbol sp) a r
setOptional sp Nothing r = deleteImpl (reflectSymbol sp) r

foreign import setImpl :: forall a required optional. String -> a -> ORecord required optional -> ORecord required optional
foreign import deleteImpl :: forall required optional. String -> ORecord required optional -> ORecord required optional

-- Helpers

rowKeys :: forall r. RowKeys r => RProxy r -> Array String
rowKeys = List.toUnfoldable <<< _rowKeys

class RowKeys (r :: # Type) where
  _rowKeys :: RProxy r -> List String

instance _rowKeysInstance :: ( RowToList r rl
                            , RowKeysRL rl
                            ) => RowKeys r where
  _rowKeys _ = _rowKeysRL (RLProxy :: _ rl)

class RowKeysRL (rl :: RowList) where
  _rowKeysRL :: RLProxy rl -> List String

instance _rowKeysRLNil :: RowKeysRL Nil where
  _rowKeysRL _ = mempty

instance _rowKeysRLCons ::
  ( IsSymbol name
  , RowKeysRL tl
  ) => RowKeysRL (Cons name typ tl) where
  _rowKeysRL _ = List.Cons (reflectSymbol (SProxy :: _ name)) (_rowKeysRL (RLProxy :: _ tl))

foreign import undefined :: forall a. a
