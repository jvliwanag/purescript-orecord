module ORecord
       ( ORecord
       , orecord
       , class ToORecord
       , class ToORecordRL
       , toORecord
       , fromORecord
       , class RowKeys
       , _rowKeys
       , class RowKeysRL
       , _rowKeysRL
       , class MapMaybe
       , class MapMaybeRL
       ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Prelude (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ORecord :: # Type -> # Type -> Type

orecord :: forall g required optional o o'. Union required o g => Union o o' optional => Nub g g => { |g } -> ORecord required optional
orecord = unsafeCoerce

allRequired :: forall required optional. RowKeys required => ORecord required optional -> { |required }
allRequired = allRequiredImpl (rowKeys (RProxy :: _ required))

foreign import allRequiredImpl :: forall r o. Array String -> ORecord r o -> { |r }

class ToORecord
      (r :: # Type)
      (required :: # Type)
      (optional :: # Type)
      | r -> required optional

instance toORecordInstance ::
  ( RowToList r rl
  , ToORecordRL rl requiredRl optionalRl
  , RowToList required requiredRl
  , RowToList optional optionalRl
  ) => ToORecord r required optional

class ToORecordRL
      (rl :: RowList)
      (requiredRl :: RowList)
      (optionalRl :: RowList)
      | rl -> requiredRl optionalRl

instance toORecordRLNil :: ToORecordRL Nil Nil Nil

else instance toORecordRLConsOptional ::
  ( ToORecordRL tailRl requiredRl optionalTailRl
  , IsSymbol name
  ) => ToORecordRL (Cons name (Maybe typ) tailRl) requiredRL (Cons name typ optionalTailRl)

else instance toORecordRlConsRequired ::
  ( ToORecordRL tailRl requiredTailRl optionalRl
  ) => ToORecordRL (Cons name typ tailRl) (Cons name typ requiredTailRl) optionalRl

toORecord
 :: forall r required optional
    . ToORecord r required optional
    => RowKeys optional
    => { |r }
    -> ORecord required optional
toORecord = toORecordImpl (fromMaybe undefined) (rowKeys (RProxy :: _ optional))

foreign import toORecordImpl :: forall a a' r required optional. (Maybe a -> a') -> Array String -> { |r } -> ORecord required optional

class MapMaybe (inr :: # Type) (outr :: # Type) | inr -> outr, outr -> inr

instance mapMaybeInstance ::
  ( RowToList inr inRl
  , MapMaybeRL inRl outRl
  , ListToRow outRl outr
  ) => MapMaybe inr outr

class MapMaybeRL
      (inRl :: RowList)
      (outRl :: RowList)
      | inRl -> outRl, outRl -> inRl

instance mapMaybeRLNil :: MapMaybeRL Nil Nil
instance mapMaybeRLCons :: MapMaybeRL tl tl' => MapMaybeRL (Cons k t tl) (Cons k (Maybe t) tl')

fromORecord ::
  forall required optional maybes r
  . MapMaybe optional maybes
  => Union required maybes r
  => RowKeys optional
  => ORecord required optional
  -> { |r }
fromORecord =
  fromORecordImpl Nothing Just (rowKeys (RProxy :: _ optional))

foreign import fromORecordImpl :: forall required optional r. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array String -> ORecord required optional -> { |r }

foreign import undefined :: forall a. a

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
