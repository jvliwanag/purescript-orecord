module ORecord
       ( ORecord
       , mkORecord
       , class ToORecord
       , class ToORecordRL
       , toORecord
       , fromORecord
       , class RowKeys
       , rowKeys
       , class RowKeysRL
       , rowKeysRL
       ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data ORecord :: # Type -> # Type -> Type

type OptionalKeys = List String

class ToORecord
      (r :: # Type)
      (required :: # Type)
      (optional :: # Type)
      | r -> required optional, required optional -> r

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
      | rl -> requiredRl optionalRl, requiredRl optionalRl -> rl

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
toORecord = toORecordImpl (fromMaybe undefined) (List.toUnfoldable (rowKeys (RProxy :: _ optional)))

foreign import fromORecord :: forall r required optional. ToORecord r required optional => ORecord required optional -> { |r }

mkORecord :: forall g required optional o o'. Union required o g => Union o o' optional =>  { |g } -> ORecord required optional
mkORecord = unsafeCoerce

foreign import toORecordImpl :: forall a a' r required optional. (Maybe a -> a') -> Array String -> { |r } -> ORecord required optional

foreign import undefined :: forall a. a

class RowKeys (r :: # Type) where
  rowKeys :: RProxy r -> List String

instance rowKeysInstance :: ( RowToList r rl
                            , RowKeysRL rl
                            ) => RowKeys r where
  rowKeys _ = rowKeysRL (RLProxy :: _ rl)

class RowKeysRL (rl :: RowList) where
  rowKeysRL :: RLProxy rl -> List String

instance rowKeysRLNil :: RowKeysRL Nil where
  rowKeysRL _ = mempty

instance rowKeysRLCons ::
  ( IsSymbol name
  , RowKeysRL tl
  ) => RowKeysRL (Cons name typ tl) where
  rowKeysRL _ = List.Cons (reflectSymbol (SProxy :: _ name)) (rowKeysRL (RLProxy :: _ tl))
