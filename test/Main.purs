module Test.Main
       ( main
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import ORecord (ORecord, fromORecord, getOptional, getRequired, orecord, setOptional, setRequired, toORecord)
import Test.Assert (assert', assertFalse')

type O = ORecord (i :: Int, s :: String) (b :: Boolean, n :: Number)

sample :: O
sample = orecord { i: 10, s: "foo", b: true }

main :: Effect Unit
main = do
  assertFalse' "should not have key for undefined optional" $
    hasKey "n" sample

  assert' "store optionals directly" $
    unsafeGet "b" sample == true

  assert' "to orecord" $
    sample == toORecord { i: 10
                        , s: "foo"
                        , b: Just true
                        , n: Nothing
                        }

  assert' "from orecord" $
    fromORecord sample == { i: 10
                          , s: "foo"
                          , b: Just true
                          , n: Nothing
                          }

  assert' "gets required member" $
    getRequired i_ sample == 10

  assert' "gets defined optional member" $
    getOptional b_ sample == Just true

  assert' "gets undefined optional member" $
    getOptional n_ sample == Nothing

  assert' "get after set required" $
    getRequired i_ (setRequired i_ 20 sample) == 20

  assert' "get after set optional" $
    getOptional b_ (setOptional b_ (Just false) sample) == (Just false)

  assert' "get after unset optional" $
    getOptional b_ (setOptional b_ Nothing sample) == Nothing

  assertFalse' "unsetting removes key" $
    sample # setOptional b_ Nothing # hasKey "b"

i_ :: SProxy "i"
i_ = SProxy

s_ :: SProxy "s"
s_ = SProxy

b_ :: SProxy "b"
b_ = SProxy

n_ :: SProxy "n"
n_ = SProxy

foreign import hasKey :: forall optional required. String -> ORecord optional required -> Boolean

foreign import unsafeGet :: forall optional required a. String -> ORecord optional required -> a
