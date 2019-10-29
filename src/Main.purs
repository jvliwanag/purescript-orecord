module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import ORecord (ORecord, fromORecord, orecord, toORecord)

main :: Effect Unit
main = do
  traceM  bar
  traceM  bar2
  traceM (fromORecord bar2)
  traceM $ rec1 == rec2
  log "🍝"

bar :: ORecord (x :: Int, a :: Boolean) (z :: String, b :: Int)
bar = toORecord { x: 10, z: Just "Foo", a: false, b: Nothing }

bar2 :: ORecord (x :: Int, a :: Boolean) (z :: String, b :: Int)
bar2 = orecord { x: 10, z: "Foo", a: false }

--bla :: { x :: Int, a :: Boolean, z :: Maybe String, b :: Maybe Int }
--bla = fromORecord bar

type R = { a :: Int, b :: String, c :: Boolean }
rec1 :: R
rec1 = { a: 10, b: "foo", c: false }

rec2 :: R
rec2 = { a: 10, b: "foo", c: false }
