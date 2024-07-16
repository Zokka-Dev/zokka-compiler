module Tests exposing (..)

import Test exposing (Test, describe)
import AnotherBadClosure exposing (anotherBadClosureTest)
import BadOccursCheck exposing (badOccursCheckTest)
import TCOProducesBadClosures exposing (tcoProducesBadClosuresTest)
import TCOMiscompilation0 exposing (tcoMiscompilation0Test)
-- This causes hanging in vanilla Elm, uncomment once I have a way of failing a test after it runs for too long
-- import TCOMiscompilation1 exposing (tcoMiscompilation1Test)
import TCOMiscompilation2 exposing (tcoMiscompilation2Test)
import TCOMiscompilation3 exposing (tcoMiscompilation3Test)
import TCOMiscompilation4 exposing (tcoMiscompilation4Test0, tcoMiscompilation4Test1)


suite : Test
suite = describe "All tests" 
    --[ anotherBadClosureTest
    --, tcoProducesBadClosuresTest
    --, tcoMiscompilation0Test
    ---- This causes hanging in vanilla Elm, uncomment once I have a way of failing a test after it runs for too long
    ---- , tcoMiscompilation1Test
    --, tcoMiscompilation2Test
    --, tcoMiscompilation3Test
    --, tcoMiscompilation4Test0, tcoMiscompilation4Test1
    --]
    [ badOccursCheckTest
    ]
