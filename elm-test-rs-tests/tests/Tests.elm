module Tests exposing (..)

import Test exposing (Test, describe)
import AnotherBadClosure exposing (anotherBadClosureTest)
import TCOProducesBadClosures exposing (tcoProducesBadClosuresTest)
import TCOMiscompilation0 exposing (tcoMiscompilation0Test)
-- This causes hanging in vanilla Elm, uncomment once I have a way of failing a test after it runs for too long
-- import TCOMiscompilation1 exposing (tcoMiscompilation1Test)
import TCOMiscompilation2 exposing (tcoMiscompilation2Test)


suite : Test
suite = describe "TCO tests" 
    [ anotherBadClosureTest
    , tcoProducesBadClosuresTest
    , tcoMiscompilation0Test
    -- This causes hanging in vanilla Elm, uncomment once I have a way of failing a test after it runs for too long
    -- , tcoMiscompilation1Test
    , tcoMiscompilation2Test
    ]
