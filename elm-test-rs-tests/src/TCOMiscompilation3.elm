module TCOMiscompilation3 exposing (tcoMiscompilation3Test)

-- From https://github.com/elm/compiler/issues/2017#issue-522563527

import Test exposing (Test, test)
import Expect

windDownCrashes : Int -> (Int -> Bool) -> Bool
windDownCrashes value continuation =
    if value > 0 then
        windDownCrashes
            (value - 1)
           (\newValue -> continuation newValue)

    else
        continuation 0

windDownDoesNotCrash : Int -> (Int -> Bool) -> Bool
windDownDoesNotCrash value continuation =
    if value > 0 then
        windDownDoesNotCrash
            (value - 1)
            continuation -- Works!
           -- (\newValue -> continuation newValue) -- Crashes; max stack size exceeded

    else
        continuation 0

tcoMiscompilation3Test = test "Another example where TCO should not cause a stack overflow in CPSed code" <|
    \_ -> windDownCrashes 1 (\_ -> True) |> Expect.equal (windDownDoesNotCrash 1 (\_ -> True))
