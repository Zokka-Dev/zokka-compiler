module TCOMiscompilation4 exposing (tcoMiscompilation4Test0, tcoMiscompilation4Test1)

-- From https://github.com/elm/compiler/issues/2017#issuecomment-870199418

import Test exposing (Test, test)
import Expect

type Thunk
    = Thunk (Int -> ( Int, Maybe Thunk ))


type Expr
    = Sum Expr Expr
    | Int Int


type Message
    = BuggyEval Expr
    | HackyEval Expr


type Interpreter
    = Stopped
    | StackOverflow
    | Done Int


type alias Model =
    Interpreter

testProgram0 =
  Sum (Int 2) (Sum (Int 1) (Int 1))

testProgram1 =
   Sum (Sum (Int 1) (Int 1)) (Int 2)


buggyEval : Expr -> Thunk -> ( Int, Maybe Thunk )
buggyEval expr k =
    case expr of
        Sum e1 e2 ->
            buggyEval e1
                (Thunk
                    (\e1ret ->
                        buggyEval e2
                            (Thunk
                                (\e2ret ->
                                    ( e1ret + e2ret, Just k )
                                )
                            )
                    )
                )

        Int x ->
            ( x, Just k )


hackyEval : Expr -> Thunk -> ( Int, Maybe Thunk )
hackyEval expr k =
    case expr of
        Sum e1 e2 ->
            let
                myK =
                    k
            in
            hackyEval e1
                (Thunk
                    (\e1ret ->
                        hackyEval e2
                            (Thunk
                                (\e2ret ->
                                    ( e1ret + e2ret, Just myK )
                                )
                            )
                    )
                )

        Int x ->
            ( x, Just k )


trampoline : Int -> ( Int, Maybe Thunk ) -> Interpreter
trampoline maxdepth ( v, thunk ) =
    if maxdepth <= 0 then
        StackOverflow

    else
        case thunk of
            Just (Thunk k) ->
                trampoline (maxdepth - 1) (k v)

            Nothing ->
                Done v


update : Message -> Model -> Model
update msg _ =
    case msg of
        BuggyEval expr ->
            trampoline 1000 (buggyEval expr (Thunk (\v -> ( v, Nothing ))))

        HackyEval expr ->
            trampoline 1000 (hackyEval expr (Thunk (\v -> ( v, Nothing ))))


interpreterState : Interpreter -> String
interpreterState interpreter =
    case interpreter of
        Stopped ->
            "Interpreter stopped"

        StackOverflow ->
            "Stack Overflow"

        Done v ->
            "Result: " ++ String.fromInt v

view : Interpreter -> String
view interpreter = interpreterState interpreter

tcoMiscompilation4Test0 = test "TCO shouldn't change the results of the hacky eval or the buggy eval (example 0)" <|
    \_ -> update (BuggyEval testProgram0) Stopped |> Expect.equal (update (HackyEval testProgram0) Stopped)

tcoMiscompilation4Test1 = test "TCO shouldn't change the results of the hacky eval or the buggy eval (example 1)" <|
    \_ -> update (BuggyEval testProgram1) Stopped |> Expect.equal (update (HackyEval testProgram0) Stopped)
