module BadOccursCheck1 exposing (..)
-- From an error that showed up on incremental-elm Slack

-- This file will cause the vanilla Elm compiler to hang when compiling.

type Effect msg = Effect

type Msg = Msg1 Int | Msg2 Int

-- All our functions will be infinite loops since this is only meant to test
-- type checking and we don't care about any runtime stuff
-- We won't use Debug.todo just in case we want to double-check that all of
-- this works fine when we run `--optimize` (although presently I can't
-- think of any reason why --optimize would change anything WRT
-- typechecking, but just incase!)
applyIf : Bool -> (a -> a) -> a -> a
applyIf x = applyIf x

update : model -> ( model, Effect msg )
update x = update x

withQuery : (data -> msg) -> Effect msg -> ( model, Effect msg ) -> ( model, Effect msg )
withQuery f = withQuery f

-- A nice hack to get us values without resorting to Debug.todo
-- Can't just directly do x = x because the Elm compiler detects that
makeAnything : a -> b
makeAnything x = makeAnything x

query1 = makeAnything ()

query2 = makeAnything ()

model = makeAnything ()

result condition =
    model
        |> update
        |> applyIf condition withQuery Msg1 query1
        |> applyIf condition withQuery Msg1 query2
