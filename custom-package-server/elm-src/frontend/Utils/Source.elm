module Utils.Source exposing (findLine)


import Parser exposing (..)



findLine : String -> String -> Maybe Int
findLine tag code =
  case String.uncons tag of
    Just (firstChar, _) ->
      run (lineParser firstChar tag) code
        |> Result.toMaybe

    Nothing ->
      Nothing


lineParser : Char -> String -> Parser Int
lineParser firstChar tag =
  loop () (lineParserHelp firstChar tag)

lineParserHelp : Char -> String -> () -> Parser (Step () Int)
lineParserHelp firstChar tag _ =
  oneOf
    [ if Char.isUpper firstChar then
        typeLine tag
      else if Char.isLower firstChar then
        functionLine tag
      else
        infixLine tag
    , map Loop (multiComment "{-" "-}" Nestable)
    , continue
    ]


functionLine : String -> Parser (Step () Int)
functionLine functionName =
  succeed Done
    |. keyword functionName
    |= map Tuple.first getPosition


typeLine : String -> Parser (Step () Int)
typeLine typeName =
  succeed ()
    |. keyword "type"
    |. someSpaces
    |. oneOf
      [ keyword "alias"
      , succeed ()
      ]
    |. someSpaces
    |> andThen
      (\_ ->
        oneOf
          [ succeed Done
            |. keyword typeName
            |= map Tuple.first getPosition
            , succeed (Loop ())
          ]
      )

infixLine : String -> Parser (Step () Int)
infixLine operatorName =
  succeed ()
      |. keyword "infix"
      |. someSpaces
      |. oneOf
        [ keyword "left"
        , keyword "right"
        , keyword "non"
        ]
      |. someSpaces
      |. int
      |. someSpaces
      |> andThen
        (\_ ->
          oneOf
            [ succeed Done
                |. keyword ("(" ++ operatorName ++ ")")
                |= map Tuple.first getPosition
            , succeed (Loop ())
            ]
        )

continue : Parser (Step () Int)
continue =
  succeed Loop
    |. chompWhile
      (\c ->
        case c of
          '\n' -> False
          '{' -> False
          _ -> True
      )
    |= oneOf [ symbol "\n", symbol "{" ]


someSpaces : Parser ()
someSpaces =
  chompWhile (\c -> c == ' ')
