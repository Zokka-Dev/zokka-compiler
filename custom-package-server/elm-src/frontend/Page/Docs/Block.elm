module Page.Docs.Block exposing
  ( view
  , Info
  , makeInfo
  )


import Dict
import Elm.Docs as Docs
import Elm.Type as Type
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Href
import Utils.Markdown as Markdown



-- CONSTANTS


defaultMaxWidth : Int
defaultMaxWidth =
  62



-- VIEW


view : Info -> Docs.Block -> Html msg
view info block =
  case block of
    Docs.MarkdownBlock markdown ->
      span [class "markdown-block"] [ Markdown.block markdown ]

    Docs.ValueBlock value ->
      viewValue info value

    Docs.BinopBlock binop ->
      viewBinop info binop

    Docs.AliasBlock alias ->
      viewAlias info alias

    Docs.UnionBlock union ->
      viewUnion info union

    Docs.UnknownBlock name ->
      span
        [ class "TODO-make-this-red" ]
        [ text "It seems that "
        , code [] [ text name ]
        , text " does not have any docs. Please open a bug report "
        , a [ href "https://github.com/elm/package.elm-lang.org/issues" ] [ text "here" ]
        , text " with the title “UnknownBlock found in docs” and with a link to this page in the description."
        ]


viewCodeBlock : Info -> String -> String -> List (Line msg) -> Html msg
viewCodeBlock {author, project, version, moduleName} name comment header =
  let
    url =
      Href.toModule author project version moduleName (Just name)
  in
  div [ class "docs-block", id name ]
    [ div [ class "docs-header" ] (List.map (div []) header)
    , div [ class "docs-comment" ]
        [ a [ href url, class "permalink" ] [ text "" ]
        , Markdown.block comment
        ]
    ]



-- VIEW VALUE BLOCK


viewValue : Info -> Docs.Value -> Html msg
viewValue info { name, comment, tipe } =
  let
    nameHtml =
      toBoldLink info name name

  in
  viewCodeBlock info name comment <|
    case toLines info Other (String.length name + 3) tipe of
      One _ line ->
        [ nameHtml :: space :: colon :: space :: line ]

      More x xs ->
        [ nameHtml, space, colon ] :: indentFour x :: List.map indentFour xs


indentFour : Line msg -> Line msg
indentFour =
  (::) (text "    ")



-- VIEW BINOP BLOCK


viewBinop : Info -> Docs.Binop -> Html msg
viewBinop info { name, comment, tipe } =
  let
    nameHtml =
      toBoldLink info name ("(" ++ name ++ ")")
  in
  viewCodeBlock info name comment <|
    case toLines info Other (String.length name + 3) tipe of
      One _ line ->
        [ nameHtml :: space :: colon :: space :: line ]

      More x xs ->
        [ nameHtml, space, colon ] :: indentFour x :: List.map indentFour xs



-- VIEW ALIAS BLOCK


viewAlias : Info -> Docs.Alias -> Html msg
viewAlias info { name, args, comment, tipe } =
  let
    varsString =
      String.concat (List.map ((++) " ") args)

    aliasNameLine =
      [ keyword "type", space, keyword "alias", space
      , toBoldLink info name name, text varsString, space
      , equals
      ]
  in
  viewCodeBlock info name comment <|
    aliasNameLine :: List.map indentFour (linesToList (toLines info Other 4 tipe))



-- VIEW UNION


viewUnion : Info -> Docs.Union -> Html msg
viewUnion info {name, comment, args, tags} =
  let
    varsString =
      String.concat <| List.map ((++) " ") args

    nameLine =
      [ keyword "type", space, toBoldLink info name name, text varsString ]
  in
  viewCodeBlock info name comment <|
    case tags of
      [] ->
        [ nameLine ]

      t :: ts ->
        nameLine :: linesToList (toMoreLines info.maxWidth (unionMore info) t ts)


unionMore : Info -> MoreSettings (String, List Type.Type) msg
unionMore info =
  let
    ctorToLines maxWidth (ctor,args) =
      toOneOrMore maxWidth (toLines info Other 0 (Type.Type ctor args))
  in
  { open = [ text "    = " ]
  , sep = text "    | "
  , close = Nothing
  , openIndent = 6
  , sepIndent = 6
  , toLines = ctorToLines
  }



-- INFO


type alias Info =
  { author : String
  , project : String
  , version : Maybe V.Version
  , moduleName : String
  , typeNameDict : TypeNameDict
  , maxWidth : Int
  }


type alias TypeNameDict =
  Dict.Dict String (String, String)


makeInfo : String -> String -> Maybe V.Version -> String -> List Docs.Module -> Int -> Info
makeInfo author project version moduleName docsList width =
  let
    addUnion home union docs =
      Dict.insert (home ++ "." ++ union.name) (home, union.name) docs

    addModule docs dict =
      List.foldl (addUnion docs.name) dict docs.unions

    blockWidth = width - 40
    maxWidth = 
        if blockWidth > 0 && blockWidth < 600 then
            blockWidth * defaultMaxWidth // 600
        else
            defaultMaxWidth
  in
    { author = author
    , project = project
    , version = version
    , moduleName = moduleName
    , typeNameDict = List.foldl addModule Dict.empty docsList
    , maxWidth = maxWidth
    }



-- CREATE LINKS


toBoldLink : Info -> String -> String -> Html msg
toBoldLink {author, project, version, moduleName} name humanName =
  let
    url =
      Href.toModuleWithSource author project version moduleName (Just name)
  in
  a [ href url, bold ] [ text humanName ]


bold : Attribute msg
bold =
  style "font-weight" "bold"


makeLink : Info -> List (Attribute msg) -> String -> String -> Html msg
makeLink {author, project, version, moduleName} attrs tagName humanName =
  let
    url =
      Href.toModule author project version moduleName (Just tagName)
  in
  a (href url :: attrs) [ text humanName ]


toLinkLine : Info -> String -> Lines (Line msg)
toLinkLine info qualifiedName =
  case Dict.get qualifiedName info.typeNameDict of
    Nothing ->
      let
        shortName =
          last qualifiedName (String.split "." qualifiedName)
      in
      One (String.length shortName) [ span [ title qualifiedName ] [ text shortName ] ]

    Just (moduleName, name) ->
      One (String.length name) [ makeLink { info | moduleName = moduleName } [] name name ]


last : a -> List a -> a
last backup list =
  case list of
    [] ->
      backup

    x :: [] ->
      x

    _ :: xs ->
      last backup xs



-- LINES


type alias Line msg =
  List (Html msg)


type Lines line
  = One Int line
  | More line (List line)


type Context = Func | App | Other


toLines : Info -> Context -> Int -> Type.Type -> Lines (Line msg)
toLines info context prefixWidth tipe =
  case tipe of
    Type.Var x ->
      One (String.length x) [text x]

    Type.Lambda arg result ->
      let
        lambdaToLine =
          if context == Other then
            toLinesHelp info.maxWidth (lambdaOne prefixWidth) lambdaMore
          else
            toLinesHelp info.maxWidth lambdaOneParens lambdaMoreParens
      in
      lambdaToLine (toLines info Func 0 arg) <|
        List.map (toLines info Func 0) (collectArgs [] result)

    Type.Tuple [] ->
      One 2 [text "()"]

    Type.Tuple (arg :: args) ->
      toLinesHelp info.maxWidth tupleOne tupleMore
        (toLines info Other 0 arg)
        (List.map (toLines info Other 0) args)

    Type.Type name args ->
      let
        needsParens =
          context == App && not (List.isEmpty args)
      in
      toLinesHelp
        info.maxWidth
        (typeOne needsParens)
        (typeMore needsParens)
        (toLinkLine info name)
        (List.map (toLines info App 0) args)

    Type.Record [] Nothing ->
      One 2 [text "{}"]

    Type.Record [] (Just ext) ->
      One (6 + String.length ext) [ text <| "{ " ++ ext ++ " | }" ]

    Type.Record (f :: fs) extension ->
      let
        toLns ( field, fieldType ) =
          ( field, toLines info Other 0 fieldType )
      in
      case extension of
        Nothing ->
          if List.isEmpty fs then
            toLinesHelp info.maxWidth recordOne recordMore (toLns f) (List.map toLns fs)
          else
            toMoreLines info.maxWidth recordMore (toLns f) (List.map toLns fs)

        Just ext ->
          case toLinesHelp info.maxWidth (recordOneExt ext) recordMoreExt (toLns f) (List.map toLns fs) of
            One width line ->
              One width line

            More first rest ->
              More [ text "{ ", text ext ] (first :: rest ++ [ [ text "}" ] ])



-- FUNCTIONS


collectArgs : List Type.Type -> Type.Type -> List Type.Type
collectArgs revArgs tipe =
  case tipe of
    Type.Lambda arg result ->
      collectArgs (arg :: revArgs) result

    _ ->
      List.reverse (tipe :: revArgs)


lambdaOne : Int -> OneSettings (Lines (Line msg)) msg
lambdaOne openWidth =
  { open = []
  , sep = [ text " -> " ]
  , close = []
  , openWidth = openWidth
  , sepWidth = 4
  , closeWidth = 0
  , toLine = toLine
  }


lambdaMore : MoreSettings (Lines (Line msg)) msg
lambdaMore =
  { open = []
  , sep = text "-> "
  , close = Nothing
  , openIndent = 0
  , sepIndent = 3
  , toLines = toOneOrMore
  }


lambdaOneParens : OneSettings (Lines (Line msg)) msg
lambdaOneParens =
  { open = [ text "(" ]
  , sep = [ text " -> " ]
  , close = [ text ")" ]
  , openWidth = 1
  , sepWidth = 4
  , closeWidth = 1
  , toLine = toLine
  }


lambdaMoreParens : MoreSettings (Lines (Line msg)) msg
lambdaMoreParens =
  { open = [ text "( " ]
  , sep = text "  -> "
  , close = Just [ text ")" ]
  , openIndent = 2
  , sepIndent = 5
  , toLines = toOneOrMore
  }



-- TUPLES


tupleOne : OneSettings (Lines (Line msg)) msg
tupleOne =
  { open = [ text "( " ]
  , sep = [ text ", " ]
  , close = [ text " )" ]
  , openWidth = 2
  , sepWidth = 2
  , closeWidth = 2
  , toLine = toLine
  }


tupleMore : MoreSettings (Lines (Line msg)) msg
tupleMore =
  { open = [ text "( " ]
  , sep = text ", "
  , close = Just [text ")"]
  , openIndent = 2
  , sepIndent = 2
  , toLines = toOneOrMore
  }



-- TYPES


typeOne : Bool -> OneSettings (Lines (Line msg)) msg
typeOne needsParens =
  if needsParens then
    { open = [ text "(" ]
    , sep = [ text " " ]
    , close = [ text ")" ]
    , openWidth = 1
    , sepWidth = 1
    , closeWidth = 1
    , toLine = toLine
    }

  else
    { open = []
    , sep = [ text " " ]
    , close = []
    , openWidth = 0
    , sepWidth = 1
    , closeWidth = 0
    , toLine = toLine
    }


typeMore : Bool -> MoreSettings (Lines (Line msg)) msg
typeMore needsParens =
  if needsParens then
    { open = [text "("]
    , sep = text "    "
    , close = Just [text ")"]
    , openIndent = 0
    , sepIndent = 4
    , toLines = toOneOrMore
    }

  else
    { open = []
    , sep = text "    "
    , close = Nothing
    , openIndent = 0
    , sepIndent = 4
    , toLines = toOneOrMore
    }



-- RECORDS


recordOne : OneSettings (String, Lines (Line msg)) msg
recordOne =
  { open = [ text "{ " ]
  , sep = [ text ", " ]
  , close = [ text " }" ]
  , openWidth = 2
  , sepWidth = 2
  , closeWidth = 2
  , toLine = fieldToLine
  }


recordMore : MoreSettings (String, Lines (Line msg)) msg
recordMore =
  { open = [ text "{ " ]
  , sep = text ", "
  , close = Just [ text "}" ]
  , openIndent = 6
  , sepIndent = 6
  , toLines = fieldToLines
  }



-- EXTENDED RECORDS


recordOneExt : String -> OneSettings (String, Lines (Line msg)) msg
recordOneExt extension =
  let
    open =
      "{ " ++ extension ++ " | "
  in
  { open = [ text open ]
  , sep = [ text ", " ]
  , close = [ text " }" ]
  , openWidth = String.length open
  , sepWidth = 2
  , closeWidth = 2
  , toLine = fieldToLine
  }


recordMoreExt : MoreSettings (String, Lines (Line msg)) msg
recordMoreExt =
  { open = [ text "    | " ]
  , sep = text "    , "
  , close = Nothing
  , openIndent = 10
  , sepIndent = 10
  , toLines = fieldToLines
  }



-- RECORD HELPERS


fieldToLine : (String, Lines (Line msg)) -> Maybe (Int, Line msg)
fieldToLine ( field, lines ) =
  case lines of
    More _ _ ->
      Nothing

    One width line ->
      Just ( String.length field + 3 + width, text field :: space :: colon :: space :: line )


fieldToLines : Int -> (String, Lines (Line msg)) -> OneOrMore (Line msg)
fieldToLines maxWidth ( field, lines ) =
  case lines of
    One width line ->
      let
        potentialWidth =
          String.length field + 3 + width
      in
      if potentialWidth < maxWidth then
        OneOrMore (text field :: space :: colon :: space :: line) []
      else
        OneOrMore [ text field, space, colon ] [ line ]

    More x xs ->
      OneOrMore [ text field, space, colon ] (x :: xs)



-- HELPERS


toLine : Lines line -> Maybe (Int, line)
toLine lines =
  case lines of
    One width line ->
      Just ( width, line )

    More _ _ ->
      Nothing


linesToList : Lines line -> List line
linesToList lines =
  case lines of
    One _ line ->
      [line]

    More x xs ->
      x :: xs


type OneOrMore a =
  OneOrMore a (List a)


toOneOrMore : Int -> Lines line -> OneOrMore line
toOneOrMore maxWidth lines =
  case lines of
    One _ line ->
      OneOrMore line []

    More x xs ->
      OneOrMore x xs



-- TO LINES HELP


type alias OneSettings a msg =
  { open : Line msg
  , sep : Line msg
  , close : Line msg
  , openWidth : Int
  , sepWidth : Int
  , closeWidth : Int
  , toLine : a -> Maybe (Int, Line msg)
  }


type alias MoreSettings a msg =
  { open : Line msg
  , sep : Html msg
  , close : Maybe (Line msg)
  , openIndent : Int
  , sepIndent : Int
  , toLines : Int -> a -> OneOrMore (Line msg)
  }


toLinesHelp : Int -> OneSettings a msg -> MoreSettings a msg -> a -> List a -> Lines (Line msg)
toLinesHelp maxWidth one more x xs =
  let
    maybeOneLine =
      toOneLine maxWidth one.openWidth one.open one (x::xs)
  in
  case maybeOneLine of
    Just ( width, line ) ->
      One width line

    Nothing ->
      toMoreLines maxWidth more x xs


toOneLine : Int -> Int -> Line msg -> OneSettings a msg -> List a -> Maybe (Int, Line msg)
toOneLine maxWidth chunkWidth chunk one entries =
  case entries of
    [] ->
      Just ( one.closeWidth, one.close )

    entry :: remainingEntries ->
      case one.toLine entry of
        Nothing ->
          Nothing

        Just (entryWidth, line) ->
          case toOneLine maxWidth one.sepWidth one.sep one remainingEntries of
            Nothing ->
              Nothing

            Just (remainingWidth, remainingLine) ->
              let
                width =
                  chunkWidth + entryWidth + remainingWidth
              in
              if width < maxWidth then
                Just ( width, chunk ++ line ++ remainingLine )
              else
                Nothing


toMoreLines : Int -> MoreSettings a msg -> a -> List a -> Lines (Line msg)
toMoreLines maxWidth s x xs =
  let
    (OneOrMore firstLine firstRest) =
      s.toLines maxWidth x

    openIndentation =
      text (String.repeat s.openIndent " ")

    sepIndentation =
      text (String.repeat s.sepIndent " ")

    toChunk (OneOrMore y ys) =
      (s.sep :: y) :: List.map ((::) sepIndentation) ys

    otherLines =
      List.map ((::) openIndentation) firstRest
      ++ List.concatMap (toChunk << s.toLines maxWidth) xs
  in
    More (s.open ++ firstLine) <|
      case s.close of
        Nothing ->
          otherLines

        Just closer ->
          otherLines ++ [ closer ]



-- HELPERS


keyword : String -> Html msg
keyword kw =
  span [class "hljs-keyword"] [text kw]


space : Html msg
space =
  text " "


arrow : Html msg
arrow =
  span [] [text "->"]


colon : Html msg
colon =
  span [] [text ":"]


equals : Html msg
equals =
  span [] [text "="]
