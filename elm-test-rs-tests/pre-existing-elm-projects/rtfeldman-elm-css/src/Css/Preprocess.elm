module Css.Preprocess exposing (Snippet(..), SnippetDeclaration(..), Style(..), StyleBlock(..), Stylesheet, mapAllLastProperty, mapLastProperty, mapProperties, stylesheet, toMediaRule, toPropertyStrings, unwrapSnippet)

{-| A representation of the preprocessing to be done. The elm-css DSL generates
the data structures found in this module.
-}

import Css.Structure as Structure exposing (MediaQuery, Property, mapLast)


stylesheet : List Snippet -> Stylesheet
stylesheet snippets =
    { charset = Nothing
    , imports = []
    , namespaces = []
    , snippets = snippets
    }


type alias Stylesheet =
    { charset : Maybe String
    , imports : List ( String, List MediaQuery )
    , namespaces : List ( String, String )
    , snippets : List Snippet
    }


type Style
    = AppendProperty Property
    | ExtendSelector Structure.RepeatableSimpleSelector (List Style)
    | NestSnippet Structure.SelectorCombinator (List Snippet)
    | WithPseudoElement Structure.PseudoElement (List Style)
    | WithMedia (List MediaQuery) (List Style)
    | WithKeyframes String
    | ApplyStyles (List Style)


type Snippet
    = Snippet (List SnippetDeclaration)


type SnippetDeclaration
    = StyleBlockDeclaration StyleBlock
    | MediaRule (List MediaQuery) (List StyleBlock)
    | SupportsRule String (List Snippet)
    | DocumentRule String String String String StyleBlock
    | PageRule (List Property)
    | FontFace (List Property)
    | Viewport (List Property)
    | CounterStyle (List Property)
    | FontFeatureValues (List ( String, List Property ))


type StyleBlock
    = StyleBlock Structure.Selector (List Structure.Selector) (List Style)


toMediaRule : List MediaQuery -> Structure.Declaration -> Structure.Declaration
toMediaRule mediaQueries declaration =
    case declaration of
        Structure.StyleBlockDeclaration structureStyleBlock ->
            Structure.MediaRule mediaQueries [ structureStyleBlock ]

        Structure.MediaRule newMediaQueries structureStyleBlocks ->
            Structure.MediaRule (mediaQueries ++ newMediaQueries) structureStyleBlocks

        Structure.SupportsRule str declarations ->
            Structure.SupportsRule str (List.map (toMediaRule mediaQueries) declarations)

        -- TODO give these more descriptive names
        Structure.DocumentRule str1 str2 str3 str4 structureStyleBlock ->
            Structure.DocumentRule str1 str2 str3 str4 structureStyleBlock

        Structure.PageRule _ ->
            declaration

        Structure.FontFace _ ->
            declaration

        Structure.Keyframes _ ->
            declaration

        Structure.Viewport _ ->
            declaration

        Structure.CounterStyle _ ->
            declaration

        Structure.FontFeatureValues _ ->
            declaration


mapProperties : (Property -> Property) -> Style -> Style
mapProperties update style =
    case style of
        AppendProperty property ->
            AppendProperty (update property)

        ExtendSelector selector styles ->
            ExtendSelector selector (mapAllProperties update styles)

        NestSnippet _ _ ->
            style

        WithPseudoElement _ _ ->
            style

        WithMedia _ _ ->
            style

        WithKeyframes _ ->
            style

        ApplyStyles otherStyles ->
            ApplyStyles (List.map (mapProperties update) otherStyles)


mapAllProperties : (Property -> Property) -> List Style -> List Style
mapAllProperties update styles =
    case styles of
        [] ->
            styles

        only :: [] ->
            [ mapProperties update only ]

        first :: rest ->
            first :: mapAllProperties update rest


mapLastProperty : (Property -> Property) -> Style -> Style
mapLastProperty update style =
    case style of
        AppendProperty property ->
            AppendProperty (update property)

        ExtendSelector selector styles ->
            ExtendSelector selector (mapAllLastProperty update styles)

        NestSnippet _ _ ->
            style

        WithPseudoElement _ _ ->
            style

        WithMedia _ _ ->
            style

        WithKeyframes _ ->
            style

        ApplyStyles otherStyles ->
            ApplyStyles (mapLast (mapLastProperty update) otherStyles)


mapAllLastProperty : (Property -> Property) -> List Style -> List Style
mapAllLastProperty update styles =
    case styles of
        [] ->
            styles

        only :: [] ->
            [ mapLastProperty update only ]

        first :: rest ->
            first :: mapAllLastProperty update rest


unwrapSnippet : Snippet -> List SnippetDeclaration
unwrapSnippet (Snippet declarations) =
    declarations


toPropertyStrings : List Style -> List String
toPropertyStrings styles =
    case styles of
        [] ->
            []

        (AppendProperty (Structure.Property str)) :: rest ->
            str :: toPropertyStrings rest

        (ApplyStyles otherStyles) :: rest ->
            toPropertyStrings otherStyles ++ toPropertyStrings rest

        _ :: rest ->
            toPropertyStrings rest
