module Icu.Error exposing (print)

import Html exposing (Html)
import Html.Attributes as Attributes
import List.Extra as List
import Parser exposing (..)
import Parser.LanguageKit exposing (..)


print : Error -> Html msg
print ({ row, col, source, problem, context } as error) =
    let
        message =
            [ Html.text "I ran into a problem while parsing "
            , context
                |> List.head
                |> Maybe.map .description
                |> Maybe.map blueText
                |> Maybe.withDefault (Html.text "")
            , context
                |> List.drop 1
                |> List.head
                |> Maybe.map .description
                |> Maybe.map (\desc -> " of " ++ desc)
                |> Maybe.map Html.text
                |> Maybe.withDefault (Html.text "")
            , Html.text "."
            ]

        contextRow =
            context
                |> List.head
                |> Maybe.map .row
                |> Maybe.withDefault row

        contextCol =
            context
                |> List.head
                |> Maybe.map .col
                |> Maybe.withDefault col

        printLine y =
            if y > 0 then
                source
                    |> String.split "\n"
                    |> List.drop (y - 1)
                    |> List.head
                    |> Maybe.map
                        (\line ->
                            [ Html.text (" " ++ toString y ++ "| ")
                            , -- TODO: proper multiline coloring
                              Html.text line
                            , Html.text "\n"
                            ]
                        )
                    |> Maybe.withDefault []
            else
                []

        code =
            [ printLine (row - 2)
            , printLine (row - 1)
            , printLine row
            , [ redText arrow ]
            ]
                |> List.concat

        arrow =
            String.repeat (col + 3) " " ++ "^"
    in
    [ message
    , [ Html.text "\n\n\n" ]
    , code
    , [ Html.text "\n\n" ]
    , printProblem problem
    ]
        |> List.concat
        |> Html.code
            [ Attributes.style
                [ "font-family" => "'Source Code Pro', Consolas, \"Liberation Mono\", Menlo, Courier, monospace" ]
            ]


flattenProblem :
    Parser.Problem
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
flattenProblem problem =
    flattenProblemHelper problem
        { keywords = []
        , symbols = []
        , others = []
        }
        |> (\collected ->
                { collected
                    | keywords = List.unique collected.keywords
                    , symbols = List.unique collected.symbols
                }
           )


flattenProblemHelper :
    Parser.Problem
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
flattenProblemHelper problem collected =
    case problem of
        ExpectingKeyword keyword ->
            { collected | keywords = keyword :: collected.keywords }

        ExpectingSymbol symbol ->
            { collected | symbols = symbol :: collected.symbols }

        BadOneOf problems ->
            problems
                |> List.foldl flattenProblemHelper collected

        _ ->
            { collected | others = problem :: collected.others }


printKeywords : List String -> Maybe (List (Html msg))
printKeywords keywords =
    case keywords of
        [] ->
            Nothing

        _ ->
            [ [ Html.text "one of the following keywords:\n\n" ]
            , keywords
                |> List.map
                    (\keyword ->
                        [ Html.text "  "
                        , greenText keyword
                        ]
                    )
                |> List.intersperse [ Html.text ",\n" ]
                |> List.concat
            ]
                |> List.concat
                |> Just


printSymbols : List String -> Maybe (List (Html msg))
printSymbols symbols =
    case symbols of
        [] ->
            Nothing

        _ ->
            [ [ Html.text "one of the following symbols:\n\n  " ]
            , symbols
                |> List.map
                    (\symbol ->
                        [ Html.text "'"
                        , greenText symbol
                        , Html.text "'"
                        ]
                    )
                |> List.intersperse [ Html.text ",  " ]
                |> List.concat
            ]
                |> List.concat
                |> Just


printProblem : Parser.Problem -> List (Html msg)
printProblem problem =
    let
        { keywords, symbols, others } =
            flattenProblem problem
    in
    [ [ Html.text "I expected " ]
    , [ printKeywords keywords
      , printSymbols symbols
      , case others of
            [] ->
                Nothing

            _ ->
                others
                    |> List.map printSingleProblem
                    |> List.concat
                    |> List.intersperse (Html.text " or ")
                    |> Just
      ]
        |> List.filterMap identity
        |> List.intersperse [ Html.text ",\n\nor " ]
        |> List.concat
    , [ Html.text "." ]
    ]
        |> List.concat


printSingleProblem : Parser.Problem -> List (Html msg)
printSingleProblem problem =
    case problem of
        BadOneOf problems ->
            []

        BadInt ->
            [ greenText "a number" ]

        BadRepeat ->
            []

        ExpectingSymbol symbol ->
            [ Html.text "the symbol '"
            , greenText <|
                escapeSymbol symbol
            , Html.text "'"
            ]

        ExpectingKeyword keyword ->
            [ Html.text "the keyword '"
            , greenText keyword
            , Html.text "'"
            ]

        ExpectingVariable ->
            [ greenText "a variable" ]

        Fail message ->
            [ greenText message ]

        _ ->
            [ Html.text <| toString problem ]


escapeSymbol : String -> String
escapeSymbol symbol =
    case symbol of
        "\n" ->
            "\\n"

        "\t" ->
            "\\t"

        _ ->
            symbol



---- VIEW HELPER


redText : String -> Html msg
redText text =
    coloredText red text


greenText : String -> Html msg
greenText text =
    coloredText green text


blueText : String -> Html msg
blueText text =
    coloredText blue text


coloredText : String -> String -> Html msg
coloredText color text =
    Html.span
        [ Attributes.style
            [ "color" => color ]
        ]
        [ Html.text text ]


red : String
red =
    "#cc0000"


green : String
green =
    "#73d216"


blue : String
blue =
    "#3465a4"


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
