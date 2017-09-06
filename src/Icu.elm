module Icu exposing (Message, parse, print)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Set
import String.Extra as String


type alias Message =
    List MessagePart


type MessagePart
    = MessageText String
    | Argument ArgNameOrNumber ArgDetails


type ArgDetails
    = NoneArg
    | SimpleArg ArgType
    | PluralArg PluralStyle
    | SelectArg SelectStyle


type ArgNameOrNumber
    = ArgName String
    | ArgNumber Int


type ArgType
    = Number
    | Date
    | Time
    | Spellout
    | Ordinal
    | Duration


type alias SelectStyle =
    List Selector


type Selector
    = Selector String Message


type PluralStyle
    = PluralStyle (Maybe Int) (List PluralSelector)


type PluralSelector
    = PluralSelector SelectorName Message


type SelectorName
    = ExplicitValue Int
    | Zero
    | One
    | Two
    | Few
    | Many
    | Other


parse : String -> Result Parser.Error Message
parse text =
    Parser.run icu text



---- PARSER


icu =
    message


message : Parser Message
message =
    let
        handleEmptyList message =
            case message of
                [] ->
                    [ MessageText "" ]

                _ ->
                    message
    in
    oneOf
        [ lazy (\_ -> messagePart)
        , end |> map (\_ -> MessageText "")
        ]
        |> andThen
            (\text -> messageHelper [ text ])
        |> map handleEmptyList


messageHelper : Message -> Parser Message
messageHelper previous =
    oneOf
        [ lazy (\_ -> messagePart)
            |> andThen
                (\text -> messageHelper (text :: previous))
        , succeed (List.reverse previous)
        ]


messagePart : Parser MessagePart
messagePart =
    oneOf
        [ inContext "an argument" <|
            succeed Argument
                |. symbol "{"
                |. spaces
                |= argNameOrNumber
                |. spaces
                |= lazy (\_ -> argDetails)
                |. spaces
                |. symbol "}"
        , inContext "some text" <|
            succeed MessageText
                |= lazy (\_ -> messageText)
        ]


messageText : Parser String
messageText =
    source <|
        ignore oneOrMore notPatternSyntax


notPatternSyntax : Char -> Bool
notPatternSyntax c =
    not
        ((c == '{')
            || (c == '}')
        )


argDetails : Parser ArgDetails
argDetails =
    oneOf
        [ inContext "the details" <|
            succeed identity
                |. symbol ","
                |. spaces
                |= oneOf
                    [ succeed (SimpleArg Number)
                        |. keyword "number"
                    , succeed (SimpleArg Date)
                        |. keyword "date"
                    , succeed (SimpleArg Time)
                        |. keyword "time"
                    , succeed (SimpleArg Spellout)
                        |. keyword "spellout"
                    , succeed (SimpleArg Ordinal)
                        |. keyword "ordinal"
                    , succeed (SimpleArg Duration)
                        |. keyword "duration"
                    , succeed SelectArg
                        |. keyword "select"
                        |. spaces
                        |. symbol ","
                        |. spaces
                        |= lazy (\_ -> selectStyle)
                    , succeed PluralArg
                        |. keyword "plural"
                        |. spaces
                        |. symbol ","
                        |. spaces
                        |= lazy (\_ -> pluralStyle)
                    ]
        , succeed NoneArg
        ]


selectStyle : Parser SelectStyle
selectStyle =
    repeat oneOrMore (lazy (\_ -> selector))


selector : Parser Selector
selector =
    inContext "a selector" <|
        delayedCommit spaces <|
            succeed Selector
                |= variable isFirstVarChar isVarChar Set.empty
                |. spaces
                |= (inContext "a sub-message" <|
                        succeed identity
                            |. symbol "{"
                            |= (lazy (\_ -> messagePart)
                                    |> andThen
                                        (\text -> messageHelper [ text ])
                               )
                            |. symbol "}"
                   )
                |. spaces


pluralStyle : Parser PluralStyle
pluralStyle =
    let
        isOther (PluralSelector selectorName _) =
            case selectorName of
                Other ->
                    True

                _ ->
                    False
    in
    succeed PluralStyle
        |= (inContext "the offset value" <|
                oneOf
                    [ succeed identity
                        |= offsetValue
                        |. space
                    , succeed Nothing
                    ]
           )
        |. spaces
        |= (repeat oneOrMore (lazy (\_ -> pluralSelector))
                |> andThen
                    (\pluralSelectors ->
                        if pluralSelectors |> List.any isOther then
                            succeed pluralSelectors
                        else
                            fail "A plural argument needs at least the 'other' selector."
                    )
           )


offsetValue : Parser (Maybe Int)
offsetValue =
    succeed
        (\value ->
            if value < 0 then
                Nothing
            else
                Just value
        )
        |. keyword "offset:"
        |= int


pluralSelector : Parser PluralSelector
pluralSelector =
    inContext "a selector" <|
        succeed PluralSelector
            |. spaces
            |= selectorName
            |. spaces
            |= (inContext "a sub-message" <|
                    succeed identity
                        |. symbol "{"
                        |= (lazy (\_ -> messagePart)
                                |> andThen
                                    (\text -> messageHelper [ text ])
                           )
                        |. symbol "}"
               )
            |. spaces


selectorName : Parser SelectorName
selectorName =
    inContext "the name" <|
        oneOf
            [ inContext "an explicit value" <|
                succeed ExplicitValue
                    |. symbol "="
                    |= (int |> andThen isPositive)
            , keyword "zero" |> map (always Zero)
            , keyword "one" |> map (always One)
            , keyword "two" |> map (always Two)
            , keyword "few" |> map (always Few)
            , keyword "many" |> map (always Many)
            , keyword "other" |> map (always Other)
            ]


argNameOrNumber : Parser ArgNameOrNumber
argNameOrNumber =
    oneOf
        [ succeed ArgName
            |= variable isFirstVarChar isVarChar Set.empty
        , succeed ArgNumber
            |= (int |> andThen isPositive)
        ]


isPositive : Int -> Parser Int
isPositive int =
    if int < 0 then
        fail "negative integer"
    else
        succeed int


space : Parser ()
space =
    oneOf
        [ symbol " "
        , symbol "\n"
        , symbol "\t"
        ]


spaces : Parser ()
spaces =
    ignore zeroOrMore
        (\c ->
            (c == ' ')
                || (c == '\x0D')
                || (c == '\n')
                || (c == '\t')
        )



---- ERROR MESSAGES


print : Parser.Error -> Html msg
print ({ row, col, source, problem, context } as error) =
    let
        _ =
            Debug.log "error" error
    in
    Html.code
        [ Attributes.style
            [ "font-family" => "'Source Code Pro', Consolas, \"Liberation Mono\", Menlo, Courier, monospace" ]
        ]
        (printSource row col source problem context)


printSource :
    Int
    -> Int
    -> String
    -> Parser.Problem
    -> List Parser.Context
    -> List (Html msg)
printSource row col source problem context =
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
                            [ [ Html.text (" " ++ toString y ++ "| ") ]
                            , -- TODO: proper multiline coloring
                              if y == row then
                                if contextRow < row then
                                    [ line
                                        |> String.left (col - contextCol)
                                        |> blueText
                                    , line
                                        |> String.dropLeft (col - 1)
                                        |> Html.text
                                    ]
                                else if contextCol < col then
                                    [ line
                                        |> String.left (contextCol - 1)
                                        |> Html.text
                                    , line
                                        |> String.dropLeft (contextCol - 1)
                                        |> String.left (col - contextCol)
                                        |> blueText
                                    , line
                                        |> String.dropLeft (col - 1)
                                        |> Html.text
                                    ]
                                else
                                    [ Html.text line ]
                              else
                                [ Html.text line ]
                            , [ Html.text "\n" ]
                            ]
                                |> List.concat
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


printProblem : Parser.Problem -> List (Html msg)
printProblem problem =
    case problem of
        BadOneOf problems ->
            let
                keywords problems =
                    problems
                        |> List.foldl isKeywordProblem []

                isKeywordProblem next collected =
                    case next of
                        ExpectingKeyword keyword ->
                            keyword :: collected

                        BadOneOf problems ->
                            keywords problems ++ collected

                        _ ->
                            collected

                symbols problems =
                    problems
                        |> List.foldl isSymbolProblem []

                isSymbolProblem next collected =
                    case next of
                        ExpectingSymbol symbol ->
                            symbol :: collected

                        BadOneOf problems ->
                            symbols problems ++ collected

                        _ ->
                            collected
            in
            case
                ( keywords problems |> List.isEmpty
                , symbols problems |> List.isEmpty
                )
            of
                ( True, True ) ->
                    [ [ Html.text "I expected " ]
                    , problems
                        |> List.map printSingleProblem
                        |> List.concat
                        |> List.intersperse (Html.text " or ")
                    , [ Html.text "." ]
                    ]
                        |> List.concat

                ( False, True ) ->
                    [ [ Html.text "I expected one of the following keywords:\n\n" ]
                    , keywords problems
                        |> List.map
                            (\keyword ->
                                [ Html.text "  "
                                , greenText keyword
                                ]
                            )
                        |> List.intersperse [ Html.text ",\n" ]
                        |> List.concat
                    , [ Html.text "." ]
                    ]
                        |> List.concat

                ( True, False ) ->
                    [ [ Html.text "I expected one of the following symbols:\n\n" ]
                    , symbols problems
                        |> List.map
                            (\symbol ->
                                [ Html.text "  "
                                , Html.text "'"
                                , greenText <|
                                    escapeSymbol symbol
                                , Html.text "'"
                                ]
                            )
                        |> List.intersperse [ Html.text ",\n" ]
                        |> List.concat
                    , [ Html.text "." ]
                    ]
                        |> List.concat

                ( False, False ) ->
                    [ [ Html.text "I expected one of the following keywords:\n\n" ]
                    , keywords problems
                        |> List.map
                            (\keyword ->
                                [ Html.text "  "
                                , greenText keyword
                                ]
                            )
                        |> List.intersperse [ Html.text ",\n" ]
                        |> List.concat
                    , [ Html.text ",\n\nor one of the following symbols:\n\n" ]
                    , symbols problems
                        |> List.map
                            (\symbol ->
                                [ Html.text "  "
                                , Html.text "'"
                                , greenText <|
                                    escapeSymbol symbol
                                , Html.text "'"
                                ]
                            )
                        |> List.intersperse [ Html.text ",\n" ]
                        |> List.concat
                    , [ Html.text "." ]
                    ]
                        |> List.concat

        Fail message ->
            [ Html.text message ]

        _ ->
            [ [ Html.text "I expected " ]
            , printSingleProblem problem
            , [ Html.text "." ]
            ]
                |> List.concat


printSingleProblem : Parser.Problem -> List (Html msg)
printSingleProblem problem =
    case problem of
        BadOneOf problems ->
            problems
                |> List.map printSingleProblem
                |> List.concat
                |> List.intersperse (Html.text " or ")

        BadInt ->
            [ greenText "a number" ]

        BadRepeat ->
            [ Html.text "some characters" ]

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
            [ greenText "a selector" ]

        Fail message ->
            [ Html.text message ]

        _ ->
            [ Html.text <| toString problem ]



---- HELPER


escapeSymbol : String -> String
escapeSymbol symbol =
    case symbol of
        "\n" ->
            "\\n"

        "\t" ->
            "\\t"

        _ ->
            symbol


isLetter : Char -> Bool
isLetter c =
    Char.isLower c || Char.isUpper c


isFirstVarChar : Char -> Bool
isFirstVarChar char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || (char == '_')


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


redText text =
    coloredText red text


greenText text =
    coloredText green text


blueText text =
    coloredText blue text


coloredText color text =
    Html.span
        [ Attributes.style
            [ "color" => color ]
        ]
        [ Html.text text ]


red =
    "#cc0000"


green =
    "#73d216"


blue =
    "#3465a4"
