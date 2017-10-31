module Icu
    exposing
        ( Message
        , evaluate
        , numberArguments
        , parse
        , print
        , selectArguments
        , textArguments
        , viewNumberArgumentInput
        , viewSelectArgumentInput
        , viewTextArgumentInput
        )

import Char
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import List.Extra as List
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Set exposing (Set)
import String.Extra as String


type alias Message =
    List Part


type Part
    = Text String
    | Argument String Details
    | Hash


type Details
    = None
    | Simple Type (Maybe SimpleStyle)
    | Plural (Maybe Int) (List PluralSelector)
    | Select (List SelectSelector)
    | Selectordinal (Maybe Int) (List PluralSelector)


type Type
    = Number
    | Date
    | Time
    | Spellout
    | Ordinal
    | Duration


type SimpleStyle
    = Integer
    | Currency
    | Percent
    | Short
    | Medium
    | Long
    | Full
    | Custom String


type SelectSelector
    = SelectSelector String Message


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


selectSelectorMessage : SelectSelector -> Message
selectSelectorMessage (SelectSelector _ message) =
    message


pluralSelectorMessage : PluralSelector -> Message
pluralSelectorMessage (PluralSelector _ message) =
    message



---- ARGUMENT RETRIEVAL


numberArguments : Message -> Set String
numberArguments message =
    message
        |> List.map numberArgumentsFromPart
        |> List.foldl Set.union Set.empty


numberArgumentsFromPart : Part -> Set String
numberArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                Simple Number _ ->
                    Set.singleton name

                Simple Ordinal _ ->
                    Set.singleton name

                Simple Duration _ ->
                    Set.singleton name

                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> numberArguments)
                        |> List.foldl Set.union (Set.singleton name)

                Select selectSelectors ->
                    selectSelectors
                        |> List.map (selectSelectorMessage >> numberArguments)
                        |> List.foldl Set.union Set.empty

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> numberArguments)
                        |> List.foldl Set.union (Set.singleton name)

                _ ->
                    Set.empty

        _ ->
            Set.empty


selectArguments : Message -> Dict String (Set String)
selectArguments message =
    message
        |> List.map selectArgumentsFromPart
        |> List.foldl Dict.union Dict.empty


selectArgumentsFromPart : Part -> Dict String (Set String)
selectArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> selectArguments)
                        |> List.foldl Dict.union Dict.empty

                Select selectSelectors ->
                    let
                        selectArgument =
                            selectSelectors
                                |> List.map (\(SelectSelector selector _) -> selector)
                                |> Set.fromList
                    in
                    selectSelectors
                        |> List.map (selectSelectorMessage >> selectArguments)
                        |> List.foldl Dict.union (Dict.singleton name selectArgument)

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> selectArguments)
                        |> List.foldl Dict.union Dict.empty

                _ ->
                    Dict.empty

        _ ->
            Dict.empty


textArguments : Message -> Set String
textArguments message =
    message
        |> List.map textArgumentsFromPart
        |> List.foldl Set.union Set.empty


textArgumentsFromPart : Part -> Set String
textArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                None ->
                    Set.singleton name

                Simple Date _ ->
                    Set.singleton name

                Simple Time _ ->
                    Set.singleton name

                Simple Spellout _ ->
                    Set.singleton name

                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> textArguments)
                        |> List.foldl Set.union Set.empty

                Select selectSelectors ->
                    selectSelectors
                        |> List.map (selectSelectorMessage >> textArguments)
                        |> List.foldl Set.union Set.empty

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> textArguments)
                        |> List.foldl Set.union Set.empty

                _ ->
                    Set.empty

        _ ->
            Set.empty



---- VIEW


viewNumberArgumentInput : (String -> String -> msg) -> String -> Html msg
viewNumberArgumentInput updateNumberValue name =
    Html.div []
        [ Html.text name
        , Html.input
            [ Events.onInput (updateNumberValue name)
            , Attributes.type_ "number"
            , Attributes.min "0"
            ]
            []
        ]


viewSelectArgumentInput : (String -> String -> msg) -> String -> Set String -> Html msg
viewSelectArgumentInput updateSelectValue name values =
    let
        viewOption value =
            Html.option
                [ Attributes.value value ]
                [ Html.text value ]
    in
    Html.div []
        [ Html.text name
        , values
            |> Set.toList
            |> List.map viewOption
            |> Html.select
                [ Events.onInput (updateSelectValue name) ]
        ]


viewTextArgumentInput : (String -> String -> msg) -> String -> Html msg
viewTextArgumentInput updateValue name =
    Html.div []
        [ Html.text name
        , Html.input
            [ Events.onInput (updateValue name) ]
            []
        ]



---- EVALUATER


type alias Values =
    { number : Dict String Int
    , select : Dict String String
    , text : Dict String String
    }


evaluate : Values -> Message -> String
evaluate values message =
    message
        |> List.map (evaluatePart values Nothing)
        |> String.concat


evaluatePart : Values -> Maybe Int -> Part -> String
evaluatePart values maybeHash part =
    case part of
        Text text ->
            text

        Argument name details ->
            case details of
                None ->
                    Dict.get name values.text
                        |> Maybe.withDefault ("{" ++ name ++ "}")

                Simple tvpe maybeStyle ->
                    "{TODO}"

                Plural maybeOffset pluralSelectors ->
                    case Dict.get name values.number of
                        Just value ->
                            evaluatePluralSelectors
                                values
                                maybeOffset
                                value
                                pluralSelectors

                        Nothing ->
                            "{" ++ name ++ "}"

                Select selectSelectors ->
                    case Dict.get name values.select of
                        Just value ->
                            evaluateSelectSelectors values
                                value
                                selectSelectors

                        Nothing ->
                            "{" ++ name ++ "}"

                Selectordinal maybeOffset pluralSelectors ->
                    case Dict.get name values.number of
                        Just value ->
                            evaluatePluralSelectors
                                values
                                maybeOffset
                                value
                                pluralSelectors

                        Nothing ->
                            "{" ++ name ++ "}"

        Hash ->
            maybeHash
                |> Maybe.map toString
                |> Maybe.withDefault "#"


evaluatePluralSelectors : Values -> Maybe Int -> Int -> List PluralSelector -> String
evaluatePluralSelectors values maybeOffset value pluralSelectors =
    let
        printedValue =
            maybeOffset
                |> Maybe.map ((-) value)
                |> Maybe.withDefault value

        evaluateMessage message =
            message
                |> List.map (evaluatePart values (Just printedValue))
                |> String.concat
    in
    pluralSelectors
        |> List.filterMap
            (\(PluralSelector name message) ->
                case name of
                    ExplicitValue v ->
                        if v == value then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    Zero ->
                        if value == 0 then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    One ->
                        if value == 1 then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    Two ->
                        if value == 2 then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    Few ->
                        if value <= 12 then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    Many ->
                        if value > 12 then
                            Just (evaluateMessage message)
                        else
                            Nothing

                    Other ->
                        Just (evaluateMessage message)
            )
        |> List.head
        |> Maybe.withDefault "{missingValue}"


evaluateSelectSelectors : Values -> String -> List SelectSelector -> String
evaluateSelectSelectors values value selectSelectors =
    let
        evaluateMessage message =
            message
                |> List.map (evaluatePart values Nothing)
                |> String.concat
    in
    selectSelectors
        |> List.filterMap
            (\(SelectSelector v message) ->
                if v == value then
                    Just (evaluateMessage message)
                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault "{missingValue}"



---- PARSER


parse : String -> Result Parser.Error Message
parse text =
    Parser.run icu text


icu : Parser Message
icu =
    message True


message : Bool -> Parser Message
message allowHash =
    oneOf
        (if allowHash then
            [ textPart allowHash
            , lazy (\_ -> argumentPart)
            ]
         else
            [ textPart allowHash
            , lazy (\_ -> argumentPart)
            , succeed Hash
                |. symbol "#"
            ]
        )
        |> repeat oneOrMore
        |> map joinTextParts
        |> inContext "a message"


joinTextParts : List Part -> List Part
joinTextParts parts =
    case parts of
        [] ->
            []

        (Text first) :: (Text second) :: rest ->
            joinTextParts (Text (first ++ second) :: rest)

        first :: rest ->
            first :: joinTextParts rest


textPart : Bool -> Parser Part
textPart allowHash =
    let
        forbiddenSymbols =
            if allowHash then
                [ '{', '}', '\'' ]
            else
                [ '{', '}', '#', '\'' ]
    in
    inContext "some text" <|
        succeed Text
            |= oneOf
                [ source (ignore oneOrMore (isNotOneOf forbiddenSymbols))
                , delayedCommit (symbol "'") <|
                    succeed identity
                        |= source (ignore oneOrMore (isNotOneOf [ '\'' ]))
                        |. symbol "'"
                , delayedCommit (symbol "'") <|
                    succeed "'"
                        |. symbol "'"
                , fail "some text"
                ]


isNotOneOf : List Char -> Char -> Bool
isNotOneOf chars char =
    chars |> List.any (\c -> c == char) |> not


argumentPart : Parser Part
argumentPart =
    inContext "an argument" <|
        succeed Argument
            |. symbol "{"
            |. spaces
            |= argName
            |. spaces
            |= oneOf
                [ succeed identity
                    |. symbol ","
                    |. spaces
                    |= lazy (\_ -> details)
                    |. spaces
                    |. symbol "}"
                , succeed None
                    |. spaces
                    |. symbol "}"
                ]


details : Parser Details
details =
    let
        isOtherPlural (PluralSelector selectorName _) =
            case selectorName of
                Other ->
                    True

                _ ->
                    False

        isOtherSelect (SelectSelector selectorName _) =
            case selectorName of
                "other" ->
                    True

                _ ->
                    False
    in
    inContext "argument details" <|
        oneOf
            [ succeed Simple
                |= tvpe
                |= oneOf
                    [ delayedCommit spaces
                        (succeed Just
                            |. symbol ","
                            |. spaces
                            |= simpleStyle
                        )
                    , succeed Nothing
                    ]
                |> map normalizeSimpleArg
            , succeed Plural
                |. keyword "plural"
                |. spaces
                |. symbol ","
                |. spaces
                |= inContext "the offset value"
                    (oneOf
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
                                if pluralSelectors |> List.any isOtherPlural then
                                    succeed pluralSelectors
                                else
                                    fail "at least an 'other' selector"
                            )
                   )
            , succeed Selectordinal
                |. keyword "selectordinal"
                |. spaces
                |. symbol ","
                |. spaces
                |= inContext "the offset value"
                    (oneOf
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
                                if pluralSelectors |> List.any isOtherPlural then
                                    succeed pluralSelectors
                                else
                                    fail "at least an 'other' selector"
                            )
                   )
            , succeed Select
                |. keyword "select"
                |. spaces
                |. symbol ","
                |. spaces
                |= (repeat oneOrMore (lazy (\_ -> selectSelector))
                        |> andThen
                            (\selectors ->
                                if selectors |> List.any isOtherSelect then
                                    succeed selectors
                                else
                                    fail "at least an 'other' selector"
                            )
                   )
            ]


tvpe : Parser Type
tvpe =
    oneOf
        [ keyword "number" |> map (always Number)
        , keyword "date" |> map (always Date)
        , keyword "time" |> map (always Time)
        , keyword "spellout" |> map (always Spellout)
        , keyword "ordinal" |> map (always Ordinal)
        , keyword "duration" |> map (always Duration)
        ]


simpleStyle : Parser SimpleStyle
simpleStyle =
    let
        toArgStyle style =
            case style of
                "integer" ->
                    Integer

                "currency" ->
                    Currency

                "percent" ->
                    Percent

                "short" ->
                    Short

                "medium" ->
                    Medium

                "long" ->
                    Long

                "full" ->
                    Full

                _ ->
                    Custom style
    in
    variable isFirstVarChar isVarChar Set.empty
        |> map toArgStyle


selectSelector : Parser SelectSelector
selectSelector =
    inContext "a selector" <|
        delayedCommit spaces <|
            succeed SelectSelector
                |= variable isFirstVarChar isVarChar Set.empty
                |. spaces
                |. symbol "{"
                |= lazy (\_ -> message True)
                |. symbol "}"
                |. spaces


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
                        |= lazy (\_ -> message False)
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


argName : Parser String
argName =
    oneOf
        [ variable isFirstVarChar isVarChar Set.empty
        , succeed toString
            |= (int |> andThen isPositive)
        ]



---- PARSER HELPER


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


normalizeSimpleArg : Details -> Details
normalizeSimpleArg argDetails =
    case argDetails of
        Simple argType (Just argStyle) ->
            case argType of
                Number ->
                    case argStyle of
                        Integer ->
                            argDetails

                        Currency ->
                            argDetails

                        Percent ->
                            argDetails

                        _ ->
                            Simple argType (Just (Custom (printStyle argStyle)))

                Date ->
                    case argStyle of
                        Short ->
                            argDetails

                        Medium ->
                            argDetails

                        Long ->
                            argDetails

                        Full ->
                            argDetails

                        _ ->
                            Simple argType (Just (Custom (printStyle argStyle)))

                Time ->
                    case argStyle of
                        Short ->
                            argDetails

                        Medium ->
                            argDetails

                        Long ->
                            argDetails

                        Full ->
                            argDetails

                        _ ->
                            Simple argType (Just (Custom (printStyle argStyle)))

                Spellout ->
                    Simple argType (Just (Custom (printStyle argStyle)))

                Ordinal ->
                    Simple argType (Just (Custom (printStyle argStyle)))

                Duration ->
                    Simple argType (Just (Custom (printStyle argStyle)))

        _ ->
            argDetails


printStyle : SimpleStyle -> String
printStyle argStyle =
    case argStyle of
        Integer ->
            "integer"

        Currency ->
            "currency"

        Percent ->
            "percent"

        Short ->
            "short"

        Medium ->
            "medium"

        Long ->
            "long"

        Full ->
            "full"

        Custom style ->
            style



---- PARSER ERROR MESSAGES


print : Parser.Error -> Html msg
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



---- HELPER


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
