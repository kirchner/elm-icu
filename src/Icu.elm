module Icu
    exposing
        ( Message
        , Values
        , evaluate
        , generateFunction
        , noneArguments
        , parse
        , pluralArguments
        , selectArguments
        )

{-| This module exposes a type representation of the [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html)
called `Message`.

  - You can obtain these types by parsing `String`s using `parse : String
    -> Result Error Message`

  - You can evaluate `Message`'s using `evaluate : Values -> Message ->
    String` by providing dictionaries containing the needed values.

  - You can generate elm code of functions from `Message`'s using
    `generateFunction : String -> Message -> String`. For example

        [ "{count, plural,"
        , "=0{There is no {item}.} "
        , "=1{There is one {item}.} "
        , "other{There are # {item}s.}"
        , "}"
        ]
            |> String.concat
            |> parse
            |> Result.map (generateFunction "itemCountInfo")

    will result in the following elm code

        itemCountInfo : Int -> String -> String
        itemCountInfo count item =
            case count of
                1 ->
                    [ "There is one "
                    , item
                    , "."
                    ]
                        |> String.concat

                0 ->
                    [ "There is no "
                    , item
                    , "."
                    ]
                        |> String.concat

                _ ->
                    [ "There are "
                    , count
                    , " "
                    , item
                    , "s."
                    ]
                        |> String.concat


# Open things

  - code generation for simple and selectordinal arguments


# EBNF

Check out the original [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
The parser in this module is based on the following simplified grammar:

```ebnf
message = messageText, { argument, messageText } ;

messageText = ? any text. {, } and # must be quoted like '{', '' becomes ' ? ;

argument
    = ( "{", argName, "}" )
    | ( "{", argName, ",", argType, [ ",", argStyle ], "}" )
    | ( "{", argName, ",", "plural", ",", pluralStyle "}" )
    | ( "{", argName, ",", "select", ",", selectStyle "}" )
    | ( "{", argName, ",", "selectordinal", ",", pluralStyle, "}" ) ;

argName = lower case letter, { letter | "_" } ;

argType
    = "number"
    | "date"
    | "time"
    | "spellout"
    | "ordinal"
    | "duration" ;

argStyle
    = "short"
    | "medium"
    | "long"
    | "full"
    | "integer"
    | "currency"
    | "percent"
    | argStyleText ;

argStyleText = ? depends on the argType ? ;


pluralStyle =
    [ "offset:", ? integer ? ],
    { pluralSelector, "{", message, "}" },
    "other", "{", message, "}",
    { pluralSelector, "{", message, "}" } ;

pluralSelector
    = ( '=', ? integer ? )
    | "zero"
    | "one"
    | "two"
    | "few"
    | "many"


selectStyle =
    { selector, "{", message, "}" },
    "other", "{", message, "}",
    { selector, "{", message, "}" } ;

selector = lower case letter, { letter | "_" } ;
```

The allowed patterns for `argStyleText` are the following:
[number](https://docs.oracle.com/javase/9/docs/api/java/text/DecimalFormat.html),
[date and
time](https://docs.oracle.com/javase/9/docs/api/java/text/SimpleDateFormat.html),
[spellout, ordinal and
duration](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/RuleBasedNumberFormat.html)


# Types

@docs Message, noneArguments, pluralArguments, selectArguments


# Parsing

@docs parse


# Evalution

@docs evaluate, Values


# Code generation

@docs generateFunction

-}

import Char
import Dict exposing (Dict)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Set exposing (Set)
import String.Extra as String


{-| This represents an ICU Message.
-}
type Message
    = Message (List Part)


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


{-| Return the names of all none arguments in the `Message`.
-}
noneArguments : Message -> Set String
noneArguments (Message message) =
    message
        |> List.map noneArgumentsFromPart
        |> List.foldl Set.union Set.empty


noneArgumentsFromPart : Part -> Set String
noneArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                None ->
                    Set.singleton name

                Simple _ _ ->
                    Set.empty

                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> noneArguments)
                        |> List.foldl Set.union Set.empty

                Select selectSelectors ->
                    selectSelectors
                        |> List.map (selectSelectorMessage >> noneArguments)
                        |> List.foldl Set.union Set.empty

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> noneArguments)
                        |> List.foldl Set.union Set.empty

        _ ->
            Set.empty


{-| Return the names of all plural arguments in the `Message`.
-}
pluralArguments : Message -> Set String
pluralArguments (Message message) =
    message
        |> List.map pluralArgumentsFromPart
        |> List.foldl Set.union Set.empty


pluralArgumentsFromPart : Part -> Set String
pluralArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                None ->
                    Set.empty

                Simple _ _ ->
                    Set.empty

                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> pluralArguments)
                        |> List.foldl Set.union (Set.singleton name)

                Select selectSelectors ->
                    selectSelectors
                        |> List.map (selectSelectorMessage >> pluralArguments)
                        |> List.foldl Set.union Set.empty

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> pluralArguments)
                        |> List.foldl Set.union (Set.singleton name)

        _ ->
            Set.empty


{-| Return the names of all select arguments along with their possible
choices.
-}
selectArguments : Message -> Dict String (Set String)
selectArguments (Message message) =
    message
        |> List.map selectArgumentsFromPart
        |> List.foldl Dict.union Dict.empty


selectArgumentsFromPart : Part -> Dict String (Set String)
selectArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                None ->
                    Dict.empty

                Simple _ _ ->
                    Dict.empty

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


internalSelectArguments : Message -> Dict String (List SelectSelector)
internalSelectArguments (Message message) =
    message
        |> List.map internalSelectArgumentsFromPart
        |> List.foldl Dict.union Dict.empty


internalSelectArgumentsFromPart : Part -> Dict String (List SelectSelector)
internalSelectArgumentsFromPart part =
    case part of
        Argument name details ->
            case details of
                None ->
                    Dict.empty

                Simple _ _ ->
                    Dict.empty

                Plural _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> internalSelectArguments)
                        |> List.foldl Dict.union Dict.empty

                Select selectSelectors ->
                    selectSelectors
                        |> List.map (selectSelectorMessage >> internalSelectArguments)
                        |> List.foldl Dict.union (Dict.singleton name selectSelectors)

                Selectordinal _ pluralSelectors ->
                    pluralSelectors
                        |> List.map (pluralSelectorMessage >> internalSelectArguments)
                        |> List.foldl Dict.union Dict.empty

        _ ->
            Dict.empty



---- PARSER


{-| Parse a `String` containing an ICU Message.
-}
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
        |> map Message


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
            , keyword "zero" |> map (\_ -> ExplicitValue 0)
            , keyword "one" |> map (\_ -> ExplicitValue 1)
            , keyword "two" |> map (\_ -> ExplicitValue 2)
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



---- EVALUATER


{-| -}
type alias Values =
    { noneArguments : Dict String String
    , pluralArguments : Dict String Int
    , selectArguments : Dict String String
    }


{-| Evaluate an ICU Message.
-}
evaluate : Values -> Message -> String
evaluate values (Message message) =
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
                    Dict.get name values.noneArguments
                        |> Maybe.withDefault ("{" ++ name ++ "}")

                Simple tvpe maybeStyle ->
                    "{TODO}"

                Plural maybeOffset pluralSelectors ->
                    case Dict.get name values.pluralArguments of
                        Just value ->
                            evaluatePluralSelectors
                                values
                                maybeOffset
                                value
                                pluralSelectors

                        Nothing ->
                            "{" ++ name ++ "}"

                Select selectSelectors ->
                    case Dict.get name values.selectArguments of
                        Just value ->
                            evaluateSelectSelectors values
                                value
                                selectSelectors

                        Nothing ->
                            "{" ++ name ++ "}"

                Selectordinal maybeOffset pluralSelectors ->
                    case Dict.get name values.pluralArguments of
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
            (\(PluralSelector name (Message message)) ->
                case name of
                    ExplicitValue v ->
                        if v == value then
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
            (\(SelectSelector v (Message message)) ->
                if v == value then
                    Just (evaluateMessage message)
                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault "{missingValue}"



---- GENERATE ELM CODE


{-| -}
generateFunction : String -> Message -> String
generateFunction translationKey message =
    let
        arguments =
            [ pluralArguments message
                |> Set.toList
                |> List.map (\argument -> ( "Int", argument ))
            , noneArguments message
                |> Set.toList
                |> List.map (\argument -> ( "String", argument ))
            , internalSelectArguments message
                |> Dict.map
                    (\argumentName selectSelectors ->
                        selectType translationKey argumentName selectSelectors
                    )
                |> Dict.toList
                |> List.map
                    (\( argumentName, { name } ) ->
                        ( name, argumentName )
                    )
            ]
                |> List.concat
    in
    [ translationKey
    , " : "
    , [ arguments
            |> List.map Tuple.first
      , [ "String" ]
      ]
        |> List.concat
        |> String.join " -> "
    , "\n"
    , (translationKey :: (arguments |> List.map Tuple.second))
        |> String.join " "
    , " =\n"
    , generateMessage translationKey Nothing message
        |> indent
    , generateSelectTypes translationKey message
    ]
        |> String.concat


generateSelectTypes : String -> Message -> String
generateSelectTypes translationKey message =
    let
        selectTypes =
            internalSelectArguments message
                |> Dict.map
                    (\argumentName selectSelectors ->
                        selectType translationKey argumentName selectSelectors
                    )
    in
    selectTypes
        |> Dict.toList
        |> List.map
            (\( argumentName, selectType ) ->
                generateSelectType argumentName selectType
            )
        |> String.concat


generateSelectType : String -> SelectType -> String
generateSelectType argumentName { name, possibilities } =
    [ "\n\n\ntype "
    , name
    , "\n"
    , [ "= "
      , possibilities
            |> Dict.toList
            |> List.map Tuple.first
            |> String.join "\n| "
      ]
        |> String.concat
        |> indent
    ]
        |> String.concat


generateMessage : String -> Maybe ( String, Int ) -> Message -> String
generateMessage translationKey maybeHash (Message message) =
    let
        parts =
            message
                |> List.map (generatePart translationKey maybeHash)
    in
    case parts of
        [] ->
            "\"\""

        part :: [] ->
            part

        _ ->
            [ "[ "
            , parts
                |> String.join "\n, "
            , "\n]\n"
            , indent "|> String.concat"
            ]
                |> String.concat


generatePart : String -> Maybe ( String, Int ) -> Part -> String
generatePart translationKey maybeHash part =
    case part of
        Text text ->
            [ "\""
            , text
            , "\""
            ]
                |> String.concat

        Hash ->
            case maybeHash of
                Just ( hashName, offset ) ->
                    [ hashName
                    , " - "
                    , toString offset
                    ]
                        |> String.concat

                Nothing ->
                    Debug.crash "no hashname"

        Argument name details ->
            case details of
                None ->
                    name

                Plural maybeOffset pluralSelectors ->
                    generatePlural translationKey name maybeOffset pluralSelectors

                Select selectSelectors ->
                    generateSelect translationKey name selectSelectors

                _ ->
                    "TODO"


generatePlural : String -> String -> Maybe Int -> List PluralSelector -> String
generatePlural translationKey argumentName maybeOffset pluralSelectors =
    let
        offset =
            maybeOffset |> Maybe.withDefault 0

        generatePluralSelector (PluralSelector selectorName message) =
            case selectorName of
                Many ->
                    ( -1
                    , selectorName
                    , generateMessage
                        translationKey
                        (Just ( argumentName, offset ))
                        message
                    )

                Few ->
                    ( -2
                    , selectorName
                    , generateMessage
                        translationKey
                        (Just ( argumentName, offset ))
                        message
                    )

                Other ->
                    ( -3
                    , selectorName
                    , generateMessage
                        translationKey
                        (Just ( argumentName, offset ))
                        message
                    )

                ExplicitValue amount ->
                    ( amount
                    , selectorName
                    , generateMessage
                        translationKey
                        (Just ( argumentName, offset ))
                        message
                    )
    in
    [ "case "
    , argumentName
    , " of\n"
    , pluralSelectors
        |> List.map generatePluralSelector
        |> List.sortBy (\( ord, _, _ ) -> -1 * ord)
        |> List.map
            (\( _, selectorName, text ) ->
                case selectorName of
                    ExplicitValue amount ->
                        [ toString amount
                        , " ->\n"
                        , indent text
                        ]
                            |> String.concat

                    Other ->
                        [ "_ ->\n"
                        , indent text
                        ]
                            |> String.concat

                    _ ->
                        "TODO"
            )
        |> List.intersperse "\n\n"
        |> String.concat
        |> indent
    ]
        |> String.concat


generateSelect : String -> String -> List SelectSelector -> String
generateSelect translationKey argumentName selectSelectors =
    let
        { name, possibilities } =
            selectType translationKey argumentName selectSelectors
    in
    [ "case "
    , argumentName
    , " of\n"
    , possibilities
        |> Dict.toList
        |> List.map
            (\( selectorName, message ) ->
                [ selectorName
                , " ->\n"
                , generateMessage translationKey Nothing message |> indent
                ]
                    |> String.concat
            )
        |> List.intersperse "\n\n"
        |> String.concat
        |> indent
    ]
        |> String.concat


type alias SelectType =
    { name : String
    , possibilities : Dict String Message
    }


selectType : String -> String -> List SelectSelector -> SelectType
selectType translationKey argumentName selectSelectors =
    let
        name =
            [ translationKey |> String.toSentenceCase
            , argumentName |> String.toSentenceCase
            ]
                |> String.concat

        possibility (SelectSelector selectorName message) =
            ( [ name
              , selectorName |> String.toSentenceCase
              ]
                |> String.concat
            , message
            )
    in
    { name = name
    , possibilities =
        selectSelectors
            |> List.map possibility
            |> Dict.fromList
    }



---- HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
