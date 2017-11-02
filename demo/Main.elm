module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Icu
import Icu.Error
import Parser
import Set exposing (Set)


parse : String -> Result Parser.Error Icu.Message
parse text =
    Icu.parse text


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { text : String
    , noneValues : Dict String String
    , pluralValues : Dict String Int
    , selectValues : Dict String String
    }


model : Model
model =
    { text =
        [ "{genderOfHost, select,"
        , [ "female"
          , "{{numGuests, plural, offset:1 "
          , "=0 {{host} does not give a party.}"
          , "=1 {{host} invites {guest} to her party.}"
          , "=2 {{host} invites {guest} and one other person to her party.}"
          , "other {{host} invites {guest} and # other people to her party.}}}"
          ]
            |> String.concat
        , [ "male"
          , "{{numGuests, plural, offset:1 "
          , "=0 {{host} does not give a party.}"
          , "=1 {{host} invites {guest} to his party.}"
          , "=2 {{host} invites {guest} and one other person to his party.}"
          , "other {{host} invites {guest} and # other people to his party.}}}"
          ]
            |> String.concat
        , [ "other"
          , "{{numGuests, plural, offset:1 "
          , "=0 {{host} does not give a party.}"
          , "=1 {{host} invites {guest} to their party.}"
          , "=2 {{host} invites {guest} and one other person to their party.}"
          , "other {{host} invites {guest} and # other people to their party.}}}"
          ]
            |> String.concat
        , "}"
        ]
            |> String.concat
    , noneValues = Dict.empty
    , pluralValues = Dict.empty
    , selectValues = Dict.empty
    }


type Msg
    = NoOp
    | UpdateText String
    | UpdateNoneValue String String
    | UpdatePluralValue String Int
    | UpdateSelectValue String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateText newText ->
            { model | text = newText }

        UpdateNoneValue name value ->
            { model | noneValues = Dict.insert name value model.noneValues }

        UpdatePluralValue name value ->
            { model | pluralValues = Dict.insert name value model.pluralValues }

        UpdateSelectValue name value ->
            { model | selectValues = Dict.insert name value model.selectValues }


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.style
            [ "display" => "flex"
            , "flex-flow" => "column"
            , "width" => "700px"
            , "margin" => "10px"
            ]
        ]
        [ Html.textarea
            [ Attributes.rows 10
            , Attributes.style
                [ "font-family" => "'Source Code Pro', Consolas, \"Liberation Mono\", Menlo, Courier, monospace" ]
            , Events.onInput UpdateText
            , Attributes.defaultValue model.text
            ]
            []
        , Html.pre
            [ Attributes.style
                [ "height" => "400px"
                , "margin" => "0"
                , "padding" => "10px"
                , "background-color" => "rgb(254,254,254)"
                , "border-style" => "solid"
                , "border-width" => "1px"
                , "border-color" => "rgb(245,245,245)"
                , "border-radius" => "6px"
                , "overflow-x" => "auto"
                ]
            ]
            [ case model.text |> Icu.parse of
                Ok data ->
                    Html.div
                        []
                        [ data
                            |> Icu.noneArguments
                            |> Set.toList
                            |> List.map viewNoneArgumentInput
                            |> Html.div
                                [ Attributes.style
                                    [ "display" => "flex"
                                    , "flex-flow" => "column"
                                    , "margin" => "5px"
                                    ]
                                ]
                        , data
                            |> Icu.pluralArguments
                            |> Set.toList
                            |> List.map viewPluralArgumentInput
                            |> Html.div
                                [ Attributes.style
                                    [ "display" => "flex"
                                    , "flex-flow" => "column"
                                    , "margin" => "5px"
                                    ]
                                ]
                        , data
                            |> Icu.selectArguments
                            |> Dict.toList
                            |> List.map
                                (\( name, values ) ->
                                    viewSelectArgumentInput name values
                                )
                            |> Html.div
                                [ Attributes.style
                                    [ "display" => "flex"
                                    , "flex-flow" => "column"
                                    , "margin" => "5px"
                                    ]
                                ]
                        , Html.code
                            [ Attributes.style
                                [ "font-family" => "'Source Code Pro', Consolas, \"Liberation Mono\", Menlo, Courier, monospace" ]
                            ]
                            [ Icu.evaluate
                                { noneArguments = model.noneValues
                                , pluralArguments = model.pluralValues
                                , selectArguments = model.selectValues
                                }
                                data
                                |> Html.text
                            ]
                        ]

                Err error ->
                    Icu.Error.print error
            ]
        ]


viewNoneArgumentInput : String -> Html Msg
viewNoneArgumentInput name =
    Html.div []
        [ Html.text name
        , Html.input
            [ Events.onInput (UpdateNoneValue name) ]
            []
        ]


viewPluralArgumentInput : String -> Html Msg
viewPluralArgumentInput name =
    Html.div []
        [ Html.text name
        , Html.input
            [ Events.onInput <|
                \value ->
                    case value |> String.toInt of
                        Ok int ->
                            UpdatePluralValue name int

                        Err _ ->
                            NoOp
            , Attributes.type_ "number"
            , Attributes.min "0"
            ]
            []
        ]


viewSelectArgumentInput : String -> Set String -> Html Msg
viewSelectArgumentInput name values =
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
                [ Events.onInput (UpdateSelectValue name) ]
        ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
