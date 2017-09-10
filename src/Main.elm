module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Icu
import Parser
import Set
import String.Extra as String


parse text =
    Icu.parse text


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { text : String
    , namedNumberValues : Dict String Int
    , namedTextValues : Dict String String
    }


model : Model
model =
    { text = "You {NUM_ADDS, plural,\n offset:1\n =0{did not add this}\n =1{added this}\n one{and one other person added this}\n other{and # others added this}\n}"
    , namedNumberValues = Dict.empty
    , namedTextValues = Dict.empty
    }


type Msg
    = UpdateText String
    | UpdateNamedNumberValue String String
    | UpdateNamedTextValue String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            { model | text = newText }

        UpdateNamedNumberValue name value ->
            case String.toInt value of
                Ok int ->
                    { model | namedNumberValues = Dict.insert name int model.namedNumberValues }

                Err _ ->
                    model

        UpdateNamedTextValue name value ->
            case value of
                "" ->
                    { model | namedTextValues = Dict.remove name model.namedTextValues }

                _ ->
                    { model | namedTextValues = Dict.insert name value model.namedTextValues }


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
                            |> Icu.namedTextArguments
                            |> Set.toList
                            |> List.map (Icu.viewTextArgumentInput UpdateNamedTextValue)
                            |> Html.div
                                [ Attributes.style
                                    [ "display" => "flex"
                                    , "flex-flow" => "column"
                                    , "margin" => "5px"
                                    ]
                                ]
                        , data
                            |> Icu.namedNumberArguments
                            |> Set.toList
                            |> List.map (Icu.viewNumberArgumentInput UpdateNamedNumberValue)
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
                                { namedText = model.namedTextValues
                                , namedNumber = model.namedNumberValues
                                }
                                data
                                |> Html.text
                            , Html.text "\n\n"
                            , Html.text <|
                                (data
                                    |> List.map toString
                                    |> List.map (String.softWrap 70)
                                    |> String.join "\n"
                                )
                            ]
                        ]

                Err error ->
                    Icu.print error
            ]
        ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
