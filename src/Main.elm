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
    , numberValues : Dict String Int
    , selectValues : Dict String String
    , textValues : Dict String String
    }


model : Model
model =
    { text =
        """
{gender_of_host, select,
  female {
    {num_guests, plural, offset:1
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to her party.}
      =2 {{host} invites {guest} and one other person to her party.}
      other {{host} invites {guest} and # other people to her party.}}}
  male {
    {num_guests, plural, offset:1
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to his party.}
      =2 {{host} invites {guest} and one other person to his party.}
      other {{host} invites {guest} and # other people to his party.}}}
  other {
    {num_guests, plural, offset:1
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to their party.}
      =2 {{host} invites {guest} and one other person to their party.}
      other {{host} invites {guest} and # other people to their party.}}}}
        """

    -- text = "You {NUM_ADDS, plural,\n offset:1\n =0{did not add this}\n =1{added this}\n one{and one other person added this}\n other{and # others added this}\n}"
    , numberValues = Dict.empty
    , selectValues = Dict.empty
    , textValues = Dict.empty
    }


type Msg
    = UpdateText String
    | UpdateNumberValue String String
    | UpdateSelectValue String String
    | UpdateTextValue String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            { model | text = newText }

        UpdateNumberValue name value ->
            case String.toInt value of
                Ok int ->
                    { model | numberValues = Dict.insert name int model.numberValues }

                Err _ ->
                    model

        UpdateSelectValue name value ->
            { model | selectValues = Dict.insert name value model.selectValues }

        UpdateTextValue name value ->
            case value of
                "" ->
                    { model | textValues = Dict.remove name model.textValues }

                _ ->
                    { model | textValues = Dict.insert name value model.textValues }


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
                            |> Icu.textArguments
                            |> Set.toList
                            |> List.map (viewTextArgumentInput UpdateTextValue)
                            |> Html.div
                                [ Attributes.style
                                    [ "display" => "flex"
                                    , "flex-flow" => "column"
                                    , "margin" => "5px"
                                    ]
                                ]
                        , data
                            |> Icu.numberArguments
                            |> Set.toList
                            |> List.map (viewNumberArgumentInput UpdateNumberValue)
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
                                    viewSelectArgumentInput UpdateSelectValue name values
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
                                { text = model.textValues
                                , select = model.selectValues
                                , number = model.numberValues
                                }
                                data
                                |> Html.text
                            ]
                        ]

                Err error ->
                    Icu.Error.print error
            ]
        ]


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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
