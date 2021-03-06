module Generate exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Icu
import Icu.Error


main : Program Never () msg
main =
    Html.beginnerProgram
        { model = ()
        , update = \_ model -> model
        , view = view
        }



---- VIEW


view : () -> Html msg
view _ =
    Html.pre
        [ Attributes.style
            [ "height" => "800px"
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
        [ case generate translations of
            Ok elmCode ->
                Html.text elmCode

            Err error ->
                error
        ]


generate : Dict String String -> Result (Html msg) String
generate translations =
    Dict.foldl
        (\translationKey icuMessage sum ->
            case sum of
                Ok sum ->
                    case Icu.parse icuMessage of
                        Ok icuValue ->
                            [ Icu.generate translationKey icuValue
                            , "\n\n\n"
                            ]
                                |> String.concat
                                |> String.append sum
                                |> Ok

                        Err error ->
                            Err (Icu.Error.print error)

                Err _ ->
                    sum
        )
        (Ok "")
        translations


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



---- DATA


translations : Dict String String
translations =
    [ "greeting" => "Hello!"
    , "simpleArgument" => "What is {this}?"
    , "treeCountInfo"
        => ("{treeCount, plural, "
                ++ "=0{There is no tree.} "
                ++ "=1{There is one tree.} "
                ++ "other{There are # trees.}"
                ++ "}"
           )
    , "nestedPlural"
        => ("{count, plural, "
                ++ "=0{There is no {item}.} "
                ++ "=1{There is one {item}.} "
                ++ "other{There are # {item}s.}"
                ++ "}"
           )
    , "selectTest"
        => ("{gender, select, "
                ++ "female {I saw her.}"
                ++ "male {I saw him.}"
                ++ "other {I saw them.}"
                ++ "}"
           )
    , "allTogether"
        => ([ "{genderOfHost, select,"
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
           )
    ]
        |> Dict.fromList
