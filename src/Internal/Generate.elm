module Internal.Generate
    exposing
        ( caseOf
        , function
        , indent
        , list
        , string
        , tvpe
        )

import String.Extra as String


function :
    { name : String
    , arguments : List ( String, String )
    , returnType : String
    , body : String
    }
    -> String
function { name, arguments, returnType, body } =
    let
        argumentNames =
            arguments |> List.map Tuple.first

        argumentTypes =
            arguments |> List.map Tuple.second
    in
    [ name
    , " : "
    , [ argumentNames
      , [ returnType ]
      ]
        |> List.concat
        |> String.join " -> "
    , "\n"
    , (name :: argumentTypes)
        |> String.join " "
    , " =\n"
    , body |> indent
    ]
        |> String.concat


string : String -> String
string text =
    [ "\""
    , text
    , "\""
    ]
        |> String.concat


list : List String -> String
list elements =
    [ "[ "
    , elements
        |> String.join "\n, "
    , "\n]\n"
    ]
        |> String.concat


caseOf :
    { expression : String
    , cases : List ( String, String )
    }
    -> String
caseOf { expression, cases } =
    [ "case "
    , expression
    , " of\n"
    , cases
        |> List.map
            (\( value, expression ) ->
                [ value
                , " ->\n"
                , expression |> indent
                ]
                    |> String.concat
            )
        |> List.intersperse "\n\n"
        |> String.concat
        |> indent
    ]
        |> String.concat


tvpe :
    { name : String
    , constructors : List String
    }
    -> String
tvpe { name, constructors } =
    [ "type "
    , name
    , "\n"
    , [ "= "
      , constructors |> String.join "\n| "
      ]
        |> String.concat
        |> indent
    ]
        |> String.concat


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
