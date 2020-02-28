module Letters exposing (Letter, alphabet)

import Dict exposing (Dict)
import List.Extra exposing (getAt)
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes exposing (..)


type alias Point =
    ( Float, Float )


type alias Letter =
    { points : List Point, lines : List ( Int, Int ) }


type alias Alphabet =
    Dict Char Letter


main =
    svg [ viewBox "0 0 1000 1000", stroke "black" ]
        (List.indexedMap viewLetter <| String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


viewLetter : Int -> Char -> Svg msg
viewLetter n c =
    let
        drawLetter { points, lines } =
            List.map (drawLine points) lines

        drawLine points ( i, j ) =
            let
                ( xa, ya ) =
                    Maybe.withDefault ( 0, 0 ) <| getAt i points

                ( xb, yb ) =
                    Maybe.withDefault ( 0, 0 ) <| getAt j points
            in
            line
                [ x1 <| String.fromFloat <| 20 + 20 * xa + (toFloat <| 80 * modBy 5 n)
                , y1 <| String.fromFloat <| 20 + 20 * ya + (toFloat <| 120 * (n // 5))
                , x2 <| String.fromFloat <| 20 + 20 * xb + (toFloat <| 80 * modBy 5 n)
                , y2 <| String.fromFloat <| 20 + 20 * yb + (toFloat <| 120 * (n // 5))
                ]
                []
    in
    svg [] <|
        drawLetter <|
            Maybe.withDefault (Letter [] []) <|
                Dict.get c alphabet


alphabet : Alphabet
alphabet =
    Dict.fromList
        [ ( ' ', Letter [] [] )
        , ( 'A'
          , { points = [ ( 0, 4 ), ( 0.75, 2 ), ( 2.25, 2 ), ( 1.5, 0 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 1, 3 ), ( 3, 2 ), ( 2, 4 ) ]
            }
          )
        , ( 'B'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 3, 1 ), ( 0, 2 ), ( 3, 3 ) ]
            , lines = [ ( 0, 3 ), ( 3, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 0 ) ]
            }
          )
        , ( 'C'
          , { points = [ ( 3, 0 ), ( 0, 2 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ) ]
            }
          )
        , ( 'D'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 3, 2 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 0 ) ]
            }
          )
        , ( 'E'
          , { points = [ ( 0, 2 ), ( 3, 0 ), ( 3, 2 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            }
          )
        , ( 'F'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 3, 0 ), ( 3, 2 ), ( 0, 2 ) ]
            , lines = [ ( 0, 4 ), ( 4, 1 ), ( 1, 2 ), ( 3, 4 ) ]
            }
          )
        , ( 'G'
          , { points = [ ( 1.5, 0 ), ( 0, 2 ), ( 1.5, 4 ), ( 3, 2 ), ( 1.5, 2 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ) ]
            }
          )
        , ( 'H'
          , { points = [ ( 0, 4 ), ( 0, 2 ), ( 0, 0 ), ( 3, 0 ), ( 3, 2 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 3, 4 ), ( 4, 5 ), ( 4, 1 ) ]
            }
          )
        , ( 'I'
          , { points = [ ( 1.5, 4 ), ( 1.5, 0 ) ]
            , lines = [ ( 0, 1 ) ]
            }
          )
        , ( 'J'
          , { points = [ ( 1.5, 0 ), ( 3, 0 ), ( 3, 4 ), ( 0, 4 ), ( 0, 3 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ) ]
            }
          )
        , ( 'K'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 0, 2 ), ( 3, 0 ), ( 3, 4 ) ]
            , lines = [ ( 2, 0 ), ( 2, 1 ), ( 2, 3 ), ( 2, 4 ) ]
            }
          )
        , ( 'L'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 0, 2 ) ]
            }
          )
        , ( 'M'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 1.5, 2 ), ( 3, 0 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ) ]
            }
          )
        , ( 'N'
          , { points = [ ( 0, 4 ), ( 0, 0 ), ( 3, 4 ), ( 3, 0 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
            }
          )
        , ( 'O'
          , { points = [ ( 1.5, 0 ), ( 0, 2 ), ( 1.5, 4 ), ( 3, 2 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 0 ) ]
            }
          )
        , ( 'P'
          , { points = [ ( 0, 4 ), ( 0, 2 ), ( 0, 0 ), ( 3, 1 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 1 ) ]
            }
          )
        , ( 'Q'
          , { points = [ ( 1.5, 0 ), ( 0, 2 ), ( 1.5, 4 ), ( 3, 2 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 0 ), ( 2, 4 ) ]
            }
          )
        , ( 'R'
          , { points = [ ( 0, 4 ), ( 0, 2 ), ( 0, 0 ), ( 3, 1 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 1 ), ( 1, 4 ) ]
            }
          )
        , ( 'S'
          , { points = [ ( 3, 0 ), ( 0, 1.33 ), ( 3, 2.66 ), ( 0, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
            }
          )
        , ( 'T'
          , { points = [ ( 1.5, 0 ), ( 3, 0 ), ( 0, 0 ), ( 1.5, 4 ) ]
            , lines = [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            }
          )
        , ( 'U'
          , { points = [ ( 0, 0 ), ( 0, 4 ), ( 3, 4 ), ( 3, 0 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
            }
          )
        , ( 'V'
          , { points = [ ( 0, 0 ), ( 1.5, 4 ), ( 3, 0 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ) ]
            }
          )
        , ( 'W'
          , { points = [ ( 0, 0 ), ( 1, 4 ), ( 1.5, 1 ), ( 2, 4 ), ( 3, 0 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ) ]
            }
          )
        , ( 'X'
          , { points = [ ( 1.5, 2 ), ( 3, 0 ), ( 0, 0 ), ( 0, 4 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ) ]
            }
          )
        , ( 'Y'
          , { points = [ ( 1.5, 2 ), ( 3, 0 ), ( 0, 0 ), ( 1.5, 4 ) ]
            , lines = [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            }
          )
        , ( 'Z'
          , { points = [ ( 0, 0 ), ( 3, 0 ), ( 0, 4 ), ( 3, 4 ) ]
            , lines = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
            }
          )
        ]
