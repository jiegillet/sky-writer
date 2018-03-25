module Anim exposing (..)

import Html exposing (..)
import Animation
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color
import Array exposing (Array)
import Maybe


type alias Circles =
    Array Circle


type alias Circle =
    { style : Animation.State
    , x : Float
    , y : Float
    }


type alias Segments =
    List Segment


type alias Segment =
    { style : Animation.State
    , from : Int
    , to : Int
    }


emptyCircles =
    Array.empty


emptySegments =
    []


initAnim : List ( Float, Float, Float ) -> String -> ( Circles, Segments )
initAnim pos name =
    let
        theta =
            atan2 4 (4 * n -1)

        l =
            r * sqrt ((1 - cos (2 * theta)) / 8)

        posIn =
            List.filter (\( x, y, _ ) -> x ^ 2 + y ^ 2 <= 1) pos

        draw c ( x, pos, circ, seg ) =
            ( x + 4 * l, pos, circ, seg )

        ( _, _, circ, seg ) =
            List.fold draw ( (-1) * cos theta, posIn ) (String.toList name)
    in
        ( emptyCircles, emptySegments )


initCircles : Circles
initCircles =
    Array.initialize 20
        (\n ->
            Circle
                (Animation.styleWith
                    (Animation.spring { stiffness = 100, damping = 20 })
                    [ Animation.strokeWidth 0
                    , Animation.fill Color.blue
                    , Animation.stroke Color.blue
                    ]
                )
                (toFloat <| (71235 * n) % 300)
                (toFloat <| (92362 * n) % 300)
        )


initSegments : Segments
initSegments =
    let
        s =
            Animation.style
                [ Animation.path
                    [ Animation.moveTo 0 0
                    , Animation.lineTo 0 0
                    ]
                ]
    in
        [ Segment s 0 5, Segment s 1 2, Segment s 14 19, Segment s 8 12 ]



-- ANIMATION


animateCircles : Circles -> Circles
animateCircles =
    let
        animateCircle n circ =
            { circ
                | style =
                    Animation.queue
                        [ Animation.wait <| 100 * (toFloat n)
                        , Animation.to
                            [ Animation.strokeWidth 15
                            , Animation.fill Color.red
                            , Animation.stroke Color.red
                            ]
                        , Animation.to [ Animation.strokeWidth 0 ]
                        ]
                        circ.style
            }
    in
        Array.indexedMap animateCircle


animateSegments : Circles -> Segments -> Segments
animateSegments pts =
    let
        animateSegment points seg =
            { seg
                | style =
                    let
                        ( xf, yf ) =
                            getCoord seg.from points

                        ( xt, yt ) =
                            getCoord seg.to points
                    in
                        Animation.interrupt
                            [ Animation.wait <| 1000 + 100 * (toFloat seg.from)
                            , Animation.set
                                [ Animation.path
                                    [ Animation.moveTo xf yf
                                    , Animation.lineTo xf yf
                                    ]
                                ]
                            , Animation.to
                                [ Animation.path
                                    [ Animation.moveTo xf yf
                                    , Animation.lineTo xt yt
                                    ]
                                ]
                            ]
                            seg.style
            }
    in
        List.map (animateSegment pts)


getCoord : Int -> Circles -> ( Float, Float )
getCoord n pts =
    ( Maybe.withDefault 0 <| Maybe.map .x <| Array.get n pts
    , Maybe.withDefault 0 <| Maybe.map .y <| Array.get n pts
    )



-- SUBSCRIPTIONS


type alias Msg =
    Animation.Msg


subscriptionsCircles : (Animation.Msg -> msg) -> Circles -> Sub msg
subscriptionsCircles animMsg =
    Animation.subscription animMsg << Array.toList << Array.map .style


subscriptionsSegments : (Animation.Msg -> msg) -> Segments -> Sub msg
subscriptionsSegments animMsg =
    Animation.subscription animMsg << List.map .style



-- UPDATE


updateCircles : Animation.Msg -> Circles -> Circles
updateCircles animMsg =
    let
        updateCircle animMsg circ =
            { circ | style = Animation.update animMsg circ.style }
    in
        Array.map (updateCircle animMsg)


updateSegments : Animation.Msg -> Segments -> Segments
updateSegments animMsg =
    let
        updateSegment animMsg p =
            { p | style = Animation.update animMsg p.style }
    in
        List.map (updateSegment animMsg)



-- VIEW


viewCircles : Circles -> List (Svg msg)
viewCircles =
    let
        viewCircle { style, x, y } =
            circle
                (Animation.render style
                    ++ [ cx <| toString x, cy <| toString y, r "2" ]
                )
                []
    in
        Array.toList << Array.map viewCircle


viewSegments : Segments -> List (Svg msg)
viewSegments =
    let
        viewSegment : Segment -> Svg msg
        viewSegment { style } =
            Svg.path
                (stroke "cyan" :: Animation.render style)
                []
    in
        List.map viewSegment
