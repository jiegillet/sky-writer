module Main exposing (..)

import Animation
import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Dict
import Html exposing (..)
import Html.Attributes exposing (align, disabled, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode
import Letters exposing (Letter, alphabet)
import List.Extra exposing (minimumBy)
import Maybe
import Result
import Svg exposing (Svg, circle, line, mask, rect, svg)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as T


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { address : String
    , location : Location
    , name : String
    , stars : List Star
    , stars2D : List Position
    , day : Posix
    , circles : Circles
    , segments : Segments
    , screen : Screen
    , error : String
    , size : Size
    }


type alias Star =
    { mag : Float, ra : Float, de : Float }


type alias Location =
    { loc : String, lat : Float, lng : Float }


type alias Position =
    ( Float, Float, Float )


type alias Circles =
    Array Circle


type alias Circle =
    { style : Animation.State, x : Float, y : Float, mag : Float }


type alias Segments =
    List Segment


type alias Segment =
    { style : Animation.State, from : Int, to : Int }


type alias Size =
    { width : Float, height : Float }


type Screen
    = Info
    | StarMap



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { address = ""
      , location = defaultLoc
      , name = ""
      , stars = []
      , stars2D = []
      , day = j2000
      , circles = Array.empty
      , segments = []
      , screen = Info
      , error = ""
      , size = Size 640 480
      }
    , Cmd.batch [ Task.perform GetSize Dom.getViewport, getStarData ]
    )


defaultLoc : Location
defaultLoc =
    Location "Onna, Okinawa" 26.4975 127.8535



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { circles, segments } =
    Animation.subscription Animate <|
        (Array.toList <| Array.map .style circles)
            ++ List.map .style segments



-- UPDATE


type Msg
    = Render
    | GetSize Viewport
    | ReadLoc String
    | ReadName String
    | ReadDate String
    | NewStars (Result Http.Error (List Star))
    | NewLocation (Result Http.Error Location)
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSize { viewport } ->
            ( { model | size = Size viewport.width viewport.height }, Cmd.none )

        ReadLoc address ->
            ( { model | address = address }, Cmd.none )

        ReadName name ->
            ( { model | name = name }, Cmd.none )

        ReadDate date ->
            ( { model | day = parseTime date }, Cmd.none )

        Render ->
            ( model, getLocation model.address )

        NewStars (Ok stars) ->
            ( { model | stars = stars }, Cmd.none )

        NewStars (Err err) ->
            ( model, Cmd.none )

        NewLocation result ->
            let
                location =
                    Result.withDefault defaultLoc result

                stars2D =
                    List.map (getPosition location model.day) model.stars

                ( circ, seg ) =
                    initAnim stars2D (format model.name)
            in
            ( { model
                | location = location
                , stars2D = stars2D
                , circles = animateCircles circ
                , segments = animateSegments circ seg
                , screen = StarMap
              }
            , Cmd.none
            )

        Animate animMsg ->
            let
                updateCircle aniMsg circ =
                    { circ | style = Animation.update aniMsg circ.style }

                updateSegment aniMsg p =
                    { p | style = Animation.update aniMsg p.style }
            in
            ( { model
                | circles = Array.map (updateCircle animMsg) model.circles
                , segments = List.map (updateSegment animMsg) model.segments
              }
            , Cmd.none
            )



-- format name to allow A-Z and space only


format : String -> String
format =
    let
        select c =
            if c < 'A' || 'Z' < c then
                ' '

            else
                c
    in
    String.trim << String.map select << String.toUpper



-- Getting Star Locations


getStarData : Cmd Msg
getStarData =
    let
        forCORS =
            "https://cors-anywhere.herokuapp.com/"

        url =
            "https://www.astropical.space/api.php?table=stars&which=magnitude&limit=4.9&format=json"
    in
    Http.get { url = forCORS ++ url, expect = Http.expectJson NewStars decodeStars }


decodeStars : Decode.Decoder (List Star)
decodeStars =
    let
        star =
            Decode.map3 Star
                (Decode.field "mag" Decode.float)
                (Decode.field "ra" Decode.float)
                (Decode.field "de" Decode.float)
    in
    star |> Decode.list |> Decode.field "hipstars"



-- Getting location coordinates


getLocation : String -> Cmd Msg
getLocation loc =
    let
        url =
            "https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyB4-TN5fQZt0C4ZvX21n4a-8qsPfhYjfF4&address="
    in
    Http.get { url = url ++ loc, expect = Http.expectJson NewLocation decodeLocation }


decodeLocation : Decode.Decoder Location
decodeLocation =
    let
        res =
            Decode.field "results" << Decode.index 0
    in
    Decode.map3 Location
        (res <| Decode.field "formatted_address" Decode.string)
        (res <| Decode.at [ "geometry", "location", "lat" ] Decode.float)
        (res <| Decode.at [ "geometry", "location", "lng" ] Decode.float)



-- Getting Time


parseTime : String -> Posix
parseTime s =
    s
        ++ "T00:00:00Z"
        |> Iso8601.toTime
        |> Result.withDefault j2000


j2000 : Posix
j2000 =
    T.partsToPosix Time.utc (T.Parts 2000 Time.Jan 1 12 0 0 0)


gmst : Posix -> Float
gmst d =
    let
        f =
            toFloat (T.diff T.Hour Time.utc d j2000) / 24
    in
    18.697374558 + 24.06570982441908 * f


localSideralTime : Posix -> Location -> Float
localSideralTime d { lng } =
    gmst d + 24 * lng / 360


fromWeekday : Weekday -> String
fromWeekday weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


fromMonth : Month -> String
fromMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"



-- calculate position on stereographic projection


getPosition : Location -> Posix -> Star -> Position
getPosition ({ lat, lng } as loc) day { mag, ra, de } =
    let
        d =
            degrees de

        l =
            degrees (lat - 90)

        a =
            turns ((ra - localSideralTime day loc) / 24) + pi / 2

        den =
            1 - sin l * cos d * sin a + cos l * sin d
    in
    ( cos d * cos a / den, (cos l * cos d * sin a + sin l * sin d) / den, mag )



-- VIEW


size : Float
size =
    500


halfSize : String
halfSize =
    String.fromFloat (size / 2)


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Sky Writer"
        [ div
            [ Html.Attributes.style "height" <| String.fromFloat (model.size.height + 2) ++ "px"
            , Html.Attributes.style "width" <| String.fromFloat (model.size.width + 1) ++ "px"
            , Html.Attributes.style "backgroundColor" "black"
            , Html.Attributes.style "color" "white"
            , align "center"
            ]
          <|
            case model.screen of
                Info ->
                    [ h1 [ Html.Attributes.style "margin" "0" ] [ text "Welcome" ]
                    , h3 [] [ text "What is your first name?" ]
                    , div [] [ input [ placeholder "Gakkun", onInput ReadName ] [] ]
                    , h3 [] [ text "Where were you born?" ]
                    , div [] [ input [ placeholder "Onna, Okinawa", onInput ReadLoc ] [] ]
                    , h3 [] [ text "When were you born?" ]
                    , div [] [ input [ type_ "date", onInput ReadDate ] [] ]
                    , h3 [] []
                    , div []
                        [ button
                            [ onClick Render, disabled (List.isEmpty model.stars) ]
                            [ text "Continue" ]
                        ]
                    ]

                StarMap ->
                    let
                        s =
                            Basics.min model.size.height model.size.width * 7 / 10
                    in
                    [ svg
                        [ width (String.fromFloat s)
                        , height (String.fromFloat s)
                        , viewBox <| "0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size
                        , stroke "white"
                        , fill "none"
                        , strokeWidth "0.5"
                        , Svg.Attributes.mask "url(#hole)"
                        ]
                        (circleMask
                            :: bigCircle
                            :: drawParallels model.location
                            ++ drawMeridians model.location
                            ++ List.map drawStar model.stars2D
                            ++ viewCircles model.circles
                            ++ viewSegments model.segments
                        )
                    , h3 []
                        [ String.concat
                            [ Time.toWeekday Time.utc model.day |> fromWeekday
                            , ", "
                            , Time.toMonth Time.utc model.day |> fromMonth
                            , " "
                            , Time.toDay Time.utc model.day |> String.fromInt
                            , ", "
                            , Time.toYear Time.utc model.day |> String.fromInt
                            ]
                            |> text
                        ]
                    , h3 [] [ text model.location.loc ]
                    , h3 []
                        [ text <|
                            "You were born under these stars, "
                                ++ model.name
                        ]
                    , h3 [] [ text "Happy White Day" ]
                    ]
        ]



-- Drawing the star map


circleMask : Svg.Svg msg
circleMask =
    Svg.defs []
        [ Svg.mask [ id "hole" ]
            [ rect
                [ x "-5"
                , y "-5"
                , width <| String.fromFloat (size + 10) ++ "px"
                , height <| String.fromFloat (size + 10) ++ "px"
                , fill "black"
                ]
                []
            , circle [ r halfSize, cx halfSize, cy halfSize, fill "white" ] []
            ]
        ]


bigCircle : Svg.Svg msg
bigCircle =
    circle
        [ cx halfSize
        , cy halfSize
        , r halfSize
        , stroke "white"
        , strokeWidth "8"
        ]
        []


drawParallels : Location -> List (Svg.Svg msg)
drawParallels { lat } =
    let
        l =
            degrees lat

        parallel theta =
            let
                k =
                    cos theta + sin l
            in
            if abs k > 1.0e-8 then
                circle
                    [ cx halfSize
                    , cy <| scaleUp (-1 * cos l / k)
                    , r <|
                        String.fromFloat <|
                            size
                                / 2
                                * sqrt (1 - cos theta ^ 2)
                                / abs k
                    ]
                    []

            else
                line
                    [ x1 "0"
                    , x2 <| String.fromFloat size
                    , y1 <| scaleUp (-1 * cos theta)
                    , y2 <| scaleUp (-1 * cos theta)
                    ]
                    []
    in
    List.map (\n -> parallel <| pi * toFloat n / 12) (List.range 1 11)


drawMeridians : Location -> List (Svg.Svg msg)
drawMeridians { lat } =
    let
        l =
            degrees lat

        meridian phi =
            if abs (sin phi * cos l) > 1.0e-8 then
                circle
                    [ cx <| scaleUp (-1 / (tan phi * cos l))
                    , cy <| scaleUp (tan l)
                    , r <| String.fromFloat <| size / 2 / abs (sin phi * cos l)
                    ]
                    []

            else
                line
                    [ y1 "0"
                    , y2 <| String.fromFloat size
                    , x1 halfSize
                    , x2 halfSize
                    , transform <|
                        "rotate("
                            ++ String.fromFloat (phi / pi * 180)
                            ++ " "
                            ++ halfSize
                            ++ " "
                            ++ halfSize
                            ++ ")"
                    ]
                    []
    in
    List.map (\n -> meridian <| pi * toFloat n / 12) (List.range 0 11)


scaleUp : Float -> String
scaleUp x =
    String.fromFloat (size * (1 + x) / 2)


drawStar : Position -> Svg.Svg msg
drawStar ( x, y, mag ) =
    circle
        [ cx <| scaleUp x
        , cy <| scaleUp y
        , r <| String.fromFloat <| 0.9 * (6 - mag)
        , fill "white"
        ]
        []


viewCircles : Circles -> List (Svg msg)
viewCircles =
    let
        viewCircle { style, x, y, mag } =
            circle
                (Animation.render style
                    ++ [ cx <| scaleUp x
                       , cy <| scaleUp y
                       , r <| String.fromFloat <| 0.92 * (6 - mag)
                       ]
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
                (stroke "yellow" :: strokeWidth "1.5" :: Animation.render style)
                []
    in
    List.map viewSegment



-- ANIMATION


initAnim : List ( Float, Float, Float ) -> String -> ( Circles, Segments )
initAnim pos name =
    let
        theta =
            atan2 4 (5 * (toFloat <| String.length name) - 2)

        l =
            0.9 * sin theta / 2

        posIn =
            List.filter (\( x, y, _ ) -> x ^ 2 + y ^ 2 <= 1) pos

        closestPoint x0 ( xp, yp ) =
            Maybe.withDefault ( 1000, 1000, 0 ) <|
                minimumBy
                    (\( x, y, _ ) ->
                        (x - x0 - xp * l) ^ 2 + (y - yp * l + sin theta / 2) ^ 2
                    )
                    posIn

        gather c ( x0, cir, sg ) =
            let
                { points, lines } =
                    Maybe.withDefault (Letter [] []) (Dict.get c alphabet)

                newCirc =
                    List.map (closestPoint x0) points

                newSeg =
                    let
                        n =
                            List.length cir
                    in
                    List.map (\( i, j ) -> ( i + n, j + n )) lines
            in
            ( x0 + 5 * l, cir ++ newCirc, sg ++ newSeg )

        ( _, circ, seg ) =
            List.foldl gather
                ( -1 * cos theta + 0.05, [], [] )
                (String.toList name)

        mkCircles =
            let
                style =
                    Animation.style
                        [ Animation.strokeWidth 0
                        , Animation.fill (Animation.Color 255 255 255 255)
                        , Animation.stroke (Animation.Color 255 255 255 255)
                        ]
            in
            Array.fromList <|
                List.map (\( x, y, m ) -> Circle style x y m) circ

        mkSegments circles =
            let
                style =
                    Animation.style
                        [ Animation.path
                            [ Animation.moveTo 0 0, Animation.lineTo 0 0 ]
                        ]
            in
            List.map (\( i, j ) -> Segment style i j) seg
    in
    ( mkCircles, mkSegments mkCircles )


getCoord : Int -> Circles -> ( Float, Float )
getCoord n pts =
    let
        get f =
            Maybe.withDefault 1000 <| Maybe.map f <| Array.get n pts
    in
    ( get (\{ x } -> size * (1 + x) / 2)
    , get (\{ y } -> size * (1 + y) / 2)
    )


animateCircles : Circles -> Circles
animateCircles =
    let
        animateCircle circ =
            { circ
                | style =
                    Animation.queue
                        [ Animation.wait <| Time.millisToPosix <| floor <| 1000 + 100 * circ.x
                        , Animation.to
                            [ Animation.strokeWidth 10
                            , Animation.fill (Animation.Color 255 255 0 255)
                            , Animation.stroke (Animation.Color 255 255 0 255)
                            ]
                        , Animation.to [ Animation.strokeWidth 0 ]
                        ]
                        circ.style
            }
    in
    Array.map animateCircle


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
                        [ Animation.wait <| Time.millisToPosix <| floor <| 2000 + 5 * xf
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
