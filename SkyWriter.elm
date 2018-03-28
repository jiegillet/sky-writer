module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, align)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg, svg, circle, line, mask, rect)
import Svg.Attributes exposing (..)
import Time.DateTime as T exposing (DateTime)
import Http
import Json.Decode as Decode
import Result
import Maybe
import Array exposing (Array)
import Dict
import List.Extra exposing (minimumBy)
import Animation
import Color
import Letters exposing (Letter, alphabet)
import Debug


main =
    Html.program
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
    , day : DateTime
    , circles : Circles
    , segments : Segments
    , screen : Sreen
    , error : String
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


type Sreen
    = Info
    | StarMap



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { address = ""
      , location = defaultLoc
      , name = "GAKKUN"
      , stars = []
      , stars2D = []
      , day = parseTime "2017-06-14"
      , circles = Array.empty
      , segments = []
      , screen = Info
      , error = ""
      }
    , getStarData
    )


defaultLoc : Location
defaultLoc =
    Location "Naha, Okinawa" 26.213 127.679



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { circles, segments } =
    Animation.subscription Animate <|
        (Array.toList <| Array.map .style circles)
            ++ List.map .style segments



-- UPDATE


type Msg
    = Render
    | ReadLoc String
    | ReadName String
    | ReadDate String
    | NewStars (Result Http.Error (List Star))
    | NewLocation (Result Http.Error Location)
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadLoc address ->
            ( { model | address = address }, Cmd.none )

        ReadName name ->
            ( { model | name = format name }, Cmd.none )

        ReadDate date ->
            ( { model | day = parseTime date }, Cmd.none )

        Render ->
            ( model, getLocation model.address )

        NewStars (Ok stars) ->
            ( { model | stars = stars }, Cmd.none )

        NewStars (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        NewLocation result ->
            let
                location =
                    Result.withDefault defaultLoc result

                stars2D =
                    List.map (getPosition location model.day) model.stars

                ( circ, seg ) =
                    initAnim stars2D model.name
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
                updateCircle animMsg circ =
                    { circ | style = Animation.update animMsg circ.style }

                updateSegment animMsg p =
                    { p | style = Animation.update animMsg p.style }
            in
                ( { model
                    | circles = Array.map (updateCircle animMsg) model.circles
                    , segments = List.map (updateSegment animMsg) model.segments
                  }
                , Cmd.none
                )



-- format name to A-Z and space only


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
            "http://www.astropical.space/astrodb/api.php?table=stars&which=magnitude&limit=4.9&format=json"

        request =
            Http.get (forCORS ++ url) decodeStars
    in
        Http.send NewStars request


decodeStars : Decode.Decoder (List Star)
decodeStars =
    let
        dict =
            Decode.field "hipstars" (Decode.dict star)

        star =
            Decode.map3 Star
                (Decode.field "mag" Decode.float)
                (Decode.field "ra" Decode.float)
                (Decode.field "de" Decode.float)
    in
        Decode.map Dict.values dict



-- Getting location coordinates


getLocation : String -> Cmd Msg
getLocation loc =
    let
        url =
            "https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyB4-TN5fQZt0C4ZvX21n4a-8qsPfhYjfF4&address="

        request =
            Http.get (url ++ loc) decodeLocation
    in
        Http.send NewLocation request


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


parseTime : String -> DateTime
parseTime s =
    Result.withDefault j2000 <| T.fromISO8601 <| s ++ "T00:00:00Z"


j2000 : DateTime
j2000 =
    T.setHour 12 <| T.setYear 2000 <| T.dateTime T.zero


gmst : DateTime -> Float
gmst d =
    let
        f =
            (toFloat (T.delta d j2000).hours) / 24
    in
        18.697374558 + 24.06570982441908 * f


localSideralTime : DateTime -> Location -> Float
localSideralTime d { lng } =
    gmst d + 24 * lng / 360



-- calculate position on stereographic projection


getPosition : Location -> DateTime -> Star -> Position
getPosition ({ lat, lng } as loc) day { mag, ra, de } =
    let
        d =
            degrees de

        l =
            degrees (lat - 90)

        a =
            turns ((ra - localSideralTime day loc) / 24) + pi / 2

        y =
            cos d * sin a

        den =
            1 - sin l * y + cos l * sin d
    in
        ( (cos d * cos a) / den, (cos l * y + sin l * sin d) / den, mag )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "backgroundColor", "#000" )
            , ( "height", "100%" )
            , ( "color", "white" )
            ]
        , align "center"
        ]
    <|
        case model.screen of
            Info ->
                [ h1 [] [ text "Welcome" ]
                , h3 [] [ text "What is your first name?" ]
                , div [] [ input [ placeholder "Gakkun", onInput ReadName ] [] ]
                , h3 [] [ text "Where were you born?" ]
                , div [] [ input [ placeholder "Onna, Okinawa", onInput ReadLoc ] [] ]
                , h3 [] [ text "What day were you born?" ]
                , div [] [ input [ type_ "date", onInput ReadDate ] [] ]
                , h3 [] []
                , div [] [ button [ onClick Render ] [ text "Continue" ] ]
                ]

            StarMap ->
                [ svg
                    [ width "600"
                    , height "600"
                    , viewBox "0 0 600 600"
                    , stroke "white"
                    , fill "none"
                    , strokeWidth "0.5"
                    , Svg.Attributes.mask "url(#hole)"
                    ]
                    (circleMask
                        :: circle [ cx "300", cy "300", r "297", stroke "white", strokeWidth "5" ] []
                        :: drawParallels model.location
                        ++ drawMeridians model.location
                        ++ (List.map drawStar model.stars2D)
                     -- ++ viewCircles model.circles
                     -- ++ viewSegments model.segments
                    )
                ]



-- Drawing the star map


circleMask : Svg.Svg msg
circleMask =
    Svg.defs []
        [ Svg.mask [ id "hole" ]
            [ rect [ width "100%", height "100%", fill "black" ] []
            , circle [ r "300", cx "300", cy "300", fill "white" ] []
            ]
        ]


drawParallels : Location -> List (Svg.Svg msg)
drawParallels { lat } =
    let
        l =
            degrees lat

        mer theta =
            let
                k =
                    cos theta + sin l
            in
                if abs k > 1.0e-8 then
                    circle
                        [ cx "300"
                        , cy <| renorm ((-1) * cos l / k) 2
                        , r <| toString <| 300 * sqrt (1 - cos theta ^ 2) / abs k
                        ]
                        []
                else if theta <= pi / 2 then
                    line
                        [ x1 "0"
                        , x2 "600"
                        , y1 <| renorm (cos theta) (-1)
                        , y2 <| renorm (cos theta) (-1)
                        ]
                        []
                else
                    svg [] []
    in
        List.map (\n -> mer <| pi * (toFloat n) / 12) (List.range 1 11)


drawMeridians : Location -> List (Svg.Svg msg)
drawMeridians { lat } =
    let
        l =
            degrees lat

        par phi =
            if abs (sin phi * cos l) > 1.0e-8 then
                circle
                    [ cx <| renorm (-1 / (tan phi * cos l)) 2
                    , cy <| renorm (tan l) 2
                    , r <| toString <| 300 / (abs (sin phi * cos l))
                    ]
                    []
            else
                line
                    [ y1 "0"
                    , y2 "600"
                    , x1 "300"
                    , x2 "300"
                    , transform <| "rotate(" ++ toString (phi / pi * 180) ++ " 300 300)"
                    ]
                    []
    in
        List.map (\n -> par <| pi * (toFloat n) / 12) (List.range 0 11)


renorm : Float -> Float -> String
renorm x xn =
    toString (600 * (1 / 2 + x / xn))


drawStar : Position -> Svg.Svg msg
drawStar ( x, y, mag ) =
    circle
        [ cx <| renorm x 1
        , cy <| renorm y 1
        , r (toString (6 - mag))
        , fill "white"
        ]
        []


viewCircles : Circles -> List (Svg msg)
viewCircles =
    let
        viewCircle { style, x, y, mag } =
            circle
                (Animation.render style
                    ++ [ cx <| renorm x 1
                       , cy <| renorm y 1
                       , r <| toString (6 - mag)
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
                (stroke "yellow" :: strokeWidth "2" :: Animation.render style)
                []
    in
        List.map viewSegment



-- ANIMATION


initAnim : List ( Float, Float, Float ) -> String -> ( Circles, Segments )
initAnim pos name =
    let
        theta =
            atan2 4 (4 * (toFloat <| String.length name) - 1)

        l =
            0.9 * sin theta / 4

        posIn =
            List.filter (\( x, y, _ ) -> x ^ 2 + y ^ 2 <= 1) pos

        closestPoint x0 ( xp, yp ) =
            Maybe.withDefault ( 0, 0, 0 ) <|
                minimumBy
                    (\( x, y, _ ) ->
                        (x - x0 - xp * l) ^ 2 + (y - yp * l + sin theta / 2) ^ 2
                    )
                    posIn

        gather c ( x0, circ, seg ) =
            let
                { points, lines } =
                    Maybe.withDefault (Letter [] []) (Dict.get c alphabet)

                pts =
                    List.map (closestPoint x0) points

                lns =
                    List.map
                        (\( i, j ) ->
                            ( i + List.length circ, j + List.length circ )
                        )
                        lines
            in
                ( x0 + 4 * l, circ ++ pts, seg ++ lns )

        ( _, circ, seg ) =
            List.foldl gather
                ( (-1 / 2) * cos theta, [], [] )
                (String.toList name)

        mkCircles =
            Array.fromList <|
                List.map
                    (\( x, y, m ) ->
                        Circle
                            (Animation.styleWith
                                (Animation.spring { stiffness = 100, damping = 20 })
                                [ Animation.strokeWidth 0
                                , Animation.fill Color.white
                                , Animation.stroke Color.white
                                ]
                            )
                            x
                            y
                            m
                    )
                    circ

        mkSegments circles =
            List.map
                (\( i, j ) ->
                    Segment
                        (Animation.styleWith
                            (Animation.spring { stiffness = 10, damping = 5 })
                            [ Animation.path
                                [ Animation.moveTo 0 0, Animation.lineTo 0 0 ]
                            ]
                        )
                        i
                        j
                )
                seg
    in
        ( mkCircles, mkSegments mkCircles )


getCoord : Int -> Circles -> ( Float, Float )
getCoord n pts =
    ( Maybe.withDefault 0 <| Maybe.map (\{ x } -> 600 * (1 / 2 + x)) <| Array.get n pts
    , Maybe.withDefault 0 <| Maybe.map (\{ y } -> 600 * (1 / 2 + y)) <| Array.get n pts
    )


animateCircles : Circles -> Circles
animateCircles =
    let
        animateCircle circ =
            { circ
                | style =
                    Animation.queue
                        [ Animation.wait <| 1000 + 100 * circ.x
                        , Animation.to
                            [ Animation.strokeWidth 15
                            , Animation.fill Color.yellow
                            , Animation.stroke Color.yellow
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
                            [ Animation.wait <| 3000 + 10 * xf
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
