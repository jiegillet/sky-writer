module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, align, disabled)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg, svg, circle, line, mask, rect)
import Svg.Attributes exposing (..)
import Time.DateTime as T exposing (DateTime)
import Time.Format
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
      , name = ""
      , stars = []
      , stars2D = []
      , day = j2000
      , circles = Array.empty
      , segments = []
      , screen = Info
      , error = ""
      }
    , getStarData
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
            ( { model | name = name }, Cmd.none )

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

        den =
            1 - sin l * cos d * sin a + cos l * sin d
    in
        ( cos d * cos a / den, (cos l * cos d * sin a + sin l * sin d) / den, mag )



-- VIEW


size : Float
size =
    500


sizeStr : String
sizeStr =
    toString size


halfSize : String
halfSize =
    toString (size / 2)


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "height", "1000px" )
            , ( "backgroundColor", "black" )
            , ( "color", "white" )
            ]
        , align "center"
        ]
    <|
        case model.screen of
            Info ->
                [ h1 [ Html.Attributes.style [ ( "margin", "0" ) ] ]
                    [ text "Welcome" ]
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
                [ svg
                    [ width sizeStr
                    , height sizeStr
                    , viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
                    , stroke "white"
                    , fill "none"
                    , strokeWidth "0.5"
                    , Svg.Attributes.mask "url(#hole)"
                    ]
                    (circleMask
                        :: bigCircle
                        :: drawParallels model.location
                        ++ drawMeridians model.location
                        ++ (List.map drawStar model.stars2D)
                        ++ viewCircles model.circles
                        ++ viewSegments model.segments
                    )
                , h3 []
                    [ text <|
                        Time.Format.format "%A, %B %-d, %Y" (T.toTimestamp model.day)
                    ]
                , h3 [] [ text model.location.loc ]
                , h3 [] [ text <| "You were born under these stars, " ++ model.name ]
                , h3 [] [ text ("Happy White Day") ]
                ]



-- Drawing the star map


circleMask : Svg.Svg msg
circleMask =
    Svg.defs []
        [ Svg.mask [ id "hole" ]
            [ rect [ width "100%", height "100%", fill "black" ] []
            , circle [ r halfSize, cx halfSize, cy halfSize, fill "white" ] []
            ]
        ]


bigCircle : Svg.Svg msg
bigCircle =
    circle
        [ cx halfSize
        , cy halfSize
        , r <| toString (size / 2 - 3)
        , stroke "white"
        , strokeWidth "5"
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
                        , cy <| scaleUp ((-1) * cos l / k)
                        , r <|
                            toString <|
                                size
                                    / 2
                                    * sqrt (1 - cos theta ^ 2)
                                    / abs k
                        ]
                        []
                else
                    line
                        [ x1 "0"
                        , x2 sizeStr
                        , y1 <| scaleUp (-1 * cos theta)
                        , y2 <| scaleUp (-1 * cos theta)
                        ]
                        []
    in
        List.map (\n -> parallel <| pi * (toFloat n) / 12) (List.range 1 11)


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
                    , r <| toString <| size / 2 / (abs (sin phi * cos l))
                    ]
                    []
            else
                line
                    [ y1 "0"
                    , y2 sizeStr
                    , x1 halfSize
                    , x2 halfSize
                    , transform <|
                        "rotate("
                            ++ toString (phi / pi * 180)
                            ++ " "
                            ++ halfSize
                            ++ " "
                            ++ halfSize
                            ++ ")"
                    ]
                    []
    in
        List.map (\n -> meridian <| pi * (toFloat n) / 12) (List.range 0 11)


scaleUp : Float -> String
scaleUp x =
    toString (size * (1 + x) / 2)


drawStar : Position -> Svg.Svg msg
drawStar ( x, y, mag ) =
    circle
        [ cx <| scaleUp x
        , cy <| scaleUp y
        , r <| toString <| 0.9 * (6 - mag)
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
                       , r <| toString <| 0.92 * (6 - mag)
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

                newCirc =
                    List.map (closestPoint x0) points

                newSeg =
                    let
                        n =
                            List.length circ
                    in
                        List.map (\( i, j ) -> ( i + n, j + n )) lines
            in
                ( x0 + 5 * l, circ ++ newCirc, seg ++ newSeg )

        ( _, circ, seg ) =
            List.foldl gather
                ( -1 * cos theta + 0.05, [], [] )
                (String.toList name)

        mkCircles =
            let
                style =
                    Animation.style
                        [ Animation.strokeWidth 0
                        , Animation.fill Color.white
                        , Animation.stroke Color.white
                        ]
            in
                Array.fromList <|
                    List.map (\( x, y, m ) -> Circle style x y m) circ

        mkSegments circles =
            let
                style =
                    Animation.styleWith
                        (Animation.spring { stiffness = 10, damping = 5 })
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
            Maybe.withDefault 0 <| Maybe.map f <| Array.get n pts
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
                        [ Animation.wait <| 1000 + 100 * circ.x
                        , Animation.to
                            [ Animation.strokeWidth 10
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
                            [ Animation.wait <| 3000 + 5 * xf
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
