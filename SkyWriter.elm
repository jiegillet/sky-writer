module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, align)
import Html.Events exposing (onClick, onInput)
import Http exposing (getString)
import Svg exposing (svg, circle, line, mask, rect)
import Svg.Attributes exposing (..)
import Result exposing (withDefault)
import Json.Decode as Decode
import Dict
import Time.DateTime as T


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { address : String
    , location : Location
    , stars : List Star
    , day : T.DateTime
    , error : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model ""
        defaultLoc
        []
        j2000
        ""
    , getStarData
    )


defaultLoc : Location
defaultLoc =
    Location "North Pole" 90 0



-- UPDATE


type Msg
    = Render
    | ReadLoc String
    | ReadDate String
    | NewStars (Result Http.Error (List Star))
    | NewLocation (Result Http.Error Location)


type alias Star =
    { mag : Float, ra : Float, de : Float }


type alias Location =
    { loc : String, lat : Float, lng : Float }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadLoc address ->
            ( { model | address = address }, Cmd.none )

        ReadDate date ->
            ( { model | day = parseTime date }, Cmd.none )

        Render ->
            ( model, getLocation model.address )

        NewStars (Ok stars) ->
            ( { model | stars = stars }, Cmd.none )

        NewStars (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        NewLocation (Ok location) ->
            ( { model | location = location }, Cmd.none )

        NewLocation (Err err) ->
            ( { model | location = defaultLoc }, Cmd.none )



-- Getting Star Locations


getStarData : Cmd Msg
getStarData =
    let
        forCORS =
            "https://cors-anywhere.herokuapp.com/"

        url =
            "http://www.astropical.space/astrodb/api.php?table=stars&which=magnitude&limit=0.9&format=json"

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


parseTime : String -> T.DateTime
parseTime s =
    withDefault j2000 <| T.fromISO8601 <| s ++ "T00:00:00Z"


j2000 : T.DateTime
j2000 =
    T.setHour 12 <| T.setYear 2000 <| T.dateTime T.zero


gmst : T.DateTime -> Float
gmst d =
    let
        f =
            (toFloat (T.delta d j2000).hours) / 24
    in
        18.697374558 + 24.06570982441908 * f


localSideralTime : T.DateTime -> Location -> Float
localSideralTime d { lng } =
    gmst d + 24 * lng / 360



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "backgroundColor", "#000" )
            , ( "height", "100%" )
            ]
        , align "center"
        ]
        [ div []
            [ input [ placeholder "Naha, Okinawa", onInput ReadLoc ] []
            , input [ type_ "date", onInput ReadDate ] []
            , button [ onClick Render ] [ text "Ok" ]
            ]
        , svg
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
                ++ (List.map (drawStar model.location model.day) model.stars)
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
                        , y1 <| toString <| 300 * (1 - cos theta)
                        , y2 <| toString <| 300 * (1 - cos theta)
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
                    , cy <| renorm (1 * tan l) 2
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


drawStar : Location -> T.DateTime -> Star -> Svg.Svg msg
drawStar ({ lat, lng } as loc) day { mag, ra, de } =
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
        circle
            [ cx <| toString <| 300 * (1 + (cos d * cos a) / den)
            , cy <| toString <| 300 * (1 + (cos l * y + sin l * sin d) / den)
            , r (toString (6 - mag))
            , fill "white"
            ]
            []
