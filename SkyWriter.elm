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
    , error : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" (Location "" 0 0) [] "", Cmd.none )



-- UPDATE


type Msg
    = ReadLoc String
    | GetLoc
    | GetStars
    | NewStars (Result Http.Error (List Star))
    | NewLocation (Result Http.Error Location)


type alias Star =
    { mag : Float, ra : Float, de : Float }


type alias Location =
    { loc : String, lat : Float, lng : Float }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStars ->
            ( model, getStarData )

        NewStars (Ok stars) ->
            ( { model | stars = stars }, Cmd.none )

        NewStars (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        ReadLoc address ->
            ( { model | address = address }, Cmd.none )

        GetLoc ->
            ( model, getLocation model.address )

        NewLocation (Ok location) ->
            ( { model | location = location }, Cmd.none )

        NewLocation (Err err) ->
            ( { model | location = Location "Location not found" 0 0 }, Cmd.none )


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
            , button [ onClick GetLoc ] [ text "Get Location" ]
            , button [ onClick GetStars ] [ text "Get Stars" ]
            ]
        , svg
            [ width "600", height "600", viewBox "0 0 600 600" ]
            (circleMask
                :: circle [ cx "300", cy "300", r "297", stroke "white", strokeWidth "5" ] []
                :: (drawParallels model.location.lat
                        ++ drawMeridians model.location.lat
                   )
            )

        --            (List.map drawStar model.stars)
        ]


drawParallels : Float -> List (Svg.Svg msg)
drawParallels lambda =
    let
        mer theta =
            let
                k =
                    cos theta + sin lambda
            in
                if abs k > 1.0e-8 then
                    circle
                        [ cx "300"
                        , cy <| renorm (cos lambda / k) 2
                        , r <| toString <| 300 * sqrt (1 - cos theta ^ 2) / abs k
                        , stroke "white"
                        , fill "none"
                        , strokeWidth "0.5"
                        , Svg.Attributes.mask "url(#hole)"
                        ]
                        []
                else if lambda == pi / 2 then
                    line
                        [ x1 "0"
                        , x2 "600"
                        , y1 "300"
                        , y2 "300"
                        , stroke "white"
                        , strokeWidth "0.5"
                        ]
                        []
                else
                    svg [] []
    in
        List.map (\n -> mer <| pi * (toFloat n) / 24) <|
            List.range 1 23


drawMeridians : Float -> List (Svg.Svg msg)
drawMeridians lambda =
    let
        par phi =
            if abs (sin phi * cos lambda) > 1.0e-8 then
                circle
                    [ cx <| renorm (-1 / (tan phi * cos lambda)) 2
                    , cy <| renorm (-1 * tan lambda) 2
                    , r <| toString <| 300 / (abs (sin phi * cos lambda))
                    , stroke "white"
                    , fill "none"
                    , strokeWidth "0.5"
                    , Svg.Attributes.mask "url(#hole)"
                    ]
                    []
            else
                line
                    [ y1 "0"
                    , y2 "600"
                    , x1 "300"
                    , x2 "300"
                    , stroke "white"
                    , strokeWidth "0.5"
                    , transform <| "rotate(" ++ toString (phi / pi * 180) ++ " 300 300)"
                    ]
                    []
    in
        List.map (\n -> par <| pi * (toFloat n) / 12) <|
            List.range 0 11


renorm : Float -> Float -> String
renorm x xn =
    toString (600 * (1 / 2 + x / xn))


drawStar : Star -> Svg.Svg msg
drawStar { mag, ra, de } =
    circle
        [ cx (renorm (ra - 12) 5)
        , cy (renorm (de - 45) 40)
        , r (toString (8 - mag))
        , fill "white"
        ]
        []


circleMask : Svg.Svg msg
circleMask =
    Svg.defs []
        [ Svg.mask [ id "hole" ]
            [ rect [ width "100%", height "100%", fill "black" ] []
            , circle [ r "300", cx "300", cy "300", fill "white" ] []
            ]
        ]
