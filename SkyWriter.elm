module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (getString)
import Svg exposing (svg, circle, line)
import Svg.Attributes exposing (..)
import Result exposing (withDefault)
import Json.Decode as Decode
import Xml
import Xml.Decode
import Xml.Encode
import Xml.Query exposing (collect, tag, tags, int, float)


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
    , xml : Xml.Value
    , stars : List Star
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" (Location "" 0 0) (Xml.Encode.null) [], Cmd.none )



-- UPDATE


type Msg
    = Refresh
    | ReadLoc String
    | GetLoc
    | NewXml (Result Http.Error String)
    | NewJSON (Result Http.Error Location)


type alias Star =
    { mag : Float, ra : Float, de : Float }


type alias Location =
    { loc : String, lat : Float, lng : Float }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, getStarData )

        NewXml (Ok xmlstring) ->
            ( let
                xml =
                    withDefault Xml.Encode.null <|
                        Xml.Decode.decode xmlstring
              in
                { model
                    | xml = xml
                    , stars = xmlToStars xml
                }
            , Cmd.none
            )

        NewXml (Err err) ->
            ( model, Cmd.none )

        ReadLoc address ->
            ( { model | address = address }, Cmd.none )

        GetLoc ->
            ( model, getLocation model.address )

        NewJSON (Ok location) ->
            ( { model | location = location }, Cmd.none )

        NewJSON (Err err) ->
            ( { model | location = Location "Location not found" 0 0 }, Cmd.none )


getStarData : Cmd Msg
getStarData =
    let
        forCORS =
            "https://cors-anywhere.herokuapp.com/"

        url =
            "http://server1.sky-map.org/getstars.jsp?ra=12&de=45&angle=40&max_stars=1000&max_vmag=8"

        request =
            getString (forCORS ++ url)
    in
        Http.send NewXml request


getLocation : String -> Cmd Msg
getLocation loc =
    let
        url =
            "https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyB4-TN5fQZt0C4ZvX21n4a-8qsPfhYjfF4&address="

        request =
            Http.get (url ++ loc) decodeLocation
    in
        Http.send NewJSON request


decodeLocation : Decode.Decoder Location
decodeLocation =
    let
        res =
            Decode.field "results" << Decode.index 0

        loc =
            res <| Decode.field "formatted_address" Decode.string

        lat =
            res <| Decode.at [ "geometry", "location", "lat" ] Decode.float

        lng =
            res <| Decode.at [ "geometry", "location", "lng" ] Decode.float
    in
        Decode.map3 Location loc lat lng


xmlToStars : Xml.Value -> List Star
xmlToStars xml =
    let
        xmls =
            tags "star" xml
    in
        List.map3 (\m r d -> Star m r d)
            (collect (tag "mag" num) xmls)
            (collect (tag "ra" num) xmls)
            (collect (tag "de" num) xmls)


num : Xml.Value -> Result String Float
num val =
    case int val of
        Ok i ->
            Ok (toFloat i)

        Err _ ->
            float val


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "backgroundColor", "#000" )
            , ( "height", "100%" )
            ]
        ]
        [ div []
            [ input [ placeholder "Naha, Okinawa", onInput ReadLoc ] []
            , button [ onClick GetLoc ] [ text "Get Location" ]
            ]
        , svg
            [ width "740", height "740", viewBox "0 0 740 740" ]
            --            (List.map drawStar model.stars)
            (drawParallels (1)
                ++ drawMeridians (1)
            )
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
                        [ cx "370"
                        , cy <| renorm (cos lambda / k) 2
                        , r <| toString <| sqrt <| (370 ^ 2) * (1 - cos theta ^ 2) / k ^ 2
                        , stroke "white"
                        , fill "none"
                        ]
                        []
                else if lambda == pi / 2 then
                    line [ x1 "0", x2 "740", y1 "370", y2 "370", stroke "white" ] []
                else
                    line [ x1 "0", x2 "0", y1 "0", y2 "0" ] []
    in
        List.map mer <|
            List.map (\n -> pi * (toFloat n) / 24) <|
                List.range 1 23


drawMeridians : Float -> List (Svg.Svg msg)
drawMeridians lambda =
    let
        par phi =
            if abs (sin phi * cos lambda) > 1.0e-8 then
                circle
                    [ cx <| renorm (-1 / (tan phi * cos lambda)) 2
                    , cy <| renorm (-1 * tan lambda) 2
                    , r <| toString <| 370 / (abs (sin phi * cos lambda))
                    , stroke "white"
                    , fill "none"
                    ]
                    []
            else
                line
                    [ y1 "0"
                    , y2 "740"
                    , x1 "370"
                    , x2 "370"
                    , stroke "white"
                    , transform <| "rotate(" ++ toString (phi / pi * 180) ++ " 370 370)"
                    ]
                    []
    in
        List.map par <|
            List.map (\n -> pi * (toFloat n) / 12) <|
                List.range 0 11


renorm : Float -> Float -> String
renorm x xn =
    toString (740 * (1 / 2 + x / xn))


drawStar : Star -> Svg.Svg msg
drawStar { mag, ra, de } =
    circle
        [ cx (renorm (ra - 12) 5)
        , cy (renorm (de - 45) 40)
        , r (toString (8 - mag))
        , fill "white"
        ]
        []
