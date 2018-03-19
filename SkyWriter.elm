module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (getString)
import Svg exposing (svg, circle, line, mask, rect)
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
            [ width "800", height "800", viewBox "0 0 800 800" ]
            (circleMask
                :: circle [ cx "400", cy "400", r "400", stroke "white", strokeWidth "5" ] []
                :: (drawParallels (1)
                        ++ drawMeridians (1)
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
                        [ cx "400"
                        , cy <| renorm (cos lambda / k) 2
                        , r <| toString <| sqrt <| (400 ^ 2) * (1 - cos theta ^ 2) / k ^ 2
                        , stroke "white"
                        , fill "none"
                        , strokeWidth "0.5"
                        , Svg.Attributes.mask "url(#hole)"
                        ]
                        []
                else if lambda == pi / 2 then
                    line
                        [ x1 "0"
                        , x2 "800"
                        , y1 "400"
                        , y2 "400"
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
                    , r <| toString <| 400 / (abs (sin phi * cos lambda))
                    , stroke "white"
                    , fill "none"
                    , strokeWidth "0.5"
                    , Svg.Attributes.mask "url(#hole)"
                    ]
                    []
            else
                line
                    [ y1 "0"
                    , y2 "800"
                    , x1 "400"
                    , x2 "400"
                    , stroke "white"
                    , strokeWidth "0.5"
                    , transform <| "rotate(" ++ toString (phi / pi * 180) ++ " 400 400)"
                    ]
                    []
    in
        List.map (\n -> par <| pi * (toFloat n) / 12) <|
            List.range 0 11


renorm : Float -> Float -> String
renorm x xn =
    toString (800 * (1 / 2 + x / xn))


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
            , circle [ r "400", cx "400", cy "400", fill "white" ] []
            ]
        ]
