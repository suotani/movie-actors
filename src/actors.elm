module Actors exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing(..)
import Http
import Json.Decode exposing (Decoder, Error,maybe,  int, string, list, map2, map3, map4, field)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


-- MODEL
type alias Movie =
  { title : Maybe String
  , id : Maybe Int 
  }

type alias Actor = 
  { imagePath : Maybe String
  , name : Maybe String
  , id : Maybe Int
  -- , movies : List Movie
  }

type alias Model =
  { actors : List Actor
  , viewStatus : ViewStatus
  , query : String
  , message : String
  }

type ViewStatus
  = Loading
   | Loaded
  

init : () -> (Model, Cmd Msg)
init _ = (Model [] Loading "jon" "", getActors "jon")

-- UPDATE
type Msg
  = GotActors (Result Http.Error (List Actor))
  | SetQuery String
  | GetActors

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotActors result ->
      case result of
        Ok actors ->
          ({model | actors = actors, viewStatus = Loaded }, Cmd.none)
        
        Err e ->
          ({model | actors = [], viewStatus = Loaded, message = errorToString e }, Cmd.none)
  
    SetQuery query ->
      ( { model | query = query}, Cmd.none)
    
    GetActors ->
      (model, getActors model.query)

-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ div [class "search"]
        [ input [type_ "text", onInput SetQuery] []
        , button [type_ "button", onClick GetActors] [text "Search"]
        ]
    , case model.viewStatus of
        Loading ->
          p [] [text "Loading..."]
        
        Loaded ->
          if model.actors == [] then
            div [] 
              [ p [] [text "no actors load"]
              , p [] [text model.message]
              ]
          else
            div [] (List.map (\u -> actorView u) model.actors)
    ]

actorView : Actor -> Html Msg
actorView actor =
  case actor.name of
    Just name ->
      p []
        [ text 
          <| "name: " ++ name 
          ++ "(" ++ (Maybe.withDefault "" actor.imagePath) ++ ") "
        ]
    
    Nothing ->
      p [] []


-- HTTP

getActors : String -> Cmd Msg
getActors query =
  Http.get
    { url = "https://api.themoviedb.org/3/search/person?api_key=118f2e5c4f9f1d17942a3271a18b5ea2&query=" ++ query
    , expect = Http.expectJson GotActors actorsDecoder
    }

actorsDecoder : Decoder (List Actor)
actorsDecoder =
  field "results" (list actorDecoder)

actorDecoder : Decoder Actor
actorDecoder =
  map3 Actor
        (field "profile_path" (maybe string))
        (field "name" (maybe string))
        (field "id" (maybe int))
--         (field "known_for" (list movieDecoder))

-- movieDecoder : Decoder Movie
-- movieDecoder =
--   map2 Movie
--          (field "title" (maybe string))
--          (field "id" (maybe int))

type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage