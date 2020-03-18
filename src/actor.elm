module Actor exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing(..)
import Http
import Json.Decode exposing (Decoder, Error,maybe,  int, string, list, map2, field)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


-- MODEL

type alias Actor = 
  { imagePath : Maybe String
  , name : Maybe String
  -- , movies : List Movie
  }

type alias Model =
  { actor : Maybe Actor
  , viewStatus : ViewStatus
  , id : String
  , message : String
  }

type ViewStatus
  = Loading
   | Loaded
  

init : String -> (Model, Cmd Msg)
init id = (Model Nothing Loading id "", getActor id)

-- UPDATE
type Msg
  = GotActor (Result Http.Error Actor)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotActor result ->
      case result of
        Ok actor ->
          ({model | actor = Just actor, viewStatus = Loaded }, Cmd.none)
        
        Err e ->
          ({model | actor = Nothing, viewStatus = Loaded, message = errorToString e }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ case model.viewStatus of
        Loading ->
          p [] [text "Loading..."]
        
        Loaded ->
          if model.actor == Nothing then
            div [] 
              [ p [] [text "no actors load"]
              , p [] [text model.message]
              ]
          else
            div [] [actorView model.actor]
    ]

actorView : Maybe Actor -> Html Msg
actorView actor =
  case actor of
    Just a ->
      p []
        [ h1 [] [text <| Maybe.withDefault "" a.name]
        ]
    
    Nothing ->
      p [] []


-- HTTP

getActor : String -> Cmd Msg
getActor id =
  Http.get
    { url = "https://api.themoviedb.org/3/person/" ++ id ++ "?api_key=118f2e5c4f9f1d17942a3271a18b5ea2"
    , expect = Http.expectJson GotActor actorDecoder
    }

actorDecoder : Decoder Actor
actorDecoder =
  map2 Actor
        (field "profile_path" (maybe string))
        (field "name" (maybe string))
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