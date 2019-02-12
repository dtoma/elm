module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)

-----------
-- Types --
-----------

type ThumbnailSize = Small | Medium | Large

type alias Photo =
    { url : String
    , size: Int
    , title : String
    }

type alias Model =
    { status : Status
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

type Msg
    = ClickedPhoto String
    | GotRandomPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotPhotos (Result Http.Error (List Photo))

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

-------------
-- Globals --
-------------

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

-----------
-- Views --
-----------

view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl -> viewLoaded photos selectedUrl model.chosenSize
            Loading                   -> []
            Errored errorMessage      -> [text ("Error: " ++ errorMessage)]

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    -- Slight deviation from the book - keep the stylesheet here so we don't have to update
    -- index.html manually every time we rebuild it
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "http://elm-in-action.com/styles.css" ] []
    , h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src ( urlPrefix ++ "large/" ++ selectedUrl)]
        []
    ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ("selected", selectedUrl == thumb.url) ]
        , onClick ( ClickedPhoto thumb.url )
        ]
        []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]

-----------
-- Model --
-----------

initialModel : Model
initialModel =
    { status = Loading
    , selectedUrl = "1.jpeg"
    , chosenSize = Small
    }

-------------
-- Helpers --
-------------

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _      -> Loaded photos url
        Loading              -> status
        Errored errorMessage -> status

initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send GotPhotos

photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

------------
-- Update --
------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo -> ({ model | status = selectUrl photo.url model.status }, Cmd.none)
        ClickedPhoto url     -> ({ model | status = selectUrl url model.status }, Cmd.none)
        ClickedSize size     -> ({ model | chosenSize = size }, Cmd.none)
        ClickedSurpriseMe    ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto
                        (Random.uniform firstPhoto otherPhotos)
                    )
                Loaded [] _                          -> (model, Cmd.none)
                Loading                              -> (model, Cmd.none)
                Errored errorMessage                 -> (model, Cmd.none)
        GotPhotos (Ok photos) ->
            case photos of
                first :: rest -> ({model | status = Loaded photos first.url}, Cmd.none)
                [] -> ({model | status = Errored "No photos found."}, Cmd.none)
        GotPhotos (Err httpError) ->
            ({model | status = Errored "Server error!"}, Cmd.none)

----------
-- Main --
----------

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }