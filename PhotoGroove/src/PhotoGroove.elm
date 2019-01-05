module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

type ThumbnailSize = Small | Medium | Large
type alias Photo = { url : String }
type alias Model =
    { photos: List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }
type Msg
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ] 
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src ( urlPrefix ++ "large/" ++ model.selectedUrl)]
            []
        ]

viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ("selected", selectedUrl == thumbnail.url) ]
        , onClick ( ClickedPhoto thumbnail.url )
        ]
        []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"

initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Small
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

randomPhotoPicker : Random.Generator Int
randomPhotoPicker = Random.int 0 (Array.length photoArray - 1)

getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo -> photo.url
        Nothing -> ""

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectedIndex index -> ( { model | selectedUrl = getPhotoUrl index }, Cmd.none)
        ClickedPhoto url -> ( { model | selectedUrl = url }, Cmd.none )
        ClickedSize size -> ( { model | chosenSize = size }, Cmd.none )
        ClickedSurpriseMe -> ( model, Random.generate GotSelectedIndex randomPhotoPicker )

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }