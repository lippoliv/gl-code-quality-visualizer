module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as D

main = 
    Browser.element {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

type alias Model = 
    {
        hover: Bool,
        files: List File
    }

init : () -> (Model, Cmd Event)
init _ =
    (
        Model False [],
        Cmd.none
    )

type Event = 
    Pick |
    DragEnter |
    DragLeave |
    GotFiles File (List File)

update : Event -> Model -> (Model, Cmd Event)
update msg model =
    case msg of
        Pick -> 
            (
                model,
                Select.files ["application/json"] GotFiles
            )

        DragEnter -> 
            (
                {
                    model | hover = True
                },
                Cmd.none
            )
        
        DragLeave ->
            (
                {
                    model | hover = False
                },
                Cmd.none
            )
        
        GotFiles file files ->
            (
                {
                    model | files = file :: files,
                    hover = False
                },
                Cmd.none
            )

subscriptions : Model -> Sub Event
subscriptions model = Sub.none

view : Model -> Html Event
view model =
    div
        [
            listenTo "dragenter" (D.succeed DragEnter),
            listenTo "dragover" (D.succeed DragEnter),
            listenTo "dragleave" (D.succeed DragLeave),
            listenTo "drop" openFile
        ]
        [
            button 
                [onClick Pick]
                [text "choose"],
            span
                []
                [text (Debug.toString model)]
        ]

listenTo : String -> D.Decoder msg -> Attribute msg
listenTo event decoder =
    preventDefaultOn event (D.map dragAndDrop decoder)

dragAndDrop : msg -> (msg, Bool)
dragAndDrop msg =
    (msg, True)

openFile : D.Decoder Event
openFile =
    D.at ["dataTransfer", "files"] (D.oneOrMore GotFiles File.decoder)