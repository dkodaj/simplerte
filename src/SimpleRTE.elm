port module SimpleRTE exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import ParseHtml.Parse as ParseHtml

main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { originalHtml : String
    , updatedHtml : String
    }


type Msg
    = Bold  
    | Edited String
    | FormatBlock String
    | InsertLink
    | Italic
    | Unlink


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model sampleContent sampleContent, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    edited Edited


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let cucu = Debug.log "model" model in
    case msg of
        Bold ->
             ( model, format "bold")
            
        Edited html ->
            ( { model | updatedHtml = html }, Cmd.none )

        FormatBlock nodeType ->
            ( model, formatBlock nodeType )
            
        InsertLink ->
            ( model, insertLink True )
            
        Italic ->
            ( model, format "italic" )
            
        Unlink ->
            ( model, format "unlink" )
            

view : Model -> Browser.Document Msg
view model =
    { title = "Simple RTF in Elm"
    , body = body model
    }


--=== HELPERS


body : Model -> List (Html Msg)
body model =
    [ div
        [ Attr.style "width" "700px" 
        , Attr.style "margin" "10px 0 0 50px"
        ]
        [ toolbar
        , editable model.originalHtml
        , div
            [ Attr.style "margin-top" "30px"
            ]
            [ Html.b 
                [ class "has-text-info" 
                , Attr.style "font-size" "1.25em"
                ] 
                [ text "InnerHTML:" ] 
            ]
        , div
            [ Attr.style "margin-top" "15px"
            , class "html"
            ]
            [ text model.updatedHtml ]
        , div
            [ Attr.style "margin-top" "20px" ]
            [ Html.b 
                [ class "has-text-info" 
                , Attr.style "font-size" "1.25em"
                ] 
                [ text "Rendered from Elm (not editable):" ] 
            ]
        , div
            [ Attr.style "margin-top" "5px"
            , class "styled"
            ]
            [ ParseHtml.do model.updatedHtml ]
        , div
            [ Attr.style "margin" "15px auto" 
            , Attr.style "font-family" "monospace"
            , Attr.style "font-size" "1.25em"
            , Attr.style "font-weight" "bold"
            , Attr.style "text-align" "center"
            ]
            [ text "< "
            , Html.a 
                [ Attr.href "https://github.com/dkodaj/simplerte" ]
                [ text "code" ] 
            , text " >"
            ]
        ]
    ]


editable : String -> Html Msg
editable innerHtml =
    div
        [ Attr.id "editable"
        , Attr.contenteditable True
        , class "styled"        
        ]
        [ ParseHtml.do innerHtml ]


editorButton : String -> String -> Msg -> Html Msg
editorButton iconClass title msg =
    Html.span
        [ class "button"
        , Attr.title title
        , onClick msg
        , Attr.style "margin" "0 5px"
        ]
        [ icon iconClass ]


-- Font Awesome icon
icon : String -> Html a
icon faClass =
    Html.span
        [ class "icon" ]
        [ Html.i
            [ class faClass ]
            []
        ]

sampleContent : String
sampleContent =
    "<h1>Edit this</h1><p>Or this. Lorem ipsum dolor sit amet, consectetur adipisicing elit.<p>"


toolbar : Html Msg
toolbar =
    div
        [ class "editorBarStyle" 
        ]        
        [ editorButton "fas fa-bold" "Bold" Bold
        , editorButton "fas fa-italic" "Italic" Italic
        , editorButton "fas fa-link" "Link" InsertLink
        , editorButton "fas fa-unlink" "Unlink" Unlink
        , editorButton "fas fa-heading" "Heading" (FormatBlock "<h1>")
        , editorButton "fas fa-paragraph" "Plain text paragraph" (FormatBlock "<div>")
        ]

--== PORTS ==--

--from JavaScript to Elm

port edited : (String -> msg) -> Sub msg


--from Elm to JavaScript

port format : String -> Cmd msg

port formatBlock : String -> Cmd msg

port insertLink : Bool -> Cmd msg



