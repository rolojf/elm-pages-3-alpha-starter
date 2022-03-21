module MdConverter exposing (renderea)

import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (defaultHtmlRenderer)


renderea : String -> List (Html ())
renderea mdStr =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"

        mdAResult elMD =
            elMD
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen
                    (\ast ->
                        Markdown.Renderer.render
                            myRenderer
                            ast
                    )
    in
    case
        mdAResult mdStr
    of
        Ok rendereado ->
            rendereado

        Err errors ->
            [ text errors ]


myRenderer : Markdown.Renderer.Renderer (Html ())
myRenderer =
    let
        defaultOne =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultOne | html = procesaHtml }


procesaHtml : Markdown.Html.Renderer (List (Html ()) -> Html ())
procesaHtml =
    let
        showDiv : Maybe String -> Maybe String -> List (Html ()) -> Html ()
        showDiv clase identidad hijos =
            div
                [ case clase of
                    Just claseDef ->
                        class claseDef

                    Nothing ->
                        class ""
                , case identidad of
                    Just soyYoMero ->
                        Attr.id soyYoMero

                    Nothing ->
                        Attr.id ""
                ]
                hijos

        showSpan : String -> List (Html ()) -> Html ()
        showSpan clase children =
            Html.span
                [ class clase ]
                children
    in
    Markdown.Html.oneOf
        [ Markdown.Html.tag "div"
            showDiv
            |> Markdown.Html.withOptionalAttribute "class"
            |> Markdown.Html.withOptionalAttribute "id"
        , Markdown.Html.tag "span"
            showSpan
            |> Markdown.Html.withAttribute "class"
        ]
