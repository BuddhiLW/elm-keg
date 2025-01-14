module ElmUiRenderer exposing (customRenderer)

-- import Element.Space as Space
-- import Html.Styled

import Basics exposing (ceiling, floor, round)
import Element as E exposing (Attr, Color, Element, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Markdown.Block as Block
import Markdown.Html as MHtml
import Markdown.Parser as MParser
import Markdown.Renderer as Renderer exposing (Renderer)
import Markdown.Renderer.ElmUi as MKRender exposing (renderer)
import Parser
import Parser.Advanced
import String


{-|

    Our custom renderer. We start from `renderer` in `Markdown.Renderer.ElmUi`
    and selectively override the fields we want to customize.

-}
customRenderer : Renderer (Element msg)
customRenderer =
    { renderer
        | -- Override the thematic break to show hearts:
          -- thematicBreak =
          --   E.el [ E.centerX ] (E.text "🖤🖤🖤")
          -- Example: override the heading function
          heading = myHeading

        -- Example: override paragraph
        , paragraph = myParagraph

        -- Example: override codeBlock
        , codeBlock = myCodeBlock
        , text = myText

        -- ...and so on for any other fields you wish to replace
    }


{-|

    Example custom heading (instead of the default from `Markdown.Renderer.ElmUi`).

-}
myHeading :
    { level : Block.HeadingLevel
    , rawText : String
    , children : List (Element msg)
    }
    -> Element msg
myHeading { level, rawText, children } =
    case level of
        Block.H1 ->
            E.el [ E.centerX ]
                (E.paragraph
                    [ Font.size 42
                    , E.spacing 3
                    , E.centerX
                    , Font.bold
                    , E.paddingXY 0 30
                    ]
                    children
                )

        Block.H2 ->
            E.el
                [ E.paddingXY 20 20
                , E.centerX
                ]
                (E.link
                    [ Font.size 28
                    , Font.semiBold
                    , Font.color colors.black2
                    , E.paddingXY 10 10
                    , Region.heading 2
                    , Region.navigation
                    , Border.rounded 15
                    , Border.shadow { blur = 4, color = colors.red, offset = ( 0.1, 1 ), size = 1.2 }
                    , E.htmlAttribute <| Attr.id (rawTextToId rawText)
                    , E.htmlAttribute <| Attr.attribute "name" (rawTextToId rawText)
                    ]
                    { url = "#" ++ rawTextToId rawText
                    , label =
                        E.row []
                            ([ E.el
                                [ E.paddingXY 10 0
                                , Font.color <| colors.greenAnchor
                                , Font.size (round (28.0 * 1.4))
                                ]
                                (E.text "#")
                             ]
                                ++ children
                            )
                    }
                )

        Block.H3 ->
            E.column
                [ Font.size 24
                , Font.bold
                , E.spacing 8
                , E.padding 8
                , E.centerX
                ]
                children

        Block.H4 ->
            E.column
                [ Font.size 20
                , Font.bold
                , E.spacing 8
                , E.padding 8
                , E.centerX
                ]
                children

        Block.H5 ->
            E.column
                [ Font.size 18
                , Font.bold
                , E.spacing 8
                , E.padding 8
                , E.centerX
                ]
                children

        Block.H6 ->
            E.column
                [ Font.size 16
                , Font.bold
                , E.spacing 8
                , E.centerX
                ]
                children


{-|

    Example custom paragraph.

-}
myParagraph : List (Element msg) -> Element msg
myParagraph children =
    E.el
        [ E.centerX
        , E.width
            (E.fill
                |> E.maximum 800
            )
        ]
        (E.column
            [ E.paddingXY 30 3
            ]
            [ E.paragraph
                [ Font.wordSpacing 0
                , E.alignLeft
                ]
                children
            ]
        )


{-|

    Example code block.

-}
myCodeBlock : { body : String, language : Maybe String } -> Element msg
myCodeBlock { body, language } =
    case language of
        Just "quote" ->
            E.el
                [ E.paddingXY 40 25
                , E.centerX
                ]
            <|
                E.paragraph
                    [ E.padding 10
                    , E.spacing 8
                    , Border.widthEach { top = 0, bottom = 5, right = 0, left = 14 }
                    , E.width
                        (E.fill
                            |> E.maximum 800
                        )
                    , Border.color <| colors.red
                    , Background.color <| colors.black
                    , Font.color <| colors.white
                    ]
                    [ E.text body ]

        -- Fallback: normal code block styling
        _ ->
            E.el
                [ Background.color (E.rgb255 245 245 245)
                , Border.rounded 4
                , E.padding 8
                ]
                (E.text body)


myText :
    String
    -> Element msg
myText text =
    E.el [ E.centerX ] (E.text text)


colors :
    { green : Color
    , greenAnchor : Color
    , red : Color
    , black : Color
    , black2 : Color
    , white : Color
    }
colors =
    { green = rgb255 0 255 0
    , greenAnchor = rgb255 10 180 120
    , red = rgb255 255 0 0
    , black = rgb255 0 0 0
    , black2 = rgb255 30 30 30
    , white = rgb255 255 255 255
    }


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower
