module ElmUiRenderer exposing (customRenderer)

-- import Element.Space as Space
-- import Html.Styled

import Basics exposing (ceiling, floor, round, toFloat)
import Element as E exposing (Attr, Color, Element, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
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
    let
        fS =
            fonts fontBaseSize
    in
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
        , unorderedList =
            \items ->
                E.column
                    [ E.centerX
                    , E.width
                        (E.fill
                            |> E.maximum 700
                        )
                    , Font.size fS.unorderedList
                    ]
                    (items
                        |> List.map
                            (\item ->
                                case item of
                                    Block.ListItem task children ->
                                        let
                                            checkbox =
                                                case task of
                                                    Block.NoTask ->
                                                        E.text "• "

                                                    Block.IncompleteTask ->
                                                        Input.checkbox []
                                                            { checked = False
                                                            , icon =
                                                                \isChecked ->
                                                                    if isChecked then
                                                                        E.text "[✔]"

                                                                    else
                                                                        E.text "[  ]"
                                                            , label = Input.labelLeft [] (E.text "•")
                                                            , onChange = \_ -> Debug.todo "Handle incomplete checkbox change"
                                                            }

                                                    Block.CompletedTask ->
                                                        Input.checkbox []
                                                            { checked = True
                                                            , icon =
                                                                \isChecked ->
                                                                    if isChecked then
                                                                        E.text "[✔]"

                                                                    else
                                                                        E.text "[  ]"
                                                            , label = Input.labelLeft [] (E.text "•")
                                                            , onChange = \_ -> Debug.todo "Handle completed checkbox change"
                                                            }
                                        in
                                        E.row [ E.alignLeft ]
                                            (checkbox :: children)
                            )
                    )
        , orderedList =
            \startingIndex items ->
                E.column []
                    (items
                        |> List.indexedMap
                            (\index itemBlocks ->
                                E.row []
                                    [ E.text (String.fromInt (startingIndex + index) ++ ".")
                                    , E.column [] itemBlocks
                                    ]
                            )
                    )

        -- , orderedList =
        --     \start items ->
        --         E.column
        --             [ E.centerX
        --             , E.padding 10
        --             ]
        --             (List.indexedMap
        --                 (\index item ->
        --                     E.row []
        --                         [ E.text (String.fromInt (start + index) ++ ".")
        --                         , E.column [] item
        --                         ]
        --                 )
        --                 items
        --             )
        -- case item of
        -- Block.ListItem task children ->
        --                 let
        --                     checkbox =
        --                         case task of
        --                             Block.NoTask ->
        --                                 E.text ""
        --                             Block.IncompleteTask ->
        --                                 Input.checkbox
        --                                     [
        --                                      -- Attr.disabled True
        --                                      -- , Attr.checked False
        --                                      -- , Attr.type_ "checkbox"
        --                                     ]
        --                                     []
        -- Block.CompletedTask ->
        --     Html.input
        --         [ Attr.disabled True
        --         , Attr.checked True
        --         , Attr.type_ "checkbox"
        --         ]
        --         []
        --                     in
        --                     Html.li [] (checkbox :: children)
        --         )
        -- )
        -- , unorderedList = myUnorderedList
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
    let
        -- fS: Font Size
        fS =
            fonts fontBaseSize
    in
    case level of
        Block.H1 ->
            E.el [ E.centerX ]
                (E.paragraph
                    [ Font.size fS.header.h1
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
                        E.paragraph
                            [ Font.color <| colors.greenAnchor
                            , Font.size fS.header.h2
                            , E.paddingXY 15 3
                            ]
                            [ E.text "#"
                            , E.el
                                [ Font.color <| colors.black2
                                , Font.size fS.header.h2
                                , E.paddingXY 10 0
                                ]
                                (E.text rawText)
                            ]
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
    let
        fS =
            fonts fontBaseSize
    in
    E.el
        [ E.centerX
        , E.width
            (E.fill
                |> E.maximum 800
            )
        , Font.size fS.paragraph
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
    let
        fS =
            fonts fontBaseSize
    in
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
                    , Font.size fS.code
                    ]
                    [ E.text body ]

        -- Fallback: normal code block styling
        _ ->
            E.el
                [ Background.color (E.rgb255 245 245 245)
                , Border.rounded 4
                , E.paddingXY 40 20
                , E.spacingXY 0 30
                , E.centerX
                ]
                (E.text body)



-- myUnorderedList : List (Block.ListItem (Element msg)) -> Element msg
-- myUnorderedList items =
--     E.html
--         (Html.ul []
--             (items
--                 |> List.map
--                     (\item ->
--                         case item of
--                             Block.ListItem task children ->
--                                 let
--                                     checkbox =
--                                         case task of
--                                             Block.NoTask ->
--                                                 Html.text ""
--                                             Block.IncompleteTask ->
--                                                 Html.input
--                                                     [ Attr.disabled True
--                                                     , Attr.checked False
--                                                     , Attr.type_ "checkbox"
--                                                     ]
--                                                     []
--                                             Block.CompletedTask ->
--                                                 Html.input
--                                                     [ Attr.disabled True
--                                                     , Attr.checked True
--                                                     , Attr.type_ "checkbox"
--                                                     ]
--                                                     []
--                                 in
--                                 Html.li [] (checkbox :: List.map (E.layout []) children)
--                     )
--             )
--         )


myText :
    String
    -> Element msg
myText text =
    E.el [ E.centerX ] (E.text text)


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


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


fontBaseSize : Int
fontBaseSize =
    12



-- fbs := Font Base Size


fonts :
    Int
    ->
        { baseSize : Int
        , paragraph : Int
        , code : Int
        , header :
            { h1 : Int
            , h2 : Int
            , h3 : Int
            , h4 : Int
            , h5 : Int
            }
        , orderedList : Int
        , unorderedList : Int
        }
fonts fbs =
    { baseSize = fbs
    , paragraph = round (toFloat fbs * 1.5)
    , code = round (toFloat fbs * 1.8)
    , header =
        { h1 = round (toFloat fbs * 4.5)
        , h2 = round (toFloat fbs * 3.2)
        , h3 = round (toFloat fbs * 1.2)
        , h4 = round (toFloat fbs * 1.1)
        , h5 = fbs * 1
        }
    , orderedList = round (toFloat fbs * 1.5)
    , unorderedList = round (toFloat fbs * 1.5)
    }
