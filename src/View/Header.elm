module View.Header exposing (..)

-- import Css exposing (spaceBetween)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Elm exposing (hex)
import UrlPath exposing (UrlPath)


view :
    msg
    -> UrlPath
    -> Element msg -- Int -> UrlPath -> Element msg
view toggleMobileMenuMsg currentPath =
    Element.row
        [ Background.color (rgb 50 120 50)
        , paddingXY 35 0
        , width fill
        , height (px 64)
        , Border.color (rgb 0 0 0)
        ]
        [ logo
        , row
            [ spacing 16
            , alignRight
            , centerY
            ]
            [ headerLinkElement currentPath "showcase" "Showcase"
            , headerLinkElement currentPath "blog" "Knowledge"
            , headerLinkElement currentPath "docs" "Docs"
            ]
        ]


logo : Element msg
logo =
    Element.link
        [ Font.bold
        , Font.size 24
        , Font.color (rgb255 255 50 80) -- Neutral dark from Tailwind
        ]
        { url = "/"
        , label = text "BLW"
        }


headerLinkElement : UrlPath -> String -> String -> Element msg
headerLinkElement currentPath linkTo name =
    let
        isCurrentPath =
            List.head currentPath == Just linkTo

        linkStyle =
            if isCurrentPath then
                [ Font.color (rgb255 10 200 100)
                , Font.extraBold
                , Font.glow (rgb255 30 255 80) 6
                ]

            else
                [ Font.color (rgb 0 0 0) ]
    in
    Element.link
        [ Border.rounded 0
        , padding 10

        -- , spacingBetween 16
        -- , Border.solid (px 0)
        , Font.size 16
        ]
        { url = "/" ++ linkTo
        , label =
            el linkStyle (text name)
        }
