module Route.Blog exposing (ActionData, Data, Model, Msg, route)

import Article
import BackendTask exposing (BackendTask)
import Css exposing (alignSelf)
import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Pages.Url
import Route exposing (Route)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import UrlPath
import View exposing (View)


type alias Msg =
    ()


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState
            { view = view
            }


data : BackendTask FatalError Data
data =
    Article.allMetadata
        |> BackendTask.allowFatal


type alias Data =
    List ( Route, Article.ArticleMetadata )


type alias ActionData =
    {}


type alias RouteParams =
    {}


type alias Model =
    {}


view :
    App Data ActionData {}
    -> Shared.Model
    -> View msg
view app shared =
    { title = "elm-pages blog"
    , attributes = []
    , body =
        [ column
            [ width fill
            , Background.color (rgb255 243 244 246)
            , paddingXY 16 20
            ]
            [ el
                [ Font.size 36
                , Font.bold
                , Font.color (rgb255 17 24 39)
                ]
                (text "Blog")
            , el
                [ Font.size 30
                , Font.color (rgb255 107 114 128)
                , centerX
                , Element.paddingXY 0 60
                ]
                (text blogDescription)
            , Element.wrappedRow
                [ -- Element.centerX
                  -- Element.spaceEvenly
                  Element.spacingXY 60 60
                , width fill
                , height fill
                ]
                (List.map blogCard app.data)
            ]
        ]
    }


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = blogDescription
        , locale = Nothing
        , title = "elm-pages blog"
        }
        |> Seo.website



-- link : List (Element.Attribute msg) -> { url : String, label : Element msg } -> Element msg
-- link attrs { url, label } =
--     Element.link
--         attrs
--         { url = url, label = label }


blogCard : ( Route, Article.ArticleMetadata ) -> Element msg
blogCard ( route_, info ) =
    Element.link []
        { url = Route.toString route_
        , label =
            Element.el
                [--  Element.width (Element.fillPortion 1)
                 -- , Element.height Element.fill
                ]
                (Element.column
                    [ spacing 12 ]
                    [ el
                        [ Region.heading 1
                        , alignLeft
                        , Font.size 28
                        ]
                        (text info.title)
                    , el
                        [ Region.heading 3
                        , Font.center
                        , Font.size 14
                        ]
                        (text info.description)
                    , el
                        [ Region.heading 3
                        , Font.center
                        , Font.size 12
                        ]
                        (text (info.published |> Date.format "MMMM ddd, yyyy"))
                    ]
                )

        -- (Element.el
        --         []
        --         (text info.title)
        --     )
        -- , Element.el
        --     []
        --     (text info.title)
        -- Element.html
        --     (Html.div []
        --         [ Html.h2 [] [ Html.text info.title ]
        --         , Html.p [] [ Html.text info.description ]
        --         , Html.time [] [ Html.text (info.published |> Date.format "MMMM ddd, yyyy") ]
        --         ]
        --     )
        }


blogDescription : String
blogDescription =
    "The latest elm-pages news and articles."
