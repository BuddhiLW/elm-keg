module Route.Blog.Entry_.Slug_ exposing (ActionData, Data, Model, Msg, route)

import Article
import BackendTask exposing (BackendTask)
import Cloudinary
import Data.Author as Author exposing (Author, lookupAuthor)
import Date exposing (Date)
import DateOrDateTime
import Element exposing (..)
import Element.Border as Border
import ElmUiRenderer exposing (customRenderer)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Markdown.Block
import Markdown.Renderer
import MarkdownCodec
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import SiteOld
import StructuredData
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import TailwindMarkdownRenderer
import UnsplashImage
import UrlPath
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { entry : String
    , slug : String
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender
        { data = data
        , head = head
        , pages = pages
        }
        |> RouteBuilder.buildNoState { view = view }


pages : BackendTask FatalError (List RouteParams)
pages =
    Article.blogPostsGlob
        |> BackendTask.map
            (List.map
                (\globData ->
                    { slug = globData.slug
                    , entry = String.fromInt globData.entry
                    }
                )
            )


view : App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view app shared =
    let
        -- Example: Replace this with actual logic to lookup author
        authorName =
            "BuddhiLW"

        author =
            case lookupAuthor authorName of
                Just foundAuthor ->
                    foundAuthor

                Nothing ->
                    -- Fallback to a default author if not found
                    Author.buddhilw

        -- Convert Html.Styled.Html to Element
        htmlToElement : Html.Styled.Html msg -> Element msg
        htmlToElement styledHtml =
            styledHtml
                |> Html.Styled.toUnstyled
                |> Element.html

        -- Convert Html.Html to Element
        -- renderedMarkdown =
        --     app.data.body
        --         |> Markdown.Renderer.render TailwindMarkdownRenderer.renderer
        --         |> Result.withDefault []
        --         |> List.map htmlToElement
        renderedMarkdown =
            app.data.body
                |> Markdown.Renderer.render ElmUiRenderer.customRenderer
                |> Result.withDefault []

        -- |> List.map htmlToElement
    in
    { title = app.data.metadata.title
    , attributes = []
    , body =
        let
            imageWidth =
                300

            imageHeight =
                300

            borderWidth =
                imageWidth // 2
        in
        [ Element.el
            [ -- centerX
              -- centerY
              width fill
            ]
            (Element.column
                [ --Element.centerX
                  -- centerX
                  width fill
                , spacing 30
                , padding 10
                ]
                [ Element.el
                    [ Element.centerX
                    , clip
                    , alignTop
                    , Border.rounded borderWidth
                    ]
                    (Element.image
                        [ width (px imageWidth)
                        , height (px imageHeight)
                        ]
                        { src = Pages.Url.toString app.data.metadata.image
                        , description = app.data.metadata.description
                        }
                    )

                -- , Element.el [] (authorView author app.data |> htmlToElement)
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacingXY 0 5
                    ]
                    renderedMarkdown
                ]
            )
        ]
    }


authorView : Author -> Data -> Html.Styled.Html msg
authorView author app =
    div
        [ css
            [ Tw.flex
            , Tw.mb_16

            --, Tw.flex_shrink_0
            ]
        ]
        [ img
            [ Attr.src (author.avatar |> Pages.Url.toString)
            , css
                [ Tw.rounded_full
                , Tw.h_10
                , Tw.w_10
                ]
            ]
            []
        , div
            [ css [ Tw.ml_3 ]
            ]
            [ div
                [ css
                    []
                ]
                [ p
                    [ css
                        [ Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_color Theme.gray_900
                        ]
                    ]
                    [ span
                        []
                        [ Html.Styled.text author.name ]
                    ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.space_x_1
                    , Tw.text_sm
                    , Tw.text_color Theme.gray_500
                    , Tw.text_color Theme.gray_400
                    ]
                ]
                [ time
                    [ Attr.datetime "2020-03-16"
                    ]
                    [ Html.Styled.text (app.metadata.published |> Date.format "MMMM ddd, yyyy") ]
                ]
            ]
        ]


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    let
        metadata =
            app.data.metadata
    in
    Head.structuredData
        (StructuredData.article
            { title = metadata.title
            , description = metadata.description
            , author = StructuredData.person { name = Author.dillon.name }
            , publisher = StructuredData.person { name = Author.dillon.name }
            , url = SiteOld.canonicalUrl ++ UrlPath.toAbsolute app.path
            , imageUrl = metadata.image
            , datePublished = Date.toIsoString metadata.published
            , mainEntityOfPage =
                StructuredData.softwareSourceCode
                    { codeRepositoryUrl = "https://github.com/dillonkearns/elm-pages"
                    , description = "A statically typed site generator for Elm."
                    , author = "Dillon Kearns"
                    , programmingLanguage = StructuredData.elmLang
                    }
            }
        )
        :: (Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = "elm-pages"
                , image =
                    { url = metadata.image
                    , alt = metadata.description
                    , dimensions = Nothing
                    , mimeType = Nothing
                    }
                , description = metadata.description
                , locale = Nothing
                , title = metadata.title
                }
                |> Seo.article
                    { tags = []
                    , section = Nothing
                    , publishedTime = Just (DateOrDateTime.Date metadata.published)
                    , modifiedTime = Nothing
                    , expirationTime = Nothing
                    }
           )


type alias Data =
    { metadata : ArticleMetadata
    , body : List Markdown.Block.Block
    }


type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    MarkdownCodec.withFrontmatter Data
        frontmatterDecoder
        TailwindMarkdownRenderer.renderer
        ("content/blog/" ++ routeParams.entry ++ "/" ++ routeParams.slug ++ ".md")


type alias ArticleMetadata =
    { title : String
    , description : String
    , published : Date
    , image : Pages.Url.Url
    , draft : Bool
    }


frontmatterDecoder : Decoder ArticleMetadata
frontmatterDecoder =
    Decode.map5 ArticleMetadata
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "published"
            (Decode.string
                |> Decode.andThen
                    (\isoString ->
                        Date.fromIsoString isoString
                            |> Json.Decode.Extra.fromResult
                    )
            )
        )
        -- (Decode.oneOf
        -- [
        (Decode.field "image" imageDecoder)
        -- , Decode.field "unsplash" UnsplashImage.decoder |> Decode.map UnsplashImage.imagePath
        -- ]
        -- )
        (Decode.field "draft" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault False)
        )


imageDecoder : Decode.Decoder Pages.Url.Url
imageDecoder =
    Decode.string
        |> Decode.map
            (\imagePath ->
                let
                    combinedPath =
                        UrlPath.fromString ("/content/blog/" ++ imagePath)
                in
                Pages.Url.fromPath combinedPath
            )



-- imageDecoder : Decode.Decoder Pages.Url.Url
-- imageDecoder =
--     Decode.string
--         |> Decode.map (\cloudinaryAsset -> Cloudinary.url cloudinaryAsset Nothing 800)
