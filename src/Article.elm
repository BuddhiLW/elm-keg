module Article exposing
    ( ArticleMetadata
    , BlogPost
    , allMetadata
    , blogPostsGlob
    )

import BackendTask
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cloudinary
import Date exposing (Date)
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Pages.Url exposing (Url)
import Route
import UnsplashImage
import UrlPath exposing (UrlPath)


{-| Minimal record for blog posts captured by `blogPostsGlob`.
-}
type alias BlogPost =
    { filePath : String
    , slug : String
    }


{-| A type to hold your final metadata.
-}
type alias ArticleMetadata =
    { title : String
    , description : String
    , published : Date
    , image : Url
    , draft : Bool
    }


{-| Matches `.md` files in `content/blog/` and returns their paths + slugs.
-}
type alias Keg =
    { filePath : String
    , entry : Int
    , slug : String
    }


blogPostsGlob :
    BackendTask.BackendTask
        error
        (List Keg)
blogPostsGlob =
    -- File.bodyWithoutFrontmatter
    Glob.succeed Keg
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toBackendTask
        |> BackendTask.map
            (\kegs ->
                kegs
                    |> List.map
                        (\keg ->
                            { filePath = keg.filePath
                            , entry = keg.entry
                            , slug = keg.slug -- Dynamically use the wildcard capture
                            }
                        )
                    |> List.sortBy .entry
            )


allMetadata :
    BackendTask.BackendTask
        { fatal : FatalError, recoverable : File.FileReadError Decode.Error }
        (List ( Route.Route, ArticleMetadata ))
allMetadata =
    blogPostsGlob
        |> BackendTask.map
            (\paths ->
                paths
                    |> List.map
                        (\{ filePath, slug, entry } ->
                            --                             let
                            --                                 route =
                            --                                     Route.Blog__Entry___Slug_ { entry = String.fromInt entry, slug = slug }
                            BackendTask.map2 Tuple.pair
                                (BackendTask.succeed <| Route.Blog__Entry___Slug_ { entry = String.fromInt entry, slug = slug })
                                (File.onlyFrontmatter frontmatterDecoder filePath)
                        )
            )
        |> BackendTask.resolve
        |> BackendTask.map
            (\articles ->
                articles
                    |> List.filterMap
                        (\( route, metadata ) ->
                            if metadata.draft then
                                Nothing

                            else
                                Just ( route, metadata )
                        )
            )
        |> BackendTask.map
            (List.sortBy
                (\( route, metadata ) -> -(Date.toRataDie metadata.published))
            )


frontmatterDecoder : Decoder ArticleMetadata
frontmatterDecoder =
    Decode.map5 ArticleMetadata
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "published"
            (Decode.string
                |> Decode.andThen
                    (\isoString ->
                        case Date.fromIsoString isoString of
                            Ok date ->
                                Decode.succeed date

                            Err error ->
                                Decode.fail error
                    )
            )
        )
        -- (Decode.oneOf
        (Decode.field "image" imageDecoder)
        -- [
        -- , Decode.field "image" imageDecoder
        -- , Decode.field "unsplash" UnsplashImage.decoder |> Decode.map UnsplashImage.imagePath
        -- ]
        -- )
        (Decode.field "draft" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault False)
        )



-- imageDecoder : Decoder Url
-- imageDecoder =
--     Decode.string
--         |> Decode.map (\cloudinaryAsset -> Cloudinary.url cloudinaryAsset Nothing 800)


imageDecoder : Decoder Url
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
