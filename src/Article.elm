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
import Json.Decode as Decode
import Pages.Url exposing (Url)
import Route
import UnsplashImage


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
    { -- filePath : String
      entry : Int
    , slug : String
    }


blogPostsGlob :
    BackendTask.BackendTask
        error
        (List Keg)
blogPostsGlob =
    -- File.bodyWithoutFrontmatter
    Glob.succeed Keg
        -- |> Glob.captureFilePath
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
                            { -- filePath = keg.filePath
                              entry = keg.entry
                            , slug = keg.slug -- Dynamically use the wildcard capture
                            }
                        )
                    |> List.sortBy .entry
            )


{-| Returns a list of `(Route, ArticleMetadata)` for each `.md` file.

Instead of parsing frontmatter, this creates placeholder data:

  - title: `"Placeholder Title"`
  - description: `"Placeholder Description"`
  - published: always `2024-01-01T00:00:00Z` (or fallback date)
  - image: always `Cloudinary.url "placeholder" Nothing 800`
  - draft: `False`

We then sort descending by the `published` date,
and produce a `BackendTask` with the same error type
that you had in your template (`File.FileReadError Decode.Error`).

-}
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
                        (\{ entry, slug } ->
                            let
                                route =
                                    Route.Blog__Entry___Slug_ { entry = String.fromInt entry, slug = slug }

                                metadata =
                                    { title = "Placeholder Title"
                                    , description = "Placeholder Description"
                                    , published =
                                        case Date.fromIsoString "2024-01-01" of
                                            Ok d ->
                                                d

                                            Err _ ->
                                                case Date.fromIsoString "2023-01-01T00:00:00Z" of
                                                    Ok fallback ->
                                                        fallback

                                                    Err _ ->
                                                        Debug.todo "Failed to parse fallback date"
                                    , image =
                                        Cloudinary.url "placeholder" Nothing 800
                                    , draft = False
                                    }
                            in
                            BackendTask.succeed ( route, metadata )
                        )
            )
        |> BackendTask.resolve
        |> BackendTask.map
            (List.filterMap
                (\( route, metadata ) ->
                    if metadata.draft then
                        Nothing

                    else
                        Just ( route, metadata )
                )
            )
        |> BackendTask.map
            (List.sortBy
                (\( route, metadata ) ->
                    -(Date.toRataDie metadata.published)
                )
            )
