module TableOfContents exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.File
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import FatalError exposing (FatalError)
import List.Extra
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.Parser



-- Backend Task


backendTask :
    BackendTask FatalError (List { file | filePath : String, slug : String })
    -> BackendTask FatalError (TableOfContents Data)
backendTask docFiles =
    docFiles
        |> BackendTask.map
            (\sections ->
                sections
                    |> List.map
                        (\section ->
                            BackendTask.File.bodyWithoutFrontmatter
                                section.filePath
                                |> BackendTask.allowFatal
                                |> BackendTask.andThen (headingsDecoder section.slug)
                        )
            )
        |> BackendTask.resolve


headingsDecoder : String -> String -> BackendTask FatalError (Entry Data)
headingsDecoder slug rawBody =
    rawBody
        |> Markdown.Parser.parse
        |> Result.mapError (\_ -> FatalError.fromString "Markdown parsing error")
        |> Result.map gatherHeadings
        |> Result.andThen (nameAndTopLevel slug >> Result.mapError FatalError.fromString)
        |> BackendTask.fromResult


nameAndTopLevel :
    String
    -> List ( Block.HeadingLevel, List Block.Inline )
    -> Result String (Entry Data)
nameAndTopLevel slug headings =
    let
        h1 : Maybe (List Block.Inline)
        h1 =
            List.Extra.findMap
                (\( level, inlines ) ->
                    case level of
                        Block.H1 ->
                            Just inlines

                        _ ->
                            Nothing
                )
                headings

        h2s : List (List Block.Inline)
        h2s =
            List.filterMap
                (\( level, inlines ) ->
                    case level of
                        Block.H2 ->
                            Just inlines

                        _ ->
                            Nothing
                )
                headings
    in
    case h1 of
        Just justH1 ->
            Ok
                (Entry
                    { anchorId = slug
                    , name = styledToString justH1
                    , level = 1
                    }
                    (h2s
                        |> List.map (toData 2)
                        |> List.map (\l2Data -> Entry l2Data [])
                    )
                )

        _ ->
            Err ("Missing H1 heading for " ++ slug)


toData : Int -> List Block.Inline -> { anchorId : String, name : String, level : Int }
toData level styledList =
    { anchorId = styledToString styledList |> rawTextToId
    , name = styledToString styledList
    , level = level
    }


type alias TableOfContents data =
    List (Entry data)


type Entry data
    = Entry data (List (Entry data))


addChild : data -> Entry data -> Entry data
addChild childToAdd (Entry parent children) =
    Entry parent (children ++ [ Entry childToAdd [] ])


type alias Data =
    { anchorId : String, name : String, level : Int }


buildToc : List Block -> TableOfContents Data
buildToc blocks =
    let
        headings =
            gatherHeadings blocks
    in
    headings
        |> List.foldl
            (\( currentLevel, styledList ) ( previousLevel, entries ) ->
                let
                    childData =
                        { anchorId = styledToString styledList |> rawTextToId
                        , name = styledToString styledList
                        , level = Block.headingLevelToInt currentLevel
                        }
                in
                case entries of
                    [] ->
                        ( Block.headingLevelToInt currentLevel
                        , Entry childData [] :: entries
                        )

                    latest :: previous ->
                        if previousLevel < Block.headingLevelToInt currentLevel then
                            ( Block.headingLevelToInt currentLevel
                            , (latest |> addChild childData)
                                :: previous
                            )

                        else
                            ( Block.headingLevelToInt currentLevel
                            , Entry childData [] :: entries
                            )
            )
            ( 6, [] )
        |> Tuple.second
        |> List.reverse


gatherHeadings : List Block -> List ( Block.HeadingLevel, List Inline )
gatherHeadings blocks =
    List.filterMap
        (\block ->
            case block of
                Block.Heading level content ->
                    Just ( level, content )

                _ ->
                    Nothing
        )
        blocks


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


styledToString : List Inline -> String
styledToString inlines =
    inlines
        |> Block.extractInlineText


surround : Bool -> Bool -> List (Element msg) -> Element msg
surround showMobileMenu onDocsPage children =
    el
        [ Background.color (rgb255 0 0 0)
        , Border.rounded 8
        , Border.color (rgb255 229 231 235)
        , padding 16
        , spacing 16
        , width fill
        ]
        (column [] children)


view : Bool -> Bool -> Maybe String -> TableOfContents Data -> Element msg
view showMobileMenu onDocsPage current toc =
    surround showMobileMenu
        onDocsPage
        (toc |> List.map (level1Entry current))


level1Entry : Maybe String -> Entry Data -> Element msg
level1Entry current (Entry data children) =
    column
        [ spacing 8 ]
        [ item (current == Just data.anchorId) ("/docs/" ++ data.anchorId) data.name
        , column
            [ spacing 4 ]
            (children
                |> List.map (level2Entry data.anchorId)
            )
        ]


item : Bool -> String -> String -> Element msg
item isCurrent href body =
    link
        [ Font.size 16
        , Font.bold
        , if isCurrent then
            Font.color (rgb255 59 130 246)
            -- Blue for current item

          else
            Font.color (rgb255 107 114 128)

        -- Gray for others
        ]
        { url = href
        , label = text body
        }


level2Entry : String -> Entry Data -> Element msg
level2Entry parentPath (Entry data _) =
    el
        [ padding 4
        , Font.color (rgb255 156 163 175)
        ]
        (item False ("/docs/" ++ parentPath ++ "#" ++ data.anchorId) data.name)
