module Data.Author exposing (Author, all, buddhilw, decoder, dillon, lookupAuthor)

import Cloudinary
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Pages.Url exposing (Url)


type alias Author =
    { name : String
    , avatar : Url
    , bio : String
    }


all : List Author
all =
    [ dillon
    ]


dillon : Author
dillon =
    { name = "Dillon Kearns"
    , avatar = Cloudinary.url "v1602899672/elm-radio/dillon-profile_n2lqst.jpg" Nothing 140
    , bio = "Elm developer and educator. Founder of Incremental Elm Consulting."
    }


buddhilw : Author
buddhilw =
    { name = "Pedro Gomes"
    , avatar = Cloudinary.url "v1602899672/elm-radio/dillon-profile_n2lqst.jpg" Nothing 140
    , bio = "A guy."
    }


decoder : Decoder Author
decoder =
    Decode.string
        |> Decode.andThen
            (\lookupName ->
                case List.Extra.find (\currentAuthor -> currentAuthor.name == lookupName) all of
                    Just author ->
                        Decode.succeed author

                    Nothing ->
                        Decode.fail ("Couldn't find author with name " ++ lookupName ++ ". Options are " ++ String.join ", " (List.map .name all))
            )


lookupAuthor : String -> Maybe Author
lookupAuthor name =
    List.Extra.find (\author -> author.name == name) all
