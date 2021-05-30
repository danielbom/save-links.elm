module Utils exposing (..)


optimizeFavicon : String -> String
optimizeFavicon url =
    let
        parts =
            String.split "/" url
    in
    case List.head parts of
        Just head ->
            if head == "http:" || head == "https:" then
                parts
                    |> List.take 3
                    |> String.join "/"

            else
                head

        Nothing ->
            ""
