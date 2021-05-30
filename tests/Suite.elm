module Suite exposing (suite)

import Expect exposing (..)
import Test exposing (..)
import Utils exposing (..)


urls : List ( String, String )
urls =
    [ ( "https://web.whatsapp.com/", "https://web.whatsapp.com" )
    , ( "https://www.flickr.com/photos/95869671@N08/", "https://www.flickr.com" )
    , ( "http://www.nandgame.com/", "http://www.nandgame.com" )
    , ( "http://www.nandgame.com/qwerqwer", "http://www.nandgame.com" )
    , ( "outlook.live.com", "outlook.live.com" )
    , ( "outlook.live.com/qwerqewr", "outlook.live.com" )
    ]


assertOptimizeFavicon : ( String, String ) -> Test
assertOptimizeFavicon ( inputUrl, expectUrl ) =
    test ("On url: " ++ inputUrl) <|
        \() ->
            inputUrl
                |> optimizeFavicon
                |> Expect.equal expectUrl


suite : Test
suite =
    describe "Expect the url are optimize to get favicon" <|
        List.map assertOptimizeFavicon urls
