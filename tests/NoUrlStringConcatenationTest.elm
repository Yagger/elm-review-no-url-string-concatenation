module NoUrlStringConcatenationTest exposing (all)

import NoUrlStringConcatenation exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUrlStringConcatenation"
        [ test "should report an error for expression that concatenates forward slash" <|
            \() ->
                """module A exposing (..)
a = "/" ++ itemPath ++ "/" ++ itemId ++ "/"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedError "\"/\" ++ itemPath ++ \"/\" ++ itemId ++ \"/\""
                        , expectedError "\"/\" ++ itemId ++ \"/\""
                        , expectedError "itemId ++ \"/\""
                        ]
        , test "should report an error for expression that concatenates strings that starts or ends with a forward slash" <|
            \() ->
                """module A exposing (..)
a = baseUrl ++ "/items/" ++ itemId"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors [ expectedError "\"/items/\" ++ itemId" ]
        , test "should not report an error for expression that concatenates strings that do not start or end with a forward slash" <|
            \() ->
                """module A exposing (..)
a = aaa ++ ":" ++ bbb ++ "ddd/ccc"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors []
        ]


expectedError : String -> Review.Test.ExpectedError
expectedError under =
    Review.Test.error
        { message = "This looks like you are building URL by concatenating strings. Use `absolute`, `relative` or `crossOrigin` functions from Url.Builder module instead."
        , details = [ "Most of the time a combination of ++ and string that starts or ends with a forward slash denote to building a URL. If you are building non-url string that contains forward slash, you can use String.join." ]
        , under = under
        }
