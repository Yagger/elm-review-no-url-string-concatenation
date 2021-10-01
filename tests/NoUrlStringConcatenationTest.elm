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
a = baseUrl ++ "/intros/" ++ introId"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors [ expectedError "\"/intros/\" ++ introId" ]
        , test "should not report an error for expression that concatenates strings without forward slash" <|
            \() ->
                """module A exposing (..)
a = introName ++ ":" ++ introId"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors []
        ]


expectedError : String -> Review.Test.ExpectedError
expectedError under =
    Review.Test.error
        { message = "This looks like you are building URL by concatenating strings. Use `absolute`, `relative` or `crossOrigin` functions from Url.Builder module instead."
        , details = [ "Most of the time a combination of ++ and string with forward slash denote to building a URL. If you are building non-url string that contains forward slash, then please use String.join." ]
        , under = under
        }
