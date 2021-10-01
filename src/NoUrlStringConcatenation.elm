module NoUrlStringConcatenation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Review.Rule exposing (Error, Rule)


{-| Reports expressions concatenating strings with forward slash

    config =
        [ NoUrlStringConcatenation.rule
        ]


## Fail

    a =
        `baseUrl ++ "/items/" ++ itemId`


## Success

    a =
        `Url.Builder.crossOrigin baseUrl [ "items", "itemId" ]`


## When (not) to enable this rule

This rule is useful when there are many expressions that build URL using string concatenation.
This rule is not useful when there are plenty of expressions concatenating non-url strings that contain forward slash.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoUrlStringConcatenation
```

-}
rule : Rule
rule =
    Review.Rule.newModuleRuleSchema "NoUrlStringConcatenation" ()
        |> Review.Rule.withSimpleExpressionVisitor expressionVisitor
        |> Review.Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.OperatorApplication "++" _ node1 node2 ->
            [ errorCondition node1 node
            , errorCondition node2 node
            ]
                |> List.concat

        _ ->
            []


errorCondition : Node Expression -> Node Expression -> List (Error {})
errorCondition node rootNode =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.Literal s ->
            if String.startsWith "/" s || String.endsWith "/" s then
                [ Review.Rule.error
                    { message = "This looks like you are building URL by concatenating strings. Use `absolute`, `relative` or `crossOrigin` functions from Url.Builder module instead."
                    , details = [ "Most of the time a combination of ++ and string that starts or ends with a forward slash denote to building a URL. If you are building non-url string that contains forward slash, you can still use Url.Builder.relative or String.join." ]
                    }
                    (Elm.Syntax.Node.range rootNode)
                ]

            else
                []

        _ ->
            []
