module Cli exposing (run)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Cli.Validate
import Elm
import Elm.Annotation
import Elm.Case
import Gen.DataSource
import Gen.Effect
import Gen.Html
import Gen.Platform.Sub
import Gen.Server.Request
import Gen.Server.Response
import Gen.View
import Pages.Generate exposing (Type(..))
import Pages.Script as Script exposing (Script)


type alias CliOptions =
    { moduleName : String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "module"
                        |> Option.validate (Cli.Validate.regex moduleNameRegex)
                    )
            )


moduleNameRegex : String
moduleNameRegex =
    "([A-Z][a-zA-Z0-9_]*)(\\.([A-Z][a-zA-Z_0-9_]*))*"


run : Script
run =
    Script.withCliOptions program
        (\cliOptions ->
            let
                file : Elm.File
                file =
                    createFile (cliOptions.moduleName |> String.split ".")
            in
            Script.writeFile
                { path = "app/" ++ file.path
                , body = file.contents
                }
         --Script.log "Hello!"
        )


createFile : List String -> Elm.File
createFile moduleName =
    Pages.Generate.serverRender
        { moduleName = moduleName
        , action =
            ( Alias (Elm.Annotation.record [])
            , \routeParams ->
                Gen.Server.Request.succeed
                    (Gen.DataSource.succeed
                        (Gen.Server.Response.render
                            (Elm.record [])
                        )
                    )
            )
        , data =
            ( Alias (Elm.Annotation.record [])
            , \routeParams ->
                Gen.Server.Request.succeed
                    (Gen.DataSource.succeed
                        (Gen.Server.Response.render
                            (Elm.record [])
                        )
                    )
            )
        , head = \app -> Elm.list []
        }
        --|> Pages.Generate.buildNoState
        --    { view =
        --        \_ _ _ ->
        --            Gen.View.make_.view
        --                { title = moduleName |> String.join "." |> Elm.string
        --                , body = Elm.list [ Gen.Html.text "Here is your generated page!!!" ]
        --                }
        --    }
        |> Pages.Generate.buildWithLocalState
            { view =
                \maybeUrl sharedModel model app ->
                    Gen.View.make_.view
                        { title = moduleName |> String.join "." |> Elm.string
                        , body = Elm.list [ Gen.Html.text "Here is your generated page!!!" ]
                        }
            , update =
                \pageUrl sharedModel app msg model ->
                    Elm.Case.custom msg
                        (Elm.Annotation.named [] "Msg")
                        [ Elm.Case.branch0 "NoOp"
                            (Elm.tuple model
                                (Gen.Effect.none
                                    |> Elm.withType effectType
                                )
                            )
                        ]
            , init =
                \pageUrl sharedModel app ->
                    Elm.tuple (Elm.record [])
                        (Gen.Effect.none
                            |> Elm.withType effectType
                        )
            , subscriptions =
                \maybePageUrl routeParams path sharedModel model ->
                    Gen.Platform.Sub.none
            , model =
                Alias (Elm.Annotation.record [])
            , msg =
                Custom [ Elm.variant "NoOp" ]
            }


effectType : Elm.Annotation.Annotation
effectType =
    Elm.Annotation.namedWith [ "Effect" ] "Effect" [ Elm.Annotation.var "msg" ]
