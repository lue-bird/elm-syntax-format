module ElmSyntaxParserLenient exposing (module_)

import Elm.Parser.Comments
import Elm.Parser.Declarations
import Elm.Parser.Imports
import Elm.Parser.Layout
import Elm.Parser.Modules
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import ParserFast
import ParserWithComments
import Rope


module_ : ParserFast.Parser Elm.Syntax.File.File
module_ =
    ParserFast.map4
        (\moduleDefinition moduleComments imports declarationsResult ->
            { moduleDefinition = moduleDefinition.syntax
            , imports = imports.syntax
            , declarations = declarationsResult.syntax
            , comments =
                moduleDefinition.comments
                    |> Rope.prependTo moduleComments
                    |> Rope.prependTo imports.comments
                    |> Rope.prependTo declarationsResult.comments
                    |> Rope.toList
            }
        )
        (Elm.Parser.Layout.layoutStrictFollowedByWithComments
            Elm.Parser.Modules.moduleDefinition
        )
        (Elm.Parser.Layout.layoutStrictFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                )
                Elm.Parser.Comments.documentationComment
                Elm.Parser.Layout.layoutStrict
                Rope.empty
            )
        )
        (ParserWithComments.many Elm.Parser.Imports.importDefinition)
        declarations


declarations : ParserFast.Parser (ParserWithComments.WithComments (List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)))
declarations =
    ParserWithComments.many
        (Elm.Parser.Layout.moduleLevelIndentationFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> Rope.prependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                Elm.Parser.Declarations.declaration
                Elm.Parser.Layout.optimisticLayout
            )
        )
