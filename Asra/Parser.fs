module Parser

open Ast
open FParsec

let createParser (dataParser: Parser<'data, unit>) (logger: (string -> unit) option) =
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        match logger with
            | Some write ->
                fun stream ->
                    write (sprintf "%A: Entering %s" stream.Position label)
                    let reply = p stream
                    write (sprintf "%A: Leaving %s (%A)" stream.Position label reply.Status)
                    reply
            | None -> p

    let (expressionParser: Parser<Expression<'data>, unit>, expressionParserRef) = createParserForwardedToRef ()

    let (functionExpressionParser: Parser<Expression<'data>, unit>, functionExpressionParserRef) = createParserForwardedToRef ()

    let leftParensParser = skipChar '(' .>> spaces

    let rightParensParser = spaces .>> skipChar ')'

    let floatLiteralParser: Parser<Literal, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> (fun f -> 
        match f.IsInteger with
            | true -> int64 f.String |> Int
            | false -> float f.String |> Float)
    
    let intLiteralParser: Parser<Literal, unit> = pint64 |>> Int

    let mapString (str: string) (res: 'a): Parser<'a, unit> = (skipString str >>% res) <?> str
    
    let boolLiteralParser: Parser<Literal, unit> = ((mapString "false" false <|> mapString "true" true) |>> Bool) <?> "Bool literal"
    
    let unitLiteralParser: Parser<Literal, unit> = mapString "()" Unit <?> "Unit literal"
    
    let unescapedCharParser: Parser<char, unit> = satisfy (fun ch -> ch <> '\\' && ch <> '\"')
    
    let escapedCharParser: Parser<char, unit> = 
        [ 
        "\\\"",'\"'
        "\\\\",'\\'
        "\\/",'/' 
        "\\b",'\b'
        "\\f",'\f'
        "\\n",'\n'
        "\\r",'\r'
        "\\t",'\t'
        ] 
        |> List.map (fun (toMatch, result) -> skipString toMatch >>% result)
        |> choice
    
    let stringCharParser: Parser<char, unit> = unescapedCharParser <|> escapedCharParser
    
    let stringLiteralParser: Parser<Literal, unit> = (skipChar '"' >>. (manyChars stringCharParser) .>> skipChar '"' |>> String) <?> "String literal"
    
    let literalExpressionParser: Parser<Expression<'data>, unit> = 
        dataParser .>>.
        choiceL [
            floatLiteralParser
            intLiteralParser
            stringLiteralParser
            boolLiteralParser
            unitLiteralParser ] "Literal" |>> (fun (data, lit) -> Literal (lit, data)) <?> "Literal expression" <!> "Literal expression parser"

    let groupExpressionParser: Parser<Expression<'data>, unit> = (dataParser .>> leftParensParser .>>. expressionParser .>> rightParensParser |>> fun (data, expr) -> Group (expr, data)) <?> "Group expression" <!> "Group expression parser"

    let isSeparator (c: char) = System.Char.IsWhiteSpace c || c = ')' || c = '(' || c = ':'
    
    let isIdentifierStart (c: char) = (not (isDigit c)) && not (isSeparator c)
    
    let isIdentifierContinue (c: char) = not (isSeparator c)
    
    let identifierOptions = IdentifierOptions(isAsciiIdStart = isIdentifierStart, isAsciiIdContinue = isIdentifierContinue)
    
    let identifierParser: Parser<string, unit> = identifier identifierOptions
    
    let nameParser = identifierParser >>=? (fun s ->
        match s with
            | "->" -> fail ""
            | "=" -> fail ""
            | "fun" -> fail ""
            | "let" -> fail ""
            | "in" -> fail ""
            | "if" -> fail ""
            | "then" -> fail ""
            | "end" -> fail ""
            | "else" -> fail ""
            | "import!" -> fail ""
            | _ -> preturn s) <?> "Identifier"

    let keyword (kw: string) = skipString kw <?> kw <!> (sprintf "%s parser" kw)

    let isSimpleWhitespace (c: char) = c = ' ' || c = '\t'

    let ws1 = skipMany1Satisfy isSimpleWhitespace <?> "Whitespace"

    let ws = skipManySatisfy isSimpleWhitespace <?> "Whitespace"

    let (typeDeclarationParser: Parser<TypeDeclaration, unit>, typeDeclarationParserRef) = createParserForwardedToRef ()

    let (simpleTypeDeclarationParser: Parser<TypeDeclaration, unit>, simpleTypeDeclarationParserRef) = createParserForwardedToRef ()

    let namedTypeParser = nameParser |>> Name

    let genericTypeParser = skipChar ''' >>? nameParser |>> Generic

    let groupedTypeParser = between leftParensParser rightParensParser typeDeclarationParser

    let appliedTypeParser = nameParser .>>? spaces1 .>>.? sepBy1 simpleTypeDeclarationParser spaces1 |>> Parameterized

    let functionTypeParser = simpleTypeDeclarationParser .>>? spaces .>>? keyword "->" .>> spaces .>>. typeDeclarationParser |>> Function

    simpleTypeDeclarationParserRef := choiceL [
        groupedTypeParser
        genericTypeParser
        namedTypeParser
    ] "Type"

    typeDeclarationParserRef := choiceL [
        groupedTypeParser
        functionTypeParser
        genericTypeParser
        namedTypeParser
        appliedTypeParser
    ] "Type"

    let typeAnnotatedParser = nameParser .>>? spaces .>>? skipChar ':' .>>? spaces .>>.? typeDeclarationParser |>> TypeAnnotated

    let declarationParser = (between leftParensParser rightParensParser typeAnnotatedParser) <|> (nameParser |>> Named) <?> "Declaration" <!> "Declaration parser"

    let variableExpressionParser: Parser<Expression<'data>, unit> = dataParser .>>. nameParser |>> (fun (data, name) -> Variable (name, data)) <?> "Variable expression" <!> "Variable expression parser"

    let lambdaExpressionParser: Parser<Expression<'data>, unit> = dataParser .>> keyword "fun" .>>? spaces1 .>>.? (sepEndBy1 declarationParser spaces1) .>>? keyword "->" .>> spaces1 .>>. expressionParser |>> (fun ((data, parameters), expr) -> Lambda (parameters, expr, data)) <?> "Lambda expression" <!> "Lambda expression parser"

    let endParser = keyword "end"
    
    let modifierParser = mapString "rec" Recursive |> opt

    let bindingParser: Parser<LetBinding<'data>, unit> = modifierParser .>> spaces .>>. declarationParser .>> spaces1 .>> skipChar '=' .>> spaces1 .>>. expressionParser |>> (fun ((modifier, decl), expr) -> LetBinding (modifier, decl, expr))  <?> "Let binding" <!> "Let binding parser"

    let letParser: Parser<Expression<'data>, unit> = dataParser .>> keyword "let" .>>? spaces1 .>>. (sepEndBy1 bindingParser spaces1) .>> keyword "in" .>> spaces1 .>>. expressionParser .>> spaces .>> endParser |>> (fun ((data, bindings), expr) -> Let (bindings, expr, data)) <?> "Let expression" <!> "Let expression parser"

    let importParser: Parser<Expression<'data>, unit> = dataParser .>> keyword "import!" .>> spaces1 .>>. nameParser |>> (fun (data, name) -> Import (name, data)) <?> "Import expression" <!> "Import expression parser"

    let ifParser: Parser<Expression<'data>, unit> = dataParser .>> keyword "if" .>> spaces1 .>>. functionExpressionParser .>> spaces1 .>> keyword "then" .>> spaces1 .>>. expressionParser .>> spaces1 .>> keyword "else" .>> spaces1 .>>. expressionParser .>> spaces1 .>> endParser |>> (fun (((data, condExpr), ifBodyExpr), elseBodyExpr) -> If (condExpr, ifBodyExpr, elseBodyExpr, data)) <?> "If expression" <!> "If expression parser"

    //TODO: Remove restriction that function calls must happen on one line?
    let functionCallParser: Parser<Expression<'data>, unit> = dataParser .>>. functionExpressionParser .>>? ws1 .>>.? attempt (many1Till (functionExpressionParser .>>? ws) (followedByString "end" <|> followedByNewline <|> followedByString ")" <|> followedBy eof)) |>> (fun ((data, funExpr), argExprs) -> FunctionCall (funExpr, argExprs, data)) <?> "Function call" <!> "Function call expression parser"

    expressionParserRef := choiceL [
        letParser
        lambdaExpressionParser
        importParser
        ifParser
        literalExpressionParser
        functionCallParser
        groupExpressionParser
        variableExpressionParser ] "Expression" <!> "Expression parser"

    functionExpressionParserRef := choiceL [
        literalExpressionParser
        variableExpressionParser
        groupExpressionParser ] "Expression" <!> "Function expression parser"

    let programParser = spaces >>. expressionParser .>> spaces

    let parse (name: string) (code: string) = match CharParsers.runParserOnString programParser () name code with
                                                | Success (res, _, _) -> Result.Ok res
                                                | Failure (es, _, _) -> Result.Error es

    parse

let testLogger (s: string) = System.Diagnostics.Debug.WriteLine s

let testParser = createParser (preturn ()) (Some testLogger) "Test"