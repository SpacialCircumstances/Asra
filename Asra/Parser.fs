module Parser

open Ast
open FParsec
open FParsec.CharParsers

let createParser (dataParser: Parser<'data, unit>) =
    let (expressionParser: Parser<Expression<'data>, unit>, expressionParserRef) = createParserForwardedToRef ()

    let (functionExpressionParser: Parser<Expression<'data>, unit>, functionExpressionParserRef) = createParserForwardedToRef ()

    let floatLiteralParser: Parser<Literal, unit> = numberLiteral (NumberLiteralOptions.DefaultFloat) "Float literal" |>> fun f -> 
        match f.IsInteger with
            | true -> int64 f.String |> Int
            | false -> float f.String |> Float
    
    let intLiteralParser: Parser<Literal, unit> = pint64 |>> Int
    
    let unitLiteralParser: Parser<Literal, unit> = skipString "()" |>> fun _ -> Unit
    
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
    
    let stringLiteralParser: Parser<Literal, unit> = skipChar '"' >>. (manyChars stringCharParser) .>> skipChar '"' |>> String
    
    let literalExpressionParser: Parser<Expression<'data>, unit> = 
        dataParser .>>.
        choiceL [ 
            intLiteralParser
            floatLiteralParser
            stringLiteralParser
            unitLiteralParser ] "Literal" |>> fun (data, lit) -> Literal (lit, data)

    let groupExpressionParser: Parser<Expression<'data>, unit> = dataParser .>> skipChar '(' .>>? spaces .>>. expressionParser .>> spaces .>> skipChar ')' |>> fun (data, expr) -> Group (expr, data)

    let isSeparator (c: char) = System.Char.IsWhiteSpace c || c = ')' || c = '('
    
    let isIdentifierStart (c: char) = (not (isDigit c)) && not (isSeparator c)
    
    let isIdentifierContinue (c: char) = not (isSeparator c)
    
    let identifierOptions = IdentifierOptions(isAsciiIdStart = isIdentifierStart, isAsciiIdContinue = isIdentifierContinue)
    
    let identifierParser: Parser<string, unit> = identifier identifierOptions
    
    let nameParser = identifierParser >>= (fun s ->
        match s with
            | "->" -> fail ""
            | "=" -> fail ""
            | "fun" -> fail ""
            | "let" -> fail ""
            | "in" -> fail ""
            | _ -> preturn s)

    let declarationParser = nameParser |>> Named

    let variableExpressionParser: Parser<Expression<'data>, unit> = dataParser .>>. nameParser |>> fun (data, name) -> Variable (name, data)

    let lambdaExpressionParser: Parser<Expression<'data>, unit> = dataParser .>> spaces .>>? skipString "fun" .>>? spaces1 .>>. (sepBy1 declarationParser spaces1) .>> skipString "->" .>>. expressionParser |>> fun ((data, parameters), expr) -> Lambda (parameters, expr, data)

    let endParser = skipString "end" 

    let bindingParser: Parser<LetBinding<'data>, unit> = declarationParser .>> spaces .>> skipChar '=' .>> spaces .>>. expressionParser |>> LetBinding

    let letParser: Parser<Expression<'data>, unit> = dataParser .>> skipString "let" .>>? spaces1 .>>. (sepBy1 bindingParser spaces1) .>> spaces .>> skipString "in" .>>. expressionParser .>> endParser |>> fun ((data, bindings), expr) -> Let (bindings, expr, data)

    let importParser: Parser<Expression<'data>, unit> = dataParser .>> skipString "@import" .>> spaces .>>. nameParser |>> fun (data, name) -> Import (name, data)

    let ifParser: Parser<Expression<'data>, unit> = dataParser .>> skipString "if" .>> spaces1 .>>. functionExpressionParser .>> skipString "then" .>> spaces1 .>>. expressionParser .>> spaces1 .>> skipString "else" .>> spaces1 .>>. expressionParser .>> spaces1 .>> endParser |>> fun (((data, condExpr), ifBodyExpr), elseBodyExpr) -> If (condExpr, ifBodyExpr, elseBodyExpr, data)

    expressionParserRef := spaces >>. choiceL [
        letParser
        lambdaExpressionParser
        importParser
        ifParser
        literalExpressionParser
        groupExpressionParser
        variableExpressionParser ] "Expression" .>> spaces

    functionExpressionParserRef := spaces >>. choiceL [
        variableExpressionParser
        literalExpressionParser
        groupExpressionParser ] "Expression" .>> spaces

    expressionParser