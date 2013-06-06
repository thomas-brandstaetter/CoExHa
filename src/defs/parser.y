{
module Parser where

import Scanner
import TypeNames
import CodeGen

}

%name myce
%tokentype { Token }
%error { parseError }
%token

    return                  { TReturn _ }
    if                      { TIf _ }
    "=="                    { TEqual _ }
    "<"                     { TLess _ }
    ">"                     { TGreater _ }
    "<="                    { TLessOrEqual _ }
    ">="                    { TGreaterOrEqual _ }
    ";"                     { TSemicolon _ }
    "("                     { TLeftParen _ }
    ")"                     { TRightParen _ }
    "{"                     { TLeftCurlyBracket _ }
    "}"                     { TRightCurlyBracket _ }
    ","                     { TComma _ }
    "+"                     { TPlus _ }
    "-"                     { TMinus _ }
    "="                     { TEqual _ }
    "*"                     { TStar _ }
    "/"                     { TDivide _ }
    identifier              { TId _ %% }
    number                  { TNum _ %% }

%attributetype { Attrs }


%%

Program:
    Program Funcdef ";"             { Program $1 $2 }
    |                               { PEmpty }

Funcdef:
    id "(" Params ")" Block end     { Funcdef $1 $3 $5 }
    
Params:
    id                              { ParamsOne $1 }
    | id "," Params                 { Params $1 $3 }
    |                               { PaEmpty }

Block:
    "{" Stmts "}"                   { Stmts $2 }


Stmts:
    Stmt ";" Stmts                  { Stmts $1 $3 }
    |                               { SEmpty } 

Labeldef:
    id ":"                          { Labeldef $1 }
    |                               { LEmpty }

Stmt:
    return                          { StmtReturnNull }
    | return Term                   { StmtReturn $2 }
    | goto id                       { StmtGoto $2 }
    | if "(" Expr ")" Block         { StmtIf $3 $5 }
    | id "=" Expr                   { StmtDecl $2 $4 }
    | Term                          { StmtTerm $1 }
    |                               { StmtEmpty }

Expr:
    Unary                           { ExprUnary $1 }
    | Term "+" Term                 { ExprPlus $1 $3 }
    | Term "*" Term                 { ExprMult $1 $3 }
    | Term and Term                 { ExprAnd $1 $3 }


LExpr:
    id                              { LExpr $1 } 

Term:
    "(" Expr ")"                    { TermExpr $2 }
    | id                            { TermId $1 }
    | id "(" Params ")"             { TermCall $1 $3 }
    | num                           { TermNum $1 }
    
Unary:
    not Unary                       { UnaryNot $2 } 
    | "-" Unary                     { UnaryMinus $2 }
    | Term                          { UnaryTerm $1 }
--

{

parseError :: [Token] -> a
parseError _ = error "Parse error"



run = do
    inString <- getContents
    let parseTree = myce (alexScanTokens inString)
    putStrLn ("parseTree: " ++ show(parseTree))
    codegenResult <- codegen parseTree
    -- print "codegenResult" ++ codegenResult
    print "done"

}
