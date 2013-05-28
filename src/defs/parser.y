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
    end                     { TEnd _ }
    return                  { TReturn _ }
    goto                    { TGoto _ }
    if                      { TIf _ }
    then                    { TThen _ }
    var                     { TVar _ }
    not                     { TNot _ }
    and                     { TAnd _ }
    "=<"                    { TLessOrEqual _ }
    ";"                     { TSemicolon _ }
    "("                     { TLeftParen _ }
    ")"                     { TRightParen _ }
    ","                     { TComma _ }
    ":"                     { TColon _ }
    "+"                     { TPlus _ }
    "-"                     { TMinus _ }
    "="                     { TEqual _ }
    "*"                     { TStar _ }
    "#"                     { TNumSign _ }
    id                      { TId _ $$ }
    num                     { TNum _ $$ }

%attributetype { Attrs }


%%

Program:
    Program Funcdef ";"             { Program $1 $2 }
    |                               { PEmpty }

Funcdef:
    id "(" Params ")" Stmts end      { Funcdef $1 $3 $5 }
    
Params:
    id                              { ParamsOne $1 }
    | id "," Params                 { Params $1 $3 }
    |                               { PaEmpty }

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
    | if Expr then Stmts end        { StmtIf $2 $4 }
    | var id "=" Expr               { StmtDecl $2 $4 }
    | Term                          { StmtTerm $1 }
    | LExpr "=" Expr                { StmtLExpr $1 $3 }
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
    | id "(" ")"                    { TermCall $1 }
    | id "(" Args ")"               { TermCallArgs $1 $3 }
    | num                           { TermNum $1 }
    
Args:
    id                              { Arg $1 }
    | id "," Args                   { Args $1 $3 }

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
