{
module Parser where

import Scanner
import TypeNames
import CodeGen

}

%name mycehParser
%tokentype { Token }
%error { parseError }
%token

    return                  { TReturn _ }
    begin                   { TBegin _ }
    end                     { TEnd _ }
    var                     { TVar _ }
    const                   { TConst _ }
    procedure               { TProcedure _ }
    while                   { TWhile _ }
    do                      { TDo _ }
    if                      { TIf _ }
    then                    { TThen _ }
    call                    { TCall _ }
    "odd"                   { TOdd _ }
    "#"                     { TNotEqual _ }
    ":="                    { TAssign _ }
    "<"                     { TLess _ }
    ">"                     { TGreater _ }
    "<="                    { TLessOrEqual _ }
    "=>"                    { TGreaterOrEqual _ }
    ";"                     { TSemicolon _ }
    ","                     { TComma _ }
    "+"                     { TPlus _ }
    "-"                     { TMinus _ }
    "="                     { TEqual _ }
    "*"                     { TStar _ }
    "/"                     { TDivide _ }
    "."                     { TDot _ }
    "("                     { TOpenParenthesis _ }
    ")"                     { TCloseParenthesis _ }
    identifier              { TIdentifier _ $$ }
    number                  { TNumber _ $$ }

%attributetype { Attrs }


%%


-- http://en.wikipedia.org/wiki/PL/0

--
-- ---=== Program ===---
--
-- program = Block "." .

Program:
    Block "."                       { $1 }

--
-- ---=== Block ===---
--
-- block = [ "const" ident "=" number {"," ident "=" number} ";"]
--         [ "var" ident {"," ident} ";"]
--         { "procedure" ident ";" block ";" } statement .
 
Block :: { Block }
Block:
    BlockConsts 
    BlockVars 
    BlockProcedure
    BlockStatement                  { Block {  blockConsts     = $1, 
                                               blockVars       = $2, 
                                               blockProcs      = $3, 
                                               blockStatement  = $4 } }

-- Constants

BlockConsts :: { [(String, Integer)] }
BlockConsts:
    const Consts ";"                { $2 }
    |                               { [] }

Consts :: { [(String, Integer)] }
Consts: 
    identifier "=" number           { [($1, $3)] }
    | identifier "=" number "," Consts 
                                    { ($1, $3) : $5 }

-- Vars

BlockVars:
    var Vars ";"                    { $2 }
    |                               { [] }

Vars:
    identifier                      { [$1] }
    | identifier "," Vars           { $1 : $3 }


-- Procedure

BlockProcedure:
    procedure identifier ";" Block ";" 
                                    { [Procedure $2 $4] }

BlockStatement :: { Statement }
BlockStatement: 
    Statement                       { $1 }
    



--
-- ---=== Statement ===---
--
-- statement = [ ident ":=" expression | "call" ident |
--             "begin" statement {";" statement } "end" |
--             "if" condition "then" statement |
--             "while" condition "do" statement ].

Statement :: { Statement }
Statement:
    identifier ":=" Expression      { Assign  $1 $3  }
    | call identifier               { Call  $2 }
    | begin Statement end           { Begin [$2] }
    | begin Statement ";" Statement end
                                    { Begin ([$2] ++ [$4]) }
    | if Condition then Statement   { If    $2 $4 }
    | while Condition do Statement  { While $2 $4 }

--
-- ---=== Condition ===---
--
--
-- condition = "odd" expression |
--             expression ("="|"#"|"<"|"<="|">"|">=") expression .

Condition :: { Condition }
Condition:
    "odd" Expression                { Odd $2 }
    | Expression "=" Expression     { Eq $1 $3 }
    | Expression "#" Expression     { Ne $1 $3 }
    | Expression "<" Expression     { Lt $1 $3 }
    | Expression "<=" Expression    { Le $1 $3 }
    | Expression ">" Expression     { Gt $1 $3 }
    | Expression "=>" Expression    { Ge $1 $3 }


--
-- ---=== Expression ===---
--
-- expression = [ "+"|"-"] term { ("+"|"-") term}.

Expression :: { Expression }
Expression:
    "+" Term                        { $2 }
    | "-" Term                      { $2 }
    | "+" Term Expression           { Plus $2 $3 }
    | "-" Term Expression           { Minus $2 $3 }



--
-- ---=== Term ===---
--
--
-- term = factor {("*"|"/") factor}.

--Term :: { Expression }
Term:
    Factor "*" Factor               { Multiply  $1 $3 }
    | Factor "/" Factor             { Divide    $1 $3 }



--
-- ---=== Factor ===---
--
--
-- factor = ident | number | "(" expression ")".

-- Factor :: { Expression }
Factor:
    identifier                      { Identifier $1 }
    | number                        { Number $1 }
    | "(" Expression ")"            { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
