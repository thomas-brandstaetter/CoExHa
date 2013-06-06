{
    module Scanner where
}

%wrapper "posn"


@number                 = [0-9]+
@identifier             = [a-zA-Z_][0-9a-zA-Z_]*



tokens :-

    $white+                 ;
    "return"                { \p s -> TReturn p }
    "if"                    { \p s -> TIf p }
    "=="                    { \p s -> TEqual p }
    "<"                     { \p s -> TLess p }
    ">"                     { \p s -> TGreater p }
    "<="                    { \p s -> TLessOrEqual p }
    ">="                    { \p s -> TGreaterOrEqual p }
    ";"                     { \p s -> TSemicolon p }
    "("                     { \p s -> TLeftParen p }
    ")"                     { \p s -> TRightParen p }
    "{"                     { \p s -> TLeftCurlyBracket p }
    "}"                     { \p s -> TRightCurlyBracket p }
    ","                     { \p s -> TComma p }
    "+"                     { \p s -> TPlus p }
    "-"                     { \p s -> TMinus p }
    "="                     { \p s -> TEqual p }
    "*"                     { \p s -> TStar p }
    "/"                     { \p s -> TDivide p }
    @identifier             { \p s -> TId p s }
    @number                 { \p s -> TNum p (read s :: Int) }

{

-- each action has type ::AlexPosn -> String -> Token


-- The token type:
data Token =

    TReturn                 AlexPosn        |
    TIf                     AlexPosn        |
    TEqual                  AlexPosn        |
    TLess                   AlexPosn        |
    TGreater                AlexPosn        |
    TLessOrEqual            AlexPosn        |
    TGreaterOrEqual         AlexPosn        |
    TSemicolon              AlexPosn        |
    TLeftParen              AlexPosn        |
    TRightParen             AlexPosn        |
    TLeftCurlyBracket       AlexPosn        |
    TRightCurlyBracket      AlexPosn        |
    TComma                  AlexPosn        |
    TPlus                   AlexPosn        |
    TMinus                  AlexPosn        |
    TEqual                  AlexPosn        |
    TStar                   AlexPosn        |
    TDivide                 AlexPosn        |
    TId                     AlexPosn String |
    TNum                    AlexPosn Int


tokenPosn (TReturn                 p) = p
tokenPosn (TIf                     p) = p
tokenPosn (TEqual                  p) = p
tokenPosn (TLess                   p) = p
tokenPosn (TGreater                p) = p
tokenPosn (TLessOrEqual            p) = p
tokenPosn (TGreaterOrEqual         p) = p
tokenPosn (TSemicolon              p) = p
tokenPosn (TLeftParen              p) = p
tokenPosn (TRightParen             p) = p
tokenPosn (TLeftCurlyBracket       p) = p
tokenPosn (TRightCurlyBracket      p) = p
tokenPosn (TComma                  p) = p
tokenPosn (TPlus                   p) = p
tokenPosn (TMinus                  p) = p
tokenPosn (TEqual                  p) = p
tokenPosn (TStar                   p) = p
tokenPosn (TDivide                 p) = p
tokenPosn (TId                     p id) = p
tokenPosn (TNum                    p num) = p


}
