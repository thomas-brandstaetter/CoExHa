{
    module Scanner where
}

%wrapper "posn"


@number                 = [0-9]+
@identifier             = [a-zA-Z_][0-9a-zA-Z_]*



tokens :-

    $white+                 ;
    "return"                { \p s -> TReturn p }
    "begin"                 { \p s -> TBegin p }
    "end"                   { \p s -> TEnd p }
    "var"                   { \p s -> TVar p }
    "const"                 { \p s -> TConst p }
    "procedure"             { \p s -> TProcedure p }
    "while"                 { \p s -> TWhile p }
    "do"                    { \p s -> TDo p }
    "if"                    { \p s -> TIf p }
    "then"                  { \p s -> TThen p }
    "call"                  { \p s -> TCall p }
    "odd"                   { \p s -> TOdd p }
    "#"                     { \p s -> TNotEqual p }
    ":="                    { \p s -> TAssign p }
    "<"                     { \p s -> TLess p }
    ">"                     { \p s -> TGreater p }
    "<="                    { \p s -> TLessOrEqual p }
    "=>"                    { \p s -> TGreaterOrEqual p }
    ";"                     { \p s -> TSemicolon p }
    ","                     { \p s -> TComma p }
    "+"                     { \p s -> TPlus p }
    "-"                     { \p s -> TMinus p }
    "="                     { \p s -> TEqual p }
    "*"                     { \p s -> TStar p }
    "/"                     { \p s -> TDivide p }
    "."                     { \p s -> TDot p }
    "("                     { \p s -> TOpenParenthesis p }
    ")"                     { \p s -> TCloseParenthesis p }
    @identifier             { \p s -> TIdentifier p s }
    @number                 { \p s -> TNumber p (read s :: Integer) }

{

-- each action has type ::AlexPosn -> String -> Token

-- The token type:
data Token =

    TReturn                 AlexPosn        |
    TBegin                  AlexPosn        |
    TEnd                    AlexPosn        |
    TVar                    AlexPosn        |
    TConst                  AlexPosn        |
    TProcedure              AlexPosn        |
    TWhile                  AlexPosn        |
    TDo                     AlexPosn        |
    TCall                   AlexPosn        |
    TOdd                    AlexPosn        |
    TIf                     AlexPosn        |
    TThen                   AlexPosn        |
    TNotEqual               AlexPosn        |
    TAssign                 AlexPosn        |
    TLess                   AlexPosn        |
    TGreater                AlexPosn        |
    TLessOrEqual            AlexPosn        |
    TGreaterOrEqual         AlexPosn        |
    TSemicolon              AlexPosn        |
    TComma                  AlexPosn        |
    TPlus                   AlexPosn        |
    TMinus                  AlexPosn        |
    TEqual                  AlexPosn        |
    TStar                   AlexPosn        |
    TDivide                 AlexPosn        |
    TDot                    AlexPosn        |
    TOpenParenthesis        AlexPosn        |
    TCloseParenthesis       AlexPosn        |
    TIdentifier             AlexPosn String |
    TNumber                 AlexPosn Integer

tokenPosn (TReturn                 p) = p
tokenPosn (TBegin                  p) = p
tokenPosn (TEnd                    p) = p
tokenPosn (TVar                    p) = p
tokenPosn (TConst                  p) = p
tokenPosn (TProcedure              p) = p
tokenPosn (TWhile                  p) = p
tokenPosn (TDo                     p) = p
tokenPosn (TIf                     p) = p
tokenPosn (TThen                   p) = p
tokenPosn (TCall                   p) = p
tokenPosn (TOdd                    p) = p
tokenPosn (TNotEqual               p) = p
tokenPosn (TLess                   p) = p
tokenPosn (TGreater                p) = p
tokenPosn (TLessOrEqual            p) = p
tokenPosn (TGreaterOrEqual         p) = p
tokenPosn (TSemicolon              p) = p
tokenPosn (TComma                  p) = p
tokenPosn (TPlus                   p) = p
tokenPosn (TMinus                  p) = p
tokenPosn (TEqual                  p) = p
tokenPosn (TStar                   p) = p
tokenPosn (TDivide                 p) = p
tokenPosn (TDot                    p) = p
tokenPosn (TOpenParenthesis        p) = p
tokenPosn (TCloseParenthesis       p) = p
tokenPosn (TIdentifier             p identifier) = p
tokenPosn (TNumber                 p number) = p

}
