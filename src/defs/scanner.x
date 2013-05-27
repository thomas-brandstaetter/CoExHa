{
    module Scanner where
}

%wrapper "posn"


@number                 = [0-9]+
@identifier             = [a-zA-Z_][0-9a-zA-Z_]*



tokens :-

    $white+                 ;
    "end"                   { \p s -> TEnd p }
    "return"                { \p s -> TReturn p }
    "goto"                  { \p s -> TGoto p }
    "if"                    { \p s -> TIf p }
    "then"                  { \p s -> TThen p }
    "var"                   { \p s -> TVar p }
    "not"                   { \p s -> TNot p }
    "and"                   { \p s -> TAnd p }
    "=<"                    { \p s -> TLessOrEqual p }
    ";"                     { \p s -> TSemicolon p }
    "("                     { \p s -> TLeftParen p }
    ")"                     { \p s -> TRightParen p }
    ","                     { \p s -> TComma p }
    ":"                     { \p s -> TColon p }
    "+"                     { \p s -> TPlus p }
    "-"                     { \p s -> TMinus p }
    "="                     { \p s -> TEqual p }
    "*"                     { \p s -> TStar p }
    "#"                     { \p s -> TNumSign p }
    @identifier             { \p s -> TId p s }
    @number                 { \p s -> TNum p (read s :: Int) }

{

-- each action has type ::AlexPosn -> String -> Token


-- The token type:
data Token =
    TEnd            AlexPosn        |
    TReturn         AlexPosn        |
    TGoto           AlexPosn        |
    TIf             AlexPosn        |
    TThen           AlexPosn        |
    TVar            AlexPosn        |
    TNot            AlexPosn        |
    TAnd            AlexPosn        |
    TLessOrEqual    AlexPosn        |
    TSemicolon      AlexPosn        |
    TLeftParen      AlexPosn        |
    TRightParen     AlexPosn        |
    TComma          AlexPosn        |
    TColon          AlexPosn        |
    TPlus           AlexPosn        |
    TMinus          AlexPosn        |
    TEqual          AlexPosn        |
    TStar           AlexPosn        |
    TNumSign        AlexPosn        |
    TId             AlexPosn String |
    TNum            AlexPosn Int




tokenPosn (TEnd            p) = p
tokenPosn (TReturn         p) = p
tokenPosn (TGoto           p) = p
tokenPosn (TIf             p) = p
tokenPosn (TThen           p) = p
tokenPosn (TVar            p) = p
tokenPosn (TNot            p) = p
tokenPosn (TAnd            p) = p
tokenPosn (TLessOrEqual    p) = p
tokenPosn (TSemicolon      p) = p
tokenPosn (TLeftParen      p) = p
tokenPosn (TRightParen     p) = p
tokenPosn (TComma          p) = p
tokenPosn (TColon          p) = p
tokenPosn (TPlus           p) = p
tokenPosn (TMinus          p) = p
tokenPosn (TEqual          p) = p
tokenPosn (TStar           p) = p
tokenPosn (TNumSign        p) = p
tokenPosn (TId             p id) = p
tokenPosn (TNum            p num) = p

}
