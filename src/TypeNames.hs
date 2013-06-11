module TypeNames where


data Statement
    = Assign String Expression
    | Call String
    | Write String
    | If Condition Statement
    | While Condition Statement
    | Begin [Statement]
    deriving (Show, Eq)

data Condition
    = Odd Expression
    | Eq Expression Expression
    | Ne Expression Expression
    | Gt Expression Expression
    | Lt Expression Expression
    | Ge Expression Expression
    | Le Expression Expression
    deriving (Show, Eq)

data Expression
    = Identifier String
    | Number Integer
    | Plus Expression Expression
    | Minus Expression Expression
    | Multiply Expression Expression
    | Divide Expression Expression
    deriving (Show, Eq)

data Block = Block {
        blockConsts :: [(String, Integer)],
        blockVars :: [String],
        blockProcs :: [Procedure],
        blockStatement :: Statement
    }
    deriving (Show, Eq)

data Procedure = Procedure String Block
    deriving (Show, Eq)





