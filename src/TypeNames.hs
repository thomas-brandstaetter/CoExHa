module TypeNames where


data Program
    = Program Program Funcdef
    | PEmpty
    deriving (Show, Eq)

data Funcdef
    = Funcdef Ident Params Stmts
    deriving (Show, Eq)

data Params
    = Params Ident Params
    | ParamsOne Ident
    | PaEmpty
    deriving (Show, Eq)

data Labeldef
    = Labeldef Ident
    | LEmpty
    deriving (Show, Eq)

data Stmts
    = Stmts Stmt Stmts
    | SEmpty
    deriving (Show, Eq)

data Stmt
    = StmtReturn Term 
    | StmtReturnNull
    | StmtGoto Ident
    | StmtIf Expr Stmts
    | StmtDecl Ident Expr
    | StmtTerm Term
    | StmtLExpr LExpr Expr
    | StmtEmpty
    deriving (Show, Eq)
    
data Expr
    = ExprUnary Unary
    | ExprPlus Term Term
    | ExprMult Term Term
    | ExprAnd Term Term
    | ExprTerm Term
    deriving (Show, Eq)

data LExpr
    = LExpr Ident
    deriving (Show, Eq)

data Unary
    = UnaryNot Unary
    | UnaryMinus Unary
    | UnaryTerm Term
    deriving (Show, Eq)

data Term
    = TermId Ident
    | TermExpr Expr
    | TermNum Int
    | TermCall Ident Params
    deriving (Show, Eq)


type Ident = String


