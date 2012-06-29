type expr = Int of int | Var of string | Fun of string * typ * expr | App of expr * expr
  | Prim2 of string * expr * expr | Ifz of expr * expr * expr
  | Let of string * expr * expr | RFun of string * string * typ * typ * expr
  | Brk of expr | Esc of expr | Run of expr | Csp of value
and typ = TInt | TFun of typ * typ | TBrk of typ | TVar of string
and value = VInt of int | VFun of string * expr * (env list) | VRFun of string * string * expr * (env list)
  | VExp of expr
and env = E of string * expr
and tenv = V of string * typ * int
