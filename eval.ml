open Def;;

exception TCFail of string;;
exception EVFail of string;;

(* 変数リネーム用 *)
let __varnum = ref 0;;
let reset_name () = let _ = __varnum := 0 in ();;
let new_name s = let inttos n = Int64.to_string (Int64.of_int n) in let _ = __varnum := (!__varnum + 1) in
	s ^ "_" ^ inttos (!__varnum);;

(* 型推論用 *)
let __tvnum = ref 0;;
let reset_tvar () = let _ = __tvnum := 0 in ();;
let new_tvar () = if !__tvnum > 25 then raise (TCFail "Too Complicate!")
  else let ret = String.make 1 (String.get "abcdefghijklmnopqrstuvwxyz" (!__tvnum)) in
    let _ = __tvnum := (!__tvnum + 1) in ret;;

let rec print_env v = match v with
  | [] -> ()
  | E(s, _)::v' -> Printf.printf "%s\n" s; print_env v';;

let findtype s tv = try List.find (function V(x, _, _) -> if s = x then true else false) tv with
	| Not_found -> raise (TCFail ("Variable " ^ s ^" is not found"));;
let findval s v = try List.find (function E(x, _) -> if s = x then true else false) v with
	| Not_found -> print_env v; raise (EVFail ("Variable " ^ s ^" is not found"));;

let print_type t =
	let rec maketstr = function
		| TInt -> "int"
		| TFun(t1, t2) -> (maketstr t1) ^ " -> " ^ (maketstr t2)
		| TBrk t -> "(" ^ (maketstr t) ^ ") code"
    | TVar n -> "'" ^ n in
	Printf.printf "%s\n" (maketstr t);;

let rec typechk e tv n = match e with
	| Int n -> TInt
	| Var s -> let V(_, t, m) = findtype s tv in
		if n >= m then t else raise (TCFail "Cross-stage violence")
	| Fun(s, t, e') -> let t' = typechk e' (V(s, t, n)::tv) n in TFun(t, t')
	| RFun(f, s, t, t', e') -> TFun(t, t')
	| App(e1, e2) -> (match (typechk e1 tv n, typechk e2 tv n) with
		| (TFun(t1, t2), t1') -> if t1 = t1' then t2 else raise (TCFail "Function type unmatch")
		| _ -> raise (TCFail "Non-function type applied"))
	| Prim2(_, e1, e2) -> (match (typechk e1 tv n, typechk e2 tv n) with
		| (TInt, TInt) -> TInt
		| _ -> raise (TCFail "integer expected"))
	| Ifz(c, e1, e2) -> (match (typechk c tv n, typechk e1 tv n, typechk e2 tv n) with
		| (TInt, t1, t2) -> if t1 = t2 then t1 else raise (TCFail "Ifz type unmatch")
		| _ -> raise (TCFail "integer expected for conj"))
	| Let(s, e1, e2) -> let t = typechk e1 tv n in typechk e2 (V(s, t, n)::tv) n
	| Brk e' -> TBrk (typechk e' tv (n + 1))
	| Esc e' -> if n < 1 then raise (TCFail "Uncorrect escape due to stage") else (match (typechk e' tv (n - 1)) with
		| TBrk t -> t
		| _ -> raise (TCFail "Uncorrect escape due to bracket"))
	| Run e' -> (match (typechk e' tv n) with
		| TBrk t -> t
		| _ -> raise (TCFail "Uncorrect run"))
	| _ -> raise (TCFail "Uncorrect expression");;

let rec travcspe e v = match e with
  | Int n -> Int n
  | Var s -> let E(_, e') = findval s v in e'
  | Fun(s, t, e') -> Fun(s, t, travcspe e' v)
  | RFun(f, s, t1, t2, e') -> RFun(f, s, t1, t2, travcspe e' v)
  | App(e1, e2) -> App(travcspe e1 v, travcspe e2 v)
  | Prim2(op, e1, e2) -> Prim2(op, travcspe e1 v, travcspe e2 v)
  | Ifz(c, e1, e2) -> Ifz(travcspe c v, travcspe e1 v, travcspe e2 v)
  | Let(s, e1, e2) -> Let(s, travcspe e1 v, travcspe e2 v)
  | Brk e' -> Brk (travcspe e' v)
  | Esc e' -> Esc (travcspe e' v)
  | Run e' -> Run (travcspe e' v)
  | Csp c -> Csp (travcspv c v)
and travcspv a v = match a with
  | VInt n -> VInt n
  | VFun(s, e, v') -> VFun(s, travcspe e v', v)
  | VRFun(s1, s2, e, v') -> VRFun(s1, s2, travcspe e v', v)
  | VExp e -> VExp (travcspe e v);;

let rec eval e v k = match e with
  | Int n -> k (VInt n)
  | Var s -> let E(_, e') = findval s v in
    (match e' with Csp v -> k v | _ -> raise (EVFail "Corrupted type environment"))
  | Fun(s, _, e') -> k (VFun(s, e', v))
  | RFun(f, s, _, _, e') -> k (VRFun(f, s, e', v))
  | App(e1, e2) -> eval e1 v (fun v1 -> eval e2 v (fun v2 -> match v1 with
    | VFun(s, e', v') -> eval e' (E(s, Csp v2)::v') (fun v3 -> k v3)
    | VRFun(f, s, e', v') as r -> eval e' (E(s, Csp v2)::E(f, Csp r)::v') (fun v3 -> k v3)
    | _ -> raise (EVFail "Non-function value Applied")))
  | Prim2(op, e1, e2) -> eval e1 v (fun v1 -> eval e2 v (fun v2 -> match (op, v1, v2) with
    | ("+", VInt v1, VInt v2) -> k (VInt (v1 + v2))
    | ("-", VInt v1, VInt v2) -> k (VInt (v1 - v2))
    | ("*", VInt v1, VInt v2) -> k (VInt (v1 * v2))
    | ("/", VInt v1, VInt v2) -> if v2 = 0 then raise (EVFail "Division by Zero") else k (VInt (v1 / v2))
    | ("%", VInt v1, VInt v2) -> if v2 = 0 then raise (EVFail "Division by Zero") else k (VInt (v1 mod v2))
    | _ -> raise (EVFail "Non-integer value calculated")))
  | Ifz(c, e1, e2) -> eval c v (fun v1 -> match v1 with
    | (VInt c') -> if c' = 0 then eval e1 v (fun v2 -> k v2) else eval e2 v (fun v2 -> k v2)
    | _ -> raise (EVFail "Non-integer value"))
  | Let(s, e1, e2) -> eval e1 v (fun v1 -> eval e2 (E(s, Csp v1)::v) (fun v2 -> k v2))
  | Brk e' -> k (VExp (rebuild e' v 1 (fun v -> v)))
  | Run e' -> eval e' v (fun v1 -> match v1 with
    | VExp e'' -> eval e'' v (fun v2 -> k v2)
    | _ -> raise (EVFail ""))
  | Csp c -> travcspv c v
  | _ -> raise (EVFail "Not yet support")
and rebuild e v n k = match e with
  | Int n -> k (Int n)
  | Var s -> let E(_, e') = findval s v in k e'
  | Fun(s, t, e') -> let s' = new_name s in rebuild e' (E(s, Var s')::v) n (fun e1 -> k (Fun(s', t, e1)))
  | RFun(f, s, t, t', e') -> let s' = new_name s in
    rebuild e' (E(s, Var s')::E(f, Var f)::v) n (fun e1 -> k (RFun(f, s', t, t', e1)))
  | App(e1, e2) -> rebuild e1 v n (fun e1' -> rebuild e2 v n  (fun e2' -> k (App(e1', e2'))))
  | Prim2(op, e1, e2) -> rebuild e1 v n (fun e1' -> rebuild e2 v n  (fun e2' -> k (Prim2(op, e1', e2'))))
  | Ifz(c, e1, e2) -> rebuild c v n (fun c' -> rebuild e1 v n (fun e1' -> rebuild e2 v n  (fun e2' -> k (Ifz(c', e1', e2')))))
  | Let(s, e1, e2) -> let s' = new_name s in rebuild e1 v n (fun e1' -> rebuild e2 (E(s, Var s')::v) n (fun e2' -> k (Let(s', e1', e2'))))
  | Brk e' -> rebuild e' v (n + 1) (fun e'' -> k (Brk e''))
  | Esc e' -> if n = 1
    then
      (match (eval e' v (fun v -> v)) with VExp e -> k e | _ -> raise (EVFail "Not bracket"))
    else
      rebuild e' v (n - 1) (fun e'' -> k (Esc e''))
  | Run e' -> rebuild e' v n (fun e'' -> k (Run e''))
  | Csp c -> k (Csp (travcspv c v));;

let print_val = function
  | VInt n -> "- : int = " ^ (string_of_int n)
  | _ -> "somewhat";;
