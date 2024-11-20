(* define simple expression language *)

type expr = 
  | Var of string
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr


(* environment for variable lookup *)
type env = (string * int) list 

(* function to evaluate expressions in an environment *) 
let rec eval env = function
  | Var x -> List.assoc x env
  | Num n - n 
  | Add (e1, e2) -> (eval env e1) + (eval env e2)
  | Mul (e1, e2) -> (eval env e1) * (eval env e2)

(* example usage *)
let expr = Add(Mul(Num 2, Var "x"), Num 3)
let env = [("x", 5)]

let result = eval env expr
Printf.printf"Result: %d\n" result 
