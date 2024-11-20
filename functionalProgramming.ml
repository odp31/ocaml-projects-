// higher order functions
//map: applies a function to each element of a list 

let rec map f = function
  | [] -> []
  | x :: xs -> (f x) :: (map f xs)

// filters; filters a list based on a predicate 

let rec filter p = function 
  | [] -> []
  | x :: xs -> if p x then x :: (filter p xs) else filter p xs 

// fold: reduces list to single value 
let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs 

// pattern matching: list processing
let rec sum_list = function
  | [] -> 0
  | x :: xs -> x + sum_list xs

// immutability: list concatenation 
let rec append l1 l2 = 
  match l1 with
  | [] -> l2
  | x :: xs -> x :: (append xs l2)

// algebraic data types; defining custom types
type expr = 
  | Var of string
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr

//pattern matching on custom types
let rec eval env = function
  | Var x -> Env.find x env
  | Num n -> n
  | Add (e1, e2) -> (eval env e1) + (eval env e2)
  | Mul (el, e2) -> (eval env e1) * (eval env e2) 
