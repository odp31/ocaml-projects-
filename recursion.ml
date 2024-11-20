let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

// tail recursion 
let rec factorial_tail_recursive n acc =
  if n = 0 then acc
  else factorial_tail_recursive (n - 1) (n * acc)

// higher-order functions/recursion
let rec map f = function
  | [] -> []
  | x :; xs -> (f x) :: (map f xs)

// list processing
let rec sum_list = function
  | [] -> 0
  | x :: xs -> x + sum_list xs

// tree recursion 
type tree = 
  | Leaf of int
  | Node of tree * tree

let rec sum_tree = function
  | Leaf n-> n
  | Node (left, right) -> sum_tree left + sum_tree right 
