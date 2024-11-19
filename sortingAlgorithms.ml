// insertion sort

let rec insertion_sort = function
  | [] -> []
  | x :: xs -> insert x (insertion_sort xs)

and insert x = function 
  | [] -> [x]
  | y :: ys -> if x <= y then x :: y :: ys else y :: insert x ys 


// merge sort 

let rec merge_sort = function 
  | [] -> []
  | [x] -> [x]
  | xs ->
    let n = List.length xs / 2 in 
    let left = List.take n xs in 
    let right = List.drop n xs in 
    merge (merge_sort left) (merge_sort right)

  and merge xs ys = 
    match xs, ys with 
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs', y::ys' ->
        if x <= y then x :: merge xs' ys else y:: merge xs ys' 


// quick sort
let rec quicksort = function 
  | [] -> []
  | x :: xs ->
    let smaller_xs, greater_xs = List.partition(fun y-> y <= x) xs in 
    quicksort smaller_xs @ [x] @ quicksort greater_xs


// heap sort 
let heapify arr i n = 
  let largest = i in 
  let l = 2 * i + 1 in 
  let r = 2 * i + 2 in 

  if l < n && arr.(l) > arr.(largest) then
    largest <- 1; 
  if r < n && arr.(r) > arr.(largest) then
    largest <- r;

  if largest<> i then
    let tmp = arr.(i) in
    arr.(i) <- arr.(largest);
    arr.(largest) <- tmp; 
    heapify arr largest n 

let heap_sort arr = 
  let n = Array.length arr - 1 in 
  for i in (n / 2 - 1) down to 0 do 
    heapify arr i n
  done;

  for i in n downto 1 do 
    let tmp = arr.(0) in 
    arr.(0) <- arr.(i);
    arr.(i) <- tmp;
    heapify arr 0 (i - 1);
  done;
  arr
  
