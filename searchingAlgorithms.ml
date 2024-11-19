// linear search 
let rec linear_search x lst =
  match lst with
  | [] -> None
  | y :: ys -> if x = y then Some y else linear_search x ys 


// binary search - requires sorted list 
let rec binary_search x lst = 
  match lst with 
  | [] -> None
  | _ when List.length lst = 1 -> if List.hd lst = x then Some x else None 
  | _ ->
    let mid = List.length lst / 2 in 
    let pivot = List.nth lst id in 
    if x = pivot then
      Some pivot
    else if x < pivot then
      binary_search x (List.take mid lst)
    else
      binary_search x (List.drop (mid + 1) lst)


// interpolation search 
let interpolation_search x arr = 
  let n = Array.length arr - 1 in
  let mut lo = 0 in
  let mut hi = n in 

  while lo <= hi do 
    let pos = lo + (hi - lo) * (x - arr.(lo)) / (arr.(hi) - arr.(lo)) in
    if arr.(pos) = x then
      pos
    else if arr.(pos) < x then
      lo <- pos + 1
    else
      hi <- pos - 1;
  done;
  -1
  
