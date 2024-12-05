let rec calculate op x y = 
  match op with
  | "+" -> x + y
  | "-" -> x - y
  | "*" -> x * y
  | "/" -> x / y
  | _ -> failwith "Invalid operator"


let () =
  print_endline "Enter an expression: ";
  let input = read_line() in
  let tokens = Str.split(Str.regexp "[ +-/*]") input in
  match tokens with
  | [x; op; y] ->
    let x = int_of_string x in 
    let y = int_of_string y in
    let result = calculate op x y in
    Printf.printf "Result: %d\n" result
  | _ -> print_endline "invalid input"

  
