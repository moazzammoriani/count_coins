let n = try int_of_string @@ Sys.argv.(1) with _ -> 960

module L = List
(*module T = Domainslib.Task*)

let rec count_payment_possibsL (amt : int) (coins: (int * int) list) (acc: int list): (int list list) =
    match amt, coins with 
    | 0, _ -> [acc]
    | _, [] -> []
    | _, (c,q)::rst-> begin
        if c > amt then
            count_payment_possibsL amt rst acc
        else
            let coins_ = if q == 1 then
                         rst 
                         else (c, q-1)::rst in
            let left = count_payment_possibsL (amt - c) coins_ (c :: acc) in 
            let right = count_payment_possibsL amt rst acc in
            L.append left right
    end

let print_list lst = 
  print_endline "[ ";
  for i = 0  to List.length lst - 1 do 
    print_int @@ List.nth lst i ;
    print_string " "
  done;
  print_endline "\n]"

let coins_input : (int * int) list =
  let cs = [250 ; 100 ; 25 ; 10 ; 5 ; 1] in
  let qs = [55 ; 88 ; 88 ; 99 ; 122 ; 177] in
  L.combine cs qs

let _ = 
    count_payment_possibsL 25 [(10,3);(5,8); (1,10)] []


