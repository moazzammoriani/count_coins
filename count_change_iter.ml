module L = List
module A = Array

let sum lst = L.fold_left (+) 0 lst

type amt = int
type den = int
type den_qty = int

let rec next_deductible coins amt = 
    (** Returns the largest denomination den in coins such that amt > den *)
    match coins with
    | [] -> 0
    | (den, qty)::xs -> if amt - den > 0 then den else next_deductible xs amt


let rec iter (amt : amt) (coins : (den * den_qty) list) (curr : int list) (acc : (den * amt * den list) list) : (den * amt * den list) list =
    (** Recurses down a single-branched tree using coins trying to create a sequence of denominations whose sum is amt and returns a list of nodes encountered
        along the way *)
    match amt, coins with
    | 0, coins -> acc 
    | _, [] -> acc
    | _, (den,qty)::rst-> begin
        let new_amt = amt - den in
        let new_coins = (den, (qty-1))::rst in  
        let new_curr = den::curr in
        let curr_den = fst @@ L.hd @@ coins in
        if qty == 1 then 
            if den > amt then begin
                iter amt rst curr acc
            end else begin
                let curr = next_deductible (L.tl coins) new_amt in
                iter new_amt rst new_curr ((curr, new_amt, new_curr)::acc) end
        else if den > amt then begin
            iter amt rst curr acc
        end else if (amt - den) < 0 then
            acc
        else
            let curr = if new_amt < curr_den then next_deductible coins new_amt else curr_den in
            iter new_amt new_coins new_curr ((curr, new_amt, new_curr)::acc)
    end

let it amt coins curr = 
    (** wrapper for iter *)
    iter amt coins curr []

(* Selectors for tuples *)
let get_1 (x,y,z) = x 

let get_2 (x,y,z) = y

let get_3 (x,y,z) = z

let filter data n = 
    (** Selector to get a list of sequences generated from iter such that the sequence seq can be itrandfiltr'd upon with n as a its den *)
    L.map get_3
    (L.filter (fun t -> let x = get_1 t and y = get_2 t in if x > n && y - n > 0 then true else false) data)


let rec get_cmp_enum (enums : (den * amt * den list) list) : den list list =
    (** Selector to extract a seqeunce seq from the result of iter if seq has a sum equal to the amt it was supposed to reach *)
    match enums with
    | [] -> []
    | (_, 0, y)::tail -> [y]
    | _ -> []

let index_of_den (coins : (den * den_qty) list) (den : den) = 
    (** Returns the index of denomination den in coins if it exists or -1 otherwise *)
    let rec aux (count:int) (coins : (den * den_qty) list) =
        match coins with
        | [] -> -1
        | (d,q)::xs -> if d = den then count else (aux (count+1) xs) in
    aux 0 coins

let den_of_index (coins : (den * den_qty) list) (i: int) = 
    (** Returns the denomination at index i of coins *)
    fst (L.nth coins i)

let rec rest_from_den (coins : (den * den_qty) list) den = 
    (** Returns coins with denomination greater than den excluded *)
    match coins with 
    | [] -> []
    | (d,q)::xs -> if d = den then coins else (rest_from_den xs den)

let print_list l = 
    (** Prints a list of ints *)
    Printf.printf "[";
    let rec aux l =
        match l with
        | [] -> ()
        | e::l -> print_int e ; print_string " " ; aux l in
    aux l;
    Printf.printf "]"

let print_lol lol = 
    (** Prints a list of list of ints *)
    Printf.printf "[ ";
    let _ = L.map print_list lol in
    Printf.printf " ]"

let print_array arr = 
    (** Prints an array containing lists of lists of ints *)
  Printf.printf "[| ";
  for i = 0  to (Array.length arr) - 1 do 
      print_lol arr.(i);
  done;
  Printf.printf "|]\n"



let itrandfltr amt coins_list den seq f =  
    (* f contains a list of incomplete sequences at index i such that these sequences can be itrandfltr'd upon by denomination (den_of_index coins_list i) *)
    (** Performs
        1. a call to iter with a starting sequence seq trying to reach amount amt with its coins parameter starting at denomination den.
        2. filters the results from the call to iter for all denominations less than den.
        3. inserts, in a list in f, sequences that can be itrandfiltr'd upon by the denominations at the corresponding index 
        4. returns a list containing the completed sequence from the call to iter if any *)
    let iod d = index_of_den coins_list d in
    let doi i = den_of_index coins_list i in
    let enums = ref [] in
    let coins_len = L.length coins_list in
    let root_to_leaf = it amt (rest_from_den coins_list den) seq in
    enums := L.append (get_cmp_enum root_to_leaf) !enums;
    for j = ((iod den)+1) to (coins_len-1) do 
        let curr_den = doi j in
        let filtered = filter root_to_leaf curr_den in
        !f.(j) <- L.append filtered !f.(j);
    done;
    !enums

let cc amt coins = 
    (** Returns an enumeration of all possible combinations of denominations from coins that can be used to pay amount amt 
        as a list of sequences *)
    let doi i = den_of_index coins i in
    let clen = L.length coins in
    let f = ref (Array.init clen (fun _ -> [[]])) in
    let enums = ref [] in
    let seq = ref [] in
    let flist = ref [] in
    for i = 0 to clen - 1 do 
        flist := !f.(i);
        for j = 0 to (L.length !flist) -1 do
            seq := L.hd !flist;
            flist := L.tl !flist;  
            let new_amt = (amt - (sum !seq)) in (* need to skip a den if new_amt < den*)
            let cden = (doi i) in
            if new_amt >= cden then 
                enums := L.append  !enums (itrandfltr new_amt coins cden !seq f);
        done;
        !f.(i) <- !flist;
    done;
    !enums 


let _ = 
    cc 17 [(20,2);(10,2);(5,8);(1,17)]
