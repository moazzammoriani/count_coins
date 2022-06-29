module L = List
module A = Array

let sum lst = L.fold_left (+) 0 lst

type amt = int
type den = int
type den_qty = int

let rec next_deductible coins amt = 
    match coins with
    | [] -> 0
    | (den, qty)::xs -> if amt - den > 0 then den else next_deductible xs amt


let rec iter (amt : amt) (coins : (den * den_qty) list) (curr : int list) (acc : (den * amt * den list) list) : (den * amt * den list) list =
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
                iter new_amt rst new_curr ((curr_den, new_amt, new_curr)::acc) end
        else if den > amt then begin
            iter amt rst curr acc
        end else if (amt - den) < 0 then
            acc
        else
            let curr = if new_amt < curr_den then next_deductible coins new_amt else curr_den in
            iter new_amt new_coins new_curr ((curr, new_amt, new_curr)::acc)
    end

let it amt coins curr = iter amt coins curr []


let get_1 (x,y,z) = x 

let get_2 (x,y,z) = y

let get_3 (x,y,z) = z

let filter data n = 
    L.map get_3
    (L.filter (fun t -> let x = get_1 t and y = get_2 t in if x > n && y - n > 0 then true else false) data)


let rec get_cmp_enum (enums : (den * amt * den list) list) : den list list =
    match enums with
    | [] -> []
    | (_, 0, y)::tail -> [y]
    | _ -> []

let index_of_den (coins : (den * den_qty) list) (den : den) = 
    let rec aux (count:int) (coins : (den * den_qty) list) =
        match coins with
        | [] -> -1
        | (d,q)::xs -> if d = den then count else (aux (count+1) xs) in
    aux 0 coins

let den_of_index (coins : (den * den_qty) list) (index: int) = fst (L.nth (L.rev coins) index)

let index_to_last (coins : (den * den_qty) list) (index: int) =
    let rec aux count coins = 
        match coins, count with 
        | [],_ -> []
        | _,0 -> coins
        | _ -> aux (count - 1) (L.tl coins) in
    aux index coins

let f = ref (Array.init 4 (fun _ -> []))


let itrandfltr amt coins seq dindex flist_ref= 
    let doi i = den_of_index coins i in
    let enums = ref [] in
    let coins_len = L.length coins in
    let root_to_leaf = it amt coins seq in
    enums := L.append (get_cmp_enum root_to_leaf) !enums;
    for j = dindex to (coins_len-1) do 
        !flist_ref.(coins_len -1 - j) <- L.append (filter root_to_leaf (doi j)) !flist_ref.(coins_len -1 -j);
    done;
    !flist_ref
    (* initialize array for filtered enums for each den
    (* *)

    for i = 0 to (l.length) do 
        (* computer iter for denom i*)
        (* filter the results of iter and add to corresponding list for each respective denom*)
    done; 
    for i = 1 to (L.length coins) do 
        
        for j = i to (L.length coins) do 
            (* compute iter for den j for each item in the filtered list of den j*)
            (* filter results from the iteration and place in the corresponding filtered list for its respective denom*)
            
        done
    done;
    enums*)



(*let rec f_rmng (amt : int) (coins : (int * int) list) (enums : int list list) : int list list =
    let current_den = (fst (L.hd coins)) in
    let remaining_coins = L.tl coins in
    L.filter (fun x ->
        if (sum x) != amount then
            let total_value = (sum (L.map (fun (x,y) -> x*y))) in
            let rem_amt = amt - (sum x) in 
            if (rem_amt - total_value) <= 0
                true*)
    


(*let get_enums amt coins = 
    let acc = ref [] in
    let filtered_for_remaining_dens = ref [] in
    let filtered_sub_tree_for_next_den = ref [] in
    let enums = ref [] in
    begin
    for i = 0 to (L.length coins) do 
        filtered_sub_tree_for_remaining_dens := filter_for_remaining_denoms coins i sub_tree;
        filtered_sub_tree_for_next_den := filter_sub_tree amt coins j filtered_sub_tree_for_remaining_denom;
        for j = 0 to (L.length filtered_sub_tree_for_next_den) do
            begin
            let curr = (L.nth filtered_sub_tree j) in
            let sub_tree = iter (amt - (sum curr)) coins  [] in
            enums := (collect_completed_enum sub_tree) :: !enums;
            end
        done
    done;
    end;
    enums*)

