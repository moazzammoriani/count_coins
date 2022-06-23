module L = List

let sum lst = L.fold_left (+) 0 lst

let rec iter (amt : int) (coins : (int * int) list) (curr : int list) (acc : int list list) =
    match amt, coins with
    | 0, coins -> acc 
    | _, [] -> []
    | _, (den,qty)::rst-> begin
        let new_amt = amt - den in
        if qty == 1 then 
            if den > amt then begin
                iter amt rst curr acc
            end else 
                iter new_amt rst curr acc
        else if den > amt then begin
            iter amt rst curr acc
        end else if (amt - den) < 0 then
            acc
        else
            let new_coins = (den, (qty-1))::rst in  
            let new_curr = den::curr in
            iter new_amt new_coins new_curr (new_curr::acc)
    end

let rec collect_compleleted_enum (amt : int) coins enums =
    match enums with
    | [] -> []
    | _ -> L.hd enums

let rec f_rmng (amt : int) (coins : (int * int) list) (enums : int list list) : int list list =
    let current_den = (fst (L.hd coins)) in
    let remaining_coins = L.tl coins in
    L.filter (fun x ->
        if (sum x) != amount then
            let total_value = (sum (L.map (fun (x,y) -> x*y))) in
            let rem_amt = amt - (sum x) in 
            if (rem_amt - total_value) <= 0
                true
    


let get_enums amt coins = 
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
    enums

