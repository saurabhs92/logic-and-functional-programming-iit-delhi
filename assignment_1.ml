(*

        Submitted By: Saurabh Sharma
        
        Assignment 1: Representing Finite Sets

        Problem Statement: 


This functional programming assignment requires you to represent finite sets in two different ways, using two different data types:  (a) OCaml lists, and (b) OCaml functions:

(a) a set is represented as a list without duplicates.

(b) a set is represented by its characteristic function -- x \in s iff  f_s (x) = true (where f_s is the characteristic function of set s)

You need to implement the following operations:

1.    emptyset, which represents the empty set.
2.    member x s, which returns true if and only if x is in s.
3.    subset s1 s2, which returns true if and only if s1 is a subset of s2
4.    equal s1 s2, which returns true if and only if  set s1 is equal to set s2.
5.    union s1 s2, which returns the union of sets s1 and s2
6.    intersection s1 s2, which returns the intersection of s1 and s2
7.    difference s1 s2, which returns the set consisting of elements of s1 which are not in s2
8.    power s, which returns the set of subsets of s
9.    product s1 s2, which returns the cartesian product of s1 and s2.

*)



let is_empty s = match s with
| [] -> true
| z -> false

let is_member s m = List.mem m s;;

let set = [1;2;3];;

(*let print_set s = printList.iter print_int s;;*)
let rec print_set = function
| [] -> print_endline " "
| h :: t -> print_int h; print_string " "; print_set t;;

let rec print_set_str = function
| [] -> print_endline " "
| h :: t -> print_string (h ^ " "); print_set_str t;;


if is_member set 2 then print_endline "Is a member" else print_endline "Isn't a member";;

let is_singleton s = match s with 
| a :: [] -> true
| _       -> false;;

if is_singleton [3] then print_endline "[3] is singleton" else print_endline "Isn't singleton!";;



let add_member s m = match List.mem m s with
| true  -> s 
| false -> s @ m :: [] ;;

let s1 = [];;
let s2 = add_member s1 4;;
let s3 = add_member s2 4;;
print_endline "Printing list to check add_member function";;
print_set s3;;
print_endline " ";; 

let rec union s1 s2 = match s1 with 
| [] -> s2
| m :: list -> if List.mem m s2 then union list s2 else union list (s2 @ m :: []);;


(*List.iter (add_member s2) s1;;*)

let union_set = union [3;6;7] [1;3;4];;
print_set union_set;;

let rec intersection s1 s2 = match s1 with
| [] -> []
| h :: t -> if List.mem h s2 then h :: (intersection t s2) else intersection t s2 ;;


let intersctn_set = intersection [3;6;7] [1;3;4];;
print_set intersctn_set;;

let intersctn_set = intersection ["hey";"there";"!!"] ["!!";"hey";"u"];;
print_set_str intersctn_set;;


let rec difference s1 s2 = match s1 with
| [] -> []
| h :: t -> if List.mem h s2 then difference t s2 else h :: (difference t s2)

let diff_set = difference ["hey";"there";"!!"] ["!!";"hey";"u"];;
print_set_str diff_set;;


let cardinality set = List.length set;;
let card = cardinality [4;5;6;2];;
print_int card;;
print_endline " ";; 


let rec is_subset s1 s2 = match s1 with
| [] -> true
| h :: t -> if List.mem h s2 then is_subset t s2 else false;;

if is_subset [4;6;7;8] [2;4;5;6;7;8;11] then print_endline "Is a subset" else print_endline "Is not a subset";;
(*
let power_set set = List.fold_left (fun acc el -> acc @ List.map (fun ip -> acc::ip) el) [[]] set;;


let pwr_set =  power_set [1;2;3];;
print_set pwr_set;;
 *)
 let rec union s1 s2 = match s1 with 
  | [] -> s2
  | h :: t -> if List.mem h s2 then union t s2
	      else union t (h::s2);;

 let rec power_set list = match list with
   | [] -> [[]]
   | h :: tl -> let list = power_set tl in
		list @ (List.map (fun x -> h :: x) list);;


let pwr_set =  power_set [1;2;3];;

let product l1 l2 =
    List.rev (List.fold_left (fun acc x
			     -> List.fold_left (fun accum y -> (x,y)::accum
							  )  acc l2) [] l1);;

let rec print_prod l = match l with
		 | [] -> print_endline " "
		 | (a,b)::tl ->  Printf.printf "(%d,%d)\t" a b;
				 print_prod tl;;

print_prod (product [1;2;3] [4;5;6]);;  

let equal s1 s2 = ((is_subset s1 s2) && (is_subset s2 s1));;

if (equal ["a"; "b"; "c"] ["c";"a";"b"]) then print_endline "Sets are Equal"
else print_endline "Sets are Unequal";;

if (equal [1;2;3] [6;7;8;9]) then print_endline "Sets are Equal"
else print_endline "Sets are Unequal";;
		       
let emptyset () = [];;

let print_empty_set = function
  | [] -> print_endline "[]"
  | _ -> print_endline "";;

print_empty_set (emptyset ());;




  (****** Using Ocaml Functions *******)



let f_s x = match x with
  | 1 | 2 | 3 | 10 -> true
  | _ -> false

let f_s1 x = match x with
  | 1 | 4 | 11 -> true
  | _ -> false

let f_s2 x = match x with
  | 4 | 6 | 9 | 10 | 11 -> true
  | _ -> false

	 
  
let f_emptyset () = false

let f_member x s = f_s x

let int_set = [1;2;3;10];;

if (f_member 3 int_set) then print_endline "3 is a member"
else print_endline "3 is not a member";;

let f_union f_s1 f_s2 = 
  let f_s1_s2_union x = (f_s1 x || f_s2 x) in
  f_s1_s2_union

let f_sU = f_union f_s1 f_s2;;

if (f_sU 4) then print_endline "4 is a member of union"
else print_endline "4 is not a member of union";;

  
let f_intersection f_s1 f_s2 = 
  let f_s1_s2_intersection x = (f_s1 x && f_s2 x) in
  f_s1_s2_intersection;;

let f_sI = f_intersection f_s1 f_s2;;

if (f_sI 5) then print_endline "5 is a member of intersection"
else print_endline "5 is not a member of intersection";;
  
let f_difference f_s1 f_s2 =
  let f_s1_s2_intersection x = (f_s1 x && f_s2 x) in
  let f_s1_s2_difference x = if (f_s1_s2_intersection x = true)
			     then false else f_s1 x in
  f_s1_s2_difference;;

let f_sD = f_difference f_s1 f_s2;;

if (f_sD 1) then print_endline "1 is a member of difference"
else print_endline "1 is not a member of difference";;
  
				 
  
