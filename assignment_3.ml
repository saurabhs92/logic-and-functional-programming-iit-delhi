(*
        Submitted By: Saurabh Sharma
        
    Assignment 3: Representing terms in a signed algebra, substitutions

    Problem Statement: 
Consider the representation of "pre-terms" using the following data type definition

type term = V of variable | Node of symbol * (term list);;

Choose suitable type representations for types variable and symbol.


1.    Given a signature consisting of symbols and their arities (>= 0) in any suitable form -- either as a list of (symbol, arity) pairs, or as a function from symbols to arities, write a function check_sig that checks whether the signature is a valid signature (no repeated symbols, arities are non-negative etc.)
2.    Given a valid signature (checked using check_sig), define a function wfterm that checks that a given preterm is well-formed according to the signature.
3.    Define functions ht, size and vars that given a well-formed term, return its height, its size and the set of variables appearing in it respectively.  Use map, foldl and other such functions as far as possible wherever you use lists.  
4.    Define a suitable representation for substitutions.  Come up with an efficient representation of composition of substitutions. 
    Define the function subst that given a term t and a substitution s, applies the (Unique Homomorphic Extension of) s to t.  Ensure that subst is efficiently implemented. 
5.    Define the function mgu that given two terms t1 and t2, returns their most general unifier, if it exists and otherwise raises an exception NOT_UNIFIABLE.

*)

(*Define types for variables, symbols and terms*)
type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * (term list);;

(*Examples of terms for testing*)
let t1 = V "var1";;
let t2 = Node ("z", []);;
let t3 = Node ("f", [Node ("a", []); Node ("b", [])]);;
let t4 = Node ("g", [t2]);;
let t5 = Node ("f", [Node ("a", []);
             Node ("y", [V "var2"; Node ("c", [])])])
let t6 = Node ("f", [Node ("a", []);
             Node ("y", [Node ("b", []);
                 Node ("c", [Node ("d", [])])])])
let t7 = Node ("f", [V "var1";
             Node ("y", [V "var2";
                 Node ("c", [V "var3"])])])
 
(*Examle of signatures for testing*)
let ss1 = [("a", 0); ("f", 2); ("g", 1); ("b", 0)];;
let ss2 = [("b", -1); ("f", 2); ("g", 1)];;
let ss3 = [("d", 5); ("f", 2); ("g", 3)];;
let ss4 = [("a", 0); ("f", 2); ("g", 1); ("g", 1)];;
let ss5 = []

(* Function check_sig: checks if the given signature is valid or not *)
(* by applying 3 checks, returns false if any of the following is present:
     - negative arity
     - no zero arity
     - repeated symbols
*)
let check_sig sign =
  let rec check sign symbols zero_arity = match sign with
    | [] -> (zero_arity = true)
    | (symbl, arity) :: tl -> if (arity < 0 || List.mem symbl symbols )
                then false else if (arity = 0 )
                then check tl (symbl::symbols) true else
                  check tl (symbl::symbols) (false || zero_arity) in
  check sign [] false;;

(*TESTING: Check validity of above example signatures*)
print_endline "Checking whether the signatures 1 to 5 are valid or not:";;
if (check_sig ss1) then print_endline "True" else print_endline "False";;
if (check_sig ss2) then print_endline "True"
  else print_endline "False";;
if (check_sig ss3) then print_endline "True" else print_endline "False";;
if (check_sig ss4) then print_endline "True" else print_endline "False";;
if (check_sig ss5) then print_endline "True" else print_endline "False";;
 

(*Function wfterm: Checks whether a term is present in a given *)
(*signature (sign) or not and that the number of children of *)
(*each term is same as the arity in the signature*)
let rec wfterm sign = function
  | V _ -> true
  | Node (symbl, []) -> (List.mem (symbl, 0) sign)
  | Node (str, ((hd::tl) as lst))
    -> if (List.mem (str, List.length lst) sign)
       then (List.fold_left (fun bool el -> (wfterm sign el && bool)) true lst) else false;;

(*TESTING: Check if the above example terms are well formed terms*)
print_endline "Checking whether the terms t1 to t4 are well formed or not:";;
print_string "t1 ";;
if (wfterm ss1 t1) then print_endline "Is a well formed term"
  else print_endline "Is not well formed";;

print_string "t2 ";;
if (wfterm ss1 t2) then print_endline "Is a well formed term"
  else print_endline "Is not well formed";;

print_string "t3 ";;
if (wfterm ss1 t3) then print_endline "Is a well formed term"
  else print_endline "Is not well formed";;

print_string "t4 ";;
if (wfterm ss1 t4) then print_endline "Is a well formed term"
  else print_endline "Is not well formed";;

 
(*Function ht for height: Returns the height of term given as input*)
let rec ht = function
  | V var -> 0
  | Node (symbl , []) -> 0
  | Node (symbl, lst)
    -> 1 + (List.fold_left (fun acc el -> max (ht el) acc) 0 lst);;

(*TESTING: Check the height function for the above example terms*)
print_endline "Printing the heights of terms t1 to t6:";;
print_int (ht t1);; print_endline " ";;
print_int (ht t2);; print_endline " ";;
print_int (ht t3);; print_endline " ";;
print_int (ht t4);; print_endline " ";;
print_int (ht t5);; print_endline " ";;
print_int (ht t6);; print_endline " ";;


(*Function size: Returns the size of the term*)
let rec size = function
  | V var -> 1
  | Node (symbl, []) -> 1
  | Node (symbl, lst) -> 1 + (List.fold_left (fun acc el -> (size el) + acc) 0 lst);;

(*TESTING: Check the height function for the above example terms*)
print_endline "Printing the size of terms t1 to t6:";;
print_int (size t1);; print_endline " ";;
print_int (size t2);; print_endline " ";;
print_int (size t3);; print_endline " ";;
print_int (size t4);; print_endline " ";;
print_int (size t5);; print_endline " ";;
print_int (size t6);; print_endline " ";;

(*Function vars: Returns the set of variables appearing in it *)
let rec vars term =
  let rec aux list term = match term with
  | V var -> var::list
  | Node (symbl, []) -> list
  | Node (symbl, lst) -> (List.fold_left (fun list el-> (aux list el)) list lst)  in
  aux [] term;;


(*Helper function print_list to print a string list*)
let rec print_list = function
  | [] -> print_endline " "
  | hd::tl -> print_string (hd ^ "\t"); print_list tl;;

(*TESTING: check the vars function for the terms*)
print_endline "Printing the variables in terms t1, t5 and t7:";;
let l1 = vars t1;;
print_list l1;;
let l5 = vars t5;;
print_list l5;;
let l7 = vars t7;;
print_list l7;;

(*Function subst for substitution: Takes a function sigma and a term and applies the function sigma to replace all variables in the term with a term given by the function sigma*)
let rec subst sigma term = match term with
  | V var -> sigma var
  | Node (symbl, []) -> Node (symbl, [])
  | Node (symbl, lst) -> Node (symbl, (List.map (fun el -> subst sigma el) lst));;

(*Defining the sigma function for internal testing*)
let sigma1 var = match var with
  | "var1" | "var2" | "var3" | "var4" | "var5" -> Node("x", [])
  | _ -> Node ("y", []);;

(*TESTING: Check the subst function for a term and print the list of variables after applying the substitution *)  
let t8 = (subst sigma1 t7);;
let l8 = vars t8;;
print_list l8;;

(*Function substitute: substitutes term s with variable var in term t*)
let rec substitute s var t = match t with
  | V x -> if x = var then s else t
  | Node (f, lst) -> Node (f, List.map (substitute s var) lst);;
 
(*Defining type for substitution which is a tuple list of variable and term
It specifies the term to be substituted for each of the variables *)
type substitution = (variable * term) list;;

(*Examples of substitution type*)
let s1 = [("var1", Node ("m", [])); ("var2", Node("n", [])) ];;
let s2 = [("var2", Node ("i", [])); ("var3", Node("j", [])) ];;

exception INVALID_SUBSTITUTION;;
(*Helper function to print a substitution type variable*)
let rec print_subst = function
  | [] -> print_endline " "
  | (var, Node (f, lst)) :: tl
    -> print_string ("V " ^ var ^ ":" ^ "\tNode " ^ f ^ "\n");
       print_subst tl
  | _ -> raise INVALID_SUBSTITUTION ;;

(*Function apply: Applies the substituion of substitution type to all variables of a tree term*)
let apply sub term =
  List.fold_right (fun (s,t) -> substitute t s) sub term;;

(*TESTING: Check the apply function to apply the substitution*)
print_endline "List of variables in a term before applying the substitution:";;
print_list (vars t7);;
print_endline "List of variables in the term after applying the substitution:";;
let t9 = apply s1 t7;;
let l9 = vars t9;;
print_list l9;;

(*Helper function occurs to check if variable x occurs in a given term or not*)
let rec occurs x = function
    | V y -> x = y
    | Node (_, lst) -> List.exists (occurs x) lst ;;

(*Function composition: Represent composition of two substitutions s1 and s2*)
let composition s1 s2 =
   let rec aux s1 s2 s = match (s1,s2) with
      | ([],[])   -> s
      |    (l1, [])  -> l1 @ s
      | ([], l2)  -> l2 @ s
      | (((v1, t1) :: tl1), ((v2, t2) :: tl2)) -> if occurs v2 t1 then (v1, substitute t1 v2 t2)::s else (aux tl1 tl2 ([(v1, t1)] @ [(v2, t2)] @ s)) in
       aux s1 s2 [];;

let s3 = composition s1 s2;;

exception NOT_UNIFIABLE;;

(*Function mgu: returns the most general unifier of two terms
The mgu is of type substitution: (variable * term) list which specifies the terms the variables are to be replaced with to obtain the mgu*)
let mgu t1 t2 =
  let rec occurs x = function
    | V y -> x = y
    | Node (_, lst) -> List.exists (occurs x) lst in
  let rec unify list = match list with
    | [] -> []
    | (x,y) :: tl -> let t2 = unify tl in
             let t1 = unif (x,y) in
             t1 @ t2
  and unif (s,t) = match (s,t) with
    | (V x, V y) -> if x = y then [] else [(x, t)]
    | (V x, (Node (_, lst) as t)) | ((Node (_, lst)) as t, V x)
                      -> if occurs x t
                     then raise NOT_UNIFIABLE
                     else [(x, t)]
    | (Node (f, lst1), Node (g, lst2))
               -> if (f = g) &&
                   (List.length lst1 = List.length lst2)
              then
                unify (List.combine lst1 lst2)
              else raise NOT_UNIFIABLE
                               
  in
  unif (t1,t2);;

 

(*TESTING: Check the mgu function for terms t6 and t7 and print the resulting substitution*)
let m1 = (mgu t6 t7);;
print_endline "Printing the substitution to be made to find the most general unifier of t6 and t7:";;
print_subst m1;;

  
