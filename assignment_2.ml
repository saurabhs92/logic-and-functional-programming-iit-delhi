(*
        Submitted By: Saurabh Sharma
        
        Assignment 2: Propositional Logic

        Problem Statement: 


In this programming assignment, you will take the data type of propositions defined in class and write simple programs to manipulate them.

type prop = Atom of string | T | F

    | Not of prop | And of prop * prop

    | Or of prop * prop | Imply of prop * prop

    | Iff of prop * prop ;;

The functions you need to implement are:

1.    ht: prop -> int, which returns the height of a proposition (syntax tree).
2.    size: prop ->, which returns the number of nodes in a proposition (syntax tree).
3.    atoms: prop -> string set, which returns the set of propositional variables that appear in a proposition
4.    truth: prop -> (string -> bool) -> bool, which evaluates a proposition with respect to a given truth assignment to atoms
5.    nnf: prop -> prop, which converts a proposition into negation normal form.
6.    cnf: prop -> prop set set, which converts a proposition into conjunctive normal form (POS) as a set of clauses, each of which is a set of literals (a special subset of prop)..
7.    dnf: prop -> prop set set,  which converts a proposition into disjunctive normal form (SOP) as a set of terms, each of which is a set of literals (a special subset of prop)..
8.    isTautology: prop -> bool, which checks if a proposition is a tautology
9.    isSatisfiable: prop -> bool, which checks if a proposition is satisfiable
10.   isEquivalent: prop -> prop -> bool, which checks if two propositions are logically equivalent
11.   entails: prop -> prop -> bool, which checks if the second proposition is a logical consequence of the first proposition.

*)


(*Define a new type prop*)
type prop =
| Atom of string
| T
| F
| Not of prop
| And of prop * prop
| Or of prop * prop
| Imply of prop * prop
| Iff of prop * prop;;

(*Define height and size functions on this tree of type prop*)
let rec ht p = match p with
| Atom s        -> 0
| T             -> 0
| F             -> 0
| Not p1        -> 1 + (ht p1)
| And (p1,p2)   -> 1 + max (ht p1) (ht p2)
| Or (p1, p2)   -> 1 + max (ht p1) (ht p2)
| Imply (p1,p2) -> 1 + max (ht p1) (ht p2)
| Iff (p1,p2)   -> 1 + max (ht p1) (ht p2);;

let rec size p = match p with
| Atom s        -> 1
| T             -> 1
| F             -> 1
| Not p1        -> 1 + (size p1)
| And (p1, p2)  -> 1 + (size p1) + (size p2)
| Or (p1, p2)   -> 1 + (size p1) + (size p2)
| Imply (p1, p2)-> 1 + (size p1) + (size p2)
| Iff (p1, p2)  -> 1 + (size p1) + (size p2);;

let a1 = T;;
let a2 = Atom "iamgreat";;
let a3 = Not a2;;
let a4 = Imply(a2, a3);;

let rec atoms p = match p with
| Atom s        -> [s]
| T             -> []
| F             -> []
| Not p1        -> (atoms p1)
| And (p1,p2)   -> (atoms p1) @ (atoms p2)
| Or (p1,p2)    -> (atoms p1) @ (atoms p2)
| Imply (p1,p2) -> (atoms p1) @ (atoms p2)
| Iff (p1,p2)   -> (atoms p1) @ (atoms p2);;

(*Print Stuff*)
List.iter print_endline (atoms a4);;

print_endline "Size of a4:";;
print_int (size a4);;
print_endline "";;

print_endline "Height of a4:";;
print_int (ht a4);;
print_endline "";;


let conjunction x y = match (x,y) with
| (false, _) -> false
| (true, z)  -> z;;

let disjunction x y = match (x,y) with
| (true, _)  -> true
| (false, z) -> z;;

let implication x y = match (x,y) with
| (false, _) -> true
| (true, z)  -> z;;

let iff x y = x = y;;

let rho s = match s with
| "iamgreat" -> true
| "hello" -> true
| "saurabh" -> true
| z -> false;;

let rec truth p rho = match p with
| Atom s        -> rho s
| T             -> true
| F             -> false
| Not p1        -> not (truth p1 rho)
| And (p1, p2)  -> conjunction (truth p1 rho) (truth p2 rho)
| Or (p1, p2)   -> disjunction (truth p1 rho) (truth p2 rho)
| Imply (p1,p2) -> implication (truth p1 rho) (truth p2 rho)
| Iff (p1, p2)  -> iff (truth p1 rho) (truth p2 rho);;

let b = (truth a4 rho);;

print_endline "Value of b:";;
if b then print_endline "True!" else print_endline "False!";;
print_endline "";;

(*nnf function: returns a prop in negation normal form*)
let rec nnf p = match p with
| Atom s -> p
| T -> p
| F -> p
| And (p1, p2) -> And (nnf p1, nnf p2)
| Or (p1, p2) -> Or (nnf p1, nnf p2)
| Not p1 -> (match p1 with
            | Atom s -> p
            | T -> F
            | F -> T
            | Not p11 -> nnf p11
            | And (p11, p12) -> Or (nnf (Not p11), nnf (Not p12))
            | Or (p11, p12) -> And (nnf (Not p11), nnf (Not p12))
            | Imply (p11, p12) -> And (nnf p11, nnf (Not p12))
            | Iff (p11, p12) -> Or (And (nnf p11, nnf p12), And (nnf (Not p11), nnf (Not p12))))
| Imply (p1, p2) -> Or (nnf (Not p1), nnf p2)
| Iff (p1, p2) -> nnf (And (Imply (p1, p2), Imply (p2, p1)))


let rec distr_or p1 p2 = match (p1, p2) with
| (p, And (p21, p22)) -> And (distr_or p p21 , distr_or p p22)
| (And (p11, p12), p) -> And (distr_or p11 p, distr_or p12 p)
| (p1, p2) -> Or (p1, p2)

let rec cnf_prop p = match p with
| Atom s -> p
| T -> p
| F -> p
| Not p1 -> p
| And (p1, p2) -> And (cnf_prop p1, cnf_prop p2)
| Or (p1, p2) -> distr_or (cnf_prop p1) (cnf_prop p2);;


let rec flatten_or_cnf p = match p with 
 Or (p1, p2) -> (flatten_or_cnf p1) @ (flatten_or_cnf p2)
| l -> [l];;

let rec flatten_and_cnf = function
| And (p1, p2) -> (flatten_and_cnf p1) @ (flatten_and_cnf p2)
| l -> [flatten_or_cnf l];;

(*cnf function: prop -> prop set set returns cnf of a given propositions as a list of list containing unique elements*)
let cnf p = (flatten_and_cnf (cnf_prop (nnf p)));;


let rec distr_and p1 p2 = match (p1,p2) with
  | (p, Or (p21, p22)) -> Or (distr_and p p21, distr_and p p22)
  |  (Or (p11, p12), p) -> Or (distr_and p11 p, distr_and p12 p)
  | (p1, p2) -> And (p1, p2) ;;



let rec flatten_and_dnf p = match p with 
 And (p1, p2) -> (flatten_and_dnf p1) @ (flatten_and_dnf p2)
| l -> [l];;

let rec flatten_or_dnf = function
| Or (p1, p2) -> (flatten_or_dnf p1) @ (flatten_or_dnf p2)
| l -> [flatten_and_dnf l];;
        
let rec dnf_prop p = match p with
| Atom s -> p
| T -> p
| F -> p
| Not p1 -> p
| And (p1, p2) -> distr_and (dnf_prop p1)  (dnf_prop p2)
| Or (p1, p2) -> Or (dnf_prop p1, dnf_prop p2);;

(*dnf function: prop -> prop set set returns dnf of a given propositions as a list of list containing unique elements*)
let dnf p = (flatten_or_dnf (dnf_prop (nnf p)));;

let rec is_positive c = match c with
    [] -> false
    | h::t -> if (truth h rho) = true then true else (is_positive t);;
  

 (* isTautology function: checks if a given proposition is tautology*)
let  isTautology p =
    let p1 = cnf p in
    List.fold_left (fun a c -> is_positive c && a) true p1;;

(*isSatisfiable function: checks if a proposition is satisfiable *)
let isSatisfiable fm = not (isTautology (Not fm));;

(*isEquivalent function: checks if two propositions are logically equivalent*)
let isEquivalent p1 p2 = isTautology( Iff (p1,p2) );;

(*entails function: checks if the second proposition is a logical consequence of the first proposition.*)
let entails p1 p2 = isTautology (Imply (p1, p2));;



let rec print_prop p = match p with
       | Atom a ->       print_string ("'" ^ a ^ "'") ;
           | T      ->       print_string ("T")
       | F      ->       print_string ("F")
       | Not p  ->       print_string ("![");  print_prop p;
                 print_string "]"
       | And (p1, p2) -> print_string "(";print_prop p1;
                 print_string " ^ " ; print_prop p2;
                 print_string ")"
       | Or (p1, p2) ->  print_string "(";print_prop p1;
                 print_string " v " ; print_prop p2;
                 print_string ")"
       | Imply (p1, p2) ->  print_string "(";print_prop p1;
                 print_string " -> " ; print_prop p2;
                 print_string ")"
                     
       | Iff (p1, p2) ->  print_string "(";print_prop p1;
                  print_string " = " ; print_prop p2;
                  print_string ")";;
             
let expr1 = Imply (Not (Atom "Saurabh"),
               And (Iff (Not (And (Atom "hello", Not F)),
                F),
               Or(Atom "heyy", Not T)
        ));;

let expr2 = Not (And (Atom "Saurabh", Not T));;
let expr3 = Or (And (T, F), And (T, Not F));;
nnf expr1;;
nnf expr2;;

print_endline " ";;
print_prop expr1;;
print_endline " ";;
print_prop (nnf expr1);;
print_endline " ";;
print_prop (cnf_prop (nnf expr1));;
print_endline " ";;
print_prop expr2;;
print_endline " ";;
print_prop (nnf expr2);;
print_endline " ";;
print_prop (cnf_prop (nnf expr2));;
print_endline " ";;
print_prop expr3;;
print_endline " ";;
print_prop (nnf expr3);;
print_endline " ";;
print_prop (cnf_prop (nnf expr3));;
print_endline " ";;
print_endline " ";;
    
