(*
Assignment 5: Prolog Interpreter

In this assignment, you will write a simplified version of a Prolog interpreter in OCaml.

You will first define an ML data type to represent the structure of a legitimate Prolog program.

    A program is a set (list) of clauses.
    A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body. 
    The head is a single atomic formula.  A body is a sequence of atomic formulas.
    An atomic formula is a k-ary predicate symbol followed by k terms.
    A term is either a variable, a constant, or a k-ary function symbol with k subterms.
    A goal is a set (list) of atomic formulas.


You need to take your implementation of unification to use as the parameter-passing mechanism. (Note: by pretending the predicate symbol is a function symbol, you can perform resolution of goals and program clauses).

You also need to develop a back-tracking strategy to explore the resolution search space.   You need to be able to replace a goal by subgoals, as found by applying a unifier to the body of a program clause whose head unified with the chosen subgoal.
*)
#load "str.cma";;

(*
 * Define types:
 * A program is a set (list) of clauses.
 * A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body. 
 * The head is a single atomic formula.  A body is a sequence of atomic formulas.
 * An atomic formula is a k-ary predicate symbol followed by k terms.
 * A term is either a variable, a constant, or a k-ary function symbol with k subterms.
 * A goal is a set (list) of atomic formulas.
 *)
type symbol    = string 
and variable   = string 
and atom       = symbol * (term list)
and term       = Var of variable | Atom of atom | Const of string 
and head       = Atom of atom
and body       = head list
and clause     = Fact of head | Rule of head * body 
and program    = clause list
and goal       = head list;;


exception NOT_UNIFIABLE;;
exception INVALID_SUBSTITUTION;;

(* term examples *)
let t1 = Var "X"
let t2 = Const "sunny"
let t3 = Atom ("married", [Const "pandu"; Const "kunti"])
let t4 = Atom ("married", [Const "kunti"; 
			   Atom ("child", [Const "vyasa"])])
let t5 = Atom ("str1", [Var "X"; Atom ("str2", []); Var "Y"])
let t6 = Atom ("str1", [Atom ("str3", [Atom ("str4", []); Atom ("str4", [])]); Atom ("str2", []); Var "Z"])
(* clause examples *)
let c1 = Fact (Atom ("married", [Const "pandu"; Const "kunti"]))
let c2 = Rule (Atom ("grandfather", [Var "X"; Var "Y"]) , 
	       [ Atom ("father", [Var "X"; Var "Z"]) ; 
		 Atom ("parent", [Var "Z"; Var "Y"]) ])

(* function to find out most general unifier of two terms *)
let mgu t1 t2 = 
  let rec occurs x = function
    | Var y          ->  x = y
    | Const y        -> false
    | Atom (_, list) ->  List.exists (occurs x) list in
  let rec unify list = match list with 
    | []             -> []
    | (x,y) :: tl    -> let t2 = unify tl in 
			let t1 = unify_one x y in
			t1 @ t2
  and unify_one s t = match (s,t) with
    | (Var x, Var y) -> if x = y then [] else [(x, t)]
    | (Var x, (Atom (_, list) as t)) | ((Atom (_, list) as t), Var x) -> if occurs x t then 
					      raise NOT_UNIFIABLE
				           else [(x,t)]
    | (Atom (f,list1), Atom (g,list2)) -> if (f = g) &&
					      (List.length list1
					       = List.length list2)
					   then 
					     unify (List.combine list1 list2)
					 else
					   raise NOT_UNIFIABLE
    | (Const x , Const y)     -> if x = y then [] 
				 else raise NOT_UNIFIABLE

    | (Var x, (Const _ as t)) | ((Const _ as t), Var x) -> [(x,t)]

    | (Const x, Atom (_, list))
    | (Atom (_, list), Const x) -> raise NOT_UNIFIABLE
  in unify_one t1 t2;;

(* helper function to print a term *)
let rec print_term = function
  | Var x          -> print_string ("Var '"    ^ x ^ "' ")
  | Const x        -> print_string ("Const '"  ^ x ^ "' ")
  | Atom (f, [])   -> print_string ("Atom ( "  ^ f ^ " " );
		      print_string " )" 
  | Atom (f, list) -> print_string ("Atom ( "  ^ f ^ ", "); 
		      print_string "[ ";
		      print_term_list list;
		      print_string "] ";
		      print_string " )" 

(* helper function to print a term list*)
and print_term_list = function
  | [] -> print_string " "
  | [hd] -> print_term hd; 
		print_string " "
  | hd :: tl -> print_term hd; print_string "; ";
		print_term_list tl;;


(* helper function to print a substitution type variable *)
let rec print_subst = function
  | [] -> print_endline " "
  | (var, (Atom (f, list) as t)) :: tl
    -> print_string ("Var " ^ var ^ ": ");
       print_term t; print_subst tl
  | (var, ((Const x) as t)) :: tl -> print_string ("Var " ^ var ^ ": ");
				   print_term t; print_subst tl
  | (var, ((Var x) as t)) :: tl -> print_string ("Var " ^ var ^ ": ");
				   print_term t; print_subst tl ;;

let rec substitute var subs term = match subs with 
| [] -> term
| (x, t) :: tl -> if x = var then t else substitute var tl term


let rec apply subs term = match term with 
  | Const x -> Const x
  | Var x -> substitute x subs term
  | Atom (str, list) -> Atom (str, List.map (apply subs) list);;






print_string ("Examples:\n\nTerm 1\n");;
print_term t1;;
print_string ("\n\nTerm 2:\n");;
print_term t2;;
print_string ("\n\nTerm 3:\n");;
print_term t3;;
print_string ("\n\nTerm 4:\n");;
print_term t4;;
print_string ("\n\nTerm 5:\n");;
print_term t5;;
print_string ("\n\nTerm 6:\n");;
print_term t6;;

let m1 = mgu t1 t3
let m2 = mgu t1 t4
let m4 = mgu t3 t1;;
let m7 = mgu t5 t6;;

print_string ("\n\nMGU gives a list of (Variables : Terms to replace them with)\n\nMGU of Terms 1 and 3:\n");;
print_subst m1;;
print_string ("\n\nMGU of Terms 1 and 4:\n");;
print_subst m2;;
print_string ("\n\nMGU of Terms 3 and 1:\n");;
print_subst m4;;
print_string ("\n\nMGU of Terms 5 and 6:\n");;
print_subst m7;;

let t10 = apply m7 t5
let t11 = apply m7 t6;;
print_string ("\nPrinting t5 after applying substitution to unify it with t6:\n");;
print_term t10;;
print_string ("\nPrinting t6 after applying substitution to unify it with t5:\n");;
print_term t11;;  
print_endline " ";;


let p1 = [Fact (Atom ("married", [Const "pandu"; Const "kunti"]));
	  Fact (Atom ("married", [Const "arjun"; Const "draupadi"]));
	  Fact (Atom ("child", [Const "arjun"; Const "pandu"]));
	  Fact (Atom ("child", [Const "arjun"; Const "kunti"]));
	  Fact (Atom ("child", [Const "abhimanyu"; Const "arjun"]));
	  Rule (Atom ("grandparent", [Var "X"; Var "Y"]) , 
	          [Atom ("child", [Var "X"; Var "Z"]) ; 
		   Atom ("child", [Var "Z"; Var "Y"]) ]) ];;


let q1 = [Atom ("child", [Const "arjun"; Const "kunti"]) ] ;; 

let q2 = [Atom ("child", [Var "abhimanyu"; Var "Z"]) ];;
(*
let solve querylist program = match querylist with
  | [] -> true
  | hd :: tl -> lookup hd program; solve tl program;;
 *)
let vars term = 
  let rec collect term l = match term with
    | Const _ -> l
    | Var x   -> x :: l 
    | (Atom (_, list)) -> List.fold_left (fun ls t -> collect t ls) l list
  in collect term []
(*
let lookup query program = match query with
  | Const x -> false
  | Var x -> false
  | Atom (str, list) -> if ((vars query) = [])
			then List.mem (Fact (query)) program 
			else false


let m3 = mgu t2 t3
let m5 = mgu t2 t1
let m6 = mgu t3 t2

 *)

(*
let check_file_name file = 
  if (file <> "") 
  then ((String.compare file ".pl") = 0)
  (*then ((String.sub file ((String.length file) - 4) 3) = ".pl")*) 
  else false

let add_fact_to_db (clause:string) (prog:program) : program = 
  try
    ignore(Str.search_forward (Str.regexp "gita$") clause 0);
    (*print_endline (clause ^ " is a const\n");*)
  with Not_found -> ();
  try
    (*let r = (Str.regexp (Str.search_forward "^\\(.*\\)(\\(.*\\))$") clause 0)in
    Str.replace_first r "\1" clause;*)
    print_endline ("\1" ^ "\2" ^  " " ^ clause ^ " is a 1-ary atom\n");
    List.append ([(Atom("\\1", [Const "\\2"]))])  prog;
  with Not_found -> ();
  try
    ignore(Str.search_forward (Str.regexp "^\w(\w,\w)$") clause 0);
    print_endline (clause ^ " is a 2-ary atom\n");
    (Atom("\\1", [Const "\\2"; Const "\\3"]) ) :: prog
  with Not_found -> ()

let add_rule_to_db clause prog =
  print_string "\nIn add rule to db\n";
  prog;;



let add_line_to_db line prog = 
  let strippedline = String.sub line 0 ((String.length line) - 1) in 
  if (String.contains strippedline ':')
  then prog = add_rule_to_db strippedline prog
  else prog = add_fact_to_db strippedline prog
  

let process file =
  (*print_int (String.length file);
  if (check_file_name file) *) 
    print_string ("File name: " ^ file ^ "\n");
    let prog = [] in
    let cin = open_in file in 
    try
      while true do
	 (*lines := input_line cin :: !lines*)
          let line = input_line cin in
             if (line <> "\n") 
	     then print_string (line);
	     add_line_to_db line prog 
      done
    with End_of_file ->
    close_in cin;;


let handle_query query = 
  print_string ("Query: " ^ query ^ "\n")



(*
let add_rule_to_db clause prog = match clause with
 *)		    


(*
 * Main function:
 * Reads input until a . is entered  
 *)
let main () = 
    while (true) do
      print_string "?- ";
      let inp = read_line () in
      if (inp = ".") then exit 0
      else
	  if (inp.[0] = '[') then
	    let file = String.sub inp 1 ((String.length inp) - 2) in
	    process file;
	  else 
	    handle_query inp
    done;;


(*let _ = main ()                              (* Call Main *)
 *)

  *)
