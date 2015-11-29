(*
Assignment 6: Interpreters for small functional languages

In this assignment, you will have to code in OCaml, interpreters for a small functional language which has constructs for  

    constants (integers, booleans, unit type), 
    operations on these data types such as +, *, and, or, not, =, 
    if_the_else
    tupling  and projection
    variables, 
    abstractions (\lambda-abstractions, functions) and 
    function application

Do NOT code these as Church numerals or the encodings given in class.


You need to 

    implement the abstract syntax of programs as a data type
    closures as a data type, with tables also defined in(in an intelligent way).
    Invent Op-codes and represent them as a data type
    implement stacks of each sort as necessary for the machines.


Task 1:

Now implement the  SECD machine for call-by-value evaluation of the language.

This consists of

    A compile function from the language to the SECD Op-codes.  You will need to specify (over and above the compile rules for the SECD machine discussed in class) compilation rules and op-codes for 
            loading constants, 
            compilation rules/op-codes for operations +, *, and, or, not, =
            compilation rules/op-codes for creating tuples
            compilation rules/op-codes for creating and applying projection functions. 
            compilation rules/op-code for if_then_else.  You may assume an opcode COND(c1,c2), described below. 
    Execution rules for the  SECD Op-codes.  You will need to specify (over and above the SECD machine discussed in class)  execution rules for the op-codes for 

        loading constants, 
        executing the opcodes for for +, *, and, or, not, =
        executing the op-codes for creating tuples
        executing the op-codes for applying the projection functions  
        executing the op_code COND(c1,c2)  for  the if_then_else construct.  It assumes a boolean on top of the stack, and accordingly executes code list c1 or c2, depending on whether that value is true or false. 
    Provide enough test cases (programs) to show that your machine works correctly.
    How will you show that a program returning a function works correctly?
    You may assume that all the  input programs are properly typed.  


Task 2:

Now implement the Krivine machine  for call-by-name discussed in class for ONLY the core lambda calculus.


    Define closures appropriately
    You may use the abstract syntax of the program given above
    Implement the "unload" function, that unravels the resulting closure into a lambda term. 
    Again you need to provide enough input cases.



AS A BONUS: you may extend the Krivine machine to incorporate the other constructs given above.

Again you will need to provide enough input cases.  You may assume that all programs you input are well-typed.


*)

type expression = 
  | Var  of     string
  | Abs  of     string * expression
  | Let  of expression * expression
  | App  of expression * expression
  | Int  of        int
  | Bool of       bool 
  | Unit of       unit
  | Add  of expression * expression
  | Sub  of expression * expression
  | Mul  of expression * expression
  | Div  of expression * expression
  | And  of expression * expression
  | Or   of expression * expression
  | Less   of expression * expression
  | Greater   of expression * expression
  | Not  of expression
  | Cond of expression * expression * expression
  | Fun  of string * string * value * value * expression (* Function [fun f(x:s):t is e] *)


and instruction = 
  | CONST of int 
  | LOAD  of value
  | VAR   of int
  | ADD
  | SUB
  | MUL
  | DIV
  | AND
  | OR
  | LESS
  | GREATER
  | NOT
  | LOOKUP of string
  | LET
  | ENDLET
  | APPLY
  | CALL
  | CLOS of           string * (instruction list)
  | COND of instruction list *  instruction list
  | RET

and value = 
  | VInt     of int
  | VBool    of bool
  | VClosure of (string * control * env)
  | VVar     of int
  | INIT

and closure  = (string * value) list 
 
and stack    =                   value list
and env      =        (string * value) list
and control  =             instruction list
and dump     = (stack * env * control) list
;;

exception VARIABLE_NOT_FOUND;;
exception NOT_BOOL;;

let e_init = [("var", INIT); ("x", VInt 3)]

(* returns value of x in the env e *)
(* val lookup : string -> env -> value = <fun> *)
let rec lookup x e = match e with
  | []           -> raise VARIABLE_NOT_FOUND
  | (s, v) :: ee -> if s = x then v else lookup x ee 

(* val calc : instruction -> value -> value -> value = <fun> *)
let calc op v1 v2 = match (op, v1,v2) with 
  | (ADD,     VInt  n1, VInt  n2) -> VInt  (n1 +  n2)
  | (MUL,     VInt  n1, VInt  n2) -> VInt  (n1 *  n2)
  | (SUB,     VInt  n1, VInt  n2) -> VInt  (n1 -  n2)
  | (DIV,     VInt  n1, VInt  n2) -> VInt  (n1 /  n2)
  | (AND,     VBool b1, VBool b2) -> VBool (b1 && b2)
  | (OR ,     VBool b1, VBool b2) -> VBool (b1 || b2)
  | (LESS ,   VInt  n1, VInt  n2) -> VBool (n1 <  n2)
  | (GREATER, VInt  n1, VInt  n2) -> VBool (n1 >  n2)
;;

(* val not_op : value -> value = <fun> *)
let not_op = function
  | VBool b -> VBool (not b)
  | _       -> raise NOT_BOOL
;;

(* compile an instruction and output a list of instructions *)
(* val compile : expression -> instruction list = <fun> *)
let rec compile = function 
  | Int  n          -> [LOAD (VInt  n)]
  | Bool b          -> [LOAD (VBool b)]
  | Var x           -> [LOOKUP(x)]
  | Abs  (x , e)    -> [CLOS (x, (compile e))] 

(*| Let ( v, e2)    -> compile (v)  @ [LET] @ compile (e2) @ [ENDLET]*)
  | App     (e1, e2)    -> compile (e1) @ compile (e2) @ [CALL]
  | Add     (e1, e2)    -> compile (e1) @ compile (e2) @ [ADD]
  | Mul     (e1, e2)    -> compile (e1) @ compile (e2) @ [MUL]
  | Div     (e1, e2)    -> compile (e1) @ compile (e2) @ [DIV]
  | Sub     (e1, e2)    -> compile (e1) @ compile (e2) @ [SUB]
  | And     (e1, e2)    -> compile (e1) @ compile (e2) @ [AND]
  | Or      (e1, e2)    -> compile (e1) @ compile (e2) @ [OR]
  | Less    (e1, e2)    -> compile (e1) @ compile (e2) @ [LESS]
  | Greater (e1, e2)    -> compile (e1) @ compile (e2) @ [GREATER]
  | Not  (e)            -> compile (e)  @ [NOT]
  | Cond (e, e1, e2)    -> compile (e)  @ [COND ((compile e1) @ [RET],
					      (compile e2) @ [RET] )]
;;


(* for each instruction in the compiled output, change the state *)
(* val step : stack -> env -> control -> dump -> value = <fun> *)
let rec step = function 

  | (      [n],    _,            [],         [])           -> n
  | (        s,    e,    LOAD(x)::c,          d)           -> step (               x::s,  e,  c,       d)
  | (        s,    e,  LOOKUP(x)::c,          d)           -> step (  (lookup x e) :: s,  e,  c,       d)
  | (        s,    e, CLOS(x,c1)::c,          d)           -> step ( VClosure (x, c1, e)::s, e, c,        d)
  | ((VClosure (str, c1, e1)) :: x :: s, e, CALL :: c, d)  -> step ( [], (str,x) :: e1, c1,  (s, e, c) :: d)
  | (v2::v1::s,    e,      (ADD)::c,          d)           -> step ((calc ADD v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,      (MUL)::c,          d)           -> step ((calc MUL v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,      (MUL)::c,          d)           -> step ((calc MUL v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,      (SUB)::c,          d)           -> step ((calc SUB v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,      (DIV)::c,          d)           -> step ((calc DIV v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,      (AND)::c,          d)           -> step ((calc AND v1 v2)::s,  e,  c,       d)
  | (v2::v1::s,    e,       (OR)::c,          d)           -> step ((calc  OR v1 v2)::s,  e,  c,       d)
  | (v::s,         e,      (NOT)::c,          d)           -> step (      (not_op v)::s,  e,  c,       d)
  | (v2::v1::s,    e,       (LESS)::c,        d)           -> step ((calc LESS v1 v2)::s, e,  c,       d)
  | (v2::v1::s,    e,       (GREATER)::c,     d)           -> step ((calc GREATER v1 v2)::s,  e,  c,      d)
  | ((VBool  true)::s,  e, COND(c1,c2)::c,    d)           -> step (                  s,  e, c1,  ([],[], c)::d)
  | ((VBool false)::s,  e, COND(c1,c2)::c,    d)           -> step (                  s,  e, c2,  ([],[], c)::d)
  | (        s,    e,  [RET],  (_, _, c) ::   d)           -> step (                  s,  e,  c,       d)
  | (       [],    e,  [RET],  (s, e1, c1) :: d)           -> step (                  s, e1, c1,       d)
  | (        s,    e,  LOOKUP(x)::c,          d)           -> step (    (lookup x e)::s,  e,  c,       d)
;;

(* call the step function with the code to be complied *)
(* val evaluate : instruction list -> value = <fun> *)
let evaluate t = step ([], e_init, t, []);;

(* val secd : expression -> value = <fun> *)
let secd expression = evaluate (compile (expression))

(* functions to print compiler output and evaluate output *)
let print_bool = function 
  | true  -> print_string "true"
  | false -> print_string "false"
;;


(* val cmplr_output : instruction list -> unit = <fun> *)
let rec print_value = function 
  | INIT        -> print_string ("INIT") 
  | VInt n      -> print_string ("VInt  "); print_int  n; 
  | VBool b     -> print_string ("VBool "); print_bool b; 
  | VClosure (x,c,e) -> print_string ("VClosure ( '" ^ x ^ "',  \nControl:\n"); 
			cmplr_output c;
			print_string "env: \n";
			print_env e;
			print_string ("\n  ) ")


(* print value with a new line *)
and print_value_n v = print_value v; print_endline " "

(* print a environment type (= string * value) *)
and print_env = function 
  | []            -> print_string ""
  | (x, v)  :: tl -> print_string ("[ '" ^ x ^ "', "); 
		     print_value v;
		     print_string " ]  ";
		     print_env tl

(* print a binary expression *)
and print_op op a b =     print_string  (op ^ " (");
			      print_input_expr        a;
			      print_string        ", " ; 
			      print_input_expr        b; 
			      print_string        ")"

(* print the input expression to be compiled *)
and print_input_expr = function 
  | Int  n           -> print_string ("Int  " ); print_int  n
  | Bool b           -> print_string ("Bool "); print_bool b
  | Var  x           -> print_string ("Var  " ^ x);
  | Abs (x, e)       -> print_string ("Abs  " ^ x ^ " "); print_input_expr e
  | App (e1, e2)     -> print_string ("App ( "); 
			print_input_expr e1; print_string (" ) ( ");
			print_input_expr e2; print_string (" ) ")
  | Add      (a, b)  -> print_op "Add"      a b
  | Mul      (a, b)  -> print_op "Mul"      a b
  | Sub      (a, b)  -> print_op "Sub"      a b
  | Div      (a, b)  -> print_op "Div"      a b
  | And      (a, b)  -> print_op "And"      a b
  | Or       (a, b)  -> print_op "Or"       a b
  | Less     (a, b)  -> print_op "Less"     a b
  | Greater  (a, b)  -> print_op "Greater"  a b
  | Not  (a)         -> print_string "Not ( "; print_input_expr a;print_string (" ) ")
  | Cond (e, e1, e2) -> print_cond e e1 e2 

(* print a condition statement, i.e., if then else *)
and print_cond e e1 e2 =      print_string ("Cond ( ");
			      print_input_expr e;
			      print_string (" ) Then ( ");
			      print_input_expr e1;
			      print_string (" ) Else ( ");
			      print_input_expr e2;
			      print_string (" )")


(* val cmplr_output : instruction list -> unit = <fun> *)
and cmplr_output = function 
  | []                    -> print_string "" 
  | [RET]                 -> print_string ("RET ;\n");
  | CONST  n      :: tl   -> print_string ("\nCONST "); 
		             print_int n; 
		             cmplr_output tl
  | LOAD   x      :: tl   -> print_string ("LOAD "); 
		             print_value x; 
		             print_string (";\n");
			     cmplr_output tl 
  | ADD           :: tl   -> print_string ("ADD ;\n");
			     cmplr_output tl
  | DIV           :: tl   -> print_string ("DIV ;\n");
			     cmplr_output tl
  | SUB           :: tl   -> print_string ("SUB ;\n");
			     cmplr_output tl

  | MUL           :: tl   -> print_string ("MUL ;\n");
			     cmplr_output tl
  | AND           :: tl   -> print_string ("AND ;\n");
			     cmplr_output tl
  | OR            :: tl   -> print_string ("OR  ;\n");
			     cmplr_output tl
  | LESS          :: tl   -> print_string ("LESS  ;\n");
			     cmplr_output tl
  | GREATER       :: tl   -> print_string ("GREATER  ;\n");
			     cmplr_output tl
  | NOT           :: tl   -> print_string ("NOT  ;\n");
			     cmplr_output tl
  | LOOKUP x      :: tl   -> print_string ("LOOKUP " ^ x ^ ";\n") ;
			     cmplr_output tl
  | RET           :: tl   -> print_string ("RET  ;\n");
  | CALL          :: tl   -> print_string ("CALL ;\n");
  | COND (c1, c2) :: tl   -> print_string ("COND ;\n");
			     cmplr_output c1;
			     cmplr_output c2;
			     cmplr_output tl
  | CLOS (x, c)   :: tl   -> print_string ("CLOS ( " ^ x ^ ", ");
			     cmplr_output c;
                             print_string (" ) ;\n");
			     cmplr_output tl
;;

(* function to print output of a test case *)
let test expr = 
  let c = compile  expr in 
  let v = evaluate c    in
  let s = secd     expr in
  print_string "\n\n***************************\nInput expression to compiler:\n";
  print_input_expr expr;
  print_string "\n\ncompiler function output:\n";
  cmplr_output c;
  print_string "\nevaluate function output:\n";
  print_value_n v;
  print_string "\nsecd function output:\n";
  print_value_n s;
  print_string ("\n")
;;

(* function to call the test function recursively for a list of expressions *)
let rec call_test exprlist = List.iter test exprlist
;;

(* list of expression to compile *)
let exprlist = [Add (Int(3), Int(4));
		Int(3);
		Mul (Int(5), Int(3));
		And (Bool(true), Bool(false));
		Less (Int(3), Int(4));		
		Var "x";
		Abs ("y", Int 2);
		Cond ((Bool true), (Add (Int(3), Int(4)))
		      , (Mul (Int(5), Int(3))));
	        Not (Bool false);
		Cond (Less(Int(3),Int(2)),Int 4
		      , (Div (Int(1), Int(0))) );
	       ]
;;

(* call test function for each of the expressions is the list 'exprlist' *)
call_test exprlist
;;


