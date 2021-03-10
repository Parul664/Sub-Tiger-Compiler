structure Tiger = 
struct

	(* The id's used for identification of variable names, functions*)
	type id      = string
	datatype type_id = Sub of id

	(* These are the parameters of the function, and the methods inside the classes*)
	datatype tyfields = Params of (id * type_id) list
 	
	(* The the program in tiger language is either an exp or some declarations*)
 	datatype ast = E of exp | D of dec list



	and exp = (*Literals*)
				   NilExpr
				 | SExpr of string
				 | IExpr of int

				 (* Array creation*)
				 | ArrayExpr of { Type : type_id, Size : exp , Initial :  exp list}
				 
				 (*Record Creation*)
				 | RecordExpr of { Type : type_id, Values : ( id * exp) list }

				 (* Variable, records, arrays for instance array[2]*)
				 | LValueExpr of lvalue

				 (* Function Call*)
				 | FuncCallExpr of {Id : id, Param : exp list}

				 (*Operations*)
				 | NegationExpr of exp
				 | BinOpExpr of exp * BinOp * exp

				 (*A sequence of expressions*)
				 | SeqExpr of exp list

				 | AssignExpr of lvalue * exp

				 (*Control Statements*)
				 | IfExpr of {Cond : exp, Do   : exp, Else   : exp option } 
				 | WhileExpr of   { Cond : exp, Do : exp}
				 | ForExpr of {StartPos : exp, EndPos : exp, Do : exp  }
				 | BreakExpr 
				 | LetExpr of (dec list) * (exp list)

	(* Name of variables, array elements and record fields etc *)
    and lvalue   = SimpleVar of id
  					
  		         (* Variables relating to records *)
  				 | FeildVar of lvalue * id

    	 		 (*Life if array is a, then a[2]*)
    	 		 | ArrEl of lvalue * exp

   	(* This is the type of declarations that we can have in this language*)
    and dec      = 
    			 (* Type Declarations like type a = int*)
    			   TypeDec of id * ty

				 (* Variable Declaration *)
 				 (* like var a : int = 5*)
 				 | VarDec of id * type_id option * exp

 				 (* Function Declaration = Includes recursive functions
					function f() : int = g(a)
					function g(i: int) = f()
 				 *)
 				 | FuncDecs of FuncDec list


 	(*The Possible types while assigning a type to a variable - which values can possibly assigned to type variables*)
 	and ty = (*Already defined variabes, like using 'a' which is defined earlier*)
 			  AlreadyTy of type_id
 			(* An array of assignments corresponding to that of record *)
			| SeqTy of tyfields
			(* 'array of' some type*)
			| ArrayTy of type_id

	(* The AST is made under the assumption that the compiler at the end, will suppor mutual recursion of functions*)
	and FuncDec = fundec of {Id : id, Param : tyfields, RetType : type_id option, Body : exp }

	(* The Binary Operations *)
	and BinOp    = Plus 		  	(*+*)
		  		 | Minus		  	(*-*)
 		  		 | Mul		  	    (***)
 		  		 | Div		  	    (*/*)
 		  		 | Equals		  	(*=*)
 		  		 | AngBrac	  	    (*<>*)
 		  		 | GreatThan 	  	(*>*)
 		  		 | LessThan       	(*<*)
 		  		 | GreatEqualThan   (*>=*)
 		  		 | LessEqualThan    (*<=*)
 		  		 | And	    	    (*&*)
 		  		 | Or  			    (*|*)



end
