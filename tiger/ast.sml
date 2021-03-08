structure Tiger = 
struct

	(* The id's used for identification of variable names, functions*)
	type id      = string
	datatype type_id = Sub of id

	(* These are the parameters of the function, and the methods inside the classes*)
	datatype tyfields = Params of (id * type_id) list
 	
	(* The the program in tiger language is either an exp or some declarations*)
 	datatype ast = exp | decs



	datatype exp = (*Literals*)
				   NilExpr
				 | SExpr of string
				 | IExpr of int

				 (* Array declarations*)
				 | ArrayExpr of { Type : type_id, Size : exp , Initial :  exp list}
				 
				 (*Record Creation*)
				 | RecordExpr of { Type : type_id, Values : ( id * exp) list }

				 (*Object Creation*)
				 | ObjectExpr of type_id

				 (* Variable, fields, element is an array*)
				 | LValueExpr of lvalue

				 (* Function Call*)
				 | FuncCallExpr of {Id : id, Param : exp list}

				 (*Method Call *)
				 | MethodCallExpr of {Lvalue : lvalue , Id : id, Param : exp list}

				 (*Operations*)
				 | NegationExpr of exp
				 | BinOpExpr of exp * BinOp * exp
				 | SeqExpr of exp list

				 | AssignExpr of lvalue * exp

				 (*Control Statements*)
				 | IfExpr of {Cond : exp, Do   : exp, Else   : exp option } 
				 | WhileExpr of   { Cond : exp, Do : exp}
				 | ForExpr of {StartPos : exp, EndPos : exp, Do : exp  }
				 | BreakExpr 
				 | LetExpr of (dec list) * (exp list)

	(* Name of variables, elements, arrays, function, methods etc *)
    and lvalue   = id
    			 (*Getting method or variable of an object of a class*)
    	 		 | ObjEl of lvalue * id

    	 		 (*Getting element of an array*)
    	 		 | ArrEl of lvalue * exp

   	(* This is the type of declarations that we can have in this language*)
    and dec      = 
    			 (* Type Declarations *)
    			   TypeDec of id * ty

 				 (* Class Definition *)
 				 | ClassDec of {Id : id , Extends : type_id option , Classfields : classfield list}

 				 (* Variable Declaration *)
 				 (*| VarDec of id * type_id option * exp*)
 				 | VarDec of id * type_id option * exp

 				 (* Function Declaration *)
 				 | FuncDecs of FuncDec list

 	(* 
 	Classes are made following the EPITA reference manual
 	The things inside the class
		1. Methods
		2. Type definitions
 	*)
 	and classfield = (*There are either Variable Declarations in the Class*)
 					 ClassVarDec of id * type_id option * exp

 					 (*Or method declarations*)
 				     | MethodDef of {Id : id , Param : tyfields, Return : type_id option, Body: exp}


 	(*The Possible types while assigning a type to a variable - which values can possibly assigned to type variables*)
 	and ty  = AlreadyDef of type_id
 			(*An array of assignments*)
			| SeqDef of tyfields
			| ArrayDef of type_id
			| ClassDef of {Id : id , Extends : type_id option , Classfields : classfield list}

	(* The AST is made under the assumption that the compiler at the end, will suppor mutual recursion of functions*)
	and FuncDec = fundec of {Id : id, Param : tyfields, RetType : type_id option, Body : exp }

	(* The Binary Operations *)
	and BinOp    = Plus 		  (*+*)
		  		 | Minus		  (*-*)
 		  		 | Mul			  (***)
 		  		 | Div			  (*/*)
 		  		 | Equals		  (*=*)
 		  		 | AngBrac		  (*<>*)
 		  		 | GreatThan 	  (*>*)
 		  		 | LessThan       (*<*)
 		  		 | GreatEqualThan (*>=*)
 		  		 | LessEqualThan  (*<=*)
 		  		 | And	    	  (*&*)
 		  		 | Or  			  (*|*)



end