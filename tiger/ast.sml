structure Tiger = 
struct

	(* The id's used for identification of variable names, functions*)
	type id      = string
	datatype type_id = Sub of id

	(* These are the parameters of the function, and the methods inside the classes*)
	datatype tyfields = TF of (id * type_id) list
 	
	(* The the program in tiger language is either an exp or some declarations*)
	(* Since, we are not making support for import in this language, I directly took exp
	   as the ast for this language
	*)

	datatype exp = (*Literals*)
				   NilExpr
				 | SExpr of string
				 | IExpr of int

				 (* Array creation*)
				 | ArrayExpr of { Type : type_id, Size : exp , Initial :  exp}
				 
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
				 | ForExpr of {Id : id, StartPos : exp, EndPos : exp, Do : exp  }
				 | BreakExpr 
				 | LetExpr of (dec list) * (exp list)

	(* Name of variables, array elements and record fields etc *)
    and lvalue   = SimpleVar of id
  					
   				 (* Like if record is a = {b:c,d:e}, then b variable of array a*)
				 | FeildVar of lvalue * id

   				 (*Life if array is a, then a[2]*)
				 | ArrEl of lvalue * exp

   	(* This is the type of declarations that we can have in this language*)
    and dec      = 
				 (* Type Declarations like type a = int*)
				 TypeDec of id * ty

				 (* Variable Declaration *)
				 (*| VarDec of id * type_id option * exp like var a : int = 5*)
 				 | VarDec of id * type_id option * exp

				 (* Function Declaration 
					function f() : int = g(a)
					function g(i: int) = f()
 				 *)
				 | FuncDecs of FuncDec list


	(*The Possible types while assigning a type to a variable - which values can possibly assigned to type variables*)
 	and ty = (*Already defined variabes, like using 'a' which is defined earlier*)
 			  AlreadyTy of type_id
 			(*An array of assignments like that of record*)
			| SeqTy of tyfields
			(* 'array of' some type*)
			| ArrayTy of type_id

	(* The AST is made under the assumption that the compiler at the end, will suppor mutual recursion of functions*)
	and FuncDec = fundec of {Id : id, Param : tyfields, RetType : type_id option, Body : exp }

	(* The Binary Operations *)
	and BinOp    = Plus 			(*+*)
				 | Minus			(*-*)
				 | Mul				(***)
				 | Div				(*/*)
				 | Equals			(*=*)
				 | AngBrac			(*<>*)
				 | GreatThan		(*>*)
				 | LessThan			(*<*)
				 | GreatEqualThan	(*>=*)
				 | LessEqualThan	(*<=*)
				 | And				(*&*)
				 | Or				(*|*)



	(* -------------------------helper functions------------------- *)
	
	(* Functions used in tiger.grm to give semantic values to Grammer Expressions  *)

	(* Convert Array Declaration to AST *)
	fun ConvArrAST id sz init = ArrayExpr { Type    = id,
											Size    = sz,
											Initial = init
										  }

	(* Convert Record Declaration to AST *)
	fun ConvRecAST id values = RecordExpr { Type   = id,
											Values = values
										  }

	(* Convert Function call to AST *)
	fun ConvFunCallAST id param = FuncCallExpr { Id    = id,
											   	 Param = param
											   }

	(* Convert all binary Operations to AST *)
	fun plus a b        = BinOpExpr (a,Plus,b)
	fun minus a b       = BinOpExpr (a, Minus, b)
	fun mul a b         = BinOpExpr (a,Minus,b)
	fun divv a b        = BinOpExpr (a, Div, b)
	fun equals a b      = BinOpExpr (a, Equals, b)
	fun angbrac a b     = BinOpExpr (a, AngBrac, b)
	fun greaterThan a b = BinOpExpr (a, GreatEqualThan,b)
	fun lessThan a b    = BinOpExpr (a, LessThan,b)
	fun greatEqualThan a b = BinOpExpr (a, GreatEqualThan, b)
	fun lessEqualThan a b = BinOpExpr (a, LessEqualThan, b)
	fun andd a b        = BinOpExpr (a, And, b)
	fun orr a b         = BinOpExpr (a, Or, b)
	
	(* Convert If statement to AST *)
	fun ConvIfElseAST cond doo elsee = IfExpr { Cond = cond,
										 	   Do    = doo,
										 	   Else  = SOME(elsee)
	
										  	}
	(* If statement without an else *)
	fun ConvIfAST cond doo = IfExpr { Cond = cond,
									  Do   = doo,
									  Else = NONE
									}
	(* While Loop Statement *)
	fun ConvWhileAST cond doo = WhileExpr { Cond = cond,
										  Do   = doo
  										  }

	(* For Loop Statement *)
	fun ConvForAST id sInd eInd doo = ForExpr { Id   	 = id,
												StartPos = sInd,
												EndPos   = eInd,
												Do       = doo  
											  }

	(* Let Statement *)
	fun ConvLetAST decList expList = LetExpr (decList, expList)

	(* Function definition that has a return value *)
	fun ConvFunDecRetAST id param ret body = fundec { Id = id, 
													  Param = param, 
													  RetType = SOME(ret), 
													  Body = body
													}
	
	(* Function Defintion that does not have a return value *)
	fun ConvFunDecAST id param body = fundec { Id = id, 
											   Param = param, 
											   RetType = NONE, 
											   Body = body
											 }

	(* Variable defining with explicit type given eg var a : int = 5*)
	fun ConvVarDecTy id typeid ex = VarDec (id, SOME(typeid), ex)
	(* Variable without explicit return type eg var a = 5*)
	fun ConvVarDec id ex = VarDec (id, NONE, ex)

	(* For converting simple string to a type_id *)
	fun TC s = Sub s

end

