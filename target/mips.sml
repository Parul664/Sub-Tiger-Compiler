structure mips = 
struct

	(* Modelling the 32 registers in MIPS using datatype Reg*)
	datatype regs = zero 
				  | at 
				  | v0 | v1
				  | a0 | a1 | a2 | a3 
				  | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9
				  | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7
				  | k0 | k1
				  | gp | fp | sp | ra

	(* 
	Capturing all the instructions in MIPS by datatypes inst.
		1. Float Instructions are not captures since our compiler will at end be only 
		   capable of dealing with integers
	
	- Since, till now the register allocation is not taken into care, we have introduced
	the variables 'l and 't to the datatype, any label and any register later.
	
	- I used capital letters to name the constructs, since some instructions in mips are 
	reserved keywords in sml
	*)
	datatype ('l, 't) inst = (*Load and Store Instructions*)
							la of 't * 'l         
						  | lb of 't * 'l         
						  | lbu of 't * 'l        
						  | ld of 't * 'l         
						  | lh of 't * 'l         
						  | lhu of 't * 'l        
						  | lw of 't * 'l         
						  | lwcz of 't * 'l       
						  | lwl of 't * 'l        
						  | lwr of 't * 'l        
						  | sb of 't * 'l         
						  | sd of 't * 'l         
						  | sh of 't * 'l         
						  | sw of 't * 'l         
						  | swcz of 't * 'l       
						  | swl of 't * 'l        
						  | swr of 't * 'l        
						  | ulh of 't * 'l        
						  | ulhu of 't * 'l       
						  | ulw of 't * 'l        
						  | ush of 't * 'l        
						  | usw of 't * 'l        

						  (*Exception and Trap Instructions *)
						  | rfe 
						  | syscall
						  | break
						  | nop

						  (*Constant Manipulation Instructions *)
						  | li of 't * int  
						  | lui of 't * int

						  (*Arithimatic and Logical Instructions*)
						  | abs of 't * 't
						  | add of 't * 't * 't
						  | addi of 't * 't * int
						  | addu of 't * 't * 't
						  | addiu of 't * 't * int
						  | and_	 of 't * 't * 't   
						  | andi of 't * 't * int
						  | div of 't * 't * 't
						  | divu of 't * 't * 't
						  (* 2 anonymous in ref*)
						  | mul of 't * 't * 't
						  | mulo of 't * 't * 't
						  | mulou of 't * 't * 't
						  | mult of 't * 't
						  | multu of 't * 't
						  | neg of 't * 't 
						  | negu of 't * 't
						  | nor of 't * 't * 't
						  | not of 't * 't
						  | or of 't * 't * 't
						  | ori of 't * 't * int
						  | rem of 't * 't * 't
						  | remu of 't * 't * 't
						  | rol of 't * 't * 't
						  | ror of 't * 't * 't
						  | sll of 't * 't * 't
						  | sllv of 't * 't * 't
						  | sra of 't * 't * 't
						  | srav of 't * 't * 't
						  | srl of 't * 't * 't
						  | srlv of 't * 't * 't
						  | sub of 't * 't * 't
						  | subu of 't * 't * 't
						  | xor of 't * 't * 't
						  | xori of 't * 't * int

						  (*Comparison instructions*)
						  | seq of 't * 't * 't
						  | sge of 't * 't * 't
						  | sgeu of 't * 't * 't
						  | sgt of 't * 't * 't
						  | sgtu of 't * 't * 't
						  | sle of 't * 't * 't
						  | sleu of 't * 't * 't
						  | slt of 't * 't * 't
						  | slti of 't * 't * int
						  | sltu of 't * 't * 't
						  | sltiu of 't * 't * int
						  | sne of 't * 't * 't

						  (*Branch and Jump instructions*)
						  | b of 'l
						  | bczt of 'l
						  | bczf of 'l
						  | beq of 't * 't * 'l
						  | beqz of 't * 'l
						  | bge of 't * 't * 'l
						  | bgeu of 't * 't * 'l
						  | bgez of 't * 'l
						  | bgt of 't * 't * 'l
						  | bgtu of 't * 't * 'l
						  | bgtz of 't * 'l
						  | ble of 't * 't * 'l
						  | bleu of 't * 't * 'l
						  | blez of 't * 'l
						  | bgezal of 't * 'l
						  | bltzal of 't * 'l
						  | blt of 't * 't * 'l
						  | bltu of 't * 't * 'l
						  | bltz of 't * 'l
						  | bne of 't * 't * 'l
						  | bnez of 't * 'l 
						  | j of 'l
						  | jal of 'l
						  | jalr of 't
						  | jr of 't

						  (*Data movement*)
						  | mfhi of 't
						  | mflo of 't
						  | mthi of 't
						  | mtlo of 't
						  | mfcz of 't * 't
						  | mtcz of 't * 't



end