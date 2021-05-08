
structure Canon = 
struct 

    structure T = Tree
    exception WrongParam of string

    fun linearize(stm0 : T.stm) : T.stm list =
    let 

        (* reorder : exp list ->(statement, expression list) *)
        fun reorder []           = (T.EXP (T.CONST 0),[])

          (* For the case there are multiple call statement, we need to move its result into a temporray *)
          | reorder (T.CALL (e1,el) :: l) 
                                  = let val nt = Temp.newtemp()
                                    in 
                                        reorder((T.ESEQ (T.MOVE (T.TEMP nt, T.CALL (e1,el)), T.TEMP nt))::l)
                                    end

          (* Else we would just check whether the the stmt and the exp commute, and in case they do, then we'll
            directly combine the s1,s2 , otherwise, we'll have to first put e1 into a temporary register before 
            executing s2, to maintain the order.
           *)
          | reorder (e :: el)     = let val (s1,er1) = do_exp e
                                        val (s2,er2) = reorder el
                                        (* The type of expressions mentioned between definitely commute, 
                                        for other we assum that they do not commute*)
                                        fun commute (T.EXP(T.CONST _),_) = true
                                          | commute (_, T.NAME _)        = true
                                          | commute (_, T.CONST _ )      = true
                                          | commute _                    = false
                                    in
                                    (case commute (s2,er1) of 
                                          true  => (case s1 of T.EXP (T.CONST _) => (s2,er1 :: er2) | a => (T.SEQ (a,s2), er1 :: er2))
                                        | false => (let val nt = Temp.newtemp () in 
                                                      (T.SEQ(s1, T.SEQ(T.MOVE (T.TEMP nt, er1),s2)),(T.TEMP nt)::er2)
                                                    end)
                                        )
                                    end 

        (* Calls reorder for expressions and substitute that into the fxn *)
        and reorder_exp (el, fxn) = let val temp = (reorder el) 
                                    in 
                                      (#1(temp),(fxn (#2(temp)))) 
                                    end
        
        (* Calls the reorder function on all the sub-expressions extracted and then puts it back into the expression *)
        and reorder_stm (el, fxn) = let val temp = (reorder el) 
                                        val s = fxn (#2(temp))
                                    in 
                                    (case (#1(temp)) of 
                                      T.EXP (T.CONST _) => s
                                    | a                 => T.SEQ(a,s)
                                    ) end



        (* This is the sub-expression extractors for the Tree.Exp structure. *)

        and do_exp (T.CONST a)           = reorder_exp([], fn [] => T.CONST a | _ => raise WrongParam "Wrong")
          | do_exp (T.NAME l )           = reorder_exp([], fn [] => T.NAME l | _ => raise WrongParam "Wrong")
          | do_exp (T.TEMP t)            = reorder_exp([], fn [] => T.TEMP t | _ => raise WrongParam "Wrong")
          | do_exp (T.BINOP (OP, e1,e2)) = reorder_exp([e1,e2], fn [e1, e2] => T.BINOP (OP, e1,e2) | _ => raise WrongParam "Wrong")
          | do_exp (T.MEM e)             = reorder_exp([e], fn [e] => (T.MEM e) | _ => raise WrongParam "Wrong")
          | do_exp (T.CALL (e ,el))      = reorder_exp(e::el, fn e::el => T.CALL (e ,el) | _ => raise WrongParam "Wrong")
          | do_exp (T.ESEQ (s, e))       = let val ds = do_stm s
                                               val (es, ls) = do_exp e in
                                            (T.SEQ(ds, es), ls)
                                            end


        (* Extracting all the expressions out of it, so that we can reorder them and then put is back into place *)
        (* So this function is actinf like a sub-expression extractor *)
        and do_stm (T.MOVE ((T.NAME l), e))             = reorder_stm([e], fn [e] =>  (T.MOVE (T.NAME l, e)) | _ => raise WrongParam "Wrong")
          | do_stm (T.MOVE ((T.TEMP t),(T.CALL(e,el)))) = reorder_stm((e::el), fn (e::el) => (T.MOVE (T.TEMP t,T.CALL(e,el))) | _ => raise WrongParam "Wrong")
          | do_stm (T.MOVE ((T.TEMP t), e))             = reorder_stm([e], fn [e] =>  (T.MOVE (T.TEMP t, e))| _ => raise WrongParam "Wrong")
          | do_stm (T.MOVE (e1,e2))                     = reorder_stm([e1,e2], fn [e1,e2] => T.MOVE(e1,e2) | _ => raise WrongParam "Wrong")
          (* Since, call is inside EXP, we don't need its value and so there is no need to move it into a register *)
          (* By doing this we hiding CALL statements from the Reorder function *)
          | do_stm (T.EXP (T.CALL (e,el)))              = reorder_stm ((e::el), fn (e::el) => (T.EXP (T.CALL (e,el))) | _ => raise WrongParam "Wrong")
          | do_stm (T.EXP (e1))                         = reorder_stm ([e1], fn [e1] => T.EXP(e1) | _ => raise WrongParam "Wrong")
          | do_stm (T.JUMP (e1, ll))                    = reorder_stm( [e1] , fn [e1] =>(T.JUMP (e1, ll)) | _ => raise WrongParam "Wrong")
          | do_stm (T.CJUMP (OP, e1, e2, l1, l2))       = reorder_stm([e1,e2], fn [e1,e2] => (T.CJUMP(OP, e1, e2, l1, l2)) | _ => raise WrongParam "Wrong")
          | do_stm (T.SEQ (s1, s2))                     = (T.SEQ (do_stm s1, do_stm s2))
          | do_stm (T.LABEL l)                          = (T.LABEL l)
    
    
        (* At the end all a seq of SEQ instructions is formed, which can be very well represented as a list.
          So, this function make a list by removing the SEQ constructs
         *)
        fun linear (T.SEQ (a,b),l) = linear (a, (linear (b, l)))
          | linear (s,l)           = s::l 
    
    in
        linear(do_stm stm0 , [])
    end

  (* Reverse the direction of the list, The blocks inside the list of basic blocks is reversed. 
  So for that purpose, I used rev function to reverse them *)    
  fun rev [] l =  l
    | rev (a :: ab) l = rev ab (a::l)
                                        
  fun (*This is the case when, we are done scanning the list, and so append the last block with done*)
      basicBlocks []  p  fl                =  let val done = Temp.newlabel()
                                                  val temp = (T.LABEL done)::p
                                              in fl @ [(rev temp [])] end

      (* 
      The below 4 cases signify the starting statement of a block, 
        1. Label - > initiate a new block
        2. CJUMP - > a complete block (LABEL, CJUMP)
        3. JUMP  - > a complete block (LABEL, JUMP)
        4. _     - > append a label and initiate a block
      *)
    | basicBlocks ( (T.LABEL t) :: l ) []  fl         = basicBlocks l [(T.LABEL t)] fl
    | basicBlocks ( (T.CJUMP (a,b,c,d,e)):: l ) [] fl = let val n = Temp.newlabel() in 
                                                        basicBlocks l [] (fl@ [[(T.LABEL n),(T.CJUMP (a,b,c,d,e))]]) end
    | basicBlocks ( (T.JUMP (a,b)):: l ) []  fl       = let val n = Temp.newlabel() in 
                                                        basicBlocks l [] (fl@ [[(T.LABEL n),(T.JUMP (a,b))]]) end
    | basicBlocks (s :: l)  [] fl                     = let val nl = Temp.newlabel() in
                                                        basicBlocks l [s , T.LABEL nl] fl end 
    (* 
    If we get a Label, then we need to start a new block given we have have 
    the last statement as Jump or CJump else, we need to append a jump
    So the next three cases take care of that
     *)
    | basicBlocks ( (T.LABEL t) :: l ) p   fl       = (*Add a jump in the present block and append a label in the next block*)
                                                      let val a = ((T.JUMP (T.NAME t, [ t])):: p)
                                                      in basicBlocks l [T.LABEL t] (fl@[(rev p [])] )
                                                      end    

    | basicBlocks ( (T.CJUMP(a,b,c,d,e)) :: l ) p fl = (* End the block *)
                                                      let val temp = (T.CJUMP(a,b,c,d,e) :: p) 
                                                      in basicBlocks l [] (fl@[(rev temp [])])
                                                      end 
    | basicBlocks ( (T.JUMP(a,b)) :: l ) p fl        = (*End the block*)
                                                      let val temp = (T.JUMP(a,b) :: p) 
                                                      in basicBlocks l [] (fl@[(rev temp [])])
                                                      end
    (* Any other statment except JUMP, CJUMP and LABEL *)
    | basicBlocks ( s :: l ) p fl =  basicBlocks l (s::p) fl

    
                                             

end
