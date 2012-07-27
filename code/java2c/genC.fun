functor genC (S : GENC_STRUCTS) : GENC =
struct

open S

structure generator =
struct

exception AddYourCodeHere

fun genBinopTipe(b:Ast2.Binop.t) :CAst.Tipe.t= 
    case b 
     of Ast2.Binop.Plus     => CAst.Tipe.Int
      | Ast2.Binop.Minus    => CAst.Tipe.Int
      | Ast2.Binop.Times    => CAst.Tipe.Int
      | Ast2.Binop.Div      => CAst.Tipe.Int
      | Ast2.Binop.Mod      => CAst.Tipe.Int
      | Ast2.Binop.And      => CAst.Tipe.Bool
      | Ast2.Binop.Or       => CAst.Tipe.Bool
      | Ast2.Binop.Gt       => CAst.Tipe.Bool
      | Ast2.Binop.Lt       => CAst.Tipe.Bool
      | Ast2.Binop.Eq       => CAst.Tipe.Bool
      | Ast2.Binop.Le       => CAst.Tipe.Bool
      | Ast2.Binop.Ge       => CAst.Tipe.Bool
      | Ast2.Binop.Neq      => CAst.Tipe.Bool

(*use to generate name of tmp var, junk_0, junk_1, junk_2,...*)	
val refVars = ref 0
fun newVars () =
    let
      val t = !refVars
      val _ = refVars := t + 1
    in
      String.concat ["junk", "_", Int.toString t]
    end

(*use to store data structs generate from class*)		
val structList: CAst.Struct.t list ref = ref []
fun addStruct (s: CAst.Struct.t) =
    let
      val t = !structList
	  val _ = structList := t@[s]
    in
	  ()
    end

(*use to store vtable structs generate from class*)	
val structsVtblList: CAst.Struct_Vtable.t list ref = ref []
fun addStructVtbl (s: CAst.Struct_Vtable.t) =
    let
      val t = !structsVtblList
	  val _ = structsVtblList := t@[s]
    in
	  ()
    end

(*use to store methods*)	
val methodList: CAst.Method.t list ref = ref []
fun addMethod (s: CAst.Method.t) =
    let
      val t = !methodList
	  val _ = methodList := t@[s]
    in
	  ()
    end

(*use to store statements*)	
val stmList: CAst.Stm.t list ref = ref []
fun addStm (s: CAst.Stm.t) =
    let
      val t = !stmList
	  val _ = stmList := t @ [s]
    in
	  ()
    end

(* use to store declaration of local var, it can be 
 * use to judgement whether a var is local var or not
 *)	    
val localsList: {id:string, ty:CAst.Tipe.t} list ref = ref []
val localsArgsList: {id:string, ty:CAst.Tipe.t} list ref = ref []
fun addLocal (s: {id:string, ty:CAst.Tipe.t}) =
    let
      val t1 = !localsList
	  val t2 = !localsArgsList
	  val _ = localsList := t1@[s]
	  val _ = localsArgsList := t2@[s]
    in
	  ()
    end

fun find(varName:string, []):{id:string,ty:CAst.Tipe.t} = {id="", ty=CAst.Tipe.Name("")}
  | find(varName:string, (x::xs):{id:string, ty:CAst.Tipe.t} list):{id:string,ty:CAst.Tipe.t} =
    let 
	  val {id = id1, ty = ty1} = x
	  val result = 
	    if id1 = varName then x
	    else find(varName, xs)
    in
	  result
    end
fun isLocalVar(varName:string): bool = 
    let
      val {id = id1, ty = ty1} = find(varName, !localsArgsList)
    in
	  if id1 = "" then false else true
    end

(*use to store name of class being translated*)	
val currentClassName = ref ""

(*use to store reference to whole program*)	
val prog_ref: Ast2.Program.t ref = ref Ast2.Program.bogus

fun genTipe (tipe: Ast2.Tipe.t): CAst.Tipe.t = 
    case tipe of
	Ast2.Tipe.Int => CAst.Tipe.Int
      | Ast2.Tipe.Bool => CAst.Tipe.Bool
      | Ast2.Tipe.ArrayInt => CAst.Tipe.ArrayInt
      | Ast2.Tipe.Name(str) => CAst.Tipe.Name(str)

(*get type of expression*)				       
fun getExpTipe(arg: Ast2.Exp.t) : CAst.Tipe.t = 
    case arg
     of Ast2.Exp.Inti i =>  CAst.Tipe.Int
      | Ast2.Exp.True   => CAst.Tipe.Bool
      | Ast2.Exp.False  => CAst.Tipe.Bool
      | Ast2.Exp.Var {id, pointer, pos} =>
	    let
          val {id = id1, ty = ty1} = find(id, !localsArgsList)
	    in
	      if id1 = "" 
          then CAst.Tipe.Name("error type")
	      else ty1
	    end
      | Ast2.Exp.Binop (e1, b, e2, pos) => genBinopTipe(b)
      | Ast2.Exp.NewArray (e, pos) => CAst.Tipe.ArrayInt
      | Ast2.Exp.NewId (id, pos) => CAst.Tipe.Name((id ^ " *"))
      | Ast2.Exp.This pos => CAst.Tipe.Int (*this will not be executed*)     
      | Ast2.Exp.Not (e, pos) => CAst.Tipe.Bool
      | Ast2.Exp.Length (e, pos) => CAst.Tipe.Int
      | Ast2.Exp.Call (e1, ety, id, retty, es, pos) => genTipe(retty)
      | Ast2.Exp.SimpleCall (id, retty, es, pos) => genTipe(retty)
      | Ast2.Exp.Array (e1, e2, pos) => CAst.Tipe.Int 
      | Ast2.Exp.ParenExp(e, pos) => getExpTipe(e)

(* store declaration of global object of vtable struct
 *   name of vtable struct   |   name of global object of this vtable struct
 *        Vtable_A           |      obj_Vtable_A
 *)	
val globalVarList: {id:string, ty:CAst.Tipe.t} list ref = ref []
fun addGlobal (s: {id:string, ty:CAst.Tipe.t}) =
    let
      val t = !globalVarList
	  val _ = globalVarList := t@[s]
    in
	  ()
    end

(*get init global object of vtable struct, these statements are in initVtables()*)			
val initStmList: CAst.Stm.t list ref = ref []
fun addInitStm (s: CAst.Stm.t) =
    let
      val t = !initStmList
	  val _ = initStmList := t@[s]
    in
	  ()
    end	

fun genPos (pos1: Ast2.pos) = 
    let 
	  val newpos: int = pos1
	  val newpos1: CAst.pos = newpos
    in
	  newpos1
    end
    
fun genBinop(binop: Ast2.Binop.t): CAst.Binop.t = 
    case binop of 
	Ast2.Binop.Plus         => CAst.Binop.Plus
      | Ast2.Binop.Minus    => CAst.Binop.Minus
      | Ast2.Binop.Times    => CAst.Binop.Times
      | Ast2.Binop.Div      => CAst.Binop.Div
      | Ast2.Binop.Mod      => CAst.Binop.Mod
      | Ast2.Binop.And      => CAst.Binop.And
      | Ast2.Binop.Or       => CAst.Binop.Or
      | Ast2.Binop.Lt       => CAst.Binop.Lt
      | Ast2.Binop.Gt       => CAst.Binop.Gt
      | Ast2.Binop.Eq       => CAst.Binop.Eq
      | Ast2.Binop.Ge       => CAst.Binop.Ge
      | Ast2.Binop.Le       => CAst.Binop.Le
      | Ast2.Binop.Neq      => CAst.Binop.Neq
			  
fun genExps([]:Ast2.Exp.t list):(CAst.Exp.t list * CAst.Stm.t list)= ([], [])
  | genExps(x::xs) = 
    let 
	  val (e1,s1) = genExp(x)
	  val (es,ss) = genExps(xs)
    in
	  ((e1::es), (s1@ss))
    end
    
and genExp (exp: Ast2.Exp.t): (CAst.Exp.t * CAst.Stm.t list) =
    case exp of
	Ast2.Exp.Inti(i) => (CAst.Exp.Inti(i), [])
      | Ast2.Exp.True => (CAst.Exp.True, [])
      | Ast2.Exp.False => (CAst.Exp.False, [])
      | Ast2.Exp.Var{id = str, pointer = p, pos = pos1} =>
	    let 
          val newPos = genPos(pos1)
	      val result = 
		    if isLocalVar(str)
            then CAst.Exp.Var{id = str, pointer = p, pos = newPos}
		    else CAst.Exp.Var{id = "this->" ^ str, pointer = "", pos = newPos}
	    in
	        (result, [])
	    end
      | Ast2.Exp.Binop(t1, t2, t3, t4) => 
	    let
	      val (newT1,s1) = genExp(t1)
	      val newOp = genBinop(t2)
	      val (newT3,s3) = genExp(t3)
	    in
	      (CAst.Exp.Binop(newT1, newOp, newT3, t4), s1@s3)
	    end
      | Ast2.Exp.NewArray (t1, t2) => 
          let
	        val _ = stmList := []
	        val var = newVars ()
	        val _ = addLocal({id = var, ty =  CAst.Tipe.ArrayInt})
	        val left = CAst.Exp.Inti(4)
	        val (newT1,s1) = genExp(t1)
	        val size = CAst.Exp.Binop(left, CAst.Binop.Times, newT1, genPos(t2))
	        val malloc_ = CAst.Exp.Malloc(size,genPos(t2))
	        val _ = addStm(CAst.Stm.AssignId {id = var,
                                              e = malloc_,
                                              pointer = "",
                                              pos = genPos(t2)})
	      in
	        (CAst.Exp.Var{id = var, pointer = "", pos = genPos(t2)}, s1@(!stmList))
	      end
      | Ast2.Exp.NewId (t1, t2) => 
          let
	        val _ = stmList := []
	        val var = newVars ()
	        val _ = addLocal({id = var, ty =  CAst.Tipe.Name (("data_" ^ t1 ^ "*"))})
	        val struct_ = CAst.Exp.Var {id = ("struct data_" ^ t1 ^ "*"),
                                        pointer = "",
                                        pos = genPos(t2)}
	        val size = CAst.Exp.Sizeof(struct_, genPos(t2))
	        val malloc_ = CAst.Exp.Malloc(size,genPos(t2))
	        val _ = addStm(CAst.Stm.AssignId {id = var,
                                              e = malloc_,
                                              pointer = "",
                                              pos = genPos(t2)})
	        val obj = CAst.Exp.Var {id = ("obj_vtable_" ^ t1),
                                    pointer = "",
                                    pos = genPos(t2)}
	        val _ = addStm(CAst.Stm.AssignId {id = "vptr",
                                              e = obj,
                                              pointer = var,
                                              pos = genPos(t2)})
	      in
	        (CAst.Exp.Var{id = var,
                          pointer = "",
                          pos = genPos(t2)},
                          (!stmList))
	      end
      | Ast2.Exp.This (pos) => (CAst.Exp.This (genPos(pos)), [])
      | Ast2.Exp.Not (exp, pos) => 
          let
            val (e, stm) = genExp(exp)
          in
            (CAst.Exp.Not (e, genPos(pos)), stm)
          end
      | Ast2.Exp.Length (exp, pos) => 
          let
            val (e, stm) = genExp(exp)
          in
            (CAst.Exp.Length (e, genPos(pos)), stm)
          end
      | Ast2.Exp.Call(e1, ety, funName, retty, es, pos) => 
          let 
			val _ = stmList := []
			val obj_data_struct = newVars ()
			val (newE1,s1) = genExp(e1)
			val newE1Type = getExpTipe(e1)
			val CAst.Tipe.Name(className_With_Data_prefix) = newE1Type
			val _ = addLocal({ id = obj_data_struct, ty = newE1Type })
			val _ = addStm(CAst.Stm.AssignId {id = obj_data_struct,
                                              e = newE1,
                                              pointer = "",
                                              pos = genPos(pos)})	
            val obj_vtbl = newVars ()
			val len = String.size className_With_Data_prefix
			val className = String.substring(className_With_Data_prefix, 5, len-5)
			val newRetTipe = Ast2.Program.getFunRetTipe((className), funName, (!prog_ref))
			val obj_vtbl_type = CAst.Tipe.Name("vtable_" ^ className)
			val _ = addLocal({ id = obj_vtbl, ty = obj_vtbl_type })
			val obj_data_struct_vptr = CAst.Exp.Var{id = "vptr",
                                                    pointer = obj_data_struct,
                                                    pos = genPos(pos)}
			val _ = addStm(CAst.Stm.AssignId {id = obj_vtbl,
                                              e = obj_data_struct_vptr,
                                              pointer = "",
                                              pos = genPos(pos)})
			val rslt = newVars()
			val _ = addLocal({ id = rslt, ty = genTipe(newRetTipe) })
			val obj_vbtl_Var = CAst.Exp.Var {id = obj_vtbl,
                                             pointer = "",
                                             pos = genPos(pos)} 
			val (newEx,s2) = genExps(es)
			val thisArg = CAst.Exp.Var {id = obj_data_struct,
                                        pointer = "",
                                        pos = genPos(pos)}
			val newCall = CAst.Exp.Call(obj_vbtl_Var,
                                        obj_vtbl_type,
                                        (funName),
                                        genTipe(newRetTipe),
                                        (thisArg::newEx),
                                        genPos(pos))
			val _ = addStm(CAst.Stm.AssignId {id = rslt,
                                              e = newCall,
                                              pointer = "",
                                              pos = genPos(pos)})
	      in
	        (CAst.Exp.Var{id = rslt, pointer = "", pos = genPos(pos)},s1@s2@(!stmList))  
          end
       | Ast2.Exp.SimpleCall (id, retType, exp, pos) =>
          let
            val newPos = genPos(pos)
            val varThis = "this"
            val var2 = newVars()
            val expType = CAst.Tipe.Name(!currentClassName)
            val retType = Ast2.Program.getFunRetTipe((!currentClassName), id, !prog_ref)
            val var2Type = CAst.Tipe.Name("vtable_" ^ (!currentClassName))
            val _ = addLocal({id = var2, ty = var2Type})
            val var2Exp = CAst.Exp.Var {id = "vptr",
                                        pointer = varThis,
                                        pos = newPos}
            val _ = addStm(CAst.Stm.AssignId {id = var2,
                                              e = var2Exp,
                                              pointer = "",
                                              pos = newPos})
            val newCaller = CAst.Exp.Var {id = var2,
                                          pointer = "",
                                          pos = newPos}
            val (e, stm) = genExps(exp)
            val thisArg = CAst.Exp.Var {id = varThis,
                                        pointer = "",
                                        pos = newPos}
            val _ = stmList := []
          in
            (CAst.Exp.Call(newCaller,
                           expType,
                           id,
                           genTipe(retType),
                           (thisArg::e),
                           newPos), stm @ (!stmList))
          end
      | Ast2.Exp.Array(t1, t2, pos) =>
          let
            val (exp1, stm1) = genExp(t1)
            val (exp2, stm2) = genExp(t2)
            val newPos = genPos(pos)
            val _ = stmList := []

          in
            (CAst.Exp.Array (exp1, exp2, newPos), (stm1 @ stm2 @ (!stmList)))
          end
      | Ast2.Exp.ParenExp(exp, pos) =>
          let
            val (e, stm) = genExp(exp)
            val newPos = genPos(pos)
          in
            (CAst.Exp.ParenExp (e, newPos), stm)
          end


fun genVars [] = []
  | genVars (s::slist: {id: string, ty: Ast2.Tipe.t} list) = 
      let
        val {id, ty} = s
      in
        case ty 
          of Ast2.Tipe.Name(className) => 
              let
                val {id:string, ty:Ast2.Tipe.t} = s
		        val (newTy: CAst.Tipe.t) = CAst.Tipe.Name("data_" ^ className ^ "*")
	          in
		        {id = id, ty = newTy}::genVars(slist)
	          end
	      | _ =>
              let
                val {id:string, ty:Ast2.Tipe.t} = s
		        val (newTy: CAst.Tipe.t) = genTipe ty
	          in
		        {id = id, ty = newTy}::genVars(slist)
	          end
      end
    
fun genClassVars [] = []
  | genClassVars (s::slist) = 
      let
	    val {id, ty:Ast2.Tipe.t, prefix} = s
	    val (newTy: CAst.Tipe.t) = genTipe ty
      in
	    {id=id, ty=newTy, prefix=prefix}::genClassVars(slist)    
      end

fun genStms([]: Ast2.Stm.t list): CAst.Stm.t list= []
  | genStms(x::xs) = 
    let
	  val s1 = genStm(x)
    in
	  s1 @ genStms(xs)
    end
    
and genStm (stm: Ast2.Stm.t): CAst.Stm.t list=
    case stm
     of Ast2.Stm.If (e, ss1, ss2, pos) =>
          let
	        val (new_e, s1) = genExp(e)
	        val new_ss1 = genStms(ss1)
	        val new_ss2 = genStms(ss2)
	      in
	        s1 @ [CAst.Stm.If (new_e, new_ss1, new_ss2, genPos(pos))]
	      end
      | Ast2.Stm.While (e, stms, pos) =>
          let
	        val (new_e, s1) = genExp(e)
	        val new_ss = genStms(stms)
	      in
	        s1 @ [CAst.Stm.While (new_e, new_ss, genPos(pos))] 
	      end
      | Ast2.Stm.Return(e, pos) => 
	      let
	        val (new_e, s1) = genExp(e)
	      in
	        s1 @ [CAst.Stm.Return(new_e, genPos(pos))] 
	      end
      | Ast2.Stm.Print(e, pos) => 
	      let
		    val str_e:string = Ast2.Exp.printExp(0,e)
	        val (new_e, s1) = genExp(e)
	      in
	        s1 @ [CAst.Stm.Print(new_e, genPos(pos))] 
	      end
      | Ast2.Stm.AssignId{id = id1, e = e1, pointer = p, pos = pos1} => 
	      let 
	        val (new_e2,s1) = genExp(e1)
	        val result = 
              if p = ""
              then
                if isLocalVar(id1)
                then CAst.Stm.AssignId {id = id1,
                                        e = new_e2,
                                        pointer = p,
                                        pos = genPos(pos1)}
		        else CAst.Stm.AssignId {id = id1,
								        e = new_e2,
                                        pointer = "this",
                                        pos = genPos(pos1)}
		      else
                if p = "this"
                then CAst.Stm.AssignId {id = id1,
								        e = new_e2,
                                        pointer = "this",
                                        pos = genPos(pos1)}
		        else CAst.Stm.AssignId {id = id1,
                                        e = new_e2,
                                        pointer = p,
                                        pos = genPos(pos1)}
          in
	        s1 @ [result]
	      end
	
      | Ast2.Stm.AssignArray{id = id1, left = l, right = r, pointer = p, pos = pos1} => 
          let
		    val newP = 
              if isLocalVar(id1)
              then ""
			  else "this"
	        val (new_l,s1) = genExp(l)
	        val (new_r,s2) = genExp(r)
	      in
	        s1 @ s2 @ [CAst.Stm.AssignArray{id = id1,
                                            left = new_l,
                                            right = new_r,
                                            pointer = newP,
                                            pos = genPos(pos1)}]
	      end
		   
fun genMethod (f:Ast2.Method.t) =
    case f
      of Ast2.Method.T {name = name1,
                        prefix = prefix1,
                        rettype = rettype1,
                        args = args1, 
                        locals = locals1,
                        stms = stms1,
                        pos = pos1} =>
	let 
	  val newArg1 = genVars(args1)
	  val newLocals1 = genVars(locals1)
	  val _ = localsArgsList := newArg1 @ newLocals1
	  val _ = localsList := newLocals1
	  val newType = genTipe(rettype1)
	  val newArg = {id = "this", ty = Ast2.Tipe.Name((!currentClassName))}
	  val newArgs = genVars(newArg::args1)
	  val newStms = genStms(stms1)
      val newPos = genPos(pos1)
	  val newMethod = CAst.Method.T{name = ((! currentClassName) ^ "_" ^ name1),
                                    prefix = prefix1,
                                    rettype = newType,
                                    args = newArgs,
                                    locals = (!localsList),
                                    stms = newStms,
                                    pos = newPos}
	  val _ = addMethod(newMethod)
	in
	    ()
	end

					
fun genMainClass(f:Ast2.MainClass.t) =
    case f
      of Ast2.MainClass.T {name, arg, locals, stms, pos} =>
	let
      val newLocals = genVars(locals)
	  val _ = localsArgsList := newLocals
	  val _ = localsList := newLocals
	  val newStms = genStms(stms)
	  val id1 = newVars()
      val newPos = genPos(pos)
	  val stmCallInitVtbl = CAst.Stm.AssignId{id = id1,
                                              e = CAst.Exp.SimpleCall("initVtables", CAst.Tipe.Int, [], pos),
                                              pointer = "",
                                              pos = newPos}
	in
	    CAst.Main.T {locals = {id = id1, ty = CAst.Tipe.Int}::(!localsList), 
					 stms = stmCallInitVtbl::newStms,
                     pos = newPos}
	end

					     
fun genClass (class: Ast2.Class.t) =
    case class
     of Ast2.Class.T {name = className,
		              cvars = cvars1: {ty: Ast2.Tipe.t, prefix: string, id: string} list,
		              methods = methods1: Ast2.Method.t list,  
			          methodsTable = methodsTable1: {className: string, 
										             methodSig: {id: string,
                                                                 prefix: string,
                                                                 rettype: Ast2.Tipe.t}} list,
		              pos = pos1} =>
	let
	  val _ = currentClassName := className
	  val _ = List.map genMethod methods1
      val newPos = genPos(pos1)
	  val _ = addInitStm(CAst.Stm.AssignId {id = "obj_vtable_" ^ className,
                                            e = CAst.Exp.Malloc(
                                                    CAst.Exp.Sizeof(
                                                        CAst.Exp.Var {id = "struct vtable_" ^ className, 
									                                  pointer = "", 
									                                  pos = newPos}, newPos), newPos),
				                            pointer = "",
				                            pos = newPos})
	  val rec genMethodsTable = 
	     fn [] => []
	      | (m::mx) =>
			let 
			  val {className = c, methodSig = methodsig} = m
			  val {id = id1, prefix = prefix1, rettype = rettype1} = methodsig
			  val newName = String.concat[c, "_", id1]
			  val var = CAst.Exp.Var {id = (newName),
                                      pointer = "",
                                      pos = newPos}
			  val assign = CAst.Stm.AssignId {id = id1,
                                              e = var,
							                  pointer = ("obj_vtable_" ^ (!currentClassName)), 
							                  pos = newPos}
			  val _ = addInitStm(assign)
			in
			  {id = id1, prefix = prefix1, rettype = genTipe(rettype1)}::genMethodsTable(mx)
			end
	    val newMethodsTable: {id:string, prefix:string, rettype:CAst.Tipe.t} list = genMethodsTable(methodsTable1)	
	    val vtblName = "vtable_" ^ className
	    val newStructsVtbl = CAst.Struct_Vtable.T{name = vtblName,
							                      fields = newMethodsTable,
							                      pos = newPos}
	    val _ = addGlobal({id = "*obj_" ^ vtblName, ty = CAst.Tipe.Name(vtblName)})
		val vptrField = {ty = Ast2.Tipe.Name(vtblName), prefix = "", id = "vptr"}
		val newCvars = genClassVars(cvars1)
	    val newStruct = CAst.Struct.T{name = "data_" ^ className,
                                      vptr = ref newStructsVtbl,
					                  fields = newCvars,
					                  pos = newPos}
	    val _ = addStructVtbl(newStructsVtbl)
	    val _ = addStruct(newStruct)
	in
	    ()
	end
					   
fun genProg (f:Ast2.Program.t): CAst.Program.t =
    case f
      of Ast2.Program.T{mainClass = mainProg,
                        classes = classList,
                        pos = mainPos} =>
	let
	    val _ = prog_ref := f
	    val newMain: CAst.Main.t = genMainClass mainProg
	    val _ = List.map genClass classList
	    val initMethod = CAst.Method.T {name = "initVtables",
					                    prefix = "",
					                    rettype = CAst.Tipe.Int,
					                    args = []:{id:string, ty:CAst.Tipe.t} list,
					                    locals = []:{id:string, ty:CAst.Tipe.t} list,
					                    stms = !initStmList, 
					                    pos = genPos(mainPos)}
	in
	    CAst.Program.T {globals = !globalVarList,
                        structs = !structList,
                        structs_vtbls = !structsVtblList, 
		                methods = !methodList,
                        main = newMain,
			            initVtbl = initMethod,
                        pos = genPos(mainPos)}
	end

end

val gen = generator.genProg
end
