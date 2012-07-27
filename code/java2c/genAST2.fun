functor genAST2 (S : GENAST2_STRUCTS) : GENAST2 =
struct

open S

structure generator =
struct

exception Unimplemented     

fun genPos (pos1: Ast.pos) = 
    let 
	val newpos:int = pos1
	val newpos1: Ast2.pos = newpos
    in
	newpos1
    end
    
fun genBinop(binop: Ast.Binop.t): Ast2.Binop.t = 
    case binop of 
	Ast.Binop.Plus => Ast2.Binop.Plus
      | Ast.Binop.Minus => Ast2.Binop.Minus
      | Ast.Binop.Times => Ast2.Binop.Times
      | Ast.Binop.Div => Ast2.Binop.Div
      | Ast.Binop.Mod => Ast2.Binop.Mod
      | Ast.Binop.And => Ast2.Binop.And
      | Ast.Binop.Or => Ast2.Binop.Or
      | Ast.Binop.Lt => Ast2.Binop.Lt
      | Ast.Binop.Gt => Ast2.Binop.Gt
      | Ast.Binop.Eq => Ast2.Binop.Eq
      | Ast.Binop.Ge => Ast2.Binop.Ge
      | Ast.Binop.Le => Ast2.Binop.Le
      | Ast.Binop.Neq => Ast2.Binop.Neq
			 
fun genTipe (tipe: Ast.Tipe.t): Ast2.Tipe.t = 
    case tipe of
	Ast.Tipe.Int => Ast2.Tipe.Int
      | Ast.Tipe.Bool => Ast2.Tipe.Bool
      | Ast.Tipe.ArrayInt => Ast2.Tipe.ArrayInt
      | Ast.Tipe.Name(str) => Ast2.Tipe.Name(str)
			      
fun genExp (exp: Ast.Exp.t): Ast2.Exp.t =
    case exp of
	Ast.Exp.Inti(i) => Ast2.Exp.Inti(i)
      | Ast.Exp.True => Ast2.Exp.True
      | Ast.Exp.False => Ast2.Exp.False
      | Ast.Exp.Var{id = str, pointer = p, pos = pos1} => Ast2.Exp.Var{id = str, pointer = p, pos = genPos(pos1)}
      | Ast.Exp.Binop(t1, t2, t3, t4) => Ast2.Exp.Binop(genExp(t1), genBinop(t2), genExp(t3), t4)
      | Ast.Exp.NewArray(t1,t2) => Ast2.Exp.NewArray(genExp(t1),t2)
      | Ast.Exp.NewId(t1,t2) => Ast2.Exp.NewId(t1,t2)
      | Ast.Exp.This(t1) => Ast2.Exp.This(t1)
      | Ast.Exp.Not(t1,t2) => Ast2.Exp.Not(genExp(t1),t2)
      | Ast.Exp.Length(t1,t2) => Ast2.Exp.Length(genExp(t1),t2)
      | Ast.Exp.Call(t1, t2, t3, t4, t5, t6) => Ast2.Exp.Call(genExp(t1), genTipe(t2), t3, genTipe(t4), genExps(t5), t6)
      | Ast.Exp.SimpleCall(t3, t4, t5, t6) => Ast2.Exp.SimpleCall(t3, genTipe(t4), genExps(t5), t6)
      | Ast.Exp.Array(t1,t2,t3) => Ast2.Exp.Array(genExp(t1), genExp(t2), t3)
      | Ast.Exp.ParenExp(e, pos) => Ast2.Exp.ParenExp(genExp(e), pos)
and genExps es = List.map genExp es
		 
fun genVars [] = []
  | genVars (s::slist: {id:string, ty:Ast.Tipe.t} list) = 
    let
	val {id:string, ty:Ast.Tipe.t} = s
	val (newTy: Ast2.Tipe.t) = genTipe ty
    in
	{id = id, ty = newTy}::genVars(slist)
    end
    
fun genClassVars [] = []
  | genClassVars (s::slist) = 
    let
	val {id, ty:Ast.Tipe.t, prefix} = s
	val (newTy: Ast2.Tipe.t) = genTipe ty
    in
	{id=id, ty=newTy, prefix=prefix}::genClassVars(slist)    
    end
    
fun genStm (stm: Ast.Stm.t) =
    case stm
     of Ast.Stm.If (e, ss1, ss2, pos) 
	=> Ast2.Stm.If (genExp(e), genStms(ss1), genStms(ss2), genPos(pos))
      | Ast.Stm.While (e, ss, pos) 
	=> Ast2.Stm.While (genExp(e), genStms(ss), genPos(pos))
      | Ast.Stm.Return(e, pos) => Ast2.Stm.Return(genExp(e), genPos(pos))
      | Ast.Stm.Print(e, pos) => Ast2.Stm.Print(genExp(e), genPos(pos))
      | Ast.Stm.AssignId{id=id1:string, e=e1:Ast.Exp.t, pointer=p:string, pos=pos1:Ast.pos} 
	=> Ast2.Stm.AssignId{id=id1, e=genExp(e1), pointer=p, pos=genPos(pos1)} 
      | Ast.Stm.AssignArray{id = id1, left = l, right = r, pointer = p, pos = pos1}
	=> Ast2.Stm.AssignArray{id = id1, left = genExp(l), right = genExp(r), pointer = p, pos = genPos(pos1)} 
           
and genStms f = List.map genStm f
		
fun genMethod (f:Ast.Method.t) =
    case f
     of Ast.Method.T { name = name1, prefix = prefix1, rettype = rettype1, 
		       args = args1, locals = locals1, stms = stms1, pos = pos1} =>
	let 
	    val newType = genTipe(rettype1)
	    val newArgs = genVars(args1)
	    val newVars = genVars(locals1)
	    val newStms = genStms(stms1)
	in
	    Ast2.Method.T{ name = name1, prefix = prefix1, rettype = newType, 
			   args=newArgs, locals=newVars, stms=newStms, pos = genPos(pos1)}		
	end
        
fun genMainClass (f:Ast.MainClass.t) =
    case f
     of Ast.MainClass.T {name, arg, locals, stms, pos} => 
        Ast2.MainClass.T {name=name, arg=arg, locals=genVars(locals), stms=genStms(stms), pos=genPos(pos)}
	
(*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*)
datatype node
  = Node of { self : Ast.Class.t ref,
	      parent : Ast.Class.t ref}
	    
(*
 *@fun: find class named by string in classlist
 *@sig: string * t list -> t ref
 *)			
fun findClass(cname:string, []) = 
    let
	val tmp = Ast.Class.bogus
    in
	ref tmp
    end
  | findClass(cname:string, (c::cs): Ast.Class.t list) = 
    let
	val Ast.Class.T{name = name1: string, ...} = c
    in
	if name1 = cname then
	    ref c
	else
	    findClass(cname,cs)
    end
    
fun findNewClass(cname:string, []) = 
    let
	val tmp = Ast2.Class.bogus
    in
	ref tmp
    end
  | findNewClass(cname:string, (c::cs): Ast2.Class.t list) = 
    let
	val Ast2.Class.T{name = name1: string, ...} = c
    in
	if name1 = cname then
	    ref c
	else
	    findNewClass(cname,cs)
    end
(*
 *@fun: build nodeList from classList
 *@sig: Ast.Class.t list * Ast.Class.t list -> Ast.Class.t ref list * node list
 *)
fun buildInheritTree ([], classes) = ([],[])
  | buildInheritTree ((x::xs):Ast.Class.t list, classes:Ast.Class.t list) =
    let 
	val Ast.Class.T{name = name1: string, extends = extends1: string option,...} = x
	val (rootList: (Ast.Class.t ref) list, nodeList: node list) 
	  = buildInheritTree(xs, classes)
    in
	if extends1 = NONE then
	    ((ref x)::rootList, nodeList)
	else
	    let 
		val s: string = Option.valOf(extends1)
		val parent1_ref = findClass(s, classes)
		val ref_x = ref x
		val tmp_node = Node{self = ref_x, parent = parent1_ref}
	    in
		(rootList,tmp_node::nodeList)
	    end
    end

(*
 *@fun: get son from nodelist
 *@sig: node list * string -> Class.t ref list
 *)
fun get_son_list([], parent_name : string) = []
  | get_son_list((n1::nx) : node list, parent_name : string) = 
    let 
	val Node{self = self_ref : Ast.Class.t ref, parent = parent_ref : Ast.Class.t ref} = n1
	val rest: Ast.Class.t ref list = get_son_list(nx, parent_name)
	val Ast.Class.T{name = name1, ...} = !parent_ref
    in
	if name1 = parent_name then
	    self_ref::rest
	else
	    rest
    end

(*
 *@fun: translate every method of method list
 *@sig: string * Ast.Method.t list -> {className:string, methodName:string} list
 *)
fun transFunc(cname: string, []) = []
  | transFunc(cname: string, (method::methods): Ast.Method.t list) = 
    let 
	val (restMethodTable:{className:string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list) 
	  = transFunc(cname, methods)
	val Ast.Method.T{name = methodName: string, prefix = prefix1: string, rettype = retType1: Ast.Tipe.t, 
			 args = args1: {id: string, ty: Ast.Tipe.t} list,
			 locals = locals1: {id: string, ty: Ast.Tipe.t} list,
			 stms = stms1: Ast.Stm.t list,              
			 pos = pos1: Ast.pos} = method
	    
    in
	{ className = cname, 
	  methodSig = { id = methodName:string, 
			prefix = prefix1:string, 
			rettype = genTipe(retType1):Ast2.Tipe.t
		      }
	}::restMethodTable
    end

(*
 *@fun: merge two list, the greater fun is given by the last argument
 *@sig: 'a list * 'a list -> 'a list
 *)
fun mergeCVars([], y::ys) = 
    let 
	val {id = self_cvar_name:string, prefix = prefix2:string, ty = self_cvar_type: Ast.Tipe.t} = y
    in
	{id = self_cvar_name:string, prefix = prefix2:string, ty = genTipe(self_cvar_type): Ast2.Tipe.t}::mergeCVars([], ys)
    end
  | mergeCVars(xs, []) = 
    xs
  | mergeCVars( x::xs, y::ys) =
    let
	val {id = parent_cvar_name:string, prefix = prefix1:string, ty = parent_cvar_type: Ast2.Tipe.t} = x
	val {id = self_cvar_name:string, prefix = prefix2:string, ty = self_cvar_type: Ast.Tipe.t} = y
    in
	if( parent_cvar_name > self_cvar_name ) then
	    {id = self_cvar_name, prefix = prefix2, ty =  genTipe(self_cvar_type): Ast2.Tipe.t}::mergeCVars(x::xs, ys)
	else 
	    if (parent_cvar_name = self_cvar_name) then
		{id = self_cvar_name, prefix = prefix2, ty =  genTipe(self_cvar_type): Ast2.Tipe.t}::mergeCVars(xs, ys)
	    else(* parent_cvar_name < self_cvar_name *)
		{id = parent_cvar_name, prefix = prefix1, ty =  parent_cvar_type: Ast2.Tipe.t}::mergeCVars(xs, y::ys)
    end

(*
 *@fun: merge two lists of method ref, the first string argument is name of self class,
 *      the last string argument is name of parent class
 *@sig: (string * string list) * (string * string list) -> string * string list
 *)
fun mergeMethod( [], ys) = ys
  | mergeMethod( xs, []) = xs
  | mergeMethod( 
    x::self_method_table:{className: string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list , 
    y::parent_method_table:{className: string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list ) = 
    let
	val {className = c, methodSig = {id = self_method_name, ...}} = x
	val {className = c, methodSig = {id = parent_method_name, ...}} = y
    in
	if self_method_name > parent_method_name then
	    y::mergeMethod(x::self_method_table, parent_method_table)
	else
	    if self_method_name = parent_method_name then
		x::mergeMethod(self_method_table, parent_method_table)
	    else
		x::mergeMethod(self_method_table, y::parent_method_table)
    end

(*
 *@fun: translate class which have parent, so you can say the function is removing 'extends' 
 *@sig: Ast.Class.t list * Ast.Class.t ref list * Ast.Method.t list * node list -> Ast2.Class.t list
 *)
fun transSonClass(newClasses: Ast2.Class.t list, [], inheritTree: node list) 
  = []
  | transSonClass(newClasses: Ast2.Class.t list, (c::cs): Ast.Class.t ref list, inheritTree: node list) =
    let 
	val (restClasses: Ast2.Class.t list) = transSonClass(newClasses, cs, inheritTree)
	val Ast.Class.T{ name = self_name:string, extends = extends1: string option, 
			 cvars = (self_cvars: {id:string, prefix:string, ty:Ast.Tipe.t} list), 
			 methods = self_methods: Ast.Method.t list, pos = pos1}
	    = !c
	val parent_name:string = Option.valOf(extends1)
	val Ast2.Class.T{ name = parent_name1, cvars = (parent_cvars: {id:string, prefix:string, ty:Ast2.Tipe.t} list), 
			  methods = parent_methods: Ast2.Method.t list, methodsTable 
									= parent_methodTable: {className: string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list , 
			  pos = pos2}
	    = !(findNewClass(parent_name, newClasses))
	val newCvars = mergeCVars(parent_cvars, self_cvars)
	val self_methodTable: {className: string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list 
	  = transFunc(self_name, self_methods)
	val new_self_methodTable:{className: string, methodSig: {id:string, prefix:string, rettype:Ast2.Tipe.t}} list 
	  = mergeMethod(self_methodTable, parent_methodTable)
	val new_class = Ast2.Class.T{ name = self_name, cvars = newCvars, methods = List.map genMethod self_methods, 
				      methodsTable = new_self_methodTable, pos = genPos(pos1)}
	val son_list = get_son_list(inheritTree, self_name)
	val my_son_class = transSonClass(new_class::restClasses, son_list, inheritTree)
    in
	(new_class::my_son_class)@restClasses
    end

(*
 *@fun: translate class which does not have parent
 *@sig: Ast2.Class.t list * Ast.Class.t ref list * node list -> Ast2.Class.t list
 *)	
fun transRootClass(newClasses, [], inheritTree: node list) = []
  | transRootClass(newClasses, (c::cs): Ast.Class.t ref list, inheritTree: node list) =
    let 
	val (restClass: Ast2.Class.t list) = transRootClass(newClasses, cs, inheritTree)
	val Ast.Class.T{name = name1: string, extends = extends1, cvars = cvars1, methods = methods1, pos = pos1}
	    = !c
	val methodTable = transFunc(name1, methods1)
	val newCVars = mergeCVars([], cvars1)
	val newMethods = List.map genMethod methods1
	val new_class = Ast2.Class.T{ name = name1, cvars = newCVars, methods = newMethods, 
				      methodsTable = methodTable, pos = genPos(pos1)}
	val son_list = get_son_list(inheritTree, name1)
	val my_son_class = transSonClass(new_class::restClass, son_list, inheritTree)
    in
	(new_class::my_son_class)@restClass
    end
(*----------------------------------------------------------------------------*)
(*
 *@fun: generate Ast2
 *@sig: Ast.Program.t -> Ast2.Program.t
 *)                
fun genClasses (classes1:Ast.Class.t list): Ast2.Class.t list = 
    let
	val (rootNode: Ast.Class.t ref list, inheritTree: node list) 
	  = buildInheritTree(classes1, classes1)
	val (newClasses: Ast2.Class.t list)
	  = transRootClass([], rootNode, inheritTree)
    in
	newClasses
    end
    
(*
 *@fun: generate Ast2
 *@sig: Ast.Program.t -> Ast2.Program.t
 *)                
fun genProg (f:Ast.Program.t): Ast2.Program.t = 
    case f
     of Ast.Program.T{ mainClass = mainClass1, classes = classes1, pos = pos1}=>
	let
	    val (newMainClass:Ast2.MainClass.t) = genMainClass	mainClass1
	    val (newClasses: Ast2.Class.t list) = genClasses(classes1)
	in
	    Ast2.Program.T{mainClass = newMainClass, classes = newClasses, pos = genPos(pos1)}
	end
	
end

val gen = generator.genProg
end
