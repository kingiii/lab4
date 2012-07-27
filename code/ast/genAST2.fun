functor genAST2 (S : GENAST2_STRUCTS) : GENAST2 =
struct

open S

structure generator =
struct

exception Unimplemented     

fun genPos (pos1: Ast.pos) = 
	let 
	  val newpos: int = pos1
	  val newpos1: Ast2.pos = newpos
	in
	  newpos1
	end
		  
fun genBinop(binop: Ast.Binop.t) : Ast2.Binop.t = 
	case binop of 
		Ast.Binop.Plus  => Ast2.Binop.Plus
	  | Ast.Binop.Minus => Ast2.Binop.Minus
	  | Ast.Binop.Times => Ast2.Binop.Times
	  | Ast.Binop.Div   => Ast2.Binop.Div
	  | Ast.Binop.And   => Ast2.Binop.And
	  | Ast.Binop.Or    => Ast2.Binop.Or
	  | Ast.Binop.Lt    => Ast2.Binop.Lt
	  | Ast.Binop.Gt    => Ast2.Binop.Gt
	  | Ast.Binop.Eq    => Ast2.Binop.Eq
				      
fun genTipe (tipe: Ast.Tipe.t) : Ast2.Tipe.t = 
	case tipe of
		Ast.Tipe.Int        => Ast2.Tipe.Int
	  | Ast.Tipe.Bool       => Ast2.Tipe.Bool
	  | Ast.Tipe.ArrayInt   => Ast2.Tipe.ArrayInt
	  | Ast.Tipe.Name(str)  => Ast2.Tipe.Name(str)
					    
fun genExp (exp: Ast.Exp.t) : Ast2.Exp.t =
	case exp of
		Ast.Exp.Inti(i)                 => Ast2.Exp.Inti(i)
	  | Ast.Exp.True                    => Ast2.Exp.True
	  | Ast.Exp.False                   => Ast2.Exp.False
      | Ast.Exp.Array(t1,t2,t3)         => Ast2.Exp.Array(genExp(t1), genExp(t2), t3)
	  | Ast.Exp.Binop(t1, t2, t3, t4)   => Ast2.Exp.Binop(genExp(t1), genBinop(t2), genExp(t3), t4)
	  | Ast.Exp.NewArray(t1,t2)         => Ast2.Exp.NewArray(genExp(t1),t2)
	  | Ast.Exp.NewId(t1,t2)            => Ast2.Exp.NewId(t1,t2)
	  | Ast.Exp.This(t1)                => Ast2.Exp.This(t1)
	  | Ast.Exp.Not(t1,t2)              => Ast2.Exp.Not(genExp(t1),t2)
	  | Ast.Exp.Length(t1,t2)           => Ast2.Exp.Length(genExp(t1),t2)
	  | Ast.Exp.Call(t1, t2, t3, t4, t5, t6)
                => Ast2.Exp.Call(genExp(t1), genTipe(t2), t3, genTipe(t4), genExps(t5), t6)
      | Ast.Exp.Var{id = str, pointer = p, pos = pos1}
                => Ast2.Exp.Var{id = str, pointer = p, pos = genPos(pos1)}

	  	and genExps es = List.map genExp es
			       
fun genVars [] = []
  | genVars (s::slist: {id:string, ty: Ast.Tipe.t} list) = 
	  let
		val {id:string, ty: Ast.Tipe.t} = s
		val (newTy: Ast2.Tipe.t) = genTipe ty
	  in
		{id = id, ty = newTy}::genVars(slist)
	  end
		  
fun genClassVars [] = []
  | genClassVars (s::slist) = 
	  let
		val {id, ty: Ast.Tipe.t, prefix} = s
		val (newTy: Ast2.Tipe.t) = genTipe ty
	  in
		{id = id, ty = newTy, prefix = prefix}::genClassVars(slist)    
	  end
		  
fun genStm (stm: Ast.Stm.t) =
	case stm of
      Ast.Stm.If(e, ss1, ss2, pos)  => Ast2.Stm.If (genExp(e), genStms(ss1), genStms(ss2), genPos(pos))
	| Ast.Stm.While(e, ss, pos)     => Ast2.Stm.While (genExp(e), genStms(ss), genPos(pos))
	| Ast.Stm.Return(e, pos)        => Ast2.Stm.Return(genExp(e), genPos(pos))
	| Ast.Stm.Print(e, pos)         => Ast2.Stm.Print(genExp(e), genPos(pos))
	| Ast.Stm.AssignId{id = id1 : string, e = e1 : Ast.Exp.t, pointer = p : string, pos = pos1 : Ast.pos} 
		      => Ast2.Stm.AssignId{id = id1, e = genExp(e1), pointer = p, pos = genPos(pos1)} 
	| Ast.Stm.AssignArray{id = id1, left = l, right = r, pointer = p, pos = pos1}
		      => Ast2.Stm.AssignArray{id = id1, left = genExp(l), right = genExp(r), pointer = p, pos = genPos(pos1)} 
and genStms f = List.map genStm f
			      
fun genMethod (f: Ast.Method.t) =
	case f of
      Ast.Method.T {name    = name1,
                    prefix  = prefix1,
                    rettype = rettype1, 
				    args    = args1,
                    locals  = locals1,
                    stms    = stms1,
                    pos     = pos1} =>
		      let 
			    val newType = genTipe(rettype1)
			    val newArgs = genVars(args1)
			    val newVars = genVars(locals1)
			    val newStms = genStms(stms1)
		      in
			    Ast2.Method.T{name      = name1,
                              prefix    = prefix1,
                              rettype   = newType, 
					          args      = newArgs,
                              locals    = newVars,
                              stms      = newStms,
                              pos       = genPos(pos1)}		
		      end
		      
fun genMainClass (f : Ast.MainClass.t) =
	case f of 
      Ast.MainClass.T {name, arg, stms, pos} => 
		      Ast2.MainClass.T {name = name, arg = arg, stms = genStms(stms), pos = genPos(pos)}


datatype node = Node of {self: Ast.Class.t ref, parent: Ast.Class.t ref}

fun findClass(class: string, []) =
    let
      val tmpClass = Ast.Class.bogus
    in
      ref tmpClass
    end
  | findClass(class: string, (c::cs): Ast.Class.t list) =
    let
      val Ast.Class.T {name = name1: string, ...} = c
    in
      if name1 = class then ref c else findClass(class, cs)
    end

fun findNewClass(class: string, []) =
    let
      val tmpClass = Ast2.Class.bogus
    in
      ref tmpClass
    end
  | findNewClass(class: string, (c::cs): Ast2.Class.t list) =
    let
      val Ast2.Class.T {name = name1: string, ...} = c
    in
      if name1 = class then ref c else findNewClass(class, cs)
    end

fun buildTree([], classes) = ([], [])
  | buildTree((x::xs): Ast.Class.t list, classes: Ast.Class.t list) =
    let
      val Ast.Class.T{name      = name1: string,
                      extends   = extends1: string option, ...} = x
      val (rootList: Ast.Class.t ref list, nodeList: node list) = buildTree(xs, classes)
    in
      if extends1 = NONE then ((ref x)::rootList, nodeList)
      else
        let
          val s: string = Option.valOf(extends1)
          val parent_ref = findClass(s, classes)
          val self_ref = ref x
          val tmpNode = Node{self = self_ref, parent = parent_ref}
        in
          (rootList, tmpNode::nodeList)
        end
    end

fun getChildList([], parentName: string) = []
  | getChildList((n::nx): node list, parentName: string) =
    let
      val Node{self = self_ref: Ast.Class.t ref, parent = parent_ref: Ast.Class.t ref} = n
      val restNode: Ast.Class.t ref list = getChildList(nx, parentName)
      val Ast.Class.T{name = name1, ...} = !parent_ref
    in
      if name1 = parentName then self_ref::restNode else restNode
    end

fun method2Table(class: string, []) = []
  | method2Table(class: string, (m::ms): Ast.Method.t list) =
    let
      val restMethodTable: {className: string, methodName: string} list = method2Table(class, ms)
      val Ast.Method.T {name = mName: string, ...} = m
    in
      {className = class, methodName = mName}::restMethodTable
    end

fun mergeVar([], v::vs) =
    let
      val {id       = selfName: string,
           prefix   = selfPrefix: string,
           ty       = selfType: Ast.Tipe.t} = v
    in
      {id       = selfName: string,
       prefix   = selfPrefix: string,
       ty       = genTipe(selfType): Ast2.Tipe.t}::mergeVar([], vs)
    end
  | mergeVar(vs, []) = vs
  | mergeVar(v::vs, u::us) =
    let
      val{id        = parentName: string,
          prefix    = parentPrefix: string,
          ty        = parentType: Ast2.Tipe.t} = v
      val{id        = selfName: string,
          prefix    = selfPrefix: string,
          ty        = selfType: Ast.Tipe.t} = u
    in
      if parentName > selfName
      then {id      = selfName,
            prefix  = selfPrefix,
            ty      = genTipe(selfType)}::mergeVar(v::vs, us)
      else {id      = parentName,
            prefix  = parentPrefix,
            ty      = parentType}::mergeVar(vs, u::us)
    end

fun mergeMethods([], us) = us
  | mergeMethods(vs, []) = vs
  | mergeMethods(u::us: Ast2.Method.t list, v::vs: Ast2.Method.t list): Ast2.Method.t list =
    let
      val Ast2.Method.T {name = selfMethodName, ...} = u
      val Ast2.Method.T {name = parentMethodName, ...} = v
    in
      if selfMethodName = parentMethodName
      then u::mergeMethods(us, vs)
      else
        if selfMethodName < parentMethodName
        then u::mergeMethods(us, v::vs)
        else v::mergeMethods(u::us, vs)
    end

fun mergeMethodTable([], us) = us
  | mergeMethodTable(vs, []) = vs
  | mergeMethodTable(u::us: {className: string, methodName: string} list,
                     v::vs: {className: string, methodName: string} list) = 
    let
      val {className = c, methodName = selfMethodName} = u
      val {className = c, methodName = parentMethodName} = v
    in
      if selfMethodName = parentMethodName
      then u::mergeMethodTable(us, vs)
      else
        if selfMethodName < parentMethodName
        then u::mergeMethodTable(us, v::vs)
        else v::mergeMethodTable(u::us, vs)
    end

fun transformChildClass(newClass: Ast2.Class.t list, [], tree: node list) = []
  | transformChildClass(newClass: Ast2.Class.t list, (c::cs): Ast.Class.t ref list, tree: node list) =
    let
      val (restClass: Ast2.Class.t list) = transformChildClass(newClass, cs, tree)
      val Ast.Class.T {name     = selfName: string,
                       extends  = selfExtends: string option,
                       cvars    = selfVar: {id: string,
                                            prefix: string,
                                            ty: Ast.Tipe.t} list,
                       methods  = selfMethods: Ast.Method.t list,
                       pos      = selfPos} = !c
      val parentName: string = Option.valOf(selfExtends)
      val Ast2.Class.T {name            = parentName,
                        cvars           = parentVar: {id: string,
                                                      prefix: string,
                                                      ty: Ast2.Tipe.t} list,
                        methods         = parentMethods: Ast2.Method.t list,
                        methodsTable    = parentMethodsTable: {className: string,
                                                               methodName: string} list,
                        pos             = parentPos} = !(findNewClass(parentName, newClass))
      val newVar        = mergeVar(parentVar, selfVar)
      val selfMethodsTable: {className: string,
                             methodName: string} list = method2Table (selfName, selfMethods)
      val newSelfMethodsTable: {className: string,
                                methodName: string} list = mergeMethodTable (selfMethodsTable, parentMethodsTable)
      val newMethods    = mergeMethods (List.map genMethod selfMethods, parentMethods) 
      val newClass      = Ast2.Class.T {name     = selfName,
                                        cvars    = newVar,
                                        methods  = newMethods,
                                        methodsTable = newSelfMethodsTable,
                                        pos      = genPos (selfPos)}
      val childList     = getChildList(tree, selfName)
      val childClass    = transformChildClass(newClass::restClass, childList, tree)
    in
      (newClass::childClass) @ restClass
    end

fun transformRootClass(newClass, [], tree: node list) = []
  | transformRootClass(newClass, (c::cs): Ast.Class.t ref list, tree: node list) =
    let
      val restClass: Ast2.Class.t list = transformRootClass(newClass, cs, tree)
      val Ast.Class.T {name     = selfName,
                       extends  = selfExtends,
                       cvars    = selfVar,
                       methods  = selfMethods,
                       pos      = selfPos} = !c
      val newMethodsTable = method2Table(selfName, selfMethods)
      val newVar = mergeVar([], selfVar)
      val newMethods = List.map genMethod selfMethods
      val newClass = Ast2.Class.T {name     = selfName,
                                   cvars    = newVar,
                                   methods  = newMethods,
                                   methodsTable = newMethodsTable,
                                   pos      = genPos(selfPos)}
      val childList = getChildList(tree, selfName)
      val childClass = transformChildClass(newClass::restClass, childList, tree)
    in
      (newClass::childClass) @ restClass
    end
      

fun genClasses (classes1: Ast.Class.t list): Ast2.Class.t list =
    let
      val (rootNode: Ast.Class.t ref list, tree: node list) = buildTree(classes1, classes1)
      val newClasses: Ast2.Class.t list = transformRootClass([], rootNode, tree)
    in
      newClasses
    end
									                    
fun genProg (f: Ast.Program.t): Ast2.Program.t = 
	case f of
      Ast.Program.T {mainClass  = mainClass1,
                     classes    = classes1,
                     pos        = pos1} =>
		let
		  val (newMainClass: Ast2.MainClass.t)  = genMainClass (mainClass1)
		  val (newClasses: Ast2.Class.t list)   = genClasses (classes1)
		in
		  Ast2.Program.T {mainClass = newMainClass,
                          classes   = newClasses,
                          pos       = genPos(pos1)}
		end
		      
end

val gen = generator.genProg
end
