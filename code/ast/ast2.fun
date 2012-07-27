functor Ast2(): AST2 =
struct

type pos = int

fun spaces n =
    case n
     of 0 => ""
      | n => String.concat [" ", spaces (n - 1)]  
	     
structure Binop =
struct
datatype t
  = Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | Lt
  | Gt
  | Eq
    
val toString =
 fn Plus => " + "
  | Minus => " - "
  | Times => " * "
  | Div => "/"
  | And => " && "
  | Or => "||"
  | Gt => " < "
  | Lt => "> "
  | Eq => "=="
end

structure Tipe =
struct
datatype t
  = Int
  | Bool
  | ArrayInt
  | Name of string
            
fun equals (t1, t2) =
    case (t1, t2)
     of (Int, Int) => true
      | (Bool, Bool) => true
      | (ArrayInt, ArrayInt) => true
      | (Name s1, Name s2) => s1 = s2
      | _ => false
             
fun toString t = 
    case t
     of Int => "int"
      | Bool => "boolean"
      | ArrayInt => "int []"
      | Name s => s
end

structure Exp =
struct
datatype t
  = Inti of int
  | True
  | False
  | Var of {id : string, pointer : string, pos : pos}
  | Binop of t * Binop.t * t * pos
  | NewArray of t * pos
  | NewId of string * pos
  | This of pos
  | Not of t * pos
  | Length of t * pos
  | Call of t * Tipe.t * string * Tipe.t * t list * pos
  | Array of t * t * pos
             
fun toString x =
    case x
     of Inti i => Int.toString i
      | True => "true"
      | False => "false"
      | Var {id, pointer, pos} => id
      | Binop (e1, b, e2, pos) => 
        String.concat [toString e1, 
                       Binop.toString b, toString e2]
      | NewArray (e, pos) => 
        String.concat ["new int ", toString e]
      | NewId (id, pos) => 
        String.concat ["new ", id, "()"]
      | This pos => "this"
      | Not (e, pos) => 
        String.concat ["!", toString e]
      | Length (e, pos) => 
        String.concat [toString e, ".length"]
      | Call (e1, ety, id, retty, es, pos) => 
        String.concat [toString e1, ".", id, 
                       "(", expsToString es, ")"]
      | Array (e1, e2, pos) => 
        String.concat [toString e1, "[", 
                       toString e2, "]"]         
        
and expsToString es = 
    String.concat (List.map toString es)
    
end

structure Stm =
struct
datatype t
  = AssignId of {id: string, e: Exp.t, 
                 pointer: string, pos: pos}
  | AssignArray of {id: string, left: Exp.t, 
                    right : Exp.t, pointer: string, 
                    pos: pos}
  | If of Exp.t * t list * t list * pos
  | While of Exp.t * t list * pos
  | Return of Exp.t * pos
  | Print of Exp.t * pos
             
fun toString x =
    case x
     of AssignId {id, e, pointer, pos} => 
        String.concat [id, " = ", Exp.toString e, ";"]
      | AssignArray {id, left, right, pointer, pos} =>
        String.concat [id, "[", Exp.toString left, "]", 
                       " = ", Exp.toString right, ";"]
      | If (e, ss1, ss2, pos) => raise Fail "to do"
      | While _ => raise Fail "to do"
      | Return (e, pos) => 
        String.concat ["return (", Exp.toString e, ");"]
      | Print (e, pos) => 
        String.concat ["System.out.println (", 
                       Exp.toString e, ");"]       
fun printStm (sp, x) =
    case x
     of If (e, ss1, ss2, pos) =>
	String.concat [spaces sp, "if ", "(", Exp.toString e, "){\n",
                       printStms (sp + 2, ss1),
                       spaces sp, "}\n",
                       spaces sp, "else{\n",
                       printStms (sp + 2, ss2),
                       spaces sp, "}\n"]
      | While _ => raise Fail "to do"
      | _ => String.concat [spaces sp, toString x, "\n"]		
and printStms (spaces, f) = String.concat (List.map (fn x => printStm (spaces, x)) f)			   

end

structure Method =
struct
datatype t
  = T of {name: string,
          prefix: string,
          rettype: Tipe.t,
          args: {id: string, ty: Tipe.t} list,
          locals: {id: string, ty: Tipe.t} list,
          stms: Stm.t list,              
          pos: pos}
	 
val bogus = T {name = "bogus",
               prefix = "",
               rettype = Tipe.Int,
               args = [],
               locals = [],
               stms = [],
               pos = ~1}

fun compare (T {name = name1, prefix = p1, ...}, 
             T {prefix = p2, name = name2, ...}) =
    String.compare (String.concat [p1, "_", name1], 
                    String.concat [p2, "_", name2])
    
fun equals (T {name = name1, ...}, 
            T {name = name2, ...}) = name1 = name2

fun printVar {id, ty} = String.concat [Tipe.toString ty, " ", id, ";\n"]    
fun printVars vars = String.concat (List.map printVar vars)
		     
fun printArgs args =
    case args
     of [] => ""
      |[{id, ty}] => String.concat [Tipe.toString ty, " ", id] 
      | {id, ty} :: xs =>
        String.concat [Tipe.toString ty, " ", id, ", ", printArgs xs]    			
	
fun printMethod f =
    case f
     of T {name, prefix, rettype, args, locals, stms, pos} =>
        let val s1 = if prefix = ""
                     then String.concat [spaces 4, "public ", Tipe.toString rettype, " ", 
                                         name, " (", printArgs args, ") ", "{\n"]
                     else String.concat [spaces 4, "public ", Tipe.toString rettype, " ", 
                                         prefix, "_", name, " (", printArgs args, ") ", "{\n"]
            val s2 = String.concat [spaces 8, printVars locals, "\n"]
            val s3 = Stm.printStms (8, stms)
        in  String.concat [s1, s2, s3, spaces 4, "}\n"]
        end
	
end

structure Class =
struct
datatype t
  = T of {name : string,
          cvars : {ty : Tipe.t, id : string, 
                   prefix : string} list,
          methods : Method.t list,
	  methodsTable : {className: string, methodName: string} list,
          pos : pos}
	 
fun compare (T {name = n1, ...}, 
             T {name = n2, ...}) = String.compare (n1, n2) 
    
fun equals (T {name = n1, ...}, 
            T {name = n2, ...}) = n1 = n2    
    
fun equalsTyVarPrefix ({ty = ty1, id = id1, prefix = p1}, 
                       {ty = ty2, id = id2, prefix = p2})
    = id1 = id2 andalso Tipe.equals (ty1, ty2)

fun printClassVar {id, ty, prefix} =
    if prefix = "" then 
	String.concat [spaces 4, Tipe.toString ty, " ", id, ";\n"]
    else 
	String.concat [spaces 4, Tipe.toString ty, " ", prefix, "_", id, ";\n"]
	
fun printClassVars vars = String.concat (List.map printClassVar vars)

fun printMethodsTable [] = ""
  | printMethodsTable ({className=cname, methodName=mname}::ms) = 
    let 
	val mstr = String.concat ["(", cname, ",\t",mname, ")\n"]
	val restmstr = printMethodsTable(ms)
    in
	String.concat[mstr,restmstr]
    end
    
fun printClass (f: t) =
    case f
     of T{name=name1, cvars=cvars1, methods=methods1, methodsTable=methodsTable1, pos=pos1} =>
        let 
	    val s1 = String.concat ["class ", name1, "{", "\n"]
            val s2 = printClassVars cvars1
            val s3 = List.map Method.printMethod methods1
	    val s4 = "Method Table:\n"
	    val s5 = printMethodsTable(methodsTable1)
            val s6 = String.concat s3
        in  String.concat [s1, s4, s5, s2, s6, "}\n"]
        end
	
val bogus = T {name = "",
               cvars = [],
	       methods = [],
	       methodsTable = [],
	       pos = ~1}	
	    
end

structure MainClass =
struct
datatype t
  = T of {name : string,
          arg : string,
          stms : Stm.t list,
          pos : pos}
         
val bogus = T {name = "@@@", arg = "", stms = [], pos = 0} 

fun printMainClass f =
    case f
     of T {name, arg, stms, pos} => 
        let val s1 = String.concat ["class ", name, "{", "\n", "\tpublic static void main(String[] ", arg, "){\n"]
            val s2 = List.map (fn x => Stm.printStm (8, x)) stms
            val s3 = String.concat s2
        in  if name = "@@@"
            then ""
            else String.concat [s1, s3, "\t}\n}\n"]
        end

end

structure Program =
struct
datatype t
  = T of {mainClass: MainClass.t,
          classes: Class.t list,
          pos: pos}
         
val bogus = T {mainClass = MainClass.bogus,
               classes = [],
               pos = ~1}

	    
fun printProg f = 
    case f
     of T {mainClass, classes, pos} =>
        let val strMainClass = String.concat [MainClass.printMainClass mainClass, "\n\n\n"]
            val strClasses = String.concat (List.map (fn c => (Class.printClass c)^"\n\n\n") classes)
        in  String.concat [strMainClass, strClasses, "\n"]
        end
	
end

val printAST2 = Program.printProg

end
