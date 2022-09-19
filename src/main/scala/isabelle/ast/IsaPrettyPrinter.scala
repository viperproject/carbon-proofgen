package isabelle.ast

//TODO: minimize parentheses
object IsaPrettyPrinter {

  def prettyPrint(ty: TypeIsa) : String = ty match {
    case VarType(x) => x
    case ArrowType(t1, t2) => "(" + t1 + " => " + t2 + ")"
    case DataType(name, args) => "(" + name + " " + (args map prettyPrint).mkString(" ")  + ")"
    case TupleType(args) => "(" + args.mkString(",") + ")"
    case BoolType => "bool"
    case NatType => "nat"
    case IntType => "int"
  }

  def prettyPrint(bop: BinaryOpCode) : String = bop match {
    case MetaImp => "\\<Longrightarrow>"
    case Eq => "="
    case Neq => "\\<noteq>"
    case Lt => "<"
    case Le => "\\<le>"
    case Gt => ">"
    case Ge => "\\<ge>"
    case And => "\\<and>"
    case Or => "\\<or>"
    case Implies => "\\<longrightarrow>"
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
  }

  def prettyPrint(uop: UnaryOpCode) : String = uop match {
    case Not => "\\<neg>"
  }

  def prettyPrint(t: Term) : String = t match {
    case TermIdent(id) => id.toString()
    case TermApp(f, arg) => "("+prettyPrint(f) + " " + (arg map prettyPrint).mkString(" ")  +")"
    case TermWithExplicitType(t, ty) => "(" + prettyPrint(t) + ":" + prettyPrint(ty) + ")"
    case TermList(xs) => "[" + xs.mkString(",") + "]"
    case TermQuantifier(qkind, boundVars, body) =>
      val boundVarsString : Seq[String] =  (boundVars map (x => x.toString()))
      val qkindString = qkind match {
        case All => "\\<forall>"
        case Exists => "\\<exists>"
        case MetaAll => "\\<And>"
        case Lambda => "\\<lambda>"
      }
      "(" + qkindString + " " + boundVarsString.mkString(" ") + "::" + prettyPrint(body) + ")"
    case TermBinary(bop, left, right) =>
      "(" + prettyPrint(left) + prettyPrint(bop) + prettyPrint(right) + ")"
    case TermUnary(uop, arg) =>
      "(" + prettyPrint(uop) + prettyPrint(arg) + ")"
    case IntConst(i) => i.toString()
    case NatConst(i) => i.toString()
    case BoolConst(b) => if(b) "True" else "False"
    case StringConst(s) => s
  }

  def prettyPrint(d: OuterDecl, sb: StringBuilder) : StringBuilder = d match {
    case DefDecl(name, typ, equation) =>
      sb.append("definition ").append(name).append(" :: ")
      appendInner(sb, prettyPrint(typ)).append(" where ")append(System.lineSeparator())
      sb.append(" "*2)
      appendInner(sb, name + " " + (equation._1 map prettyPrint).mkString(" ") + " = " + prettyPrint(equation._2))
    case _ => sys.error("not supported outer declaration")
  }

  def appendInner(sb: StringBuilder, s: String): StringBuilder = {
    sb.append("\"").append(s).append("\"")
  }

  def prettyPrint(theory: Theory) : String  = {
    val outerDeclsString = {
      val stringBuilder = new StringBuilder()
      theory.decls.foldLeft(())(
        (_, decl) => prettyPrint(decl, stringBuilder).append(System.lineSeparator()*2)
      )
      stringBuilder.toString()
    }
    "theory " + theory.theoryName + " imports " + theory.importTheories.mkString(" ") + System.lineSeparator() +
    "begin" + System.lineSeparator() +
      outerDeclsString +
    "end"
  }


}
