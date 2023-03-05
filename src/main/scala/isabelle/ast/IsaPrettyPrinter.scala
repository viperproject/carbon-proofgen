package isabelle.ast

//TODO: minimize parentheses
object IsaPrettyPrinter {

  def prettyPrint(ty: TypeIsa) : String = ty match {
    case VarType(x) => x
    case ArrowType(t1, t2) => "(" + prettyPrint(t1) + " => " + prettyPrint(t2) + ")"
    case DataType(name, args) => "(" + (args map prettyPrint).mkString(" ")  + " " + name + ")"
    case TupleType(args) => "(" + (args map prettyPrint).mkString("\\<times>") + ")"
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
    case TermTuple(xs) => "(" + xs.mkString(",") + ")"
    case TermQuantifier(qkind, boundVars, body) =>
      val boundVarsString : Seq[String] =  (boundVars map (x => x.toString()))
      val qkindString = qkind match {
        case All => "\\<forall>"
        case Exists => "\\<exists>"
        case MetaAll => "\\<And>"
        case Lambda => "\\<lambda>"
      }
      "(" + qkindString + " " + boundVarsString.mkString(" ") + "." + prettyPrint(body) + ")"
    case TermBinary(bop, left, right) =>
      "(" + prettyPrint(left) + prettyPrint(bop) + prettyPrint(right) + ")"
    case TermUnary(uop, arg) =>
      "(" + prettyPrint(uop) + prettyPrint(arg) + ")"
    case IntConst(i) => i.toString()
    case NatConst(i) => i.toString()
    case BoolConst(b) => if(b) "True" else "False"
    case StringConst(s) => s"''$s''"
  }

  def prettyPrint(d: OuterDecl, sb: StringBuilder) : StringBuilder = d match {
    case _ : DefDecl => prettyPrintEquationDecl(d, sb)
    case _ : AbbrevDecl => prettyPrintEquationDecl(d, sb)
    case _ : FunDecl => prettyPrintEquationDecl(d, sb)
    case LemmaDecl(name, context, statements, proof) =>
      sb.append(s"lemma $name : ").newLine
      prettyPrintContextElem(context, sb).newLine
      sb.append(statements.map(stmt => innerTerm(prettyPrint(stmt))).mkString(" and ")).newLine
      prettyPrintProof(proof, sb)
    case LocaleDecl(name, context, body) =>
      sb.append(s"locale $name =").newLine
      prettyPrintContextElem(context, sb).newLine
      sb.append("begin").newLine
      prettyPrintOuterDecls(body, sb)
      sb.newLine.append("end")
    case MLDecl(code ,kind) =>
      sb.append(kind.declId).append("\\<open>").newLine.append(code.mkString(System.lineSeparator())).newLine.append("\\<close>")
    case DeclareDecl(declaration) => sb.append("declare ").append(declaration)
    case _ => sys.error("not supported outer declaration")
  }

  def prettyPrintProof(p: Proof, sb: StringBuilder) : StringBuilder = {
    p.methods.foreach(proofMethod => sb.append(proofMethod).newLine)
    sb
  }

  def prettyPrintEquationDecl(d: OuterDecl, sb: StringBuilder) : StringBuilder =  {
    val (keyword, equality, typ, equations) = d match {
      case abbrev: AbbrevDecl => ("abbreviation", " \\<equiv> ", abbrev.typ, Seq(abbrev.equation))
      case defDecl: DefDecl  => ("definition", " = ", defDecl.typ, Seq(defDecl.equation))
      case funDecl: FunDecl => ("definition", " = ", funDecl.typ, funDecl.equations)
    }

    sb.append(keyword).append(" ").append(d.name)
    typ.foreach(t => { sb.append(" :: ").appendInner(prettyPrint(t)) } )
    sb.append(" where ").newLine.append(" "*2)
    printEquation(d.name, equations.head, equality, sb).newLine

    equations.tail.foreach(
      eq => {
        sb.append(" "*2).append("|")
        printEquation(d.name, eq, equality, sb)
      }
    )

    sb
  }

  def prettyPrintContextElem(c: ContextElem, sb: StringBuilder) : StringBuilder = {
    if(!c.fixedVariables.isEmpty) {
      sb.append("fixes ")

      val fixVariablesString =
        c.fixedVariables.map({ case (varName, varType) =>
          s"${prettyPrint(varName)} :: ${innerTerm(prettyPrint(varType))}"
        }).mkString(" and ")

      sb.append(fixVariablesString)
    }

    if(!c.assumptions.isEmpty) {
      sb.append("assumes ")

      var first = true
      for (assm <- c.assumptions) {
        if (first) {
          first = false
        } else {
          sb.append(" and ").newLine
        }
        assm._1.foreach(label => sb.append(s"${label}: "))
        sb.appendInner(prettyPrint(assm._2))
      }
    }

    sb
  }

  def printEquation(name: String, equation: (Seq[Term], Term), equality: String, sb: StringBuilder) :  StringBuilder = {
    sb.appendInner(name + " " + (equation._1 map prettyPrint).mkString(" ") + equality + prettyPrint(equation._2))
  }

  def innerTerm(s: String) : String = "\""+s+"\""

  def prettyPrintOuterDecls(decls: Seq[OuterDecl], sb: StringBuilder) : StringBuilder = {
    decls.foreach(
      decl => prettyPrint(decl, sb).append(System.lineSeparator()*2)
    )
    sb
  }

  def prettyPrint(theory: Theory) : String  = {
    val outerDeclsString = {
      val stringBuilder = new StringBuilder()
      prettyPrintOuterDecls(theory.decls, stringBuilder).toString
    }

    "theory " + theory.theoryName + System.lineSeparator() +
      " imports " + theory.importTheories.map(
      //make sure that actual paths that are not referring directly to the same folder are surrounded by quotes
      s => if(s.contains("\\") || s.contains("/")) { "\""+s+"\"" } else { s }
    ).mkString(" ") + System.lineSeparator() +
    "begin" + System.lineSeparator() +
      outerDeclsString +
    "end"
  }

  implicit class StringBuilderExtension(sb: StringBuilder) {
    def newLine: StringBuilder = sb.append(System.lineSeparator())

    def appendInner(s: String): StringBuilder = {
      sb.append("\"").append(s).append("\"")
    }
  }

}

