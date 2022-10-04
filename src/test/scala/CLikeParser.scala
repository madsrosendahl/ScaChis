import scala.util.parsing.combinator._

object CLikeParser extends RegexParsers {
  //-------------------------------------
  //  parser
  //
  override def skipWhitespace = true
    override val whiteSpace = "(([ \t\n\f]+)|(//[^\n]*[\n])])+".r
    def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit
    def ident: Parser[String] = {
      "[a-zA-Z][a-zA-Z0-9]*".r ^^ { str => str }
    }
    def num: Parser[String] = {
      "[0-9][0-9]*".r ^^ { str => str }
    }
    //------------------------------------------------------
    // types

    def typeid: Parser[Types] = {
      ("int" | "sint" | "uint" | "any" | "bundle" | "val") ~ opt(":" ~ (num | ident)) ^^ {
        case v ~ w =>  Typeid(v, optw(w))
      } |
      "instream"  ^^ { case x => Typeid(x,"" ) } |
      "outstream" ^^ { case x => Typeid(x,"") }
    }

    def typecomp: Parser[Types] = {
      typeid ~ "[" ~ "]" ^^ {
        case t ~ _ ~ _ => Vect(t)
      } |
      typeid ~ "<" ~ rep1sep(typewd, ",") ~ ">" ^^ {
        case v ~ _ ~ wd ~ _ => Bundle(map(t1 => addtp(v, t1), wd))
      } |
      typeid ^^ { case t => t }
    }

    def addtp(t: Types, w: Types): Types = {
      w match {
        case Typeid("", w1) => {
          t match {
            case Typeid(t2, w2) => Typeid(t2, w1)
            case _ => w
          }
        }
        case _ => w
      }
    }

    def typewd: Parser[Types] = {
      num ^^ { case s => Typeid("", s) } |
        typeid ^^ { case t => t }
    }

    def optw(w: Option[String ~ String]): String = {
      w match {
        case Some(c ~ w1) => w1;
        case None => ""
      }
    }

    def noType(): Types = { Typeid("", "") }

    //-------------------------------------------------------
    // Expressions

    def expression: Parser[Expression] = {
      expression1 ~ rep(operator0 ~ expression1) ^^ {
        case t1 ~ xs => combineExp(t1, xs)
      }
    }

    def operator0: Parser[String] = {
      "==" | "!=" | ">=" | ">" | "<=" | "<" | "&&" | "||" ^^ { s => s }
    }

    def expression1: Parser[Expression] = {
      term ~ rep(operator1 ~ term) ^^ { case t1 ~ xs => combineExp(t1, xs) }
    }

    def operator1: Parser[String] = {
      "+" | "-" ^^ { s => s }
    }

    def term: Parser[Expression] = {
      factor ~ rep(operator2 ~ factor) ^^ {
        case t1 ~ xs => combineExp(t1, xs)
      }
    }

    def operator2: Parser[String] = {
      "*" | "/" | "%" ^^ { s => s }
    }

    def factor: Parser[Expression] = {
      //"[\\-]?[0-9]+".r ^^ {str =>Num(str.toInt)} |
      "let" ~ "(" ~ opt(typecomp) ~ ident ~ "=" ~ expression ~ ":" ~ expression ~ ")" ^^ {
        case _ ~ _ ~ t ~ x ~ _ ~ e1 ~ _ ~ e2 ~ _ => Let(t.getOrElse(noType()), x, e1, e2)
      } |
      ident ~ "." ~ "[0-9]+".r ^^ { case x ~ _ ~ e => Sel(x, e.toInt) } |
      ident ~ "[" ~ expression ~ "]" ^^ { case x ~ _ ~ e ~ _ => Arr(x, e) } |
      ident1 ~ "(" ~ ")" ^^ { case x ~ _ ~ _ => Call(x, Nil) } |
      ident1 ~ "(" ~ expression ~ rep("," ~ expression) ~ ")" ^^ {
        case x ~ _ ~ e ~ es ~ _ => Call(x, e +: repList(es))
      } |
      "new" ~ident ~ "(" ~")" ^^ { case _~x~_~_ => New(x) } |
      num ^^ { str => Num(str.toInt) } |
      ident1 ^^ { case str => Var(str) } |
      "<" ~ repsep(expression, ",") ~ ">" ^^ {
        case _ ~ xs ~ _ => Cons(xs)
      } |
      "[" ~ repsep(expression, ",") ~ "]" ^^ {
        case _ ~ xs ~ _ => Exps(xs)
      } |
      "(" ~ expression ~ opt("?" ~ expression ~ ":" ~ expression) ~ ")" ^^ {
        case _ ~ e ~ None ~ _ => e
        case _ ~ e ~ Some(_ ~ e1 ~ _ ~ e2) ~ _ => Cond(e, e1, e2)
      }
    }

    def ident1: Parser[String] ={
      ident ~opt("." ~ident) ^^ {
        case x ~ Some(_ ~ y) => x +"."+y
        case x ~ None => x
      }
    }
    def noExp(): Expression = { Num(0) }

    def combineExp(e1: Expression, r: List[String ~ Expression]): Expression = {
      r match {
        case Nil => e1
        case (s ~ e2) :: rs => combineExp(Bin(s, e1, e2), r.tail)
      }
    }

    //def repList(r: List[String ~ Expression]):List[Expression] = {map(sel2of2pat,r)}
    def repList(r: List[String ~ Expression]): List[Expression] = {
      r.map(sel2of2pat)
    }

    def sel1of2pat[T](r: T ~ _): T = {
      r match {
        case (x ~ _) => x
      }
    }

    def sel2of2pat[T](r: _ ~ T): T = {
      r match {
        case (_ ~ x) => x
      }
    }

    //----------------------------------------------------------

    def statement: Parser[Statement] = {
      "while" ~ "(" ~ expression ~ ")" ~ statement ^^ {
        case _ ~ _ ~ e ~ _ ~ sb => {
          While(e, sb)
        }
      } |
        "if" ~ "(" ~ expression ~ ")" ~ statement ^^ {
          case _ ~ _ ~ e ~ _ ~ sb => {
            If(e, sb)
          }
        } |
        "for" ~ "(" ~opt("int") ~ statement ~ expression ~ ";" ~ forinc ~ ")" ~ statement ^^ {
          case _~_~_~ s ~ b ~_~ x ~_~ s1 => mkBegin(List(s, While(b, mkBegin(List(s1,x)))))
        } |
        "for" ~ "(" ~ "int" ~ ident ~ ":" ~ expression  ~ ")" ~ statement ^^ {
          case _~_~_ ~x ~_~ e ~_~ s1 => ForEach(x,e,s1)
        } |
        "break" ~ ";" ^^ { case _ ~ _ => Break() } |
        "continue" ~ ";" ^^ { case _ ~ _ => Continue() } |
        "skip" ~ ";" ^^ { case _ ~ _ => Skip() } |
        ident ~ "++" ~ ";" ^^ { case x ~ _ ~ _ => {
          Asg(x, Bin("+", Var(x), Num(1)))  } } |
        ident ~ "--" ~ ";" ^^ { case x ~ _ ~ _ => {
          Asg(x, Bin("-", Var(x), Num(1))) } } |
        ident ~ "=" ~ expression ~ ";" ^^ { case x ~ _ ~ e ~ _ => {
          Asg(x, e)
        }
        } |
        ident ~ "[" ~ expression ~ "]" ~ "=" ~ expression ~ ";" ^^ {
          case x ~ _ ~ e ~ _ ~ _ ~ e1 ~ _ => {
            AAsg(x, e, e1)
          }
        } |
        ident1 ~ "(" ~ ")" ~ ";" ^^ { case x ~ _ ~ _ ~ _ => SCall(x, Nil) } |
        ident1 ~ "(" ~ expression ~ rep("," ~ expression) ~ ")" ~ ";" ^^ {
          case x ~ _ ~ e ~ es ~ _ ~ _ => SCall(x, e +: repList(es))
        } |
        "{" ~ block ~ "}" ^^ { case _ ~ b ~ _ => b }
    }

  def forinc: Parser[Statement] = {
    ident ~ "++" ^^ { case x ~_ => Asg(x,Bin("+",Var(x),Num(1)))} |
    ident ~ "--" ^^ { case x ~_ => Asg(x,Bin("-",Var(x),Num(1)))}
  }

  def block: Parser[Statement] = {
      rep1(statement) ^^ { sl => if (sl.length == 1) sl.head; else mkBegin(sl) }
    }

    def mkBegin(sl: List[Statement]): Statement = {
      flatBegin(sl) match {
        case s :: Nil => s
        case sx => Begin(sx)
      }
    }

    def flatBegin(sl: List[Statement]): List[Statement] = {
      sl match {
        case Begin(a) :: b => flatBegin(a ++ b)
        case a :: b => a :: flatBegin(b)
        case Nil => Nil
      }
    }

    def noStat(): Statement = { Skip()  }

    //----------------------------------------------------------
    // Decl
    def declaration: Parser[Declaration] = {
      typecomp ~ ident ~ opt("=" ~ expression) ~ ";" ^^ {
        case t ~ v ~ None ~ _ => Def(t, v, noExp())
        case t ~ v ~ Some(_ ~ e) ~ _ => Def(t, v, e)
      } |
      "int" ~ ident ~ "(" ~ params ~ ")" ~ "{" ~ expression ~ "}" ^^ {
          case _ ~ v ~ _ ~ p ~ _ ~ _ ~ e ~ _ => FunE(v, p, e)
      } |
      "const" ~ opt(typecomp) ~ ident ~ "=" ~ expression ~ ";" ^^ {
        case _ ~ t ~ v ~ _ ~ e ~ _ => Const(t.getOrElse(noType()), v, e)
      } |
      "mem" ~ opt(typecomp) ~ ident ~ "=" ~ expression ~ ";" ^^ {
        case _ ~ t ~ v ~ _ ~ e ~ _ => Mem(t.getOrElse(noType()), v, e)
      } |
      "void" ~ ident ~ "(" ~ params ~ ")" ~ "{" ~ block ~ "}" ^^ {
        case _ ~ f ~ _ ~ p ~ _ ~ _ ~ b ~ _ => Fun(f, p, b)
      } |
      statement ^^ { s => Stat(s) } |
      "module" ~ ident ~ "{" ~  declarations ~ "}" ^^ {
        case _ ~ m ~ _ ~ b ~  _ => Module(m, b) }  |
      "main" ~ "{" ~ declarations ~ "}" ^^ {
        case _~ _ ~ b ~ _ => Module("Main", b) }
    }

    def declarations: Parser[List[Declaration]] = {
      rep1(declaration) ^^ { ds => ds }
    }

    def params: Parser[List[Params]] = {
      repsep(opt(typecomp) ~ ident, ",") ^^ {
        //case x =>  x.map(case (t ~ v) => Param(someOrElse(t,noType),v)}
        case x => x.map(bldParam)
      }
    }

    def bldParam(x: (Option[Types] ~ String)): Params = {
      Param(someOrElse(sel1of2pat(x), noType), sel2of2pat(x))
    }

    def noDecl(): List[Declaration] = { List(): List[Declaration] }

    //--------------------------------------


    def parserX[T](nt: Parser[T], inp: String): (Option[T], String, String) = {
      parse(nt, inp) match {
        case NoSuccess(msg, next) => {
          (None, "" + next.first, "" + next.pos)
        }
        case Success(msg, next) => {
          (Some(msg), "" + next.first, "" + next.pos)
        }
        case Failure(msg, next) => {
          (None, "" + next.first, "" + next.pos)
        }
        case Error(msg, next) => {
          (None, "" + next.first, "" + next.pos)
        }
        case x => {
          (None, "", "")
        }
      }
    }
    // ----------------------------------------------------------
    //
    //  Aux functions
    //
    def map[T, U](f: T => U, ts: List[T]): List[U] = {
      ts match {
        case Nil => Nil
        case a :: b => f(a) :: map(f, b)
      }
    }

    def sel1of3[T](p: (T, Any, Any)): T = p match {  case (a, _, _) => a }

    def someOrElse[T](x: Option[T], y: () => T): T = { //x.orElse(y)}
      x match {
        case Some(y) => y
        case None => y()
      }
    }

    def parserE(inp: String): Expression = {
      someOrElse(sel1of3(parserX(expression, inp)), noExp) }

    def parserS(inp: String): Statement = {
      someOrElse(sel1of3(parserX(statement, inp)), noStat) }

    def parserD(inp: String): List[Declaration] = {
      someOrElse(sel1of3(parserX(declarations, inp)), noDecl) }

    def parserT(inp: String): Types = {
      someOrElse(sel1of3(parserX(typecomp, inp)), noType)
    }

  def parserT1(inp:String):(Option[Types],String,String)={
    parserX(typecomp,inp) }
  def parserE1(inp:String):(Option[Expression],String,String) ={
    parserX(expression,inp) }
  def parserS1(inp:String):(Option[Statement],String,String) ={
    parserX(statement,inp) }
  def parserD1(inp:String):(Option[List[Declaration]],String,String)={
    parserX(declarations,inp) }

  }
//---------------------------


