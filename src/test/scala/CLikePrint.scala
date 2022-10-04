object CLikePrint {
  def tos(t: Types): String = {
    t match {
      case Typeid("", "") => "int"
      case Typeid(nm, "") => nm
      case Typeid(nm, w) => nm + ":" + w
      case Bundle(ts) => "<" + mapsep(tos: (Types => String), ts, ",") + ">"
      case Vect(t1) => t1 + "[]"
    }
  }

  def mapsep[T](f: T => String, ts: List[T], sp: String): String = {
    ts match {
      case Nil => ""
      case List(t1) => f(t1)
      case t1 :: tr => (f(t1) + sp + mapsep(f, tr, sp))
    }
  }

  //------------------
  def tos(e: Expression): String = {
    e match {
      case Var(v) => v
      case Num(n) => "" + n+".S"
    //  case Arr(a, x) => a + "[" + tos(x) + "]"
      case Arr(a, x) => a + "((" + tos(x) + ").asUInt)"
      case Bin(a, e1, e2) => tos1(e1) + a + tos1(e2)
      case Mon(a, e1) => a + tos1(e1)
      case Cond(a, e1, e2) => "Mux(" + tos(a) + "," + tos(e1) + "," + tos(e2) + ")"
      //case Cond(a, e1, e2) => "(" + tos(a) + "?" + tos(e1) + ":" + tos(e2) + ")"
      case Cons(es) => "<" + mapsep(tos: (Expression => String), es, ",") + ">"
      case Sel(v, n) => v + "." + n
      case Exps(es) => "[" + mapsep(tos: (Expression => String), es, ",") + "]"
      case Call(f, es) => f + "(" + mapsep(tos: (Expression => String), es, ",") + ")"
      case Let(t, v, e1, e2) => "let(" + tos(t) + " " + v + " = " + tos(e1) + ": " + tos(e2) + ")"
      case New(s) => "Module(new "+s+"())"
    }
  }

  /*def mapsepE(f: Expression => String, ts: List[Expression], sp: String): String = {
    ts match {
      case Nil => ""
      case List(t1) => f(t1)
      case t1 :: tr => (f(t1) + sp + mapsepE(f, tr, sp))
    }
  }*/
  def tos1(e: Expression): String = {
    e match {
      case Bin(_, _, _) => "(" + tos(e) + ")"
      case Mon(_, _) => "(" + tos(e) + ")"
      case _ => tos(e)
    }
  }

  //------------------
  def tos(s: Statement): String = {
    tos(s, "")
  }

  def tos(s: Statement, ix: String): String = {
    s match {
      case While(e, s1) => "while(" + tos(e) + ")" + tos1(s1, ix + "  ")
      case If(e, s1) => "if(" + tos(e) + ")" + tos1(s1, ix + "  ")
      case Begin(ss) => tos1(ss, ix + "  ")
   //   case Asg(v, e) => v + "=" + tos(e) + ";"
      case Asg(v, e) => v + " := " + tos(e)
   //  case AAsg(v, e1, e2) => v + "[(" + tos(e1) + ").asUInt] := " + tos(e2)
      case AAsg(v, e1, e2) => v + "((" + tos(e1) + ").asUInt) := " + tos(e2)
      case SCall("connect",(Var(e1)::Var(e2)::Nil)) => mkconnect(e1,e2,ix)
      case SCall("println",(Var(e1)::Nil)) => "printf(\"%d\\n\","+e1+")"
      case SCall(f, es) => f + "(" + mapsep(tos: (Expression => String), es, ",") + ");"
      case Break() => "break;"
      case Continue() => "continue;"
      case ForEach(x,e1,s1) => "foreach("+x+":"+tos(e1)+")"+tos1(s1,ix+"  ")
      case Skip() => "skip;"
    }
  }

  def tos1(s: Statement, ix: String): String = {
    s match {
      case Begin(es) => tos1(es, ix)
      case _ => tos1(List(s), ix)
    }
  }

  def tos1(ss: List[Statement], ix: String): String = {
    "{\n  " + ix + tos2(ss, ix + "  ") + "\n" + ix + "}"
  }

  def tos2(ss: List[Statement], ix: String): String = {
    ss match {
      case Nil => ""
      case a :: Nil => tos(a, ix)
      case a :: b => tos(a, ix) + "\n" + ix + tos2(b, ix)
    }
  }

  //------------------
  def tos(ds: List[Declaration]): String = {
    ds match {
      case Nil => ""
      case a :: b => tos(a) + "\n" + tos(b);
    }
  }

  def tos(d: Declaration): String = {
    d match {
      case Def(Typeid("instream",_), v, Num(0)) => "val " + v + " = new InStreamIOs()"
      case Def(Typeid("outstream",_), v, Num(0)) => "val " + v + " = new OutStreamIOs()"
      case Def(t, v, e) => tos(t) + " " + v + " = " + tos(e)
      case Const(t, v, e) => "const " + tos(t) + " " + v + " = " + tos(e)
      case Mem(t, v, e) => "mem " + tos(t) + " " + v + " = " + tos(e)
      case Fun(f, p, s) => "void " + f + tos0(p) + tos1(List(s), "  ")
      case FunE(f, p, e) => "int " + f + tos0(p) + "{" + tos(e) + "}"
      case Stat(s) => tos(s)
      case Module(m ,ds) => {
        val stms = ds.filter(isstream)
        val rest = ds.filter (!isstream(_)) // or filterNot
        val stms1 = if(stms==Nil) "" else
          "val io = IO(new Bundle{\n"+tos(stms)+"})\n"
        "class " + m + " extends Module{\n" +stms1 +
           ioxtr(stms)+ tos(rest) + "}"
      }
    }
  }
  def ioxtr(ds:List[Declaration]):String ={
    ds match{
      case Nil => ""
      case Def(Typeid("instream", _), s, _)::ds1 =>
         "  val "+s+"RecReg =RegInit(0.U(1.W))\n"+
         "  io."+s+".received := "+s+"RecReg\n"+
         "  when(io."+s+".send === 0.U ){"+s+"RecReg := 0.U}\n"
          ioxtr(ds1)
      case Def(Typeid("outstream", _), s, _)::ds1 =>
         "  val " + s + "SendReg =RegInit(0.U(1.W))\n" +
         "  val " + s + "DataReg =RegInit(0.S(32.W))\n" +
         "  io." + s + ".send := " + s + "SendReg\n" +
         "  io." + s + ".data := " + s + "DataReg\n" +
         "  when(io." + s + ".received === 1.U ){"+s+"SendReg := 0.U}\n"+
          ioxtr(ds1)
    }
  }
  def mkconnect(s1:String,s2:String,ix:String):String={
    val s1a=totext(s1,".");  val s1b=fromtext(s1,".")
    val s2a=totext(s2,".");  val s2b=fromtext(s1,".")
    s1a+".io."+s1b+".received :=" + s2a+".io."+s2b+".received\n"+ix+
    s2a+".io."+s2b+".send :=" + s1a+".io."+s1b+".send\n"+ix+
    s2a+".io."+s2b+".data :=" + s1a+".io."+s1b+".data"
  }

  def isstream(d: Declaration): Boolean = {
    d match {
      case Def(Typeid("instream", _), _, _) => true
      case Def(Typeid("outstream", _), _, _) => true
      case _ => false
  } }
  def tos0(p: List[Params]): String = {
    "(" + tos1(p) + ")"
  }

  def tos1(p: List[Params]): String = {
    p match {
      case Nil => ""
      case Param(t, v) :: Nil => tos(t) + " " + v
      case Param(t, v) :: b => tos(t) + " " + v + "," + tos1(b)
    }
  }
  def fromtext(s:String,t:String):String={
    s.substring(s.indexOf(t)+1)}
  def totext(s:String,t:String):String={
    s.substring(0,s.indexOf(t))}

  //
  def tosStates(ss:List[State]):String={
    "switch(pc){\n"+tos5(ss,"  ")+"}"
  }
  def tos5(ss:List[State],ix:String):String={
    ss match {
      case Nil => ""
      case State(n,ss1,e)::ss2 =>
        "  is("+n+".S){\n  "+ix+tos2(ss1,ix+"  ")+"\n"+
        "    pc :="+tos(e)+"\n  }\n"+tos5(ss2,ix)
    }
  }


  def mkStarter(m: String, zz: List[Declaration]): String = {
    "import chisel3._\n" +
      "import chisel3.util._\n\n" +
      "import chisel3.util.random.LFSR\n\n" +
      "class " + m + " extends Module {\n" +
      "  val pc = RegInit(0.S(32.W))\n" +
      "  val rnd = RegInit(0.U(32.W))\n" +
      mkStarter1(zz)
  }

  def mkStarter1(zz: List[Declaration]): String = {
    zz match {
      case (Def(t, v, Num(n)) :: r) =>
        "  val " + v + " = RegInit(" + n + ".S(32.W))\n" +
          mkStarter1(r)
      case (Def(Vect(t), v, Call("alloc", List(Num(n)))) :: r) =>
        // "  val " + v + " = " + "SyncReadMem("+n+", SInt(32.W))\n"+
        "  val " + v + " = " + "Reg(Vec(" + n + ", SInt(32.W)))\n" +
          mkStarter1(r)
      case (_ :: r) => mkStarter1(r)
      case Nil => ""
    }
  }


}
