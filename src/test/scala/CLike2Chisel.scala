object CLike2Chisel {
  //-------------------------------------------------------
  //
  //
  def toChisel(ds: List[Declaration]): Unit = {
    ds match{
      case (Module(m,ds)::ds1) => {toChisel(m,ds);toChisel(ds1)}
    }
  }
  def toChisel(m:String,ds: List[Declaration]): Unit = {
    println(" class "+m+" extends Module{")
    println("    val io = IO(new Bundle {")
    println("      val done = Output(SInt(32.W))")
    println("    })")
    println("    val pc = RegInit(0.S(32.W))")
    println("    val tm = RegInit(0.S(32.W))")
    println("    val rnd = RegInit(0.U(32.W))")
    println("");
    toChisel1(ds)
    println("");

    println("    io.done := 0.S")
    println("    tm := tm + 1.S(32.W)")

    toChisel(Begin(getStat(ds)))
    println(" }");
  }

  def getStat(ds: List[Declaration]): List[Statement] = {
    ds match{
      case Nil => Nil
      case (Stat(s)::ds1) => List(s)++getStat(ds1)
      case (d1::ds1) => getStat(ds1)
    }
  }
  def toChisel1(ds: List[Declaration]): Unit = {
    ds match{
      case Nil => ()
      case (Stat(s)::ds1) => toChisel1(ds1)
      case (Def(_,v,Num(n))::ds1) => {
        println("    val "+v+" = RegInit("+n+".S(32.W))")
        toChisel1(ds1)
      }
      case (Def(_, v, Call("alloc",List(Num(n)))) :: ds1) => {
        println("    val " + v + " = Reg(Vec("+n+", SInt(32.W)))")
        toChisel1(ds1)
      }
    }
  }

  def toChisel(s: Statement): Unit = {
    println(" ");
    println(" switch(pc){");
    println("   is(0.S){")
    val st1 = toChisel(s, 0);
    val st2 = st1 + 1
    println("     pc := " + st2 + ".S")
    println("     io.done := tm ")
    println("   }")
    println(" }")
    println(" ");
  }

  def toChisel(s: Statement, st: Int): Int = {
    //print(st+": "); println(s);
    s match {
      case Begin(Nil) => st
      case Begin(s1 :: sx) => {
        val st2 = toChisel(s1, st);
        val st3 = toChisel(Begin(sx), st2); st3
      }
      /*case Mem(v, Exps(es)) => {
        toChExps(v, es, 0); st
      }*/
      case Asg(v, Call("random",Nil)) => {
        val st1 = st + 1
        println ("     rnd := LFSR(16) % 1000.U ")
        println ("     pc := " + st1 + ".S")
        println ("   }")
        println ("   is(" + st1 + ".S){")
        println ("     "+ v + " := rnd.asSInt ");
        st1
      }
      case Asg(v, e) => {
        print("     " + v + " := ");
        toChExp(e);
        println();
        st
      }
      //case AAsg(v,e1,e2) => {print("     "+v+".write( (");toChExp(e1); print(").asUInt,");
      //  toChExp(e2); println(")") ; st}
      case AAsg(v, e1, e2) => {
        print("     " + v + "( (");
        toChExp(e1);
        print(").asUInt) := ");
        toChExp(e2);
        println("");
        st
      }
      case Skip() => {
        val st1 = st + 1
        println("     pc := " + st1 + ".S")
        println("   }")
        println("   is(" + st1 + ".S){")
        st1
      }
      case While(b, s) => {
        val st1 = st + 1
        val st2 = st + 2
        val st3 = st + 3
        println("     pc := " + st1 + ".S")
        println("   }")
        println("   is(" + st1 + ".S){")
        print("     pc := Mux(");
        toChExp(b);
        println(", " + st2 + ".S, " + st3 + ".S)")
        println("   }")
        println("   is(" + st2 + ".S){")
        val st4 = toChisel(s, st3)
        println("     pc := " + st1 + ".S")
        println("   }")
        println("   is(" + st3 + ".S){")
        st4
      }
      case If(b, s) => {
        val st1 = st + 1
        val st2 = st + 2
        val st3 = st + 3
        println("     pc := " + st1 + ".S")
        println("   }")
        println("   is(" + st1 + ".S){")
        print("     pc := Mux(");
        toChExp(b);
        println(", " + st2 + ".S, " + st3 + ".S)")
        println("   }")
        println("   is(" + st2 + ".S){")
        val st4 = toChisel(s, st3)
        println("     pc := " + st3 + ".S")
        println("   }")
        println("   is(" + st3 + ".S){")
        st4
      }
      case SCall("println", List(Var(x)) ) => {
        println("     printf(\"%d\\n\"," + x + ")");
        st
      }
      case x => {
        println("toChisel stat");
        println(x); st
      }
    }
  }

  def toChExps(v: String, es: List[Expression], i: Int): Unit = {
    es match {
      case Nil => ()
      case Num(e1) :: es2 => {
        //println("     " + v + ".write(" + i + ".U," + e1 + ".S)");
        println("     " + v + "(" + i + ".U) := " + e1 + ".S");
        toChExps(v, es2, i + 1)
      }
      case x => {
        println("toChExps");
        println(x)
      }
    }
  }

  def toChExp(e: Expression): Unit = {
    e match {
      case Num(n) => print("" + n + ".S")
      case Var(v) => print("" + v)
      case Bin("==", e1, e2) => {
        toChExp(e1);
        print(" === ");
        toChExp(e2)
      }
      case Bin(v, e1, e2) => {
        toChExp(e1);
        print("" + v);
        toChExp(e2)
      }
      //case Arr(v,e1) => {print(v+".read( ("); toChExp(e1);print(").asUInt )")}
      case Arr(v, e1) => {
        print(v + "( ("); toChExp(e1);
        print(").asUInt )")
      }
      case Cond(b, e1, e2) => {
        print("Mux(");
        toChExp(b);
        print(",");
        toChExp(e1);
        print(",");
        toChExp(e2);
        print(")")
      }
    }
  }

}
