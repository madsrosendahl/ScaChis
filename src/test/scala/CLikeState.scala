case class State(n:Int,ss:List[Statement],nxt:Expression);

object CLikeState {
    var stcnt = 0

  /*def makeState(ss: List[Statement]): List[State] = {
      stcnt = 0
      makeState(0,1000,ss)
    }*/

  def makeState(ss: List[Declaration]): List[State] = {
    stcnt = 0
    makeState(0, 1000, getStat(ss))
  }

  def makeState(fst: Int, nxt: Int, ss: List[Statement]): List[State] = {
      ss match {
        case While(b, s) :: ss1 => {
          val cnd = stcnt + 1
          val bod = stcnt + 2
          val aft = stcnt + 3
          stcnt = stcnt + 3
          val sts0 = State(fst, Nil, Num(cnd))
          val sts1 = State(cnd, Nil, Cond(b, Num(bod), Num(aft)))
          val sts2 = makeState(bod, cnd, List(s))
          val sts3 = makeState(aft, nxt, ss1)
          List(sts0,sts1) ++ sts2++sts3
        }
        case If(b, s) :: ss1 => {
          val bod = stcnt + 1
          val aft = stcnt + 2
          stcnt = stcnt + 2
          val st1 = State(fst, Nil, Cond(b, Num(bod), Num(aft)))
          val sts1 = makeState(bod, aft, List(s))
          val sts2 = makeState(aft, nxt, ss1)
          st1 :: (sts1 ++ sts2)
        }
        case Begin(ss) :: ss1 => makeState(fst, nxt, ss ++ ss1)
        case Skip() :: ss1 => {
          val mid = stcnt + 1
          stcnt = stcnt + 1
          val st1 = State(fst, Nil, Num(mid))
          val sts = makeState(mid, nxt, ss1)
          st1 :: sts
        }
        case s1 :: ss1 => {
          val sts = makeState(fst, nxt, ss1)
          sts match {
            case State(ix, ss, nx) :: sts1 => State(ix, s1 :: ss, nx) :: sts1
            case Nil => State(fst, List(s1), Num(nxt)) :: Nil
          }
        }
        case Nil => List(State(fst, List(), Num(nxt)))
      }

    }

  def getStat(ds: List[Declaration]): List[Statement] = {
    ds match {
      case Stat(Begin(ss)) :: ds1 => ss ++ getStat(ds1)
      case Stat(s) :: ds1 => List(s) ++ getStat(ds1)
      case Nil => Nil
      case _ :: ds1 => getStat(ds1)
    }
  }

  def isSimpleState(s:State):Boolean={
    s match { case State(_,s,Num(_)) => isSimpleStm(s)
              case State(_,s,_) => false}
  }
  def isSimpleStm(ss:List[Statement]):Boolean ={
    ss match {
      case Nil => true
      case (Asg(_,e1)::ss2) => isSimpleExp(e1) && isSimpleStm(ss2)
      case _ => false
    }
  }
  def isSimpleExp(e:Expression):Boolean ={
    e match{
      case Var(_) => true
      case Num(_) => true
      case Bin(_,e1,e2) => isSimpleExp(e1)&&isSimpleExp(e2)
      case Mon(_,e1) => isSimpleExp(e1)
      case _ => false
    }
  }

  //  make call to radom  a two step process
  def changeRandom(ds:List[Declaration]):List[Declaration] ={
    ds.map(changeRandom)
  }
  def changeRandom(ds:Declaration):Declaration = {
    ds match {
      case Stat(s) => Stat(changeRandomS(s))
      case x => x
    }
  }
  def changeRandomS(s:Statement):Statement = {
      CLikeParser.mkBegin(changeRandomSs(s::Nil))
  }
  def changeRandomSs(s:List[Statement]):List[Statement] = {
    s match {
      case (Begin(s1) :: r) => changeRandomSs(s1 ++ r)
      case (Asg(x, Call("random", es)) :: r) =>
        Asg("rnd", Var("LFSR(16)")) :: Skip() :: Asg(x, Var("rnd.asSInt")) :: changeRandomSs(r)
      case (While(e,s) :: r) => While(e,changeRandomS(s)) :: changeRandomSs(r)
      case (If(e,s) :: r) => If(e,changeRandomS(s)) :: changeRandomSs(r)
      case x :: r => x :: changeRandomSs(r)
      case Nil => Nil
    }
  }
  /*

    def statNxtGrf(lst: List[State]): List[(Int, List[Int])] = {
      lst.map({ case State(a, _, c) => (a, expNxtStat(c)) })
    }

    def expNxtStat(e: Expression): List[Int] = {
      e match {
        case Num(n) => List(n)
        case Cond(_, e1, e2) => expNxtStat(e1) ++ expNxtStat(e2)
        case _ => List()
      }
    }

    //
    def revGrf(lst: List[(Int, List[Int])]): List[(Int, List[Int])] = {
      lst match {
        case Nil => Nil
        case ((a, b) :: r) => addToSet(a, b, revGrf(r))
      }
    }

    def addToSet(a: Int, b: List[Int], r: List[(Int, List[Int])]): List[(Int, List[Int])] = {
      b match {
        case Nil => r
        case c :: d => addToSet(a, c, addToSet(a, d, r))
      }
    }

    def addToSet(a: Int, b: Int, lst: List[(Int, List[Int])]): List[(Int, List[Int])] = {
      lst match {
        case Nil => (b, List(a)) :: lst
        case (c, d) :: r => if (b == c) (c, add(a, d)) :: r else (c, d) :: addToSet(a, b, r)
      }
    }

    def mem[T](a: T, b: List[T]): Boolean = {
      b match {
        case Nil => false
        case (c :: d) => if (a == c) true else mem(a, d)
      }
    }

    def add[T](a: T, b: List[T]): List[T] = {
      b match {
        case Nil => List(a)
        case (c :: d) => if (a == c) b else c :: add(a, d)
      }
    }


   */
}

    /*
    def main(args: Array[String]): Unit = {
      println("----")
      val prg = readFile("src/main/scala/Dijkstra.txt")
      val (y1, a, b) = CLikeParser.parserD1(prg)
      println("Done " + a + " " + b)
      println("====")
      val y2 = CLikeParser.someOrElse(y1, CLikeParser.noDecl)
      //println(CLikePrint.tos(y2))
      val y3 = CLikeMacro.expandMacro(y2)
      //println(CLikePrint.tos(y3))
      val y4 = getStat(y3)
      //y4.foreach((x)=> println(x))
      //
      val y5 = makeState(0, 1000, y4)
      //
      val y6 = statNxtGrf(y5)
      val y7 = revGrf(y6)
      //
      //val zz1=List((1,List(2,3)),(4,List(2,5)))
      //val zz2 =revGrf(zz1)
      //println(zz1)
      //println(zz2)
      y6.foreach((x) => println(x))
      println()
      y7.foreach((x) => println(x))
      //
      //y5.foreach({case (State(x,y,z)) => {
      // println(x); println(CLikePrint.tos1(Begin(y),""));
      // println("pc = "+CLikePrint.tos(z)); println("");println("");() }})
    }
    */
