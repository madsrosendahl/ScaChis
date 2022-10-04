import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.parsing.combinator._
//import scala.control._

class MyTest extends AnyFlatSpec with ChiselScalatestTester {
  "MyModule1" should "pass" in {
    test(new Bubble()) {
    //test(new MyModule1()) {
      dut => { var i=1; while(i<1000){dut.clock.step(); i=i+1; }}
      // dut => RunParser.compileModule("MyModule1")
      //   dut => RunParser.compileModule("Bubble")
    }
  }


}

object RunParser {
  def compileModule(m: String):String = {
    val ifile ="src\\test\\scala\\"+m+".cl"
    val ofile ="src\\main\\scala\\"+m+".scala"
    //println(System.getProperty("user.dir"))
    val lines = scala.io.Source.fromFile(ifile).mkString
    //println(lines)
    val y1 = lines.replace('\r', ' ')
    val y0 = CLikeParser.parserD(y1)
    val y = CLikeState.changeRandom(y0)
    //println(y)
    //println("---")
    val z = CLikeState.makeState(y)
    //val z1 = z.filter(CLikeState.isSimpleState)
    //println("---")
    val xs = CLikePrint.mkStarter(m,y)+CLikePrint.tosStates(z)+"\n}"
    println(xs)
    //println("---")
    //println(CLikePrint.tos(y))

    printToFile(ofile,xs)
    ""
  }
  def printToFile(f: String)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.File(f))
    try { op(p) } finally { p.close() }
  }

  def printToFile(f: String,s: String ) {
    val p = new java.io.PrintWriter(new java.io.File(f))
    try { p.println(s) } finally { p.close() }
  }
}


/*

//dut => RunParser.makeCode()
//dut => CLikeUnit.runTest()
//dut => CLikeUnit.tstM2()


"Main2" should "pass" in {
  test(new Main3()) { dut =>
    var i = 1;
    dut.clock.setTimeout(10000)
    //while (i <  10000) {
    while (i <  0) {
      dut.clock.step()
      val tm = dut.io.done.peek().litValue.toInt
      if(tm>0){i=100000;println("Done! "+tm)}
    }
    //RunParser.makeCode()
    CLikeUnit.runTest()
  }
}

def makeCode(): Int = {
  println("MakeCode")
  //val y = CLikeParser.parserE("2+3")
  val y1 = prog2a.replace('\r', ' ')
  val y = CLikeParser.parserD(y1)
  //println(y)
  // val z = CLikeState
  val y3=CLikePrint.tos(y)
  println(y3)
  val z = CLikeState.makeState(y)
  val z1 = z.filter(CLikeState.isSimpleState)
  println(CLikePrint.tosStates(z1))
  //val z = CLike2Chisel.toChisel(y)
  0
}


val prog2 =
"""
  int n; int m; int h1; int h2; int h3; int i; int j;
  int data = alloc(100);
  n = 100;
  m=2;
  while(m>0){
    for(i=0; i<n; i++){ h3 = random(); skip; data[i] = h3;}
    for(i=1; i<n; i++){
      for(j=n-1; j>=i; j--){
        h1 = data[j]; skip;
        h2 = data[j-1]; skip;
        if(h2 > h1){
          data[j-1] = h1; skip;
          data[j] = h2;
        }
      }
    }
    for(i=0; i<n; i++){ h1 = data[i]; skip; println(h1); }
    m--;
  }
"""
//  ,21,8,35,11,41];

val prog2a =
  """
    i=1;
    for(j=n-1; j>=i; j--){
      h1 = data[j]; skip;
      h2 = data[j-1]; skip;
      if(h2 > h1){
        data[j-1] = h1; skip;
        data[j] = h2;
      }
    }
 """

val prog1a =
  """
main {
 val sendr = new Sender();
 val recvr = new Receiver();
 val fifo = new Fifo();
 connect(sendr.out1,fifo.in1);
 connect(fifo.out1,recvr.in1);
}
"""

val prog1 =
  """
 main {
    val sendr = new Sender();
    val recvr = new Receiver();
    val fifo = new Fifo();
    connect(sendr.out1,fifo.in1);
    connect(fifo.out1,recvr.in1);
 }

 module Sender {
    outstream out1;
    for(int i=1; i<=10;i++)
      out1.write(i);
 }
 module Receiver {
   instream in1;
   for(int x:in1)
     println(x);
 }
 module Fifo {
   instream in1;
   outstream out1;
   for(int x:in1)
     out1.write(x);
 }
"""


*/



/*
*  Add parsecombinator to project
* fetch jar from https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parser-combinators_2.12/2.1.1
*scala-parser-combinators_2.12-2.1.1.jar
* project > open module settings
* > Libraries + Docs1/prog/lib/scala
*
* */

abstract class Statement
abstract class Expression
abstract class Val
abstract class Params
abstract class Types
abstract class Declaration

case class While( e: Expression,  s:Statement ) extends Statement
case class ForEach( x:String, e: Expression,  s:Statement ) extends Statement
case class If( e: Expression, s:Statement ) extends Statement
case class Begin( sl: List[Statement] ) extends Statement
case class Asg( v: String, e:Expression ) extends Statement
case class AAsg( v: String,  i:Expression,  e:Expression ) extends Statement
case class SCall( f:String , exs1:List[Expression] ) extends Statement
case class Break( ) extends Statement
case class Continue( ) extends Statement
case class Skip( ) extends Statement

case class Param(ty:Types,v:String) extends Params

case class Var( v: String ) extends Expression
case class Num( v: Int ) extends Expression
case class Arr( v: String, e:Expression ) extends Expression
case class Bin( v: String, e1:Expression, e2:Expression ) extends Expression
case class Mon( v: String, e:Expression ) extends Expression
case class Cond( b: Expression, e1:Expression, e2:Expression ) extends Expression
case class Cons( e1:List[Expression] ) extends Expression
case class Sel( v: String, n:Int ) extends Expression
case class Exps( exs1:List[Expression] ) extends Expression
case class Call( f:String , exs1:List[Expression] ) extends Expression
case class Let( t:Types , v:String,e1:Expression,e2:Expression ) extends Expression
case class New( n: String ) extends Expression

case class I( v: Int ) extends Val
case class P( vs: List[Int] ) extends Val


case class Def( t:Types,v: String, e:Expression ) extends Declaration
case class Const( t: Types, v: String, e:Expression ) extends Declaration
case class Mem( t: Types, v: String, e:Expression ) extends Declaration
case class Fun( v: String,  as: List[Params],b:Statement ) extends Declaration
case class FunE( v: String, as: List[Params],b:Expression ) extends Declaration
case class Stat( s:Statement ) extends Declaration
case class Module( m:String, s:List[Declaration] ) extends Declaration

case class Typeid(s:String,w:String) extends Types
case class Bundle(ts:List[Types]) extends Types
case class Vect(t:Types) extends Types

