object CLikeUnit {

  def runTest():Unit ={
    tstE2()
    tstS2()
    tstD2()
    tstT2()
  }

  def tstE(s: String): Unit = {
    CLikeParser.parserE1(s) match {
      case (Some(t), " ", p) => {
        println(s + " [" + p + "] " + t);
        println("  " + CLikePrint.tos(t));
        ()
      }
      case (None, x, p) => {
        println(s + " '" + x + "' [" + p + "] ");
        assert(false);
        ()
      }
    }
  }

  def tstS(s: String): Unit = {
    CLikeParser.parserS1(s) match {
      case (Some(t), " ", p) => {
        println(s + " [" + p + "] " + t);
        println("  " + CLikePrint.tos(t));
        ()
      }
      case (None, x, p) => {
        println(s + " '" + x + "' [" + p + "] ");
        assert(false);
        ()
      }
    }
  }

  def tstD(s: String): Unit = {
    CLikeParser.parserD1(s) match {
      case (Some(t), " ", p) => {
        println(s + " [" + p + "] " + t);
        println("  " + CLikePrint.tos(t));
        ()
      }
      case (None, x, p) => {
        println(s + " '" + x + "' [" + p + "] ");
        assert(false);
        ()
      }
    }
  }

  def tstT(s: String): Unit = {
    CLikeParser.parserT1(s) match {
      case (Some(t), " ", p) => {
        println(s + " [" + p + "] " + t);
        println("  " + CLikePrint.tos(t));
        ()
      }
      case (None, x, p) => {
        println(s + " '" + x + "' [" + p + "] ");
        assert(false);
        ()
      }
    }
  }

  def tstM(s: String): Unit = {
    CLikeParser.parserD1(s) match {
      case (Some(t), " ", p) => {
        println(s + " [" + p + "] " + t);
        println("  " + CLikePrint.tos(t));
        val ds = CLikeMacro.expandMacro(t)
        println("  " + CLikePrint.tos(ds));
        ()
      }
      case (None, x, p) => {
        println(s + " '" + x + "' [" + p + "] ");
        assert(false);
        ()
      }
    }
  }

  def tstE2(): Unit = {
    tstE("  12  ")
    tstE("  x  ")
    tstE("  12+13*14-15  ")
    tstE("  x*y%14  ")
    tstE("  ((x))  ")
    tstE("  ((x?p:z))  ")
    tstE("  let(x=13:(x?p:z))  ")
    tstE("  f(x,y,z)  ")
    tstE("  f(x,y,z)+g(x,y,z)  ")
    tstE("  [x,3,5]  ")
    tstE("  ar[1]+ar[2]  ")
    tstE("  <x,3,5>  ")
    tstE("  p.2 + p.1  ")
  }

  def tstS2(): Unit = {
    tstS("  x=10;  ")
    tstS("  f();  ")
    tstS("  f(x+y,z);  ")
    tstS("  if(p){x++;}  ")
    tstS("  while(p||q)x++;  ")
    tstS("  for(i=10;i<20;i++){x++;ar[i]=0;}  ")
  }

  def tstD2(): Unit = {
    tstD("  int x;  ")
    tstD("  sint:w x=3;  ")
    tstD("  const x=3;  ")
    tstD("  const sint x =3;  ")
    tstD("  const grf=[1,2,3];  ")
    tstD("  x=3;  ")
    tstD("  int f(){ 3 }  ")
    tstD("  void f(){ x=3; }  ")
    tstD("  int f(x,int y){ x+y }  ")
    tstD("  void f(z){ z=3; }  ")
    //tstD("  void f() x=3; }  ")
  }

  def tstT2(): Unit = {
    tstT("  int:12  ")
    tstT("  sint:w  ")
    tstT("  sint:12[]  ")
    tstT("  int<12,8,12>  ")
    tstT("  int<int:12,uint:8,sint:12>  ")
  }

  def tstM2(): Unit ={
    tstM("  void f(x){z = x; x=3;}  f(y); f(2); ")
    tstM("  int f(x){ x*2 }  z= f(2); ")
    tstM("  int y=3; int f(x){ x*2 }  z= f(y); ")
    tstM("  const y=3; int f(x){ x*2 }  z= f(y); ")
  }

}
