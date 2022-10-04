object CLikeMacro {

  def expandMacro(y:List[Declaration]):List[Declaration] ={
    val ds= y.map(x => expandMacro(x,y,0))
    val ds1=ds.filter({case Stat(_) => true case _ => false })
    val ds2=ds.filter({case Stat(_) => false case _ => true })
    val ds3=ds1.flatMap({case Stat(sx) => List(sx) case _ => Nil })
    Stat(mkBegin(ds3))+:ds2 //++ds1
  }
  def expandMacro(d:Declaration,ds:List[Declaration],n:Int):Declaration ={
    d match {
      case Stat(s) => Stat(expandMacro(s,ds,n))
      case Def(t,v,e) => Def(expandMacro(t,ds,n),v,expandMacro(e,ds,n))
      case Const(t,v,e) =>Const(expandMacro(t,ds,n),v,expandMacro(e,ds,n))
      case Mem(t,v,e) =>Mem(expandMacro(t,ds,n),v,expandMacro(e,ds,n))
      case d1 => d1
    }
  }
  def expandMacro(s:Statement,ds:List[Declaration],n:Int):Statement ={
    //println("expandE "+s);
    s match {
      case Asg(v,e) =>  Asg(v,expandMacro(e,ds,n))
      case AAsg(v,e1,e2) =>  AAsg(v,expandMacro(e1,ds,n),expandMacro(e2,ds,n))
      case SCall(f,es) =>  {
        val d1=findDecl(f,ds);
        //println("dcl "+f+" "+d1);
        d1 match {
          case Fun(g,vs,s2) => {//println("match: "+e2);
            val env= bldConstEnv(vs,expandMacro1(es,ds,n));
            //println("Env "+env);
            if(n>20)s2 else expandMacro(s2,env++ds,n+1) }
          case _ => s
        }}
      case While(e1,s1)=>While(expandMacro(e1,ds,n),expandMacro(s1,ds,n))
      case If(e1,s1)=>If(expandMacro(e1,ds,n),expandMacro(s1,ds,n))
      case Begin(ss)=>mkBegin(ss.map((s1:Statement)=>expandMacro(s1,ds,n)))
      case _ => s
    }
  }
  def mkBegin(ss:List[Statement]):Statement = {
    CLikeParser.flatBegin(ss) match {
      case a :: Nil => a
      case ss1 => Begin(ss1)
    }
  }

  def expandMacro(e:Expression,ds:List[Declaration],n:Int):Expression = {
    //println("expandE "+e);
    e match{
      case Num(n) => e
      case Var(v) => {
        val d1 = findDecl(v,ds);
        //println(v+" "+d1);
        d1 match {
          case Const(t,w,e1) => if(n>20) e1 else expandMacro(e1,ds,n+1);
          case _ => Var(v)
        }
      }
      case Call(f,es) => { val d1=findDecl(f,ds);
        //println(f+" "+d1);
        d1 match {
          case FunE(g,vs,e2) => {//println("match: "+e2);
            val env= bldConstEnv(vs,expandMacro1(es,ds,n));
            //println("Env "+env);
            if(n>20)e2 else expandMacro(renameLet(e2),env++ds,n+1) }
          case _ => e
        }}
      case Bin(x,e1,e2) => {
        (x, expandMacro(e1, ds,n), expandMacro(e2, ds,n)) match {
          case ("+",Num(a1),Num(a2)) => Num(a1+a2)
          case ("*",Num(a1),Num(a2)) => Num(a1*a2)
          case ("-",Num(a1),Num(a2)) => Num(a1-a2)
          case ("/",Num(a1),Num(a2)) => Num(a1/a2)
          case ("%",Num(a1),Num(a2)) => Num(a1%a2)
          case ("==",Num(a1),Num(a2)) => Num(if(a1==a2)1 else 0)
          case ("!=",Num(a1),Num(a2)) => Num(if(a1!=a2)1 else 0)
          case (">=",Num(a1),Num(a2)) => Num(if(a1>=a2)1 else 0)
          case (">", Num(a1),Num(a2)) => Num(if(a1> a2)1 else 0)
          case ("<=",Num(a1),Num(a2)) => Num(if(a1<=a2)1 else 0)
          case ("<", Num(a1),Num(a2)) => Num(if(a1< a2)1 else 0)
          case ("&&",Num(a1),Num(a2)) => Num(if((a1&a2)==1)1 else 0)
          case ("||",Num(a1),Num(a2)) => Num(if((a1|a2)==1)1 else 0)
          case(y,a1,a2) => Bin(y,a1,a2)
        }
      }
      case Cond(e1,e2,e3) => {
        expandMacro(e1,ds,n) match {
          case Num(0) => expandMacro(e3,ds,n)
          case Num(1) => expandMacro(e2,ds,n)
          case e4 => Cond(e4,expandMacro(e2,ds,n),expandMacro(e3,ds,n))
        }
      }
      case Let(t,v,e1,e2) => {
        expandMacro(e1,ds,n) match {
          case Num(w) => expandMacro(e2,Const(t,v,Num(w))::ds,n)
          case e3 => Let(t,v,e3,expandMacro(e2,ds,n))
        }
      }
      case Arr(a,e1) =>{
        Arr(a,expandMacro(e1,ds,n))
      }
      case _ => e
    }
  }
  def expandMacro1(es:List[Expression],ds:List[Declaration],n:Int):List[Expression] ={
    es.map(e => expandMacro(e,ds,n))
  }

  def expandMacro(t:Types,ds:List[Declaration],n:Int):Types ={
    //println("expand "+t)
    t match{
      case Typeid(v1,v2) => Typeid(v1,expandStr(v2,ds))
      case Vect(t1) => Vect(expandMacro(t1,ds,n))
      case Bundle(ts) => Bundle(ts.map( (t1:Types)=>expandMacro(t1,ds,n)))
    }
  }
  def expandStr(s:String,ds:List[Declaration]):String={
    findDecl(s,ds) match{
      case Const(_,_,Num(n)) => ""+n
      case _ => s
    }
  }
  def renameLet(e:Expression):Expression={
    //println("TraverseE "+e)
    //traverseE((e1:Expression)=>e1,e)
    traverseE(renameLet1,e)

  }
  def renameLet1(e:Expression):Expression={
    e match{ case Let(t,v,e1,e2) => {
      val w=gensym(v); //println("gensym "+w);
      Let(t,w,traverseE((e3:Expression) => renameLet2(v,w,e3),e1),
        traverseE((e3:Expression) => renameLet2(v,w,e3),e2))

    } case _ => e}
  }
  def renameLet2(v1:String,v2:String,e:Expression):Expression={
    e match{ case Var(v3) => if(v3==v1)Var(v2) else e case _ => e}
  }
  var gensymN=0:Int;
  def gensym(v:String):String ={gensymN= gensymN+1; v+""+gensymN }
  def traverseE(f:Expression=>Expression,e:Expression):Expression={
    f(e match {
      case Var(_) => e
      case Num(_) => e
      case Arr(s,e1) => Arr(s,traverseE(f,e1))
      case Bin(s,e1,e2) => Bin(s,traverseE(f,e1),traverseE(f,e2))
      case Mon(s,e1) => Mon(s,traverseE(f,e1))
      case Cond(e1,e2,e3) => Cond(traverseE(f,e1),traverseE(f,e2),traverseE(f,e3))
      case Cons(es) => Cons(es.map((e1:Expression)=>traverseE(f,e1)))
      case Sel(_,_) => e
      case Exps(es) => Exps(es.map((e1:Expression)=>traverseE(f,e1)))
      case Call(g,es) => Call(g,es.map((e1:Expression)=>traverseE(f,e1)))
      case Let(t,v,e1,e2) => Let(t,v,traverseE(f,e1),traverseE(f,e2))
    })
  }


  def findDecl(f:String,ds:List[Declaration]):Declaration ={
    ds match{
      case Nil => Stat(Skip())
      case FunE(g,vs,e1)::r => if(f==g)ds.head; else findDecl(f,r)
      case Fun(g,vs,s)::r => if(f==g)ds.head; else findDecl(f,r)
      case Const(t,v,e)::r => if(f==v)ds.head; else findDecl(f,r)
      case a::b => findDecl(f,b)
    }
  }
  def bldConstEnv(vs:List[Params],es:List[Expression]):List[Declaration] ={
    (vs,es) match{
      case (Param(t,v)::vsr,e::esr) => Const(t,v,e)::bldConstEnv(vsr,esr)
      case (a,b) => Nil
    }
  }

}
