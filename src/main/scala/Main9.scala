import chisel3._
import chisel3.util._

class Main9 extends Module {

  val pc = RegInit(0.S(32.W))
  val i = RegInit(0.S(32.W))

  switch(pc) {
    is(0.S) {

      pc := 1.S
    }
    is(1.S) {

      pc := Mux(i < 10.S, 2.S, 3.S)
    }
    is(2.S) {
      printf("%d\n",i)
      //println(i);
      i := i + 1.S
      pc := 1.S
    }
    is(3.S) {

      pc := 1000.S
    }
  }
}

/*
  val xx="""
  MakeCode {
    i = 1;
    while (i < n) {
      j = n - 1;
      while (j >= i) {
        h1 = data[j];
        skip;
        h2 = data[j - 1];
        skip;
        if (h2 > h1) {
          data[j - 1] = h1;
          skip;
          data[j] = h2;
        }
        j = j - 1;
      }
      i = i + 1;
    }
  }
"""
  val pc = RegInit(0.S(32.W))
  val tm = RegInit(0.S(32.W))
  val rnd = RegInit(0.U(32.W))

  val n = RegInit(0.S(32.W))
  val m = RegInit(0.S(32.W))
  val h1 = RegInit(0.S(32.W))
  val h2 = RegInit(0.S(32.W))
  val h3 = RegInit(0.S(32.W))
  val i = RegInit(0.S(32.W))
  val j = RegInit(0.S(32.W))
  val data = Reg(Vec(100, SInt(32.W)))


  switch(pc){
    is(0.S){
      i := 1.S
      pc :=Mux(i<n,1.S,2.S)
    }
    is(1.S){
      j := n-1.S
      pc :=Mux(j>=i,3.S,4.S)
    }
    is(3.S){
      h1 := data((j).asUInt)
      pc :=5.S
    }
    is(5.S){
      h2 := data((j-1.S).asUInt)
      pc :=6.S
    }
    is(6.S){

      pc :=Mux(h2>h1,7.S,8.S)
    }
    is(7.S){
      data((j-1.S).asUInt) := h1
      pc :=9.S
    }
    is(9.S){
      data((j).asUInt) := h2
      pc :=8.S
    }
    is(8.S){
      j := j-1.S
      pc :=1.S
    }
    is(4.S){
      i := i+1.S
      pc :=0.S
    }
    is(2.S){

      pc :=1000.S
    }
  }
*/


