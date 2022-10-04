import chisel3._
import chisel3.util._

import chisel3.util.random.LFSR

class Bubble extends Module {
  val pc = RegInit(0.S(32.W))
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
    n := 10.S
    m := 2.S
    pc :=1.S
  }
  is(1.S){
    
    pc :=Mux(m>0.S,2.S,3.S)
  }
  is(2.S){
    i := 0.S
    pc :=4.S
  }
  is(4.S){
    
    pc :=Mux(i<n,5.S,6.S)
  }
  is(5.S){
    rnd := LFSR(16)
    pc :=7.S
  }
  is(7.S){
    h3 := rnd.asSInt
    pc :=8.S
  }
  is(8.S){
    data((i).asUInt) := h3
    i := i+1.S
    pc :=4.S
  }
  is(6.S){
    i := 1.S
    pc :=9.S
  }
  is(9.S){
    
    pc :=Mux(i<n,10.S,11.S)
  }
  is(10.S){
    j := n-1.S
    pc :=12.S
  }
  is(12.S){
    
    pc :=Mux(j>=i,13.S,14.S)
  }
  is(13.S){
    h1 := data((j).asUInt)
    pc :=15.S
  }
  is(15.S){
    h2 := data((j-1.S).asUInt)
    pc :=16.S
  }
  is(16.S){
    
    pc :=Mux(h2>h1,17.S,18.S)
  }
  is(17.S){
    data((j-1.S).asUInt) := h1
    pc :=19.S
  }
  is(19.S){
    data((j).asUInt) := h2
    pc :=18.S
  }
  is(18.S){
    j := j-1.S
    pc :=12.S
  }
  is(14.S){
    i := i+1.S
    pc :=9.S
  }
  is(11.S){
    i := 0.S
    pc :=20.S
  }
  is(20.S){
    
    pc :=Mux(i<n,21.S,22.S)
  }
  is(21.S){
    h1 := data((i).asUInt)
    pc :=23.S
  }
  is(23.S){
    printf("%d\n",h1)
    i := i+1.S
    pc :=20.S
  }
  is(22.S){
    m := m-1.S
    pc :=1.S
  }
  is(3.S){
    
    pc :=1000.S
  }
}
}
