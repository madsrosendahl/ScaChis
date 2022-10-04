import chisel3._
import chisel3.util._

class MyModule1 extends Module {
  val pc = RegInit(0.S(32.W))
  val i = RegInit(1.S(32.W))
switch(pc){
  is(0.S){
    
    pc :=1.S
  }
  is(1.S){
    
    pc :=Mux(i<10.S,2.S,3.S)
  }
  is(2.S){
    printf("%d\n",i)
    i := i+1.S
    pc :=1.S
  }
  is(3.S){
    
    pc :=1000.S
  }
}
}
