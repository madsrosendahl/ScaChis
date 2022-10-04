import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

/*

int n = 5;
int data = alloc(100);
int h1; int h2; int h3; int i; int j;
n = 6;
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

 */


class Main3 extends Module {
  val io = IO(new Bundle {
    val done = Output(SInt(32.W))
  })
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

  io.done := 0.S
  tm := tm + 1.S(32.W)

  switch(pc) {
    is(0.S) {
      n := 30.S
      m := 2.S
      pc := 1.S
    }
    is(1.S) {
      pc := Mux(m > 0.S, 2.S, 3.S)
    }
    is(2.S) {
      i := 0.S
      pc := 4.S
    }
    is(4.S) {
      pc := Mux(i < n, 5.S, 6.S)
    }
    is(5.S) {
      rnd := LFSR(16) % 1000.U
      pc := 7.S
    }
    is(7.S) {
      h3 := rnd.asSInt
      pc := 8.S
    }
    is(8.S) {
      data((i).asUInt) := h3
      i := i + 1.S
      pc := 4.S
    }
    is(6.S) {
      i := 1.S
      pc := 9.S
    }
    is(9.S) {
      pc := Mux(i < n, 10.S, 11.S)
    }
    is(10.S) {
      j := n - 1.S
      pc := 12.S
    }
    is(12.S) {
      pc := Mux(j >= i, 13.S, 14.S)
    }
    is(13.S) {
      h1 := data((j).asUInt)
      pc := 15.S
    }
    is(15.S) {
      h2 := data((j - 1.S).asUInt)
      pc := 16.S
    }
    is(16.S) {
      pc := 17.S
    }
    is(17.S) {
      pc := Mux(h2 > h1, 18.S, 19.S)
    }
    is(18.S) {
      data((j - 1.S).asUInt) := h1
      pc := 20.S
    }
    is(20.S) {
      data((j).asUInt) := h2
      pc := 19.S
    }
    is(19.S) {
      j := j - 1.S
      pc := 12.S
    }
    is(14.S) {
      i := i + 1.S
      pc := 9.S
    }
    is(11.S) {
      i := 0.S
      pc := 21.S
    }
    is(21.S) {
      pc := Mux(i < n, 22.S, 23.S)
    }
    is(22.S) {
      h1 := data((i).asUInt)
      pc := 24.S
    }
    is(24.S) {
      printf("%d\n", h1)
      i := i + 1.S
      pc := 21.S
    }
    is(23.S) {
      m := m - 1.S
      pc := 1.S
    }
    is(3.S) {
      pc := 25.S
      io.done := tm
    }
  }

}


class Sender1 extends Module {
  val io = IO(new Bundle {
    val out1 = new OutStreamIOs()
  })
  val dataReg = RegInit(1.S(32.W))
  val sendReg = RegInit(0.U(1.W))
  io.out1.send := sendReg
  io.out1.data := dataReg
  when(io.out1.received === 0.U && sendReg === 0.U){
    sendReg := 1.U
    dataReg := dataReg+1.S(32.W)
    printf("send      %d\n",dataReg.asSInt)
  }
  when(dataReg>3.S){io.out1.data := -1.S(32.W)}
  when(io.out1.received === 1.U){ sendReg := 0.U}
  printf("Sendr1 %d %d\n",sendReg.asUInt,dataReg.asSInt)
}

class Receiver1 extends Module {
  val io = IO(new Bundle {
    val in1 = new InStreamIOs()
  })
  val recReg = RegInit(0.U(1.W))
  val timeReg = RegInit(0.S(32.W))
  io.in1.received := recReg
  when(io.in1.send === 1.U && recReg === 0.U){
    recReg := 1.U
    printf("receive   %d\n",io.in1.data.asSInt)
  }
  when(io.in1.send === 0.U){ recReg := 0.U}
  timeReg := timeReg+1.S(32.W)
  printf("Recivr %d \n",recReg.asUInt) //,timeReg.asSInt)
}

class OutStreamIOs extends Bundle {
  val received = Input(UInt(1.W))
  val send = Output(UInt(1.W))
  val data = Output(SInt(32.W))
}
class InStreamIOs extends Bundle {
  val received = Output(UInt(1.W))
  val send = Input(UInt(1.W))
  val data = Input(SInt(32.W))
}

class Main1d extends Module {
  val rec = Module(new Receiver1())
  val snd = Module(new Sender1())
  val timeReg = RegInit(0.S(32.W))

  snd.io.out1.received := rec.io.in1.received
  rec.io.in1.send := snd.io.out1.send
  rec.io.in1.data := snd.io.out1.data
  timeReg := timeReg+1.S(32.W)
  printf("Main1d --------%d ------------\n",timeReg.asSInt)
}
