import chisel3._
import chisel3.util._

class EkstraStuff {

}



class Main1c extends Module{
  val io = IO(new Bundle {
    val out1 = new WriterIOs()
    val in1 = new ReaderIOs()
  })
  io.out1.full := false.B
  io.in1.empty := true.B
  val dataReg = RegInit(42.S(32.W))
  io.in1.din :=  dataReg  //42.S(32.W)

  dataReg := dataReg + 1.S
}


class WriterIOs extends Bundle {
  val write = Input(Bool())
  val full = Output(Bool())
  val dout = Input(SInt(32.W))
}

class ReaderIOs extends Bundle {
  val read = Input(Bool())
  val empty = Output(Bool())
  val din = Output(SInt(32.W))
}



class Main extends Module {
  val io = IO(new Bundle {
    val led = Output(UInt(1.W))
  })
  //val CNT_MAX = (100000000 / 2 - 1).U
  val CNT_MAX = (100000 / 2 - 1).U

  val cntReg = RegInit(0.U(32.W))
  val blkReg = RegInit(0.U(1.W))

  cntReg := cntReg + 1.U
  when(cntReg === CNT_MAX) {
    cntReg := 0.U
    blkReg := ~blkReg
  }
  io.led := blkReg
}

class Main1 extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val out = Output(UInt(2.W))
    //val out1 = new WriterIOs()
    val out1data = Output(SInt(32.W))
    val out1full = Output(UInt(1.W))
    val out1ready = Input(UInt(1.W))
    //val in1 = new ReaderIOs()

  })
  val dataReg = RegInit(42.S(32.W))
  io.out1data := 0.S
  io.out1full := 0.U

  io.out := io.a & io.b
  //io.out1data := 42.S  //dataReg
  when(io.out1ready === 1.U) {
    io.out1data := dataReg
    io.out1full := 1.U
  }.otherwise{

  }
  //io.in1.empty := 0.B
  //io.out1.full := 1.B
  dataReg := dataReg + 1.S
}


class Main1b extends Module {
  val io = IO(new Bundle {
    val bin = new ReaderIOs()
  })

  io.bin.din := 41.S(32.W) + Mux(io.bin.read,1.S(32.W),2.S(32.W))
  io.bin.empty := false.B
}

/*

//    val out1write = Input(Bool())
//    val out1full = Output(Bool())
//    val out1dout = Input(SInt(16.W))
//    val in1read = Input(Bool())
//    val in1empty = Output(Bool())
//    val in1din = Output(SInt(16.W))


//io.in1empty := 0.B
//io.out1full := 1.B


io.in1 <> io.out1
when(!io.in1.empty){
  println("read "+io.in1.data)
  io.in1.empty := 1.B
}
when(!io.out1.full) {
  println("write " )
  io.out1.data := 42.S(32)
  io.out1.full := 1.B
}*/


/**
 * An object extending App to generate the Verilog code.
 */
object MyMain extends App {
  println("Hello World, I will now generate the Verilog file!")
  emitVerilog(new Main())
}
/*
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }
}*/

object MakeFSM{
  def makeFsm() ={
    println("hello there")

  }
  def fsm(nm:String,body:String):String ={
    nm+"  "+body
  }
  val fifo=fsm( "Fifo","""
      instream in1;
      outstream out1;
      for(int x:in1){
        out1.write(x);
      }
      out1.close();
    """)
  val swopper = fsm("Swopper",
    """
    instream in1;
    outstream out1;
    int buffer;
    bool hasvalue=false;
    for(int x:in1){
      if(hasvalue){
        out1.write(x); out1.write(buffer);
        hasvalue=false;
      }else{
        buffer=x;
        hasvalue=true;
      }
    }
    if(hasvalue){
      out1.write(buffer);
    }
    out1.close();
  """)
}

class WriterIO(size: Int) extends Bundle {
  val write = Input(Bool())
  val full = Output(Bool())
  val din = Input(UInt(size.W))
}

class ReaderIO(size: Int) extends Bundle {
  val read = Input(Bool())
  val empty = Output(Bool())
  val dout = Output(UInt(size.W))
}

class FifoRegister(size: Int) extends Module {
  val io = IO(new Bundle {
    val enq = new WriterIO(size)
    val deq = new ReaderIO(size)
  })
  val empty :: full :: Nil = Enum(2)
  val stateReg = RegInit(empty)
  val dataReg = RegInit(0.U(size.W))
  when(stateReg === empty) {
    when(io.enq.write) {
      stateReg := full
      dataReg := io.enq.din
    }
  }.elsewhen(stateReg === full) {
    when(io.deq.read) {
      stateReg := empty
      dataReg := 0.U // just to better see empty slots in the waveform
    }
  }.otherwise {
    // There should not be an otherwise state
  }
  io.enq.full := (stateReg === full)
  io.deq.empty := (stateReg === empty)
  io.deq.dout := dataReg
}

class BubbleFifo(size: Int, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = new WriterIO(size)
    val deq = new ReaderIO(size)
  })
  val buffers = Array.fill(depth) { Module(new
      FifoRegister(size)) }
  for (i <- 0 until depth - 1) {
    buffers(i + 1).io.enq.din := buffers(i).io.deq.dout
    buffers(i + 1).io.enq.write := ~buffers(i).io.deq.empty
    buffers(i).io.deq.read := ~buffers(i + 1).io.enq.full
  }
  io.enq <> buffers(0).io.enq
  io.deq <> buffers(depth - 1).io.deq
}
class UartIO extends DecoupledIO(UInt(8.W)) {
}

class Tx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val txd = Output(UInt(1.W))
    val channel = Flipped(new UartIO())
  })
  val BIT_CNT = ((frequency + baudRate / 2) / baudRate -
    1).asUInt
  val shiftReg = RegInit(0x7ff.U)
  val cntReg = RegInit(0.U(20.W))
  val bitsReg = RegInit(0.U(4.W))
  io.channel.ready := (cntReg === 0.U) && (bitsReg === 0.U)
  io.txd := shiftReg(0)
  when(cntReg === 0.U) {
    cntReg := BIT_CNT
    when(bitsReg =/= 0.U) {
      val shift = shiftReg >> 1
      shiftReg := 1.U ## shift(9, 0)
      bitsReg := bitsReg - 1.U
    } .otherwise {
      when(io.channel.valid) {
        // two stop bits , data , one start bit
        shiftReg := 3.U ## io.channel.bits ## 0.U
        bitsReg := 11.U
      } .otherwise {
        shiftReg := 0x7ff.U
      }
    }
  } .otherwise {
    cntReg := cntReg - 1.U
  }
}

class Buffer extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new UartIO())
    val out = new UartIO()
  })
  val empty :: full :: Nil = Enum(2)
  val stateReg = RegInit(empty)
  val dataReg = RegInit(0.U(8.W))
  io.in.ready := stateReg === empty
  io.out.valid := stateReg === full
  when(stateReg === empty) {
    when(io.in.valid) {
      dataReg := io.in.bits
      stateReg := full
    }
  } .otherwise { // full
    when(io.out.ready) {
      stateReg := empty
    }
  }
  io.out.bits := dataReg
}

class Rx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val rxd = Input(UInt(1.W))
    val channel = new UartIO()
  })
  val BIT_CNT = ((frequency + baudRate / 2) / baudRate - 1).U
  val START_CNT = ((3 * frequency / 2 + baudRate / 2) /
    baudRate - 1).U
  // Sync in the asynchronous RX data , reset to 1 to not
  // start reading after a reset
  val rxReg = RegNext(RegNext(io.rxd, 1.U), 1.U)
  val shiftReg = RegInit(0.U(8.W))
  val cntReg = RegInit(0.U(20.W))
  val bitsReg = RegInit(0.U(4.W))
  val validReg = RegInit(false.B)
  when(cntReg =/= 0.U) {
    cntReg := cntReg - 1.U
  } .elsewhen(bitsReg =/= 0.U) {
    cntReg := BIT_CNT
    shiftReg := rxReg ## (shiftReg >> 1)
    bitsReg := bitsReg - 1.U
    // the last bit shifted in
    when(bitsReg === 1.U) {
      validReg := true.B
    }
  } .elsewhen(rxReg === 0.U) {
    // wait 1.5 bits after falling edge of start
    cntReg := START_CNT
    bitsReg := 8.U
  }
  when(validReg && io.channel.ready) {
    validReg := false.B
  }
  io.channel.bits := shiftReg
  io.channel.valid := validReg
}

class BufferedTx(frequency: Int, baudRate: Int) extends
  Module {
  val io = IO(new Bundle {
    val txd = Output(UInt(1.W))
    val channel = Flipped(new UartIO())
  })
  val tx = Module(new Tx(frequency , baudRate))
  val buf = Module(new Buffer())
  buf.io.in <> io.channel
  tx.io.channel <> buf.io.out
  io.txd <> tx.io.txd
}
class Sender(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val txd = Output(UInt(1.W))
  })
  val tx = Module(new BufferedTx(frequency , baudRate))
  io.txd := tx.io.txd
  val msg = "Hello World!"
  val text = VecInit(msg.map(_.U))
  val len = msg.length.U
  val cntReg = RegInit(0.U(8.W))
  tx.io.channel.bits := text(cntReg)
  tx.io.channel.valid := cntReg =/= len
  when(tx.io.channel.ready && cntReg =/= len) {
    cntReg := cntReg + 1.U
  }
}

class Echo(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val txd = Output(UInt(1.W))
    val rxd = Input(UInt(1.W))
  })
  val tx = Module(new BufferedTx(frequency , baudRate))
  val rx = Module(new Rx(frequency , baudRate))
  io.txd := tx.io.txd
  rx.io.rxd := io.rxd
  tx.io.channel <> rx.io.channel
}

class Top extends Module {
  val io = IO(new Bundle {
    val done = Output(Bool())
  })
  io.done := 0.B

}


/*
  "Sendr" should "pass" in {
    test(new Sender1()) { dut =>
      dut.io.out1.received.poke(0)
      for(i <- 1 to 1) {
        println(i+ "  Res1c : " + dut.io.out1.data.peek().litValue.toInt +" "
          +   (dut.io.out1.send.peek().asUInt.litValue ==1 ) +" " + (dut.io.out1.received.peek().asUInt.litValue ==1  ))
        if(dut.io.out1.send.peek().asUInt.litValue==1 &&(dut.io.out1.received.peek().asUInt.litValue==0)){
          dut.io.out1.received.poke(1)
          println("read "+dut.io.out1.data.peek().litValue.toInt)
        }
        if(dut.io.out1.send.peek().asUInt.litValue==0 &&(dut.io.out1.received.peek().asUInt.litValue==1)){
            dut.io.out1.received.poke(0)
            println("done receive")
          //dut.clock.step()
        }
        dut.clock.step()
      }
    }
  }
*/

/*
"Main1c" should "pass" in {
    test(new Main1c()) { dut =>
      dut.io.in1.read.poke(1)
      dut.io.out1.write.poke(1)
      dut.io.out1.dout.poke(1)
      for(i <- 1 to 5) {
        println("Res1c : " + dut.io.in1.din.peek().litValue.toInt +" "
          +   (dut.io.in1.empty.asUInt == true) +" " + (dut.io.out1.full.asUInt ==true))
        dut.clock.step()
      }
    }
  }

  "Main2" should "pass" in {
    test(new Main1()) { dut =>
      dut.io.out1ready.poke(0)
      dut.io.a.poke(3)
      dut.io.b.poke(1)
      dut.clock.step()
      println("res : "+dut.io.out.peek().litValue.toInt)
      dut.io.a.poke(3)
      dut.io.b.poke(2)
      dut.clock.step()
      println("res : " + dut.io.out.peek().litValue.toInt)
      //dut.io.out1dout.peek().toString
      //
      //dut.io.out1write.poke(0)
      //dut.io.out1dout.poke(41)
      //dut.io.in1read.poke(1)
      dut.clock.step()
      println("fin : " + dut.io.out1data.peek().litValue.toInt)
      dut.io.out1ready.poke(1)
      dut.clock.step()
      println("fin : " + dut.io.out1data.peek().litValue.toInt)
      dut.clock.step()
      println("fin : " + dut.io.out1data.peek().litValue.toInt)
      dut.clock.step()

    }
  }

  "Main1b" should "pass" in {
      test(new Main1b()) { dut =>
        dut.io.bin.read.poke(1)
        //dut.io.bin.dout.poke(1)
        dut.clock.step()
        println("Res : " + dut.io.bin.din.peek().litValue.toInt)
      }
  }
*/
/*
"Main" should "pass" in {
test(new Main()) { dut =>
  var ledStatus = -1
  println("Start the blinking LED")
  dut.clock.setTimeout(0)
  for (i <- 0 until 10) { //100
    dut.clock.step(10000)
    val ledNow = dut.io.led.peek.litValue.toInt
    val s = if (ledNow == 0) "o" else "*"
    if (ledStatus != ledNow) {
      System.out.println(s)
      ledStatus = ledNow
    }
  }
  println("End the blinking LED")
}
}
"Main1" should "pass" in {
test(new Main()) { dut =>
  println(MakeFSM.fifo)
}
}
*/
