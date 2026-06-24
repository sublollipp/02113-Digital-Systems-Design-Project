import chisel3._
import chisel3.util._

class SevenSegmentDisplay extends Module {

  val io = IO(new Bundle {

    val digit0 = Input(UInt(4.W)) 
    val digit1 = Input(UInt(4.W))
    val digit2 = Input(UInt(4.W))
    val digit3 = Input(UInt(4.W)) 

    val seg = Output(UInt(7.W))
    val an  = Output(UInt(4.W))
  })

  val refreshCounter = RegInit(0.U(20.W))
  refreshCounter := refreshCounter + 1.U

  val activeDigit = refreshCounter(19,18)

  val currentDigit = Wire(UInt(4.W))

  currentDigit := 0.U

  switch(activeDigit) {
    is(0.U) { currentDigit := io.digit0 }
    is(1.U) { currentDigit := io.digit1 }
    is(2.U) { currentDigit := io.digit2 }
    is(3.U) { currentDigit := io.digit3 }
  }

  io.an := "b1111".U

  switch(activeDigit) {
    is(0.U) { io.an := "b1110".U }
    is(1.U) { io.an := "b1101".U }
    is(2.U) { io.an := "b1011".U }
    is(3.U) { io.an := "b0111".U }
  }

  io.seg := MuxLookup(currentDigit, "b1111111".U, Seq(
    0.U -> "b1000000".U,
    1.U -> "b1111001".U,
    2.U -> "b0100100".U,
    3.U -> "b0110000".U,
    4.U -> "b0011001".U,
    5.U -> "b0010010".U,
    6.U -> "b0000010".U,
    7.U -> "b1111000".U,
    8.U -> "b0000000".U,
    9.U -> "b0010000".U
  ))
}