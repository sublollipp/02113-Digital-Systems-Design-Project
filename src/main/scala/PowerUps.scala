import chisel3._
import chisel3.util._

class PowerUps extends Module {
  val io = IO(new Bundle {
    val frameUpdate = Input(Bool())
    val carX = Input(Bool())
    val carY = Input(Bool())
    })

  val mysteryBox = RegInit(0.U(10.W))



}
