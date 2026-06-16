import chisel3._
import chisel3.util._

class CarCollision extends Module {
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))

    val aiX = Input(SInt(12.W))
    val aiY = Input(SInt(11.W))

    val collision = Output(Bool())
  })

  val carWidth  = 32.S(12.W)
  val carHeight = 32.S(11.W)

  io.collision :=
    (io.carX < io.aiX + carWidth) &&
    (io.carX + carWidth > io.aiX) &&
    (io.carY < io.aiY + carHeight) &&
    (io.carY + carHeight > io.aiY)
}