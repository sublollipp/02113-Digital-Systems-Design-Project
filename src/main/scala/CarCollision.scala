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

  // Constants

  val carWidth  = 22.S(12.W)
  val carHeight = 22.S(11.W)
  val offset    = 3.S

  // Collision detection

  io.collision :=
    (io.carX + offset < io.aiX + offset + carWidth) &&
    (io.carX + offset + carWidth > io.aiX + offset) &&
    (io.carY + offset < io.aiY + offset + carHeight) &&
    (io.carY + offset + carHeight > io.aiY + offset)
}