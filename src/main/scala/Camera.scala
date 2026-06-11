import chisel3._
import chisel3.util._

class Camera extends Module{
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))
    val camX = Output(SInt(11.W))
    val camY = Output(SInt(10.W))
  })

  val desiredCameraX = io.carX - 304.S
  val desiredCameraY = io.carY - 224.S

  when(desiredCameraX < 0.S) {
    io.camX := 0.S
  }.elsewhen(desiredCameraX > 640.S) {
    io.camX := 640.S
  }.otherwise {
    io.camX := desiredCameraX
  }

  when(desiredCameraY < 0.S) {
    io.camY := 0.S
  }.elsewhen(desiredCameraY > 480.S) {
    io.camY := 480.S
  }.otherwise {
    io.camY := desiredCameraY
  }
}
