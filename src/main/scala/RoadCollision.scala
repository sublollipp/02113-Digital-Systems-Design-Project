import chisel3._
import chisel3.util._

class RoadCollision extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(12.W))
    val y = Input(SInt(11.W))
    val onRoad = Output(Bool())
  })

  // Vejen venstre del (lodret)
  val road1 =
    io.x >= 64.S &&
    io.x <= 192.S &&
    io.y >= 128.S &&
    io.y <= 832.S

 // Vejen højre del (vandret)
  val road2 =
    io.x >= 96.S &&
    io.x <= 1120.S &&
    io.y >= 96.S &&
    io.y <= 225.S

 // Vejen øverste del (lodret)
  val road3 =
    io.x >= 992.S &&
    io.x <= 1120.S &&
    io.y >= 225.S &&
    io.y <= 544.S

 // Vejen nederste del (lodret)
  val road4 =
    io.x >= 832.S &&
    io.x <= 960.S &&
    io.y >= 416.S &&
    io.y <= 704.S

 // Vejen nederste del (vandret)
  val road5 =
    io.x >= 96.S &&
    io.x <= 960.S &&
    io.y >= 704.S &&
    io.y <= 832.S

 // Vejen mellem road3 og road4 del (vandret)
  val road6 = 
    io.x >= 832.S &&
    io.x <= 1120.S &&
    io.y >= 416.S &&
    io.y <= 576.S


  io.onRoad :=
    road1 ||
    road2 ||
    road3 ||
    road4 ||
    road5 ||
    road6
}