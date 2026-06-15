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
    io.x >= 54.S &&
    io.x <= 202.S &&
    io.y >= 118.S &&
    io.y <= 842.S

 // Vejen højre del (vandret)
  val road2 =
    io.x >= 86.S &&
    io.x <= 1130.S &&
    io.y >= 86.S &&
    io.y <= 235.S

 // Vejen øverste del (lodret)
  val road3 =
    io.x >= 982.S &&
    io.x <= 1130.S &&
    io.y >= 215.S &&
    io.y <= 554.S

 // Vejen nederste del (lodret)
  val road4 =
    io.x >= 822.S &&
    io.x <= 970.S &&
    io.y >= 406.S &&
    io.y <= 714.S

 // Vejen nederste del (vandret)
  val road5 =
    io.x >= 86.S &&
    io.x <= 970.S &&
    io.y >= 694.S &&
    io.y <= 842.S

 // Vejen mellem road3 og road4 del (vandret)
  val road6 = 
    io.x >= 822.S &&
    io.x <= 1130.S &&
    io.y >= 406.S &&
    io.y <= 586.S


  io.onRoad :=
    road1 ||
    road2 ||
    road3 ||
    road4 ||
    road5 ||
    road6
}