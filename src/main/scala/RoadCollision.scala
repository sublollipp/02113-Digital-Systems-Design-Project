import chisel3._
import chisel3.util._

class RoadCollision extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(12.W))
    val y = Input(SInt(11.W))
    val onRoad = Output(Bool())
  })

  // Left part of the road (vertical)
  val road1 =
    io.x >= 54.S &&
    io.x <= 202.S &&
    io.y >= 118.S &&
    io.y <= 842.S

 // Upper part of the road (horizontal)
  val road2 =
    io.x >= 86.S &&
    io.x <= 1130.S &&
    io.y >= 86.S &&
    io.y <= 235.S

 // Right part of the road (vertical)
  val road3 =
    io.x >= 982.S &&
    io.x <= 1130.S &&
    io.y >= 215.S &&
    io.y <= 532.S

 // Lower part of the road (vertical)
  val road4 =
    io.x >= 822.S &&
    io.x <= 970.S &&
    io.y >= 406.S &&
    io.y <= 714.S

 // Bottom part of the road (horizontal)
  val road5 =
    io.x >= 86.S &&
    io.x <= 970.S &&
    io.y >= 694.S &&
    io.y <= 842.S

 // Middle part of the road (horizontal)
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