import chisel3._
import chisel3.util._

class CarVelocityController extends Module {
  val io = IO(new Bundle {
    val speed = Input(SInt(8.W))
    val ang = Input(UInt(6.W))
    val frameUpdate = Input(Bool())
    val oldXPos = Input(SInt(11.W))
    val oldYPos = Input(SInt(10.W))
    val newXPos = Output(SInt(11.W))
    val newYPos = Output(SInt(10.W))
  })

  val sinTable = Reg(Vec(64, SInt(9.W)))
  val cosTable = Reg(Vec(64, SInt(9.W)))

  val xRemainder = RegInit(0.S(9.W))
  val yRemainder = RegInit(0.S(9.W))

  val xAdded = WireInit(0.S(9.W))
  val yAdded = WireInit(0.S(9.W))

  for(i <- 0 to 63) {
    sinTable(i) := (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.asSInt
    cosTable(i) := (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.asSInt
  }

  val sinOfAngle = sinTable(io.ang)
  val cosOfAngle = cosTable(io.ang)

  val highResSpeedY = ((sinOfAngle * io.speed) >> 6).asSInt
  val highResSpeedX = ((cosOfAngle * io.speed) >> 6).asSInt

  io.newXPos := io.oldXPos
  io.newYPos := io.oldYPos

  when (io.frameUpdate) {
    xRemainder := (highResSpeedX + xRemainder) % 128.S
    yRemainder := (highResSpeedY + yRemainder) % 128.S

    // These will only update on next clock cycle when frameupdate is false.
    xAdded := (highResSpeedX + xRemainder) - xRemainder
    yAdded := (highResSpeedY + yRemainder) - yRemainder

    io.newXPos := io.oldXPos + (xAdded >> 7).asSInt
    io.newYPos := io.oldYPos + (yAdded >> 7).asSInt
  }

}
