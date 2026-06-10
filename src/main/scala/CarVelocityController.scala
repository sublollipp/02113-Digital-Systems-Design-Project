import chisel3._
import chisel3.util._

class CarVelocityController extends Module {
  val io = IO(new Bundle {
    val speed = Input(SInt(10.W))
    val ang = Input(UInt(6.W))
    val frameUpdate = Input(Bool())
    val oldXPos = Input(SInt(11.W))
    val oldYPos = Input(SInt(10.W))
    val newXPos = Output(SInt(11.W))
    val newYPos = Output(SInt(10.W))
  })

  val sinTable = Reg(Vec(64, SInt(8.W)))
  val cosTable = Reg(Vec(64, SInt(8.W)))

  val xRemainder = RegInit(0.S(11.W))
  val yRemainder = RegInit(0.S(10.W))

  for(i <- 0 to 63) {
    sinTable(i) := (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.S(8.W)
    cosTable(i) := (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.S(8.W)
  }

  val sinOfAngle = sinTable(io.ang)
  val cosOfAngle = cosTable(io.ang)

  val highResSpeedY = ((sinOfAngle * io.speed) >> 6).asSInt
  val highResSpeedX = ((cosOfAngle * io.speed) >> 6).asSInt

  io.newXPos := io.oldXPos
  io.newYPos := io.oldYPos

  when (io.frameUpdate) {
    val accumX = (highResSpeedX + xRemainder)
    val accumY = (highResSpeedY + yRemainder)

    val nextX = io.oldXPos + (accumX >> 7).asSInt
    val nextY = io.oldYPos + (accumY >> 7).asSInt

    when(nextX < 0.S) {
      io.newXPos := 0.S
    }.elsewhen(nextX > 1248.S) { // 1280 - 32
      io.newXPos := 1248.S
    }.otherwise {
      io.newXPos := nextX
    }

    when(nextY < 0.S) {
      io.newYPos := 0.S
    }.elsewhen(nextY > 928.S) { // 960 - 32
      io.newYPos := 928.S
    }.otherwise {
      io.newYPos := nextY
    }

    xRemainder := accumX - ((accumX >> 7) << 7).asSInt
    yRemainder := accumY - ((accumY >> 7) << 7).asSInt
  }
}
