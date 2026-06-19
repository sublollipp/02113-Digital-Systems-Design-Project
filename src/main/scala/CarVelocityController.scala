import chisel3._
import chisel3.util._

class CarVelocityController extends Module {
  val io = IO(new Bundle {
    val speed = Input(SInt(11.W))
    val ang = Input(UInt(6.W))
    val frameUpdate = Input(Bool())
    val oldXPos = Input(SInt(12.W))
    val oldYPos = Input(SInt(11.W))
    val newXPos = Output(SInt(12.W))
    val newYPos = Output(SInt(11.W))
    val updateDone = Output(Bool())
  })

  val xRemainder = RegInit(0.S(12.W))
  val yRemainder = RegInit(0.S(11.W))

  val sinValues = (0 until 64).map { i =>
    (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  }

  val cosValues = (0 until 64).map { i =>
    (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  }

  val sinTable = VecInit(sinValues)
  val cosTable = VecInit(cosValues)

  val sinOfAngle = sinTable(io.ang)
  val cosOfAngle = cosTable(io.ang)

  val highResSpeedX = Reg(SInt(11.W))
  val highResSpeedY = Reg(SInt(11.W))

  io.newXPos := io.oldXPos
  io.newYPos := io.oldYPos

  val idle :: computeSpeed :: computePos :: Nil = Enum(3)
  val state = RegInit(idle)

  val accumX = (highResSpeedX + xRemainder)
  val accumY = (highResSpeedY + yRemainder)

  val nextX = io.oldXPos + (accumX >> 7).asSInt
  val nextY = io.oldYPos + (accumY >> 7).asSInt

  io.updateDone := false.B

  switch (state) {
    is (idle) {
      io.updateDone := true.B
      when (io.frameUpdate) {
        state := computeSpeed
      }
    }
    is (computeSpeed) {
      state := computePos
      highResSpeedY := ((sinOfAngle * io.speed) >> 6).asSInt
      highResSpeedX := ((cosOfAngle * io.speed) >> 6).asSInt
    }
    is (computePos) {
      state := idle
      when(nextX < 0.S) {
        io.newXPos := 0.S
      }.elsewhen(nextX > 1248.S) { // 1280 - 32
        io.newXPos := 1248.S
      }.otherwise {
        io.newXPos := nextX
      }

      when(nextY < 64.S) {
        io.newYPos := 64.S
      }.elsewhen(nextY > 928.S) { // 960 - 32
        io.newYPos := 928.S
      }.otherwise {
        io.newYPos := nextY
      }

      xRemainder := accumX - ((accumX >> 7) << 7).asSInt
      yRemainder := accumY - ((accumY >> 7) << 7).asSInt
    }
  }
}
