import chisel3._
import chisel3.util._

class Shell extends Module {
  val io = IO(new Bundle {
    val spawn = Input(Bool())

    val startX = Input(SInt(12.W))
    val startY = Input(SInt(11.W))
    val startAngle = Input(UInt(6.W))

    val frameUpdate = Input(Bool())

    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val visible = Output(Bool())
  })

  val active = RegInit(false.B)

  val xPos = RegInit(0.S(12.W))
  val yPos = RegInit(0.S(11.W))

  val angleReg = RegInit(0.U(6.W))

  // 60 FPS * 10 sekunder
  val lifeCounter = RegInit(0.U(10.W))

  when(io.spawn && !active) {
    active := true.B
    xPos := io.startX
    yPos := io.startY
    angleReg := io.startAngle
    lifeCounter := 600.U
  }

  val speed = 8.S

  val sinValues = (0 until 64).map(i =>
    (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  )

  val cosValues = (0 until 64).map(i =>
    (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  )

  val sinTable = VecInit(sinValues)
  val cosTable = VecInit(cosValues)

  when(active && io.frameUpdate) {

    xPos := xPos + ((cosTable(angleReg) * speed) >> 6).asSInt
    yPos := yPos + ((sinTable(angleReg) * speed) >> 6).asSInt

    when(lifeCounter === 0.U) {
      active := false.B
    }.otherwise {
      lifeCounter := lifeCounter - 1.U
    }
  }

  io.posX := xPos
  io.posY := yPos
  io.visible := active
}