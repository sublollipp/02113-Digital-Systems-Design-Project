import chisel3._
import chisel3.util._

class Shell extends Module {
  val io = IO(new Bundle {
    val spawn = Input(Bool())

    val startX = Input(SInt(12.W))
    val startY = Input(SInt(11.W))
    val startAngle = Input(UInt(6.W))

    val frameUpdate = Input(Bool())

    val hitObstacle = Input(Bool())

    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val visible = Output(Bool())

    val hitPlayer = Output(Bool())
    val hitAi = Output(Bool())

    val playerX = Input(SInt(12.W))
    val playerY = Input(SInt(11.W))

    val aiX = Input(SInt(12.W))
    val aiY = Input(SInt(11.W))
  })

  val active = RegInit(false.B)

  val xPos = RegInit(0.S(12.W))
  val yPos = RegInit(0.S(11.W))

  val angleReg = RegInit(0.U(6.W))

  // 60 FPS * 10 sekunder
  val lifeCounter = RegInit(0.U(10.W))

  // Immunitet lige efter spawn
  val armCounter = RegInit(0.U(4.W))

  when(io.spawn && !active) {
    active := true.B
    xPos := io.startX
    yPos := io.startY
    angleReg := io.startAngle
    lifeCounter := 600.U
    armCounter := 4.U
  }

  val speed = 6.S

  val sinValues = (0 until 64).map(i =>
    (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  )

  val cosValues = (0 until 64).map(i =>
    (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  )

  val sinTable = VecInit(sinValues)
  val cosTable = VecInit(cosValues)

  val dx = ((cosTable(angleReg) * speed) >> 6).asSInt
  val dy = ((sinTable(angleReg) * speed) >> 6).asSInt

  val nextX = xPos + dx
  val nextY = yPos + dy

  when(active && io.frameUpdate) {

    when(armCounter =/= 0.U) {
      armCounter := armCounter - 1.U
    }

    when(nextX < 0.S || nextX > 1248.S) {
      angleReg := (32.U - angleReg)(5,0)
    }.otherwise {
      xPos := nextX
    }

    when(nextY < 0.S || nextY > 928.S) {
      angleReg := (64.U - angleReg)(5,0)
    }.otherwise {
      yPos := nextY
    }

    when(lifeCounter === 0.U) {
      active := false.B
    }.otherwise {
      lifeCounter := lifeCounter - 1.U
    }
  }

  val shellSize = 32.S

  val playerHit =
    active &&
    (armCounter === 0.U) &&
    (xPos < io.playerX + shellSize) &&
    (xPos + shellSize > io.playerX) &&
    (yPos < io.playerY + shellSize) &&
    (yPos + shellSize > io.playerY)

  val aiHit =
    active &&
    (armCounter === 0.U) &&
    (xPos < io.aiX + shellSize) &&
    (xPos + shellSize > io.aiX) &&
    (yPos < io.aiY + shellSize) &&
    (yPos + shellSize > io.aiY)

  io.hitPlayer := playerHit
  io.hitAi := aiHit

 when(playerHit || aiHit || io.hitObstacle) {
  active := false.B
}

  io.posX := xPos
  io.posY := yPos
  io.visible := active
}