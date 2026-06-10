import chisel3._
import chisel3.util._

class Car extends Module{
  val io = IO(new Bundle {
    val btnUp = Input(Bool())
    val btnDown = Input(Bool())
    val btnLeft = Input(Bool())
    val btnRight = Input(Bool())
    val update = Input(Bool())
    val posX = Output(SInt(11.W))
    val posY = Output(SInt(10.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val shownSprite = Output(Vec(3, Bool()))
  })

  val xPosReg = RegInit(60.S(11.W))
  val yPosReg = RegInit(170.S(10.W))

  val speed = WireInit(0.S(10.W))
  val angle = WireInit(0.U(6.W))

  val speedControl = Module(new CarSpeedController(1.U, 300.S, -150.S, 3.S))
  speedControl.io.btnFwd := io.btnUp
  speedControl.io.btnBckwd := io.btnDown
  speedControl.io.frameUpdate := io.update
  speed := speedControl.io.speed

  val angleControl = Module(new CarAngleController(3.U))
  angleControl.io.btnLeft := io.btnLeft
  angleControl.io.btnRight := io.btnRight
  angleControl.io.frameUpdate := io.update
  angle := angleControl.io.angle

  val velControl = Module(new CarVelocityController)
  velControl.io.oldXPos := xPosReg
  velControl.io.oldYPos := yPosReg
  velControl.io.ang := angle
  velControl.io.speed := speed
  velControl.io.frameUpdate := io.update
  xPosReg := velControl.io.newXPos
  yPosReg := velControl.io.newYPos

  val xSpeedCounter = RegInit(0.U(4.W))
  val ySpeedCounter = RegInit(0.U(4.W))

  val uu :: ur :: rr :: dr :: dd :: dl :: ll :: ul :: Nil = Enum(8)
  val upSprite :: diagSprite :: rightSprite :: Nil = Enum(3)

  val dirReg = RegInit(uu)
  val flipSpriteH = RegInit(false.B)
  val flipSpriteV = RegInit(false.B)
  val sprite = WireDefault(upSprite)

  val shownSprite = RegInit(VecInit(true.B, false.B, false.B))

  io.shownSprite := shownSprite

  when (io.update) {
    when ((61.U <= angle || angle >= 0.U) && angle <= 4.U) {
      dirReg := rr
    }.elsewhen(5.U <= angle && angle <= 12.U) {
      dirReg := dr
    }.elsewhen(13.U <= angle && angle <= 20.U) {
      dirReg := dd
    }.elsewhen(21.U <= angle && angle <= 28.U) {
      dirReg := dl
    }.elsewhen(29.U <= angle && angle <= 36.U) {
      dirReg := ll
    }.elsewhen(37.U <= angle && angle <= 44.U) {
      dirReg := ul
    }.elsewhen(45.U <= angle && angle <= 52.U) {
      dirReg := uu
    }.otherwise {
      dirReg := ur
    }

    switch(sprite) {
      is(upSprite) {
        shownSprite(0) := true.B
        shownSprite(1) := false.B
        shownSprite(2) := false.B
      }
      is(diagSprite) {
        shownSprite(0) := false.B
        shownSprite(1) := true.B
        shownSprite(2) := false.B
      }
      is(rightSprite) {
        shownSprite(0) := false.B
        shownSprite(1) := false.B
        shownSprite(2) := true.B
      }
    }

    // Sprite
    switch(dirReg) {
      is (uu) {
        sprite := upSprite
        flipSpriteH := false.B
        flipSpriteV := false.B
      }
      is (dd) {
        sprite := upSprite
        flipSpriteH := false.B
        flipSpriteV := false.B
      }
      is (rr) {
        sprite := rightSprite
        flipSpriteH := false.B
        flipSpriteV := false.B
      }
      is (ll) {
        sprite := rightSprite
        flipSpriteH := true.B
        flipSpriteV := false.B
      }
      is (ur) {
        sprite := diagSprite
        flipSpriteH := false.B
        flipSpriteV := false.B
      }
      is (dr) {
        sprite := diagSprite
        flipSpriteH := false.B
        flipSpriteV := true.B
      }
      is (dl) {
        sprite := diagSprite
        flipSpriteH := true.B
        flipSpriteV := true.B
      }
      is (ul) {
        sprite := diagSprite
        flipSpriteH := true.B
        flipSpriteV := false.B
      }
    }
  }

  io.posX := xPosReg
  io.posY := yPosReg
  io.flipH := flipSpriteH
  io.flipV := flipSpriteV
}
