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
  })

  val xPosReg = RegInit(60.S(11.W))
  val yPosReg = RegInit(170.S(10.W))

  val xFramesPerPixel = RegInit(10.U(5.W)) // pt 10 = standstill
  val yFramesPerPixel = RegInit(10.U(5.W)) // pt 10 = standstill

  val xAbsFramesPerPixel = Reg(UInt(4.W))
  val yAbsFramesPerPixel = Reg(UInt(4.W))

  val accelerationDividerCounter = RegInit(0.U(5.W))

  when (xFramesPerPixel < 10.U) {
    xAbsFramesPerPixel := xFramesPerPixel
  }.elsewhen(xFramesPerPixel > 10.U) {
    xAbsFramesPerPixel := 20.U - xFramesPerPixel
  }.otherwise {
    xAbsFramesPerPixel := 0.U
  }

  when (yFramesPerPixel < 10.U) {
    yAbsFramesPerPixel := yFramesPerPixel
  }.elsewhen(yFramesPerPixel > 10.U) {
    yAbsFramesPerPixel := 20.U - yFramesPerPixel
  }.otherwise {
    yAbsFramesPerPixel := 0.U
  }

  val xSpeedCounter = RegInit(0.U(4.W))
  val ySpeedCounter = RegInit(0.U(4.W))

  val uu :: ur :: rr :: dr :: dd :: dl :: ll :: ul :: Nil = Enum(8)
  val upSprite :: diagSprite :: rightSprite :: Nil = Enum(3)

  val dirReg = RegInit(uu)
  val flipSpriteH = RegInit(false.B)
  val flipSpriteV = RegInit(false.B)
  val sprite = RegInit(upSprite)

  when (io.update) {

    accelerationDividerCounter := Mux(accelerationDividerCounter === 20.U, 0.U, accelerationDividerCounter + 1.U)

    when (accelerationDividerCounter === 0.U) {
    // Inputs og acceleration
    // X inputs
    when (io.btnLeft && !io.btnRight) {
      xFramesPerPixel := Mux(xFramesPerPixel === 1.U, 1.U, xFramesPerPixel - 1.U)
    }.elsewhen (!io.btnLeft && io.btnRight) {
      xFramesPerPixel := Mux(xFramesPerPixel === 19.U, 19.U, xFramesPerPixel + 1.U)
    }.otherwise {
      when(xFramesPerPixel > 10.U) {
        xFramesPerPixel := xFramesPerPixel - 1.U
      }.elsewhen(xFramesPerPixel < 10.U) {
        xFramesPerPixel := xFramesPerPixel + 1.U
      }
    }
    // Y inputs
    when (io.btnUp && !io.btnDown) {
      yFramesPerPixel := Mux(yFramesPerPixel === 1.U, 1.U, yFramesPerPixel - 1.U)
    }.elsewhen (!io.btnUp && io.btnDown) {
      yFramesPerPixel := Mux(yFramesPerPixel === 19.U, 19.U, yFramesPerPixel + 1.U)
    }.otherwise {
      when(yFramesPerPixel > 10.U) {
        yFramesPerPixel := yFramesPerPixel - 1.U
      }.elsewhen(yFramesPerPixel < 10.U) {
        yFramesPerPixel := yFramesPerPixel + 1.U
      }
    }
    }

    // Velocity til position
    // X
    when (xSpeedCounter >= xAbsFramesPerPixel) {
      xSpeedCounter := 0.U
      when (xFramesPerPixel < 10.U && xPosReg > 0.S) {
        xPosReg := xPosReg - 1.S
      }.elsewhen(xFramesPerPixel > 10.U && xPosReg < 640.S) {
        xPosReg := xPosReg + 1.S
      }
    }.otherwise {
      xSpeedCounter := xSpeedCounter + 1.U
    }
    // y
    when (ySpeedCounter >= yAbsFramesPerPixel) {
      ySpeedCounter := 0.U
      when (yFramesPerPixel < 10.U && yPosReg > 0.S) {
        yPosReg := yPosReg - 1.S
      }.elsewhen(yFramesPerPixel > 10.U && yPosReg < 360.S) {
        yPosReg := yPosReg + 1.S
      }
    }.otherwise {
      ySpeedCounter := ySpeedCounter + 1.U
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
