import chisel3._
import chisel3.util._

class Gamesprite(SpriteNumber: Int, BackTileNumber: Int) extends Module {
  val io = IO(new Bundle {
    //Buttons
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    //Switches
    val sw = Input(Vec(8, Bool()))

    //Leds
    val led = Output(Vec(8, Bool()))

    //GraphicEngineVGA
    val spriteXPosition = Output(Vec(SpriteNumber, SInt(11.W)))
    val spriteYPosition = Output(Vec(SpriteNumber, SInt(10.W)))
    val spriteVisible = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipHorizontal = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipVertical = Output(Vec(SpriteNumber, Bool()))

    val viewBoxX = Output(UInt(10.W))
    val viewBoxY = Output(UInt(9.W))

    val backBufferWriteData = Output(UInt(log2Up(BackTileNumber).W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())

    val newFrame = Input(Bool())
    val frameUpdateDone = Output(Bool())
  })

  /////////////////////////////////////////////////////////////////
  // DEFAULT OUTPUTS
  /////////////////////////////////////////////////////////////////

  io.led := Seq.fill(8)(false.B)

  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteVisible := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  io.viewBoxX := 0.U
  io.viewBoxY := 0.U

  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  io.frameUpdateDone := false.B

  /////////////////////////////////////////////////////////////////
  // GAME LOGIC (sprite 3 only)
  /////////////////////////////////////////////////////////////////

  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  // Fixed sprite position (64,64)
  val spriteXReg = RegInit(64.S(11.W))
  val spriteYReg = RegInit(64.S(10.W))

  switch(stateReg) {

    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
      }
    }

    is(compute1) {
      stateReg := done
    }

    is(done) {
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }

  /////////////////////////////////////////////////////////////////
  // SPRITE OUTPUT (ONLY SPRITE 3)
  /////////////////////////////////////////////////////////////////

  io.spriteVisible(3) := true.B
  io.spriteXPosition(3) := spriteXReg
  io.spriteYPosition(3) := spriteYReg
  io.spriteFlipHorizontal(3) := false.B
  io.spriteFlipVertical(3) := false.B
}