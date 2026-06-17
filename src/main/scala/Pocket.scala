import chisel3._
import chisel3.util._

class Pocket extends Module {
  val io = IO(new Bundle {
    val useBtn = Input(Bool())
    val frameUpdate = Input(Bool())
    val carPosX = Input(SInt(12.W))
    val carPosY = Input(SInt(11.W))
    val carAngle = Input(UInt(6.W))
    val hitMysteryBox = Input(Bool())
    val rngInput = Input(Vec(4, Bool()))
    val shownSprite = Output(Vec(2, Bool()))
    val showShell = Output(Bool())
    val showShroom = Output(Bool())
  })

val none :: shell :: shroom :: Nil = Enum(3)

val item = RegInit(none)

when(io.hitMysteryBox) {

  when(io.rngInput(0)) {
    item := shell
  }

  when(io.rngInput(1)) {
    item := shroom
  }
}

io.showShell := item === shell
io.showShroom := item === shroom


}
