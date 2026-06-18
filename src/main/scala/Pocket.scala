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
    val shownSprite = Output(Vec(2, Bool()))
    val showShell = Output(Bool())
    val showShroom = Output(Bool())
    val useShell = Output(Bool())
    val useShroom = Output(Bool())
  })

  val rng = Module(new RNG(2))

  val none :: shell :: shroom :: Nil = Enum(3)

  val item = RegInit(none)

  val prevUseBtn = RegNext(io.useBtn, false.B)
  val usePressed = io.useBtn && !prevUseBtn

  io.useShell := false.B
  io.useShroom := false.B

  when (io.hitMysteryBox && item === none) {
    when (rng.io.randomVal === 0.U) {
      item := shell
    }.elsewhen (rng.io.randomVal === 1.U) {
      item := shell
    }
  }

  when (usePressed) {
    when (item === shell) {
      io.useShell := true.B
      item := none
    }
    .elsewhen (item === shroom) {
      io.useShroom := true.B
      item := none
    }
  }

  io.showShell := item === shell
  io.showShroom := item === shroom
  io.shownSprite(0) := item === shell
  io.shownSprite(1) := item === shroom
}