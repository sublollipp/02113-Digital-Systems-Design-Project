import chisel3._
import chisel3.util._

class RotatingSpriteController(steps: Array[Int] = Array(61, 0, 4, 12, 20, 28, 36, 44, 52)) extends Module {
  val io = IO(new Bundle {
    val angle = Input(UInt(6.W))
    val spriteOH_UDR = Output(Vec(3, Bool()))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
  })

  val angle = io.angle

  val uu :: ur :: rr :: dr :: dd :: dl :: ll :: ul :: Nil = Enum(8)
  val upSprite :: diagSprite :: rightSprite :: Nil = Enum(3)

  val dir = WireDefault(uu)
  val flipSpriteH = RegInit(false.B)
  val flipSpriteV = RegInit(false.B)
  val sprite = WireDefault(upSprite)
  val shownSprite = WireDefault(VecInit(true.B, false.B, false.B))

  when ((steps(0).U <= angle || angle >= steps(1).U) && angle <= steps(2).U) {
    dir := rr
  }.elsewhen(steps(2).U < angle && angle <= steps(3).U) {
    dir := dr
  }.elsewhen(steps(3).U < angle && angle <= steps(4).U) {
    dir := dd
  }.elsewhen(steps(4).U < angle && angle <= steps(5).U) {
    dir := dl
  }.elsewhen(steps(5).U < angle && angle <= steps(6).U) {
    dir := ll
  }.elsewhen(steps(6).U < angle && angle <= steps(7).U) {
    dir := ul
  }.elsewhen(steps(7).U < angle && angle <= steps(8).U) {
    dir := uu
  }.otherwise {
    dir := ur
  }


  // Sprite
  switch(dir) {
    is (uu) {
      sprite := upSprite
      flipSpriteH := false.B
      flipSpriteV := false.B
    }
    is (dd) {
      sprite := upSprite
      flipSpriteH := false.B
      flipSpriteV := true.B
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


  io.spriteOH_UDR := shownSprite
  io.flipH := flipSpriteH
  io.flipV := flipSpriteV
}
