import chisel3._
import chisel3.util._

class RotatingSpriteController extends Module {
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

  when ((61.U <= angle || angle >= 0.U) && angle <= 4.U) {
    dir := rr
  }.elsewhen(5.U <= angle && angle <= 12.U) {
    dir := dr
  }.elsewhen(13.U <= angle && angle <= 20.U) {
    dir := dd
  }.elsewhen(21.U <= angle && angle <= 28.U) {
    dir := dl
  }.elsewhen(29.U <= angle && angle <= 36.U) {
    dir := ll
  }.elsewhen(37.U <= angle && angle <= 44.U) {
    dir := ul
  }.elsewhen(45.U <= angle && angle <= 52.U) {
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
