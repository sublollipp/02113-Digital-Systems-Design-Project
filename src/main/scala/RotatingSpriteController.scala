import chisel3._
import chisel3.util._

class RotatingSpriteController(steps: Array[Int] = Array(62, 0, 1, 6, 9, 14, 17, 22, 25, 30, 33, 38, 41, 46, 49, 54, 57)) extends Module {
  val io = IO(new Bundle {
    val angle = Input(UInt(6.W))
    val spriteOH_UDR = Output(Vec(5, Bool()))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
  })

  val angle = io.angle

  val uu :: ur :: rr :: dr :: dd :: dl :: ll :: ul :: uur :: urr :: drr :: ddr :: dll :: ddl :: uul :: ull :: Nil = Enum(16)
  val upSprite :: diagSprite :: rightSprite :: diagUpSprite :: diagSideSprite :: Nil = Enum(5)

  val dir = WireDefault(uu)
  val flipSpriteH = WireDefault(true.B)
  val flipSpriteV = WireDefault(true.B)
  val sprite = WireDefault(upSprite)
  val shownSprite = WireDefault(VecInit(false.B, false.B, false.B, false.B, false.B))

  when ((steps(0).U <= angle || angle >= steps(1).U) && angle <= steps(2).U) {
    dir := rr
  }.elsewhen(steps(2).U < angle && angle <= steps(3).U) {
    dir := drr
  }.elsewhen(steps(3).U < angle && angle <= steps(4).U) {
    dir := dr
  }.elsewhen(steps(4).U < angle && angle <= steps(5).U) {
    dir := ddr
  }.elsewhen(steps(5).U < angle && angle <= steps(6).U) {
    dir := dd
  }.elsewhen(steps(6).U < angle && angle <= steps(7).U) {
    dir := ddl
  }.elsewhen(steps(7).U < angle && angle <= steps(8).U) {
    dir := dl
  }.elsewhen(steps(8).U < angle && angle <= steps(9).U) {
    dir := dll
  } .elsewhen(steps(9).U < angle && angle <= steps(10).U) {
    dir := ll
  } .elsewhen(steps(10).U < angle && angle <= steps(11).U) {
    dir := ull
  } .elsewhen(steps(11).U < angle && angle <= steps(12).U) {
    dir := ul
  } .elsewhen(steps(12).U < angle && angle <= steps(13).U) {
    dir := uul
  } .elsewhen(steps(13).U < angle && angle <= steps(14).U) {
    dir := uu
  } .elsewhen(steps(14).U < angle && angle <= steps(15).U) {
    dir := uur
  } .elsewhen(steps(15).U < angle && angle <= steps(16).U) {
    dir := ur
  } .otherwise {
    dir := urr
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
    is (uur) {
      sprite := diagUpSprite
      flipSpriteH := false.B
      flipSpriteV := true.B
    }
    is (urr) {
      sprite := diagSideSprite
      flipSpriteH := false.B
      flipSpriteV := true.B
    }
    is (drr) {
      sprite := diagSideSprite
      flipSpriteH := false.B
      flipSpriteV := false.B
    }
    is (ddr) {
      sprite := diagUpSprite
      flipSpriteH := false.B
      flipSpriteV := false.B
    }
    is (dll) {
      sprite := diagSideSprite
      flipSpriteH := true.B
      flipSpriteV := false.B
    }
    is (ddl) {
      sprite := diagUpSprite
      flipSpriteH := true.B
      flipSpriteV := false.B
    }
    is (ull) {
      sprite := diagSideSprite
      flipSpriteH := true.B
      flipSpriteV := true.B
    }
    is (uul) {
      sprite := diagUpSprite
      flipSpriteH := true.B
      flipSpriteV := true.B
    }
  }

  switch(sprite) {
    is(upSprite) {
      shownSprite(0) := true.B
    }
    is(diagSprite) {
      shownSprite(1) := true.B
    }
    is(rightSprite) {
      shownSprite(2) := true.B
    }
    is(diagUpSprite) {
      shownSprite(3) := true.B
    }
    is(diagSideSprite) {
      shownSprite(4) := true.B
    }
  }

  io.spriteOH_UDR := shownSprite

  io.flipH := flipSpriteH
  io.flipV := flipSpriteV
}
