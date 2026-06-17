import chisel3._
import chisel3.util._

class RaceStartLight(redFrames: Int,
                     redYellowFrames: Int,
                     greenFrames: Int) extends Module {

  val io = IO(new Bundle {
    val update = Input(Bool())

    val showRed       = Output(Bool())
    val showYellow = Output(Bool())
    val showGreen     = Output(Bool())

    val raceStarted   = Output(Bool())
    val visible       = Output(Bool())
  })

  val red :: redYellow :: green :: finished :: Nil = Enum(4)

  val state = RegInit(red)

  val counter = RegInit(0.U(16.W))

  io.showRed := false.B
  io.showYellow := false.B
  io.showGreen := false.B

  io.visible := true.B
  io.raceStarted := false.B

  switch(state) {

    is(red) {

      io.showRed := true.B

      when(io.update) {
        counter := counter + 1.U
      }

      when(counter === (redFrames - 1).U) {
        state := redYellow
        counter := 0.U
      }
    }

    is(redYellow) {

      io.showYellow := true.B

      when(io.update) {
        counter := counter + 1.U
      }

      when(counter === (redYellowFrames - 1).U) {
        state := green
        counter := 0.U
      }
    }

    is(green) {

      io.showGreen := true.B
      io.raceStarted := true.B

      when(io.update) {
        counter := counter + 1.U
      }

      when(counter === (greenFrames - 1).U) {
        state := finished
      }
    }

    is(finished) {

      io.visible := false.B
      io.raceStarted := true.B
    }
  }
}