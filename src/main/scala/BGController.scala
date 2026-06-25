import chisel3._
import chisel3.util._

class BGController extends Module {
  val io = IO(new Bundle {
    val showSplash = Input(Bool()) // show___ switches background on a rising edge
    val showGame = Input(Bool())
    val backBufferWriteData = Output(UInt(6.W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())
    val bgUpdateDone = Output(Bool())
    val doneAckn = Input(Bool())
  })

  val gameBackBufferMemory = Module(new Memory(log2Up(1300), 11, "memory_init/game_backbuffer_init.mem"))
  val splashBackBufferMemory = Module(new Memory(log2Up(1300), 11, "memory_init/backbuffer_init.mem"))

  splashBackBufferMemory.io.address := 0.U
  gameBackBufferMemory.io.address := 0.U
  splashBackBufferMemory.io.enable := false.B
  gameBackBufferMemory.io.enable := false.B
  splashBackBufferMemory.io.writeEnable := false.B
  gameBackBufferMemory.io.writeEnable := false.B
  splashBackBufferMemory.io.dataWrite := 0.U
  gameBackBufferMemory.io.dataWrite := 0.U

  val showSplash = io.showSplash
  val showGame = io.showGame && !showSplash

  val splash :: game :: goingToSplash :: goingToGame :: Nil = Enum(4)
  val currentTile = RegInit(0.U(11.W))

  val updateTile = RegInit(false.B)

  val state = RegInit(splash)

  val bgUpdateDone = RegInit(false.B)
  io.bgUpdateDone := bgUpdateDone

  io.backBufferWriteEnable := false.B
  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U

  switch (state) {
    is (splash) {
      when (io.doneAckn) {
        bgUpdateDone := false.B
      }
      when (showGame) {
        currentTile := 0.U
        state := goingToGame
      }
    }
    is (game) {
      when (io.doneAckn) {
        bgUpdateDone := false.B
      }
      when (showSplash) {
        currentTile := 0.U
        state := goingToSplash
      }
    }
    is (goingToSplash) {
      when (updateTile) {
        currentTile := currentTile + 1.U
        io.backBufferWriteEnable := true.B
      }
      when (currentTile === 1199.U) {
        state := splash
        bgUpdateDone := true.B
        currentTile := 0.U
      }
      io.backBufferWriteAddress := currentTile
      splashBackBufferMemory.io.enable := true.B
      splashBackBufferMemory.io.address := currentTile
      io.backBufferWriteData := splashBackBufferMemory.io.dataRead
      updateTile := !updateTile
    }
    is (goingToGame) {
      when (updateTile) {
        currentTile := currentTile + 1.U
        io.backBufferWriteEnable := true.B
      }
      when (currentTile === 1199.U) {
        state := game
        bgUpdateDone := true.B
        currentTile := 0.U
      }
      gameBackBufferMemory.io.enable := true.B
      io.backBufferWriteAddress := currentTile
      gameBackBufferMemory.io.address := currentTile
      io.backBufferWriteData := gameBackBufferMemory.io.dataRead
      updateTile := !updateTile
    }
  }

}
