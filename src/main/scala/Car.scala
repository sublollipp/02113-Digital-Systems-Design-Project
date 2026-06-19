import chisel3._
import chisel3.util._

class Car extends Module{
  val io = IO(new Bundle {
    val btnUp = Input(Bool())
    val btnDown = Input(Bool())
    val btnLeft = Input(Bool())
    val btnRight = Input(Bool())
    val resetSpeed = Input(Bool())
    val update = Input(Bool())
    val shroomBoost = Input(Bool())
    val colBoost = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))                                                                                                                                                                                                                                                                                         
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val shownSprite = Output(Vec(3, Bool()))
    val boostFrames = Input(UInt(10.W))
    val boostSpeed = Input(SInt(11.W))
    val debugLed = Output(Bool())
    val angleOut = Output(UInt(6.W))
    val updateDone = Output(Bool())
  })

  val xPosReg = RegInit(160.S(12.W))
  val yPosReg = RegInit(420.S(11.W))

  val speed = WireInit(0.S(11.W))
  val angle = WireInit(48.U(6.W))

  val speedControl = Module(new CarSpeedController(1, 2, 500, -150, 125, 3))
  speedControl.io.resetSpeed := io.resetSpeed
  speedControl.io.btnFwd := io.btnUp
  speedControl.io.btnBckwd := io.btnDown
  speedControl.io.frameUpdate := io.update
  speedControl.io.shroomBoost := io.shroomBoost
  speedControl.io.colBoost := io.colBoost
  speedControl.io.boostFrames := io.boostFrames
  speedControl.io.boostSpeed := io.boostSpeed
  speed := speedControl.io.speed
  io.debugLed := speedControl.io.debugLed

  val roadCollision = Module(new RoadCollision)
  val offRoadController = Module(new OffRoadSpeedController)

  speedControl.io.offRoad := !roadCollision.io.onRoad

  roadCollision.io.x := xPosReg
  roadCollision.io.y := yPosReg

  offRoadController.io.speedIn := speedControl.io.speed
  offRoadController.io.onRoad := roadCollision.io.onRoad
  offRoadController.io.frameUpdate := io.update

  val angleControl = Module(new CarAngleController(3))
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
  io.updateDone := velControl.io.updateDone && speedControl.io.updateDone && angleControl.io.updateDone

  val spriteControl = Module(new RotatingSpriteController)
  spriteControl.io.angle := angle
  io.flipH := spriteControl.io.flipH
  io.flipV := spriteControl.io.flipV
  io.shownSprite := spriteControl.io.spriteOH_UDR

  io.posX := xPosReg
  io.posY := yPosReg
  io.angleOut := angle
}
