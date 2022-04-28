package io
import patmos.{BootingIO,PCIO,BootROMIO}
import patmos.Constants._
import chisel3._
class WishboneIO extends Bundle{
  val stb, cyc, we = Input(Bool())
  val sel = Input(UInt(4.W))
  val din, addr = Input(UInt(32.W))
  val ack = Output(Bool())
  val dout = Output(UInt(32.W))
}
/*
  Scala loops through bundles in a bottom up fashion, ie. last declared val of a bundle will be the first element accessed.
  Address linking will be: bootAddr -> 0x30000000, reset -> 0x30000004, stall -> 0x30000008, dataOdd -> 0x3000000C ...
  We can enforce specific address linking with a listmap if needed
 */
class WishboneSlave(baseAddr : Int  = 0x30000000, addrWidth: Int = ADDR_WIDTH) extends Module{
  val io = IO(new Bundle{
    val wb = new WishboneIO
    val patmos = Output(new Bundle{
      val boot = new BootingIO(addrWidth)
    })
  })
  // Initialization wishbone register groups
  val initPC = 0.U(1.W) ## 1.U(1.W) ## 1.U(30.W) // Init: stall = 0, reset = 1, bootAddr = 1
  val initROM = 0.U // Init: all signals = 0
  // List of wishbone register groups
  val WBReg = Seq(RegInit(initPC.asTypeOf(new PCIO)), RegInit(initROM.asTypeOf(new BootROMIO(addrWidth))))
  // Connecting them to the outer world
  io.patmos.boot.pc <> WBReg(0)
  io.patmos.boot.rom <> WBReg(1)
  io.wb.dout := 0.U
  var addrOffset = 0
  val validAddr = WireDefault(false.B)
  io.wb.ack := RegNext(io.wb.stb & validAddr) // Always acknowledge
  val read = io.wb.stb & io.wb.cyc & !io.wb.we
  val write = io.wb.stb & io.wb.cyc & io.wb.we
  for(i <- WBReg.indices){ // Loop through all wishbone register groups
    for((id, reg) <- WBReg(i).elements){
      when(io.wb.addr === (baseAddr + addrOffset).U){ // Check if current address input matches the given wb register
        validAddr := true.B
        when(io.wb.ack){ // Check for acknowledgement
          when(write){
            reg := io.wb.din // Write data from bus to wb register
          } . elsewhen(read){
            io.wb.dout := reg // Write data from wb register to bus
          }
        }
      }
      addrOffset+=4
    }
  }
}
