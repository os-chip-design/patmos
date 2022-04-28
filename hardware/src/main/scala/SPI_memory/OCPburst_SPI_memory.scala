package SPI_memory

import chisel3._
import chisel3.util._
import ocp._
import chisel3.experimental.chiselName
import io._
import patmos.Constants._

object OCPburst_SPI_memory extends DeviceObject {
  def init(params: Map[String, String]) = { }

  def create(params: Map[String, String]): OCPburst_SPI_memory = {
    Module(new OCPburst_SPI_memory(count_s_clock = 1, startup_count_to = 0x3FFF))
  }

  trait Pins extends patmos.HasPins {
    override val pins = new Bundle {
      val spiOut = Output(new Bundle {
        val CE = Bits(width = 1.W)
        val MOSI = Bits(width = 1.W)
        val S_CLK = Bits(width = 1.W)
      })
      val spiIn = Input(new Bundle{
        val MISO = Bits(width = 1.W)
      })
    }
  }
}

class OCPburst_SPI_memory(count_s_clock: Int = 4, startup_count_to : Int = 0x3FFF) extends BurstDevice(21) {
  override val io = new BurstDeviceIO(21) with OCPburst_SPI_memory.Pins  

  //Defaults
  io.ocp.S.Resp := 0.U
  io.ocp.S.CmdAccept := false.B
  io.ocp.S.DataAccept := false.B
  io.ocp.S.Data := 0.U


  val SPI = Module(new SPI(count_s_clock, startup_count_to))
  for(i <- 0 until 4){
    SPI.io.WriteData(i) := 0.U
  }

  SPI.io.Address := 0.U
  SPI.io.ReadEnable := false.B
  SPI.io.WriteEnable := false.B
  SPI.io.ByteEnable := 0.U

  //io.SPI_CntReg := SPI.io.CntReg;
  //io.SPI_POS_REG := SPI.io.PosReg;

  io.pins.spiOut.MOSI := SPI.io.SPI_interface.MOSI
  io.pins.spiOut.CE := SPI.io.SPI_interface.CE
  SPI.io.SPI_interface.MISO := io.pins.spiIn.MISO
  io.pins.spiOut.S_CLK := SPI.io.SPI_interface.S_CLK;

  val slave_resp = RegInit(OcpResp.NULL)
  io.ocp.S.Resp := slave_resp;

  val idle :: read :: sampleData :: read_transmit :: write :: Nil = Enum(5)
  val StateReg = RegInit(idle)


  val CntReg = RegInit(0.U(8.W))

  val WriteData = Reg(Vec(4,UInt(32.W)))
  val WriteByteEN = Reg(Vec(4, UInt(4.W)))


  val address = RegInit(0.U(24.W))
  address := 0.U;

  switch(StateReg) {
    is(idle) {
      slave_resp := OcpResp.NULL;
      switch(io.ocp.M.Cmd) {
        is(OcpCmd.WR) {
          CntReg := 0.U
          StateReg := sampleData
          address := io.ocp.M.Addr;
        }
        is(OcpCmd.RD) {
          CntReg := 0.U
          StateReg := read
          address := io.ocp.M.Addr;
        }
      }
    }
    is(read) {
      address := address;
      SPI.io.Address := address;
      SPI.io.ReadEnable := true.B
      io.ocp.S.CmdAccept := true.B;

      when(SPI.io.DataValid){
        CntReg := 0.U
        StateReg := read_transmit
      }
    }
    is(read_transmit) {
      io.ocp.S.Data := SPI.io.ReadData(CntReg)
      CntReg := CntReg + 1.U
      SPI.io.ReadEnable := false.B
      io.ocp.S.Resp := OcpResp.DVA

      when(CntReg === 3.U) {
        CntReg := 0.U
        StateReg := idle
      }
    }
    is(sampleData) {
      address := address;
      when(CntReg === 1.U){
        io.ocp.S.CmdAccept := true.B
      }.otherwise{
        io.ocp.S.CmdAccept := false.B
      }
      io.ocp.S.DataAccept := true.B

      when(io.ocp.M.DataValid.toBool()) {
        WriteData(CntReg) := io.ocp.M.Data;
        WriteByteEN(CntReg) := io.ocp.M.DataByteEn
        CntReg := CntReg + 1.U
      }

      when(CntReg === 3.U) {
        CntReg := 0.U
        StateReg := write
      }
    }
    is(write) {
      address := address;
      SPI.io.Address := address;
      SPI.io.WriteEnable := true.B
      StateReg := write

      SPI.io.WriteData := WriteData
      SPI.io.ByteEnable := (WriteByteEN(3) << 12).asUInt + (WriteByteEN(2) << 8).asUInt + (WriteByteEN(1) << 4).asUInt + WriteByteEN(0)

      when(SPI.io.WriteCompleted) {
        slave_resp := OcpResp.DVA;
        StateReg := idle
      }
    }


  }
}

