package SPI_memory

import chisel3._
import chisel3.util._
import ocp._
import chisel3.experimental.chiselName

class OCPburst_SPI_memory(count_s_clock: Int = 4, startup_count_to : Int = 0x3FFF) extends Module {
  val io = IO(new Bundle {

    val OCP_interface = new OcpBurstSlavePort(24, 32, 4)

    val CE = Output(Bool())
    val MOSI = Output(Bool())
    val MISO = Input(Bool())
    val S_CLK = Output(Bool())

    //DEBUG io
    val SR = Output(UInt(4.W))
    val CntReg = Output(UInt(8.W))
    val SPI_DATA_VALID = Output(Bool());
    val SPI_write_complete = Output(Bool());
    //val SPI_CntReg = Output(UInt(8.W))
    //val SPI_POS_REG = Output(UInt(4.W))
  })

  //Defaults
  io.OCP_interface.S.Resp := 0.U
  io.OCP_interface.S.CmdAccept := false.B
  io.OCP_interface.S.DataAccept := false.B
  io.OCP_interface.S.Data := 0.U


  val SPI = Module(new SPI(count_s_clock, startup_count_to))
  for(i <- 0 until 4){
    SPI.io.WriteData(i) := 0.U
  }

  SPI.io.Address := 0.U
  SPI.io.ReadEnable := false.B
  SPI.io.WriteEnable := false.B
  SPI.io.ByteEnable := 0.U

  io.SPI_DATA_VALID := SPI.io.DataValid;
  io.SPI_write_complete := SPI.io.WriteCompleted;
  //io.SPI_CntReg := SPI.io.CntReg;
  //io.SPI_POS_REG := SPI.io.PosReg;

  io.MOSI := SPI.io.SPI_interface.MOSI
  io.CE := SPI.io.SPI_interface.CE
  SPI.io.SPI_interface.MISO := io.MISO
  io.S_CLK := SPI.io.SPI_interface.S_CLK;

  val slave_resp = RegInit(OcpResp.NULL)
  io.OCP_interface.S.Resp := slave_resp;

  val idle :: read :: sampleData :: read_transmit :: write :: Nil = Enum(5)
  val StateReg = RegInit(idle)
  io.SR := StateReg;

  val CntReg = RegInit(0.U(8.W))
  io.CntReg := CntReg

  val WriteData = Reg(Vec(4,UInt(32.W)))
  val WriteByteEN = Reg(Vec(4, UInt(4.W)))


  val address = RegInit(0.U(24.W))
  address := 0.U;

  switch(StateReg) {
    is(idle) {
      slave_resp := OcpResp.NULL;
      switch(io.OCP_interface.M.Cmd) {
        is(OcpCmd.WR) {
          CntReg := 0.U
          StateReg := sampleData
          address := io.OCP_interface.M.Addr;
        }
        is(OcpCmd.RD) {
          CntReg := 0.U
          StateReg := read
          address := io.OCP_interface.M.Addr;
        }
      }
    }
    is(read) {
      address := address;
      SPI.io.Address := address;
      SPI.io.ReadEnable := true.B
      io.OCP_interface.S.CmdAccept := true.B;

      when(SPI.io.DataValid){
        CntReg := 0.U
        StateReg := read_transmit
      }
    }
    is(read_transmit) {
      io.OCP_interface.S.Data := SPI.io.ReadData(CntReg)
      CntReg := CntReg + 1.U
      SPI.io.ReadEnable := false.B
      io.OCP_interface.S.Resp := OcpResp.DVA

      when(CntReg === 3.U) {
        CntReg := 0.U
        StateReg := idle
      }
    }
    is(sampleData) {
      address := address;
      when(CntReg === 0.U){
        io.OCP_interface.S.CmdAccept := true.B
      }.otherwise{
        io.OCP_interface.S.CmdAccept := false.B
      }
      io.OCP_interface.S.DataAccept := true.B

      when(io.OCP_interface.M.DataValid.toBool()) {
        WriteData(CntReg) := io.OCP_interface.M.Data;
        WriteByteEN(CntReg) := io.OCP_interface.M.DataByteEn
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

