package SPI_memory

import chisel3._
import chisel3.util._
import SPI_CMDS._

object SPI_CMDS{
  val CMDResetEnable = 102.U(8.W)
  val CMDReset = 153.U(8.W)
  val CMDSPIRead = 3.U(8.W)
  val CMDSPIWrite = 2.U(8.W)
}

class SPI_Interface() extends Bundle(){
  val CE = Output(Bool())
  val MOSI = Output(Bool())
  val MISO = Input(Bool())
  val S_CLK = Output(Bool())
}

class SPI_memory_port() extends Bundle() {
  val ReadEnable = Input(Bool())
  val WriteEnable = Input(Bool())
  val Address = Input(UInt(24.W))
  val WriteData = Input(Vec(4,UInt(32.W)))
  val ByteEnable = Input(UInt(16.W))

  val ReadData = Output(Vec(4, UInt(32.W)))

  val DataValid = Output(Bool())
  val WriteCompleted = Output(Bool())
  val Completed = Output(Bool())

  //SPI pins
  val SPI_interface = new SPI_Interface();
  //val StateReg = Output()
  //val CntReg = Output(UInt(8.W));
  //val PosReg = Output(UInt(4.W));
}

class SPI(count_s_clock: Int, startup_count_to : Int = 0x3FFF) extends Module {
  val io = IO(new SPI_memory_port())

  // Defaults

  val DataReg = RegInit(0.U(128.W))

  io.SPI_interface.CE := true.B
  io.SPI_interface.MOSI := false.B
  io.DataValid := false.B
  io.Completed := false.B

  io.DataValid := false.B

  io.ReadData(0) := DataReg(127,96)
  io.ReadData(1) := DataReg(95,64)
  io.ReadData(2) := DataReg(63,32)
  io.ReadData(3) := DataReg(31,0)

  val WriteCompleted_reg = RegInit(false.B)
  io.WriteCompleted := WriteCompleted_reg;

  val boot :: resetEnable :: resetWait :: setReset :: idle :: read :: write :: Nil = Enum(7)
  val StateReg = RegInit(boot)
  StateReg := boot;

  val transmitCMD :: transmitAddress :: transmitData :: writeDelay :: receiveData :: computeAddress :: Nil = Enum(6)
  val SubStateReg = RegInit(transmitCMD)

  val CntReg = RegInit(0.U(16.W))
  CntReg := 0.U;

  /*
  TempAddress is the starting address of the current burst.
  When ByteEn of a specific byte is low,
  this address will jump to the starting address of the next valid byte
  */

  val TempAddress = RegInit(0.U(24.W))

  val PosReg = RegInit(0.U(4.W)) //PosReg is a pointer to the current byte in the byteEn integer being written to memory

  val Carry = Wire(Vec(17, Bool())) // Used to find read address when byte enable is low
  for(i <- 0 until 17) {
    Carry(i) := false.B
  }

  // Clock stuff

  val ClkReg = RegInit(0.U(1.W))
  val ClkCounter = RegInit(0.U(8.W))

  val ClkRegDelay = RegInit(0.U(1.W)) // Used to determine rising and falling edge
  ClkRegDelay := ClkReg

  val NextState = Wire(Bool()) // Goes high one clock cycle before rising edge of SCLK
  val NextStateInv = Wire(Bool()) // Goes high one clock cycle before falling edge of SCLK

  val ClockEn = Wire(Bool()) // Enables SCLK output
  val ClockReset = Wire(Bool()) // Resets clock to 0

  val RisingEdge = Wire(Bool())
  val FallingEdge = Wire(Bool())


  NextState := false.B
  NextStateInv := false.B

  ClockEn := false.B
  ClockReset := false.B

  RisingEdge := false.B
  FallingEdge := false.B

  io.SPI_interface.S_CLK := (ClkReg & ClockEn)

  ClkCounter := ClkCounter + 1.U

  when(ClkCounter === count_s_clock.U){
    ClkReg := !ClkReg
    ClkCounter := 0.U

    when(!ClkReg.asBool){
      NextState := true.B
    }
    when(ClkReg.asBool){
      NextStateInv := true.B
    }
  }

  when(ClockReset){
    ClkReg := false.B
    ClkCounter := 0.U
  }

  when(ClkReg.asBool && !ClkRegDelay.asBool){
    RisingEdge := true.B
  }

  when(!ClkReg.asBool && ClkRegDelay.asBool){
    FallingEdge := true.B
  }


  // Actual state machine


  switch(StateReg) {
    is(boot){
      // Resets clock for reset command
      ClockEn := false.B
      CntReg := CntReg + 1.U
      StateReg := boot

      when(CntReg === startup_count_to.U){
        io.SPI_interface.CE := false.B
        ClockReset := true.B
        StateReg := resetEnable
        CntReg := 0.U
      }
    }
    is(resetEnable) {
      io.SPI_interface.CE := false.B
      ClockEn := true.B

      StateReg := resetEnable
      io.SPI_interface.MOSI := CMDResetEnable(7.U - CntReg)
      CntReg := CntReg;

      when(NextStateInv){
        CntReg := CntReg + 1.U
      }

      when(CntReg === 7.U && NextStateInv) {
        io.SPI_interface.CE := true.B
        CntReg := 0.U
        io.SPI_interface.MOSI := 0.U
        StateReg := resetWait
      }
    }
    is(resetWait){
      // A Delay between the two commands
      io.SPI_interface.CE := true.B

      StateReg := resetWait

      when(NextStateInv){
        ClockReset := true.B
        StateReg := setReset
      }
    }
    is(setReset) {
      io.SPI_interface.CE := false.B
      ClockEn := true.B

      StateReg := setReset
      CntReg := CntReg;
      io.SPI_interface.MOSI := CMDReset(7.U - CntReg)

      when(NextStateInv){
        CntReg := CntReg + 1.U
      }

      when(CntReg === 7.U && NextStateInv) {
        io.SPI_interface.CE := true.B
        CntReg := 0.U
        io.Completed := true.B

        StateReg := idle
      }
    }
    is(idle) {
      // Waits for command from main
      StateReg := idle
      io.SPI_interface.CE := true.B
      WriteCompleted_reg := false.B;

      when(io.ReadEnable) {
        StateReg := read
        SubStateReg := transmitCMD
        io.SPI_interface.CE := false.B
        ClockReset := true.B
      }
      . elsewhen(io.WriteEnable) {
        StateReg := write
        SubStateReg := computeAddress
      }
    }
    is(read) {
      StateReg := read

      switch(SubStateReg) {
        is(transmitCMD) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          CntReg := CntReg;

          io.SPI_interface.MOSI := CMDSPIRead(7.U - CntReg)
          SubStateReg := transmitCMD

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }

        is(transmitAddress) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          CntReg := CntReg;

          io.SPI_interface.MOSI := io.Address(23.U - CntReg)
          SubStateReg := transmitAddress

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 23.U && NextStateInv) {
            //io.SPI_interface.MOSI := io.Address(0.U)
            CntReg := 0.U
            SubStateReg := receiveData
          }
        }
        is(receiveData) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          SubStateReg := receiveData
          CntReg := CntReg;
          // Reads on the rising edge of SCLK

          when(RisingEdge){
            DataReg := Cat(DataReg, io.SPI_interface.MISO.asUInt)
            CntReg := CntReg + 1.U
          }

          when(CntReg === 128.U && NextStateInv) {
            io.DataValid := true.B
            StateReg := idle
          }
        }
      }
    }
    is(write) {
      //SubStateReg := computeAddress
      StateReg := write

      switch(SubStateReg) {
        is(computeAddress) {
          when(PosReg === 15.U){
            PosReg := 0.U
            WriteCompleted_reg := true.B
            StateReg := idle
            io.SPI_interface.CE := true.B
          }

          // The following code scans through the ByteEn "array", to find the next position where ByteEn is high

          for(i <- 0 until 16){
            when(i.U === 0.U && io.ByteEnable(i) && PosReg === 0.U){
              TempAddress := io.Address
              Carry(i+1) := true.B
            }.elsewhen(Carry(i)){
              Carry(i+1) := true.B
            }.elsewhen(i.U > PosReg && io.ByteEnable(i) && !Carry(i)){
              TempAddress := io.Address + i.U
              PosReg := i.U
              Carry(i + 1) := true.B
            }.otherwise{
              Carry(i + 1) := false.B
            }
          }

          when(!Carry(15).asBool){
            PosReg := 0.U
            WriteCompleted_reg := true.B
            StateReg := idle
            io.SPI_interface.CE := true.B
          }

          SubStateReg := transmitCMD

          io.SPI_interface.CE := false.B
          ClockReset := true.B

        }
        is(transmitCMD) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          CntReg := CntReg;

          io.SPI_interface.MOSI := CMDSPIWrite(7.U - CntReg)
          SubStateReg := transmitCMD

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            //io.SPI_interface.MOSI := CMDSPIWrite(0.U)
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }
        is(transmitAddress) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          CntReg := CntReg;

          io.SPI_interface.MOSI := TempAddress(23.U - CntReg)
          SubStateReg := transmitAddress

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 23.U && NextStateInv){
            io.SPI_interface.MOSI := TempAddress(0.U)
            CntReg := 0.U
            SubStateReg := transmitData
          }
        }
        is(transmitData) {
          io.SPI_interface.CE := false.B
          ClockEn := true.B

          CntReg := CntReg;

          io.SPI_interface.MOSI := io.WriteData(  (PosReg >> 2).asUInt)(31.U - Cat(PosReg(1,0),CntReg(2,0)).asUInt   )
          SubStateReg := transmitData

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            PosReg := PosReg + 1.U
            CntReg := 0.U

            // When ByteEn for the next byte is low, the following code restarts the write.

            when(!io.ByteEnable(PosReg + 1.U)) {
              CntReg := 0.U
              SubStateReg := writeDelay
              io.SPI_interface.CE := true.B
            }
          }

          when((CntReg + (PosReg << 3).asUInt) === 127.U && NextStateInv) {
            CntReg := 0.U
            PosReg := 0.U
            WriteCompleted_reg := true.B
            StateReg := idle
            io.SPI_interface.CE := true.B
          }
        }
        is(writeDelay){
          // Delay required between two write bursts

          // TODO check datasheet for actual required delay

          io.SPI_interface.CE := true.B

          when(NextStateInv){
            SubStateReg := computeAddress
          }
        }
      }
    }
  }
}
