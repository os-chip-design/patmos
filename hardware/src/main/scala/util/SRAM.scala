package util

import chisel3._
import chisel3.util.HasBlackBoxPath


object SRAM {
  def _8x1024(clock: Clock): sky130_sram_1kbyte_1rw1r_8x1024_8 = {
    val sram = Module(new sky130_sram_1kbyte_1rw1r_8x1024_8)
    sram.io.clk0 := clock
    sram.io.clk1 := clock
    sram.io.csb0 := 0.B // always selected
    sram.io.csb1 := 0.B // always selected
    sram.io.wmask0 := 1.U // the 1 byte write value is always enabled
    sram
  }
  def _32x256(clock: Clock): sky130_sram_1kbyte_1rw1r_32x256_8 = {
    val sram = Module(new sky130_sram_1kbyte_1rw1r_32x256_8)
    sram.io.clk0 := clock
    sram.io.clk1 := clock
    sram.io.csb0 := 0.B // always selected
    sram.io.csb1 := 0.B // always selected
    sram.io.wmask0 := 0.U // the 1 byte write value is always enabled
    sram
  }
}

class sky130_sram_1kbyte_1rw1r_8x1024_8 extends BlackBox with HasBlackBoxPath {

  val io = IO(new Bundle {
    val clk0 = Input(Clock()) // rw port clock
    val csb0 = Input(Bool()) // rw port active low chip select
    val web0 = Input(Bool()) // wr port active low write control
    val wmask0 = Input(UInt(1.W)) // write mask
    val addr0 = Input(UInt(10.W)) // rw port address
    val din0 = Input(UInt(8.W)) // rw port write data
    val dout0 = Output(UInt(8.W)) // rw port read data
    val clk1 = Input(Clock()) // r port clock
    val csb1 = Input(Bool()) // r port active low chip select
    val addr1 = Input(UInt(10.W)) // r port address
    val dout1 = Output(UInt(8.W)) // r port read data
  })
  addPath("verilog/sky130_sram_1kbyte_1rw1r_8x1024_8.v")

  // connect signals for a read
  def read(index: UInt): UInt = {
    io.addr1 := index
    io.dout0
  }

  // connect signals for a write
  def write(en: Bool)(index: UInt, data: UInt): Unit = {
    io.web0 := !en
    io.addr0 := index
    io.din0 := data
  }

}

class sky130_sram_1kbyte_1rw1r_32x256_8 extends BlackBox with HasBlackBoxPath {

  val io = IO(new Bundle {
    val clk0 = Input(Clock()) // rw port clock
    val csb0 = Input(Bool()) // rw port active low chip select
    val web0 = Input(Bool()) // wr port active low write control
    val wmask0 = Input(UInt(4.W)) // write mask
    val addr0 = Input(UInt(8.W)) // rw port address
    val din0 = Input(UInt(32.W)) // rw port write data
    val dout0 = Output(UInt(32.W)) // rw port read data
    val clk1 = Input(Clock()) // r port clock
    val csb1 = Input(Bool()) // r port active low chip select
    val addr1 = Input(UInt(8.W)) // r port address
    val dout1 = Output(UInt(32.W)) // r port read data
  })
  addPath("verilog/sky130_sram_1kbyte_1rw1r_32x256_8.v")

  // connect signals for a read
  def read(index: UInt): UInt = {
    io.addr1 := index
    io.dout0
  }

  // connect signals for a write
  def write(en: Bool)(index: UInt, data: UInt, mask: Vec[Bool]): Unit = {
    io.wmask0 := mask.asUInt
    io.web0 := !en
    io.addr0 := index
    io.din0 := data
  }

}