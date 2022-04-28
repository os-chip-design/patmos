package patmos

import Chisel._
import chisel3.dontTouch

import Constants._

import util.Utility
import util.BlackBoxRom

class BootMemory(fileName : String) extends Module{

  // Check for errors in user input
  if (!isPow2(BOOT_ROM_ENTRIES)){
    // Check if number of entries are valid
    throw new Error("BOOT_ROM_ENTRIES must be power of 2")
  } else if (!isPow2(WRITABLE_BOOT_ENTRIES)){
    // Check if number of entries are valid
    throw new Error("WRITABLE_BOOT_ENTRIES must be power of 2")
  } else if ((BOOT_ROM_ENTRIES + WRITABLE_BOOT_ENTRIES) > (BOOT_ADDR_SPACE + 1)){
    // Check if boot memory is too large
    throw new Error("Boot memory exceeds the address spaces reserved for boot memory")
  }

  val io = IO(new BootMemoryIO())

  val romContents = Utility.binToDualRom(fileName, INSTR_WIDTH, BOOT_ROM_ENTRIES)

  if ((2*romContents._1.length) > BOOT_ROM_ENTRIES){
    // Check if number of entries are valid
    throw new Error("Boot program too large for boot ROM\nProgram has length "  + 2*romContents._1.length + "\n, but ROM is configures to have " + BOOT_ROM_ENTRIES + " entries")
  }

  val rom = Module(new BlackBoxRom(romContents, BOOT_ROM_ADDR_WIDTH))

  // Address registers for ROM, to make it synchronous
  val rdAddrEvenReg = dontTouch(RegNext(io.read.addrEven))
  val rdAddrOddReg = dontTouch(RegNext(io.read.addrOdd))

  //rom.io.addressEven := RegNext(io.read.addrEven(BOOT_ROM_ADDR_WIDTH - 1, 1))
  //rom.io.addressOdd := RegNext(io.read.addrOdd(BOOT_ROM_ADDR_WIDTH - 1, 1))
  rom.io.addressEven := rdAddrEvenReg(BOOT_ROM_ADDR_WIDTH - 1, 1)
  rom.io.addressOdd := rdAddrOddReg(BOOT_ROM_ADDR_WIDTH - 1, 1)
  //io.read.dataEven := rom.io.instructionEven
  //io.read.dataOdd := rom.io.instructionOdd

  val memWithWrEven = Module(new MemBlock(size = WRITABLE_BOOT_ENTRIES / 2, INSTR_WIDTH))
  memWithWrEven.io.rdAddr := (io.read.addrEven - BOOT_ROM_ENTRIES.U)(BOOT_ROM_ADDR_WIDTH - 1, 1) // Read address input
  memWithWrEven.io.wrAddr := (io.write.addrEven - BOOT_ROM_ENTRIES.U)(BOOT_ROM_ADDR_WIDTH - 1, 1) // Write address input
  memWithWrEven.io.wrData := io.write.dataEven // Write data input
  memWithWrEven.io.wrEna := io.write.enaEven // Write enable

  // Writable memory odd
  val memWithWrOdd = Module(new MemBlock(size = WRITABLE_BOOT_ENTRIES / 2, INSTR_WIDTH))
  memWithWrOdd.io.rdAddr := (io.read.addrOdd - BOOT_ROM_ENTRIES.U)(BOOT_ROM_ADDR_WIDTH - 1, 1) // Read address input
  memWithWrOdd.io.wrAddr := (io.write.addrOdd - BOOT_ROM_ENTRIES.U)(BOOT_ROM_ADDR_WIDTH - 1, 1) // Write address input
  memWithWrOdd.io.wrData := io.write.dataOdd // Write data input
  memWithWrOdd.io.wrEna := io.write.enaOdd // Write enable

  // Switch between ROM and writable memory
  when(rdAddrEvenReg > UInt(BOOT_ROM_ENTRIES - 1)){
    io.read.dataEven := memWithWrEven.io.rdData
  }. otherwise {
    io.read.dataEven := rom.io.instructionEven
  }
  
  // Switch between ROM and writable memory
  when(rdAddrOddReg > UInt(BOOT_ROM_ENTRIES - 1)){
    io.read.dataOdd := memWithWrOdd.io.rdData
  }. otherwise {
    io.read.dataOdd := rom.io.instructionOdd
  }
}
