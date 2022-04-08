package SPI_memory

import SPI_memory.OCPburst_SPI_memory
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import ocp._
import org.scalatest.flatspec.AnyFlatSpec
import treadle.WriteVcdAnnotation
import chisel3.experimental.chiselName
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader
import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.math._
import scala.collection.mutable.Map



class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  println(Console.GREEN + "testing" + Console.RESET)

  def test_memory(mem_sim : Software_Memory_Sim, start_address : Int, expected : Array[BigInt], fail: () => Unit): Unit ={
    var failed = false;
    for(i <- 0 to expected.length - 1){

      for(x <- 0 to 3){
        val address = start_address + i * 4 + x;
        val sub_expected : Byte = ((expected(i) >> (8*(3-x))) & 0xFF).toByte //TODO should we convert the indian here?
        val data : Byte = mem_sim.memory(address);
        if(data != sub_expected){
          println(Console.RED + "failed, expected memory 0x" + (address).toHexString + " was 0x" + data.toHexString + ", but should have been : 0x" + sub_expected.toHexString);
          failed = true;
        }
      }
    }

    if(failed)
      fail();
    else{
      println(Console.GREEN + "passed memory test" + Console.RESET)
    }
  }

  
/*
  "SPI read test software" should "pass" in {
    test(new SPI(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println(Console.GREEN + "testing SPI read test software" + Console.RESET)

      val software_memory_sim = new Software_Memory_Sim(dut,
                                                        dut.io.SPI_interface.CE,
                                                        dut.io.SPI_interface.MOSI, 
                                                        dut.io.SPI_interface.MISO,
                                                        dut.io.SPI_interface.S_CLK, fail);

      dut.clock.setTimeout(11000);
      software_memory_sim.step(500);//wait for startup
      dut.io.SPI_interface.CE.expect(true.B)

      software_memory_sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.ReadEnable.poke(true.B);
      software_memory_sim.step();
      dut.io.ReadEnable.poke(false.B);

      while(dut.io.DataValid.peek().litToBoolean == false){
        software_memory_sim.step();
      }

      software_memory_sim.step(100);

      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.ReadEnable.poke(true.B);
      software_memory_sim.step();
      dut.io.ReadEnable.poke(false.B);

      while(dut.io.DataValid.peek().litToBoolean == false){
        software_memory_sim.step();
      }

      software_memory_sim.step(20);

    }
  }

  "SPI write test software" should "pass" in {
    test(new SPI(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

    println(Console.GREEN + "SPI write test software" + Console.RESET)

    val software_memory_sim = new Software_Memory_Sim(dut,
                                                      dut.io.SPI_interface.CE,
                                                      dut.io.SPI_interface.MOSI, 
                                                      dut.io.SPI_interface.MISO,
                                                      dut.io.SPI_interface.S_CLK, fail);

      dut.clock.setTimeout(11000);
      software_memory_sim.step(200);//wait for startup
      dut.io.SPI_interface.CE.expect(true.B)

      software_memory_sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.WriteEnable.poke(true.B);
      dut.io.WriteData(0).poke(0x0C0C.U);
      dut.io.WriteData(1).poke(0x0C0C.U);
      dut.io.WriteData(2).poke(0x0C0C.U);
      dut.io.WriteData(3).poke(0x0C0C.U);
      dut.io.ByteEnable.poke(0xFFFF.U);
      software_memory_sim.step();
      dut.io.WriteEnable.poke(false.B);

      while(dut.io.WriteCompleted.peek().litToBoolean == false){
        software_memory_sim.step();
      }

      software_memory_sim.step(100);

      software_memory_sim.step();
      dut.io.Address.poke(0x0000AC.U);
      dut.io.WriteEnable.poke(true.B);
      dut.io.WriteData(0).poke(0x1.U);
      dut.io.WriteData(1).poke(0xAA.U);
      dut.io.WriteData(2).poke(0xB1.U);
      dut.io.WriteData(3).poke(0x41.U);
      software_memory_sim.step();
      dut.io.WriteEnable.poke(false.B);

      while(dut.io.WriteCompleted.peek().litToBoolean == false){
        software_memory_sim.step();
      }


      test_memory(software_memory_sim, 0x0000AC, Array(0x1,0xAA,0xB1,0x41), fail);
      test_memory(software_memory_sim, 0x0F0F0F, Array(0xC0C,0xC0C,0xC0C,0xC0C), fail);

    }
  }

  "SPI write read test software" should "pass" in {
    test(new SPI(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println(Console.GREEN + "SPI write read test software" + Console.RESET)

      val software_memory_sim = new Software_Memory_Sim(dut,
        dut.io.SPI_interface.CE,
        dut.io.SPI_interface.MOSI,
        dut.io.SPI_interface.MISO,
        dut.io.SPI_interface.S_CLK, fail);

      dut.clock.setTimeout(11000);
      software_memory_sim.step(200);//wait for startup
      dut.io.SPI_interface.CE.expect(true.B)

      software_memory_sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.WriteEnable.poke(true.B);
      dut.io.WriteData(0).poke(0xAABBCCDD.U);
      dut.io.WriteData(1).poke(0x12345678.U);
      dut.io.WriteData(2).poke(0xFF00AD2A.U);
      dut.io.WriteData(3).poke(0xADFA1234.U);
      dut.io.ByteEnable.poke(0xFFFF.U);
      software_memory_sim.step();
      dut.io.WriteEnable.poke(false.B);

      while(dut.io.WriteCompleted.peek().litToBoolean == false){
        software_memory_sim.step();
      }

      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.ReadEnable.poke(true.B);
      software_memory_sim.step();
      dut.io.ReadEnable.poke(false.B);

      while(dut.io.DataValid.peek().litToBoolean == false){
        software_memory_sim.step();
      }

      dut.io.ReadData(0).expect(0xAABBCCDD.U);
      dut.io.ReadData(1).expect(0x12345678.U);
      dut.io.ReadData(2).expect(0xFF00AD2A.U);
      dut.io.ReadData(3).expect(0xADFA1234.U);

    }
  }

  "Read OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println(Console.GREEN + "Read OCP test software" + Console.RESET)

      dut.clock.setTimeout(11000);

      val software_memory_sim = new Software_Memory_Sim(dut,
                                                        dut.io.CE,
                                                        dut.io.MOSI,
                                                        dut.io.MISO,
                                                        dut.io.S_CLK,
                                                        fail);
      

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, software_memory_sim.step, fail);
      software_memory_sim.step(5000);

      ocp_tester.read_command(0x0000FF);
      software_memory_sim.step(10);
      ocp_tester.read_command(0x0F0F0F);
      //Software_Memory_Sim.step(100);
      //ocp_tester.read_command(23456);
      //ocp_tester.read_command(54321);
      //ocp_tester.read_command(23456);



    }
  }
*/

  "Write OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory(2,0x000F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println(Console.GREEN + "Write OCP test software" + Console.RESET)

      dut.clock.setTimeout(20000);

      val software_memory_sim = new Software_Memory_Sim(dut,
        dut.io.CE,
        dut.io.MOSI,
        dut.io.MISO,
        dut.io.S_CLK,
        fail);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, software_memory_sim.step, fail);
      software_memory_sim.step(500);

      //ocp_tester.write_command(141, Array(14, 1245, 114, 124), Array(0xF, 0xF, 0xF, 0xF));
      //ocp_tester.write_command(115161, Array(43451, 1355, 12355, 12512), Array(0xF, 0x0, 0x0, 0xF));
      //ocp_tester.write_command(0, Array(1, 2, 3, 4), Array(0x0, 0xF, 0xF, 0x0));
      ocp_tester.write_command(0x0F0F0F0F, Array(0xAA, 0xAA, 0xAA, 0xAA), Array(0xF, 0xF, 0xF, 0xF));

      test_memory(software_memory_sim, 141, Array(14, 1245, 114, 124), fail);
      test_memory(software_memory_sim, 115161, Array(43451, 0, 0, 12512), fail);
      test_memory(software_memory_sim, 0, Array(0, 2, 3, 0), fail);

    }
  }

  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory(2, 0x000F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      println(Console.GREEN + "Write read test software" + Console.RESET)

      dut.clock.setTimeout(200000);

      val software_memory_sim = new Software_Memory_Sim(dut,
        dut.io.CE,
        dut.io.MOSI,
        dut.io.MISO,
        dut.io.S_CLK,
        fail);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, software_memory_sim.step, fail);
      software_memory_sim.step(200);

      val ran = new scala.util.Random(System.currentTimeMillis());

      for(x <- 0 to 10){
        val my_data : Array[BigInt] = Array(ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE));
        val my_address = ran.nextInt(0xFFFFFF);

        ocp_tester.write_command(my_address, my_data, Array(0xF, 0xF, 0xF, 0xF));

        software_memory_sim.step(100);

        val read_data = ocp_tester.read_command(my_address);

        for(x <- 0 to read_data.length){
          if(my_data == read_data) {
            println("passed read write test")
          }
          else{
            println(Console.RED + "The read value was not the same as the written value. \n Written value: " + my_data(0) + ", " + my_data(1) + ", " + my_data(2) + ", " + my_data(3)
              + " \nRead value was: " + read_data(0) + ", " + read_data(1) + ", " + read_data(2) + ", " + read_data(3) + Console.RESET);
            fail();
          }
        }
      }
    }
  }


}

