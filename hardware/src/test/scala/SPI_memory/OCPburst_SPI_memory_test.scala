package SPI_memory

import SPI_memory.OCPburst_SPI_memory
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import ocp._
import org.scalatest.flatspec.AnyFlatSpec
import treadle.WriteVcdAnnotation
import chisel3.experimental.chiselName
import scala.collection.mutable.Map

object STATE extends Enumeration {
  type STATE = Value
  val NULL, RESET_ENABLE, RESET, READ, WRITE = Value

}

class Memory_helper_functions {

  var last_clock = false

  def bitsToInt(bits : Array[Boolean]) : Int = {
    var amount : Int = 0;
    for(i <- bits){
      amount = amount << 1;
      if(i) {
        amount = amount + 1
      };
    }

    if(amount > 256){
      println(Console.RED + "ERROR, byte is over 256!!" + Console.RESET)
    }

    return amount;
  }
  def rising_edge(b : Boolean): Boolean ={

    val last = last_clock
    last_clock = b;

    if(b == false)
      return false;
    else{
      if(last == true)
        return false;
      else
        return true
    }
  }
  def falling_edge(b: Boolean): Boolean = {
    val last = last_clock
    last_clock = b;

    if(b == true)
      return false;
    else{
      if(last == false)
        return false;
      else
        return true
    }   
  }

}

class Software_Memory_Sim(m : Module, CE : Bool, MOSI : Bool, MISO : Bool, S_CLK : Bool, fail_callback: () => Unit) {

  var in_bits : Array[Boolean] = new Array[Boolean](8)
  var bits_read : Int = 0

  var state = STATE.NULL;
  var clock_cycle = 0;

  var funcs : Memory_helper_functions = new Memory_helper_functions();

  var address = 0;
  var address_bytes_read = 0;

  var data_bytes_read = 0;
  var bytes_recived_string = "";

  var memory : scala.collection.mutable.Map[Int, Byte] = Map[Int, Byte]()

  def write_miso(b: Byte): Boolean = {
    var j = 0
    var index = 7
    
    if(funcs.falling_edge(S_CLK.peek().litToBoolean)) {
      val byte = b.asUInt
      MISO.poke(byte(index))
      index = index - 1
    }
    if(index == 0) {
      return true
    }
    
    return false
  }


  def set_output(b : Byte): Unit ={

  }

  def handle_byte(b : Int): Unit = {

    if (state == STATE.NULL || state == STATE.RESET_ENABLE){
      if(b == 0x66)
        state = STATE.RESET_ENABLE
      else if(b == 0x99)
        state = STATE.RESET
      else {
        println(Console.RED + "invalid byte was sent, state was: NULL/RESET_ENABLE, while bytes recived was 0x" + b.toHexString + Console.RESET);
        fail_callback()
      };
    }
    else if(state == STATE.RESET){
      if(b == 0x03) //read state
        state = STATE.READ
      else if(b == 0x02) //write state
        state = STATE.WRITE
      else if(b == 0x66)
        state = STATE.RESET_ENABLE
      else if(b == 0x99)
        state = STATE.RESET
      else {
        println(Console.RED + "invalid byte was sent, state was: RESET, while bytes recived was 0x" + b.toHexString + Console.RESET);
        fail_callback()
      };
    }
    else if(state == STATE.READ){
      //println(Console.MAGENTA + "read state" + Console.RESET)
      if(address_bytes_read < 3){
        address = address << 8;
        address = address + b;
        println(Console.MAGENTA + "address: " + address.toHexString + ", while bytes was: " + b.toHexString + Console.RESET)
        address_bytes_read += 1;
      }
      else{
        //println(Console.MAGENTA + "address: " + address + Console.RESET)
        if(!memory.contains(address)) {
          memory.put(address, 0)
        }

        if(write_miso(memory(address))) {
          address = address + 1
        }

        data_bytes_read += 1;     
      }

      if(address > scala.math.pow(2,8*address_bytes_read))
        println(Console.RED + "address is over 24 bits!?!?! " + Console.RESET)
    }
    else if(state == STATE.WRITE){
      if(address_bytes_read < 3){
        address = address << 8;
        address = address + b;
        println(Console.MAGENTA + "address: " + address.toHexString + ", while bytes was: " + b.toHexString + Console.RESET)
        address_bytes_read += 1;
      }
      else {
        memory.put(address, b.toByte);
        address = address + 1
      }
    }

  }

  def step (n : Int = 1) : Unit = {


    for( a <- 0 to n-1){

      m.clock.step();

      if(funcs.rising_edge(S_CLK.peek().litToBoolean)){

        if(CE.peek().litValue() == 0 && m.reset.peek().litValue() == 0){
          CE.expect(false.B);

          if(CE.peek().litValue() == 1){
            if(bits_read != 0){
              println(Console.RED + "#CE must be hold high for the entire operation, minimum 8 bits at a time, was was pulled high "
                + (bits_read + 1) + " bits in" + Console.RESET);
              fail_callback()
            }
          }

          //We are ready to recive data
          val MOSI_val : Boolean = MOSI.peek().litToBoolean;

          //println(Console.BLUE + "Chip clock index " + (clock_cycle + 1) + ", MOSI was: " + MOSI_val + Console.RESET);
          clock_cycle = clock_cycle + 1;

          in_bits(bits_read) = MOSI_val;
          bits_read = bits_read + 1;
          if(bits_read >= 8){
            bits_read = 0;
            val in_val = funcs.bitsToInt(in_bits);
            println(Console.BLUE + "in_val was: 0x" + in_val.toHexString + Console.RESET)
            bytes_recived_string += in_val.toHexString + "_"
            handle_byte(in_val);
          }
        }

        if(CE.peek().litValue() == 1){
          address = 0;
          address_bytes_read = 0;
          data_bytes_read = 0;
        }
      }
    }
  };

}

class OCP_master_commands(master : OcpBurstMasterSignals, slave : OcpBurstSlaveSignals, step: (Int) => Unit, fail_callback: () => Unit) {

  var funcs : Memory_helper_functions = new Memory_helper_functions();

  val r = new scala.util.Random(System.currentTimeMillis());

  def randomize_read_dont_cares(): Unit ={
    master.Data.poke(r.nextInt(Integer.MAX_VALUE).U)
    master.DataByteEn.poke(r.nextInt(Integer.MAX_VALUE).U)
  }

  def read_step(): Unit ={
    randomize_read_dont_cares();
    step(1);
  }

  def write_step(): Unit ={
    step(1);
  }

  def read_command(address : Int): Array[Int] ={

    var values = new Array[Int](4);

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    slave.Resp.expect(OcpResp.NULL);
    read_step();

    master.Cmd.poke(OcpCmd.RD)
    master.Addr.poke(address.U)
    slave.Resp.expect(OcpResp.NULL)
    read_step()

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    slave.Resp.expect(OcpResp.NULL);
    slave.CmdAccept.expect(true.B);
    read_step();


    while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue()) {
      if(slave.Resp.peek().litValue() == OcpResp.NULL.litValue()){
        read_step();
      }
      else if(slave.Resp.peek().litValue() == OcpResp.ERR.litValue()) {
        fail_callback()
      }
      else if(slave.Resp.peek().litValue() == OcpResp.FAIL.litValue()){
        fail_callback()
      }
      else {
        fail_callback()
      }
    };

    slave.Resp.expect(OcpResp.DVA) //1
    values(0) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //2
    values(1) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //3
    values(2) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //4
    values(3) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.NULL) //5

    return values
  }

  def write_command(address : Int, data : Array[BigInt], byte_en : Array[Int]): Unit ={

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataByteEn.poke(0x0.U)
    master.DataValid.poke(0.U)
    slave.Resp.expect(OcpResp.NULL)
    master.DataValid.poke(false.B);
    write_step()

    master.Cmd.poke(OcpCmd.WR)
    master.Addr.poke(address.U)
    master.Data.poke(data(0).U)
    master.DataByteEn.poke(byte_en(0).U);
    master.DataValid.poke(1.U)
    slave.Resp.expect(OcpResp.NULL)
    master.DataValid.poke(true.B);
    slave.DataAccept.expect(false.B)

    while(slave.CmdAccept.peek().litValue() != 1){
      write_step()
      slave.Resp.expect(OcpResp.NULL)
    }

    slave.DataAccept.expect(true.B)
    slave.CmdAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(1).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(byte_en(1).U)
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(2).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(byte_en(2).U)
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(3).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(byte_en(3).U)
    write_step()

    while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue()){
      write_step()
    }

    slave.Resp.expect(OcpResp.DVA)
    slave.DataAccept.expect(false.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataValid.poke(false.B);
    master.DataByteEn.poke(0x0.U)
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(false.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataValid.poke(false.B);
    master.DataByteEn.poke(0x0.U)
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(false.B)

  }
}

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "SPI read test software" should "pass" in {
    test(new SPI(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      val Software_Memory_Sim = new Software_Memory_Sim(dut,
                                                        dut.io.SPI_interface.CE,
                                                        dut.io.SPI_interface.MOSI, 
                                                        dut.io.SPI_interface.MISO,
                                                        dut.io.SPI_interface.S_CLK, fail);

      dut.clock.setTimeout(11000);
      Software_Memory_Sim.step(500);//wait for startup
      dut.io.SPI_interface.CE.expect(true.B)

      Software_Memory_Sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.ReadEnable.poke(true.B);
      Software_Memory_Sim.step();
      dut.io.ReadEnable.poke(false.B);

      while(dut.io.DataValid.peek().litToBoolean == false){
        Software_Memory_Sim.step();
      }

      Software_Memory_Sim.step(100);

      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.ReadEnable.poke(true.B);
      Software_Memory_Sim.step();
      dut.io.ReadEnable.poke(false.B);

      while(dut.io.DataValid.peek().litToBoolean == false){
        Software_Memory_Sim.step();
      }

      Software_Memory_Sim.step(20);

    }
  }

  "SPI write test software" should "pass" in {
    test(new SPI(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

    val Software_Memory_Sim = new Software_Memory_Sim(dut,
                                                      dut.io.SPI_interface.CE,
                                                      dut.io.SPI_interface.MOSI, 
                                                      dut.io.SPI_interface.MISO,
                                                      dut.io.SPI_interface.S_CLK, fail);

      dut.clock.setTimeout(11000);
      Software_Memory_Sim.step(200);//wait for startup
      dut.io.SPI_interface.CE.expect(true.B)

      Software_Memory_Sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.WriteEnable.poke(true.B);
      dut.io.WriteData(0).poke(0xC0C.U);
      dut.io.WriteData(1).poke(0xC0C.U);
      dut.io.WriteData(2).poke(0xC0C.U);
      dut.io.WriteData(3).poke(0xC0C.U);
      dut.io.ByteEnable.poke(0xFFFF.U);
      Software_Memory_Sim.step();
      dut.io.WriteEnable.poke(false.B);

      while(dut.io.WriteCompleted.peek().litToBoolean == false){
        Software_Memory_Sim.step();
      }

      Software_Memory_Sim.step(100);

      Software_Memory_Sim.step();
      dut.io.Address.poke(0x0F0F0F.U);
      dut.io.WriteEnable.poke(true.B);
      dut.io.WriteData(0).poke(0xC0C.U);
      dut.io.WriteData(1).poke(0xC0C.U);
      dut.io.WriteData(2).poke(0xC0C.U);
      dut.io.WriteData(3).poke(0xC0C.U);
      Software_Memory_Sim.step();
      dut.io.WriteEnable.poke(false.B);

      while(dut.io.WriteCompleted.peek().litToBoolean == false){
        Software_Memory_Sim.step();
      }

    }
  }

  "Read OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory(2, 0x00F)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(11000);

      val Software_Memory_Sim = new Software_Memory_Sim(dut, 
                                                        dut.io.CE,
                                                        dut.io.MOSI,
                                                        dut.io.MISO,
                                                        dut.io.S_CLK,
                                                        fail);
      

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, Software_Memory_Sim.step, fail);
      Software_Memory_Sim.step(1000);

      ocp_tester.read_command(0x0000FF);
      Software_Memory_Sim.step(10);
      ocp_tester.read_command(0x0F0F0F);
      //Software_Memory_Sim.step(100);
      //ocp_tester.read_command(23456);
      //ocp_tester.read_command(54321);
      //ocp_tester.read_command(23456);

    }
  }

  "Write OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(20000);

      val Software_Memory_Sim = new Software_Memory_Sim(dut,
        dut.io.CE,
        dut.io.MOSI,
        dut.io.MISO,
        dut.io.S_CLK,
        fail);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, Software_Memory_Sim.step, fail);
      Software_Memory_Sim.step(500);

      ocp_tester.write_command(141, Array(14, 1245, 114, 124), Array(0xF, 0xF, 0xF, 0xF));
      ocp_tester.write_command(115161, Array(43451, 1355, 12355, 12512), Array(0xF, 0x0, 0x0, 0xF));
      ocp_tester.write_command(0, Array(1, 2, 3, 4), Array(0x0, 0xF, 0xF, 0x0));

    }
  }

  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(200000);

      val Software_Memory_Sim = new Software_Memory_Sim(dut,
        dut.io.CE,
        dut.io.MOSI,
        dut.io.MISO,
        dut.io.S_CLK,
        fail);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, Software_Memory_Sim.step, fail);
      Software_Memory_Sim.step(500);

      val ran = new scala.util.Random(System.currentTimeMillis());

      for(x <- 0 to 10){
        val my_data : Array[BigInt] = Array(ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE), ran.nextInt(Integer.MAX_VALUE));
        val my_address = ran.nextInt(0xFFFFFF);

        ocp_tester.write_command(my_address, my_data, Array(0xF, 0xF, 0xF, 0xF));

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

