package SPI_memory

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import ocp.{OcpBurstMasterSignals, OcpBurstSlaveSignals, OcpCmd, OcpResp}

import scala.math.{BigInt, pow}

object STATE extends Enumeration {
  type STATE = Value
  val NULL, RESET_ENABLE, RESET, READ, WRITE = Value

}

class Memory_helper_functions {

  var last_clock = false
  var last_clock_2 = false

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
    val last = last_clock_2
    last_clock_2 = b

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

  val SIZE : Int = pow(2.0, 24.0).toInt;
  var memory = new Array[Byte](SIZE)

  var transmitData = 0
  var write_enable = false;

  def write_miso() = {
    if(funcs.falling_edge(S_CLK.peek().litToBoolean) && CE.peek().litToBoolean == false) {
      val d : Boolean = ((transmitData >> (7-bits_read)) & 0x1) == 1;
      MISO.poke(d.B);
      //val byte = transmitData.asUInt
      //val bit = byte(0.U)

      if(transmitData != 0 && false){ //remove false to debug
        println(Console.MAGENTA + "bit in write_miso was = " + d.toString + Console.RESET)
        println(Console.YELLOW + "transmitData in write_miso was = " + transmitData.toString + Console.RESET)
        println(Console.BLUE + "bits_read in write_miso was = " + bits_read.toString + Console.RESET)
      }
    }
  }

  def handle_byte(b : Int): Unit = {
    write_enable = false;
    if (state == STATE.NULL || state == STATE.RESET_ENABLE){
      transmitData = 0
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
      transmitData = 0
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
      write_enable = true;
      //println(Console.MAGENTA + "In read state")
      //println(Console.MAGENTA + "read state" + Console.RESET)
      if(address_bytes_read < 3){
        address = address << 8;
        address = address + b;
        //println(Console.MAGENTA + "Read address: " + address.toHexString + ", while bytes was: " + b.toHexString + Console.RESET)
        address_bytes_read += 1;
      }
      if (address_bytes_read == 3){
        //println(Console.MAGENTA + "address: " + address + Console.RESET)
        transmitData = memory(address)
        //println(Console.MAGENTA + "transmit data is = " + transmitData + Console.RESET)
        address += 1
        data_bytes_read += 1;
      }

      if(address > scala.math.pow(2,8*address_bytes_read))
        println(Console.RED + "address is over 24 bits!?!?! " + Console.RESET)
    }
    else if(state == STATE.WRITE){
      transmitData = 0
      if(address_bytes_read < 3){
        address = address << 8;
        address = address + b;
        //println(Console.MAGENTA + "Write address: " + address.toHexString + ", while bytes was: " + b.toHexString + Console.RESET)
        address_bytes_read += 1;
      }
      else {
        memory(address) = b.toByte;
        //println(Console.BLUE + "new data written to memory at address: 0x" + address.toHexString + ", data is: 0x" + b.toHexString + Console.RESET)
        address = address + 1;
      }
    }
  }

  def step (n : Int = 1) : Unit = {
    for( a <- 0 to n-1) {

      m.clock.step();
      write_miso();

      if(CE.peek().litValue() == 1){
        if (state == STATE.NULL || state == STATE.RESET_ENABLE){

        }
        else{
          state = STATE.RESET;
          address_bytes_read = 0;
          data_bytes_read = 0;
          address = 0;
          //println(Console.YELLOW + "RESET" + Console.RESET);
        }
      }

      if(funcs.rising_edge(S_CLK.peek().litToBoolean)){

        if(CE.peek().litValue() == 1){
          if(bits_read != 0){
            println(Console.RED + "#CE must be hold high for the entire operation, minimum 8 bits at a time, was was pulled high "
              + (bits_read + 1) + " bits in" + Console.RESET);
            fail_callback()
          }
        }

        if(CE.peek().litValue() == 0 && m.reset.peek().litValue() == 0){
          CE.expect(false.B);

          //We are ready to recive data
          val MOSI_val : Boolean = MOSI.peek().litToBoolean;

          //println(Console.BLUE + "Chip clock index " + (clock_cycle + 1) + ", MOSI was: " + MOSI_val + Console.RESET);
          clock_cycle = clock_cycle + 1;

          in_bits(bits_read) = MOSI_val;
          bits_read = bits_read + 1;
          if(bits_read >= 8){
            bits_read = 0;
            val in_val = funcs.bitsToInt(in_bits);
            //println(Console.BLUE + "in_val was: 0x" + in_val.toHexString + Console.RESET)
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
