//// https://github.com/c-thaler/DmaUnit/blob/master/src/main/scala/DmaUnit.scala
//
//package dma_unit
//
//import scala.util.Random
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.amba4.axilite._
//import spinal.lib.fsm._
//
//import spinal.lib.eda.altera._
//
//object Axi4CoreConfig{
//  def getAxi4Config = Axi4Config(
//    addressWidth = 32,
//    dataWidth    = 128,
//    useId = false,
//    useRegion = false,
//    useLock = false,
//    useQos = false,
//    useSize = false
//  )
//}
//
//class DmaUnit extends Component {
//  val io = new Bundle {
//    val irq = out Bool
//    val axi_slave = slave(AxiLite4(addressWidth=4, dataWidth=32))
//    val axi_master = master(Axi4(Axi4CoreConfig.getAxi4Config))
//  }
//
//  val axiSlaveCtrl = new AxiLite4SlaveFactory(io.axi_slave)
//  val axiReader = AxiReader(Axi4CoreConfig.getAxi4Config, 24, 32, 4)
//  val axiWriter = AxiWriter(Axi4CoreConfig.getAxi4Config, 24, 32)
//  val read_job = axiReader.readJobFactory()
//  val read_job_valid = Reg(Bool) init(False)
//  val write_job = axiWriter.writeJobFactory()
//  val write_job_valid = Reg(Bool) init(False)
//  val irq = Reg(Bool) init(False)
//  val fire = Bool
//
//  io.irq := irq
//  read_job_valid := False
//  write_job_valid := False
//  fire := False
//
//  axiSlaveCtrl.read(B(0xdead), 0x0)
//  axiSlaveCtrl.driveAndRead(read_job.address,     0x4)
//  axiSlaveCtrl.driveAndRead(read_job.word_count,  0x8)
//  axiSlaveCtrl.driveAndRead(write_job.address,    0xc)
//  axiSlaveCtrl.onWrite(0xc) {
//    fire := True
//  }
//
//  write_job.word_count := read_job.word_count
//
//  io.axi_master << axiReader.io.axi_master
//  io.axi_master << axiWriter.io.axi_master
//
//  axiReader.io.read_job.valid := read_job_valid
//  axiReader.io.read_job.payload := read_job
//  axiWriter.io.write_job.valid := write_job_valid
//  axiWriter.io.write_job.payload := write_job
//  axiWriter.io.write_in << axiReader.io.read_out
//
//  val fsm = new StateMachine {
//
//    irq := False
//
//    val idle : State = new State with EntryPoint {
//      whenIsActive(
//        when(fire) {
//          read_job_valid := True
//          write_job_valid := True
//          goto(waitForWrite)
//        }
//      )
//    }
//
//    val waitForWrite : State = new State {
//      whenIsActive(
//        when(axiWriter.io.busy) {
//          goto(process)
//        }
//      )
//    }
//
//    val process : State = new State {
//      whenIsActive(
//        when(!axiReader.io.busy && !axiWriter.io.busy) {
//          goto(idle)
//        }
//      )
//      onExit(
//        irq := True
//      )
//    }
//  }
//}
//
//object DmaUnitQSysify {
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new DmaUnit).toplevel
//
//    toplevel.io.axi_master addTag(ClockDomainTag(toplevel.clockDomain))
//    toplevel.io.axi_slave addTag(ClockDomainTag(toplevel.clockDomain))
//    toplevel.io.irq addTag(InterruptSenderTag(toplevel.clockDomain))
//
//    println("Generating QSys tcl script for ...")
//    QSysify(toplevel)
//  }
//}