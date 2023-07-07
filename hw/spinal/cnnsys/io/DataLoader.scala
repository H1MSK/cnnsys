//package cnnsys.io
//
//import cnnsys.config.UnitConfig
//import spinal.core._
//import spinal.lib._
//import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.amba4.axilite._
//import spinal.lib.bus.bram._
//import spinal.lib.bus.misc.SizeMapping
//import spinal.lib.bus.regif.{AccessType, AxiLite4BusInterface}
//import spinal.lib.fsm.{EntryPoint, State, StateMachine}
//
//import scala.language.postfixOps
//
//class DataLoader(config: UnitConfig) extends Component {
//  val io = new Bundle {
//    val control_bus = slave(AxiLite4(config.axiLite4Config))
//    val ddr_bus = master(Axi4(config.axi4Config))
//
//    val core_in_bram_io = Array.fill(4)(BRAM(BRAMConfig(
//      addressWidth = config.coreInBramAddrWidth,
//      dataWidth = config.coreInDataWidth
//    )))
//    val core_out_bram_io = Array.fill(4)(BRAM(BRAMConfig(
//      addressWidth = config.coreOutBramAddrWidth,
//      dataWidth = config.coreOutDataWidth
//    )))
//
//    val rd_finish = out Bool()
//  }
//
//  val bus_if = AxiLite4BusInterface(io.control_bus, SizeMapping(
//    base = 0x0000,
//    size = 1 KiB
//  ))
//
//  val reg_ctrl_rd = bus_if.newReg(doc = "Read control")
//  val rd_start = reg_ctrl_rd.field(Bool(), AccessType.RW, 0, doc = "Write 1 to start read task")
//  val rd_idle = reg_ctrl_rd.field(Bool(), AccessType.RO, 0, doc = "1 if read task is idle")
//  val rd_done = reg_ctrl_rd.field(Bool(), AccessType.RO, 0, doc = "1 when read task is done")
//
//  val reg_ctrl_wr = bus_if.newReg(doc = "Read control")
//  val wr_start = reg_ctrl_wr.field(Bool(), AccessType.RW, 0, doc = "Write 1 to start read task")
//  val wr_idle = reg_ctrl_wr.field(Bool(), AccessType.RO, 0, doc = "1 if read task is idle")
//  val wr_done = reg_ctrl_wr.field(Bool(), AccessType.RO, 0, doc = "1 when read task is done")
//
//  val bus_reg_rd_size = bus_if.newRegAt(address = 0x10, doc = "Read task ddr data size")
//  val bus_rd_height = bus_reg_rd_size.field(UInt(10 bits), AccessType.RW, 0, doc = "Data height")
//  bus_reg_rd_size.reserved(1 bit)
//  val bus_rd_width = bus_reg_rd_size.field(UInt(10 bits), AccessType.RW, 0, doc = "Data width")
//  bus_reg_rd_size.reserved(1 bit)
//  val bus_rd_channel = bus_reg_rd_size.field(UInt(10 bits), AccessType.RW, 0, doc = "Data channel count")
//
//  val bus_reg_rd_ddr_addr = bus_if.newReg(doc = "Read task ddr address")
//  val bus_rd_ddr_addr = bus_reg_rd_ddr_addr.field(UInt(32 bits), AccessType.RW, 0, doc = "DDR base address for read task")
//
//  val bus_reg_rd_ddr_step = bus_if.newReg(doc = "Read task ddr data step")
//  // The line data in ddr should be contiguous
//  //  val bus_rd_ddr_hstep = bus_reg_rd_ddr_addr.field(UInt(16 bits), AccessType.RW, 0, doc = "DDR horizontal step")
//  bus_reg_rd_ddr_step.reserved(16 bits)
//  val bus_rd_ddr_vstep = bus_reg_rd_ddr_step.field(UInt(16 bits), AccessType.RW, 0, doc = "DDR vertical step")
//
//  val bus_reg_rd_ddr_conf = bus_if.newReg(doc = "Read task ddr data config")
//  bus_reg_rd_ddr_conf.reserved(32 bits)
//
//  val bus_reg_rd_bram_addr = bus_if.newRegAt(address = 0x20, doc = "Read task bram address")
//  val bus_rd_bram_addr = bus_reg_rd_bram_addr.field(UInt(config.coreInBramAddrWidth bits), AccessType.RW, 0, doc = "bram base address for read task")
//
//  val bus_reg_rd_bram_step = bus_if.newReg(doc = "Read task bram step config")
//  val bus_rd_bram_hstep = bus_reg_rd_bram_addr.field(UInt(16 bits), AccessType.RW, 0, doc = "bram horizontal step")
//  val bus_rd_bram_vstep = bus_reg_rd_bram_addr.field(UInt(16 bits), AccessType.RW, 0, doc = "bram vertical step")
//  val bus_reg_rd_bram_conf = bus_if.newReg(doc = "Read task bram data config")
//  bus_reg_rd_bram_conf.reserved(32 bits)
//
//  val rd_fsm = new StateMachine {
//    val width = Reg(UInt(10 bits)) init(0)
//    val channel = Reg(UInt(10 bits)) init(0)
//    val ddr_addr = Reg(UInt(32 bits)) init(0)
//    val ddr_hstep = Reg(UInt(16 bits)) init(0)
//    val ddr_vstep = Reg(UInt(16 bits)) init(0)
//
//    val bram_addr = Reg(UInt(config.coreInBramAddrWidth bits)) init (0)
//    val bram_hstep = Reg(UInt(16 bits)) init(0)
//    val bram_vstep = Reg(UInt(16 bits)) init(0)
//
//    val line_left = height
//    val cur_line_word_left = Reg(UInt(16 bits)) init(0)
//
//    val ar = io.ddr_bus.ar
//    val r = io.ddr_bus.r
//
//    val counter_h = Reg(UInt(16 bits)) init(0)
//    val counter_v = Reg(UInt(16 bits)) init(0)
//
//    ar.valid := 0
//
//    r.ready := 0
//
//    ar.setBurstINCR()
//    ar.setCache(B"'b1111")
//    ar.setProt(B"'b110")
//
//    val state_idle : State = new State with EntryPoint {
//      onEntry {
//        rd_idle := true
//      }
//      whenIsActive {
//        rd_done := false
//        when (rd_start) {
//          width := bus_rd_width
//          line_left := bus_rd_height
//          channel := bus_rd_channel
//          ddr_addr := bus_rd_ddr_addr
//          // ddr_hstep := bus_rd_ddr_hstep
//          ddr_hstep := config.axi4Config.dataWidth / 8
//          ddr_vstep := bus_rd_ddr_vstep
//          bram_addr := bus_rd_bram_addr
//          bram_hstep := bus_rd_bram_hstep
//          bram_vstep := bus_rd_bram_vstep
//          goto(state_initiating_line_read)
//        }
//      }
//      onExit {
//        rd_idle := false
//        rd_start := false
//        counter_h := 0
//        counter_v := 0
//      }
//    }
//
//    val state_initiating_line_read : State = new State {
//      whenIsActive {
//        assert(ddr_hstep == U(config.axi4Config.dataWidth / 8))
//        ar.valid := true
//        when(ar.ready) {
//          goto(state_transfering)
//        }
//      }
//      onExit {
//        ar.valid := false
//      }
//    }
//
//    val state_transfering : State = new State {
//      onEntry {
//        r.ready := true
//      }
//      onExit {
//        r.ready := false
//      }
//      whenIsActive {
//        when (r.valid) {
//          r.payload.data
//        }
//      }
//    }
//  }
//}
