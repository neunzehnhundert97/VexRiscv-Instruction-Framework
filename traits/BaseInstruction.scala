import spinal.core._

import vexriscv.plugin.Plugin
import vexriscv.{VexRiscv, Stageable, DecoderService, VexRiscvConfig, Stage}

import RegisterAccess._

abstract class BaseInstruction(
  val rs1: RegisterAccess = Unused,
  val rs2: RegisterAccess = Unused,
  val rs3: RegisterAccess = Unused,
  val rsAlt: RegisterAccess = Unused,
  val rd: RegisterAccess = Unused,
  val memRead: SpinalEnumElement[MEM_USAGE.type] = MEM_USAGE.Unused,
  val memWrite: SpinalEnumElement[MEM_USAGE.type] = MEM_USAGE.Unused,
  val bypass: Bypass = Bypass.Managed,
  val destIsFirstOperand: Boolean = false,
  shortName: String = "",
  val macroValues: Map[String, String] = Map()
) extends Plugin[VexRiscv]
    with spinal.core.Nameable {

  /** The pattern for the instruction decoding. */
  def instructionPattern: String

  /** Mnemonic of this instruction, defaults to class name. */
  def mnemonic: String = if (shortName != "") shortName else name.replace("Instruction", "")

  /** Signal for the activation of this instruction. */
  object INSTRUCTION_ACTIVE extends Stageable(Bool)

  // Overwrite name of stageable to get a individual name by class
  INSTRUCTION_ACTIVE.setWeakName(this.getClass.getName.replace("$", "").replace(".", "") + "_IS_ACTIVE")

  /** A comma separeted list of operands for macro defintion. */
  def operands: String

  /** Returns if this instruction is using the destination according to standard encoding. */
  def usesDestination: Boolean =
    rd != Unused

  /** Returns if this instruction is using the operand RS1 according to standard encoding. */
  def usesFirstOperand: Boolean =
    rs1 != Unused

  /** Returns if this instruction is using the operand RS2 according to standard encoding. */
  def usesSecondOperand: Boolean =
    rs2 != Unused

  /** Stageable that should be signaled by the decoder. */
  def additionalStageables(pipeline: VexRiscv): Seq[(Stageable[_ <: BaseType], Any)] = Nil

  /** Register this instruction at the decoder service. */
  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    //Retrieve the DecoderService instance
    val decoderService: DecoderService = pipeline.service(classOf[DecoderService])

    // Set the default value of the activation signal to be false
    decoderService.addDefault(INSTRUCTION_ACTIVE, False)

    val more = additionalStageables(pipeline)

    // Determine, if this implementation is actually used for the instruction's implementation
    val isManaged = areas.any

    // Specify the registration to the service
    decoderService.add(
      // Define encoding pattern
      key = MaskedLiteral(instructionPattern),
      // Signals to set when the pattern is encountered
      List(
        INSTRUCTION_ACTIVE -> True,
        //Enable the register file write
        rd match {
          case Unused         => REGFILE_WRITE_VALID   -> False
          case Regular        => REGFILE_WRITE_VALID   -> True
        },
        // Signal the hazard management which registers are used
        rs1 match {
          case Unused         => RS1_USE   -> False
          case Regular        => RS1_USE   -> True
        },
        rs2 match {
          case Unused         => RS2_USE   -> False
          case Regular        => RS2_USE   -> True
        }
      ) ++ (((memRead, memWrite) match {
        // Set signal for memory access
        case (MEM_USAGE.Unused, MEM_USAGE.Unused) => Nil
        case (MEM_USAGE.Unused, usage) =>
          List(MEMORY_ENABLE -> usage, MEMORY_WR -> usage, MEMORY_AMO -> False) ++
            (if (more.exists(_._1 == OVERWRITE_SIZE_ORDER)) Nil
             else List(OVERWRITE_SIZE_ORDER -> OVERWRITE_SIZE.WORD))

        case (usageRead, usageWrite) =>
          List(MEMORY_ENABLE -> usageRead, MEMORY_WR -> usageWrite, MEMORY_AMO -> False) ++
            (if (more.exists(_._1 == OVERWRITE_SIZE_ORDER)) Nil
             else List(OVERWRITE_SIZE_ORDER -> OVERWRITE_SIZE.WORD))
        case _ => Nil
      })) // Set bypass signals
        ++ (if (!isManaged || bypass != Bypass.Managed) bypass match {
              case Bypass.NoBypass | Bypass.Managed => List(BYPASSABLE_EXECUTE_STAGE -> False, BYPASSABLE_MEMORY_STAGE -> False)
              case Bypass.AfterExecute              => List(BYPASSABLE_EXECUTE_STAGE -> True, BYPASSABLE_MEMORY_STAGE -> True)
              case Bypass.AfterMemory               => List(BYPASSABLE_EXECUTE_STAGE -> False, BYPASSABLE_MEMORY_STAGE -> True)
            }
            else
              List(
                /// Result is already accessible in the EXECUTE stage (Bypass ready)
                BYPASSABLE_EXECUTE_STAGE -> Bool(areas.writeBack.isEmpty && areas.memory.isEmpty),
                // Result is already accessible in the MEMORY stage (Bypass ready)
                BYPASSABLE_MEMORY_STAGE -> Bool(areas.writeBack.isEmpty)
              )) ++ more
    )
  }

  private val areas = new Areas()

  /** The actions to happen during decoding stage. */
  def decode(func: Ctx => Unit): Unit =
    areas.decode = Some(ctx => new Area { func(ctx) })

  /** The actions to happen during execution stage. */
  def execute(func: Ctx => Unit): Unit =
    areas.execute = Some(ctx => new Area { func(ctx) })

  /** The actions to happen during memory stage. */
  def memory(func: Ctx => Unit): Unit =
    areas.memory = Some(ctx => new Area { func(ctx) })

  /** The actions to happen during write back stage. */
  def writeBack(func: Ctx => Unit): Unit =
    areas.writeBack = Some(ctx => new Area { func(ctx) })

  /** Returns the first operand as usable value. */
  protected def firstOperand(implicit ctx: Ctx): Bits = rs1 match {
    case Unused         => throw new Exception(s"Reading an undeclared operand in ${this.getClass.getSimpleName.replace("$", "")}")
    case Regular        => ctx.stage.input(ctx.config.RS1)
  }

  /** Returns the second operand as usable value. */
  protected def secondOperand(implicit ctx: Ctx): Bits = rs2 match {
    case Unused         => throw new Exception(s"Reading an undeclared operand in ${this.getClass.getSimpleName.replace("$", "")}")
    case Regular        => ctx.stage.input(ctx.config.RS2)
  }

  /** Returns the third operand as usable value. */
  protected def thirdOperand(implicit ctx: Ctx): Bits = rs3 match {
    case Unused         => throw new Exception(s"Reading an undeclared operand in ${this.getClass.getSimpleName.replace("$", "")}")
    case Regular        => throw new Exception(s"Reading an illegal third operand in ${this.getClass.getSimpleName.replace("$", "")}")
  }

  /** Returns the third operand as usable value. */
  protected def alternativeOperand(implicit ctx: Ctx): Bits = rsAlt match {
    case Unused   => throw new Exception(s"Reading an undeclared operand in ${this.getClass.getSimpleName.replace("$", "")}")
    case Regular  => ctx.stage.input(EnhancedRegPlugin.RSALT)
  }

  /** Returns the pre modification value that was read. */
  protected def rawReadMemory(implicit ctx: Ctx): Bits =
    MEMORY_READ_MOD.read

  /** Returns the pre modification value that will be written. */
  protected def rawStoreMemory(implicit ctx: Ctx): Bits =
    MEMORY_WRITE_MOD.read

  /** Returns the requested stageable as usable value. */
  protected def readSignal[A <: BitVector](value: Stageable[A])(implicit ctx: Ctx): A =
    ctx.stage.input(value)

  /** Sets the requested stageable to the given usable value. */
  protected def writeSignal[A <: BitVector](value: Stageable[A], otherValue: A)(implicit ctx: Ctx): Unit =
    ctx.stage.insert(value) := otherValue

  /** Writes the given value as instruction output. */
  protected def writeResult(value: BitVector)(implicit ctx: Ctx): Unit =
    when(ctx.stage.arbitration.isValid && ctx.stage.input(INSTRUCTION_ACTIVE)) {
      rd match {
        case Unused =>
          throw new Exception(s"Writing to a undeclared destination in ${this.getClass.getSimpleName.replace("$", "")}")
        case Regular        => ctx.stage.output(ctx.config.REGFILE_WRITE_DATA) := value.asBits
      }
    }

  /** Writes the given value as output for a modified load. */
  protected def writeLoadMemMod(value: BitVector)(implicit ctx: Ctx): Unit =
    when(ctx.stage.arbitration.isValid && ctx.stage.input(INSTRUCTION_ACTIVE)) {
      ctx.stage.output(MEMORY_READ_MODDED) := value.asBits
    }

  /** Writes the given value as input for a modified store. */
  protected def writeStoreMemMod(value: BitVector)(implicit ctx: Ctx): Unit =
    when(ctx.stage.arbitration.isValid && ctx.stage.input(INSTRUCTION_ACTIVE)) {
      ctx.stage.output(MEMORY_WRITE_MODDED) := value.asBits
    }

  /** Describe the instructions actions. */
  override def build(pipeline: VexRiscv): Unit = {

    // Plug execution area if defined
    areas.decode.foreach(func => pipeline.decode.plug(func(Ctx(pipeline.decode, pipeline.config))))
    areas.execute.foreach(func => pipeline.execute.plug(func(Ctx(pipeline.execute, pipeline.config))))
    areas.memory.foreach(func => pipeline.memory.plug(func(Ctx(pipeline.memory, pipeline.config))))
    areas.writeBack.foreach(func => pipeline.writeBack.plug(func(Ctx(pipeline.writeBack, pipeline.config))))
  }

  implicit class BetterStageable[A <: Data](stageable: Stageable[A]) {
    def :=(value: A)(implicit ctx: Ctx): Unit =
      ctx.stage.insert(stageable) := value

    def read(implicit ctx: Ctx): A =
      ctx.stage.input(stageable)
  }

  private class Areas(
    var decode: Option[Ctx => Area] = None,
    var execute: Option[Ctx => Area] = None,
    var memory: Option[Ctx => Area] = None,
    var writeBack: Option[Ctx => Area] = None
  ) {

    /** Returns, if any area is set. */
    def any: Boolean =
      decode.nonEmpty || execute.nonEmpty || memory.nonEmpty || writeBack.nonEmpty
  }
}

object BaseInstruction {

  private var avaiableEncodings = List(
    // custom-0
    "0001011",
    // custom-1
    "0101011",
    // custom-2/rv128
    "1011011",
    // custom-3/rv128
    "1111011"
  ).map(opcode =>
    opcode -> (for (funct3 <- (0 until 8).toList)
      yield funct3 -> (0 until 128).toList).toMap
  ).toMap

  /** Returns a tuple of opcode, funct3, and funct7. */
  def requestEncoding(needFunct3: Boolean, needFunct7: Boolean): (String, String, String) = {
    if (!needFunct7 && !needFunct3) // U-Type etc, not implemented
      throw new Exception("Only opcode instructions not implemented")
    else if (needFunct7 && !needFunct3)
      throw new Exception("Illegal combination")
    else if (needFunct3 && !needFunct7)
      // Find a free encoding without used funct7 space
      avaiableEncodings.toList.flatMap {
        case (op, funct3) => funct3.find(_._2.length == 128).map(f => op -> f._1)
      }.headOption match {
        case None                   => throw new Exception("Run out of funct3 space")
        case Some((opcode, funct3)) =>
          // Remove from mapping
          avaiableEncodings = avaiableEncodings.updated(opcode, avaiableEncodings(opcode).filter(_._1 != funct3))

          val bin = funct3.toBinaryString
          (opcode, ("0" * (3 - bin.length)) + bin, "")
      }
    // I-Type or U-Type
    else // R-Type
      avaiableEncodings.toList.flatMap {
        case (op, funct3) => funct3.find(_._2.nonEmpty).map(f => (op, f._1, f._2.head))
      }.headOption match {
        case None                           => throw new Exception("Run out of funct7 space")
        case Some((opcode, funct3, funct7)) =>
          // Remove from mapping
          avaiableEncodings =
            avaiableEncodings.updated(
              opcode,
              avaiableEncodings(opcode).updated(funct3, avaiableEncodings(opcode)(funct3).drop(1))
            )

          val bin7 = funct7.toBinaryString
          val bin3 = funct3.toBinaryString
          (opcode, ("0" * (3 - bin3.length)) + bin3, ("0" * (7 - bin7.length)) + bin7)
      }
  }
}

final case class Ctx(stage: Stage, config: VexRiscvConfig)

sealed trait RegisterAccess {
  override def toString: String = this match {
    case Regular        => "32"
    case Unused         => "-"
  }
}
object RegisterAccess {
  case object Unused         extends RegisterAccess
  case object Regular        extends RegisterAccess
}

sealed trait Bypass
object Bypass {
  case object Managed      extends Bypass
  case object NoBypass     extends Bypass
  case object AfterExecute extends Bypass
  case object AfterMemory  extends Bypass
}
