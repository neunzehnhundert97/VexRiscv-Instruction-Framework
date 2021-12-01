import spinal.core._

/** Trait to strap away boiler plate from defining own instructions. */
trait ITypeInstruction { self: BaseInstruction =>

  val (opcode, funct3, _) = BaseInstruction.requestEncoding(true, false)

  override def instructionPattern: String = f"-----------------${funct3}-----${opcode}"

  def operands = List(
    if (usesDestination) List("_rd") else Nil,
    if (usesFirstOperand) List("_rs1") else Nil,
    List("_imm")
  ).flatten.mkString(",")

  protected def immediateValue(implicit ctx: Ctx): Bits =
    ctx.stage.input(ctx.config.INSTRUCTION)(31 downto 20)
}
