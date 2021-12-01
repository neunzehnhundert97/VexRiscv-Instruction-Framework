/** Trait to strap away boiler plate from defining own instructions. */
trait STypeInstruction { self: BaseInstruction =>

  val (opcode, funct3, _) = BaseInstruction.requestEncoding(true, false)

  override def instructionPattern: String = f"-----------------${funct3}-----${opcode}"

  def operands = List(
    if (usesFirstOperand) List("_rs1") else Nil,
    if (usesSecondOperand) List("_rs2") else Nil,
    List("_imm")
  ).flatten.mkString(",")
}
