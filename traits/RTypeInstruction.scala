/** Trait to strap away boiler plate from defining own instructions. */
trait RTypeInstruction { self: BaseInstruction =>

  val (opcode, funct3, funct7) = BaseInstruction.requestEncoding(true, true)

  override def instructionPattern: String = f"${funct7}----------${funct3}-----${opcode}"

  def operands = List(
    if (usesDestination) List("_rd") else Nil,
    if (usesFirstOperand) List("_rs1") else Nil,
    if (usesSecondOperand) List("_rs2") else Nil
  ).flatten.mkString(",")
}
