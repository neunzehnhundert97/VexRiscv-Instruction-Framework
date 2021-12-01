/** Trait to strap away boiler plate from defining own instructions.
  * M stands for "more", as it is a I-Type with two operands
  */
trait MTypeInstruction { self: BaseInstruction =>

  val (opcode, funct3, funct7) = BaseInstruction.requestEncoding(true, false)

  def operands = List(
    if (usesDestination) List("_rd") else Nil,
    if (usesThirdOperand) List("_rd") else Nil,
    if (usesAlterantiveOperand) List("_rd") else Nil,
    if (usesFirstOperand) List("_rs1") else Nil,
    if (usesSecondOperand) List("_rs2") else Nil,
    List("_imm")
  ).flatten.mkString(",")

  override def instructionPattern: String = f"-----------------${funct3}-----${opcode}"
}
