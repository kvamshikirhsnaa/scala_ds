package part1

object Eval extends App {

  def eval(expr: String): Int = {

    val operators = Set("+", "-", "*", "/")

    def getNumbers = expr.split(" ").filter(x => !operators(x)).map(_.toInt).toList
    def getOperators = expr.split(" ").filter(operators).toList

    def simpleOperation(op1: Int, op2: Int, operator: String) = operator match {
      case "+" => op1 + op2
      case "-" => op1 - op2
      case "*" => op1 * op2
      case "/" => op1 / op2
      case _ => throw new IllegalArgumentException(s"Illegal operator, $operator")
    }

    def priority(operator: String) = operator match {
      case "+" | "-" => 1
      case "*" | "/" => 2
      case _ => 0
    }

    def evalTailrec(
                   remainingOperands: List[Int],
                   remainingOperators: List[String],
                   operandsStack: List[Int],
                   operatorsStack: List[String]
                   ): Int = {

      if (remainingOperands.isEmpty) {
        if (operatorsStack.isEmpty) operandsStack.head
        else {
          val op2 = operandsStack.head
          val op1 = operandsStack.tail.head
          val operator = operatorsStack.head
          val result = simpleOperation(op1, op2, operator)
          evalTailrec(remainingOperands, remainingOperators, result :: operandsStack.drop(2), operatorsStack.tail)
        }
      }
      else if (remainingOperands.length > remainingOperators.length) {
        evalTailrec(remainingOperands.tail, remainingOperators, remainingOperands.head :: operandsStack, operatorsStack)
      }
      else if (operatorsStack.isEmpty || priority(remainingOperators.head) > priority(operatorsStack.head)) {
        evalTailrec(remainingOperands, remainingOperators.tail, operandsStack, remainingOperators.head :: operatorsStack)
      } else {
        val op2 = operandsStack.head
        val op1 = operandsStack.tail.head
        val operator = operatorsStack.head
        val result = simpleOperation(op1, op2, operator)
        evalTailrec(remainingOperands, remainingOperators, result :: operandsStack.drop(2), operatorsStack.tail)
      }
    }
    evalTailrec(getNumbers, getOperators, List(), List())
  }

  println(eval("1 + 2 + 3"))
  println(eval("1 + 2 * 3"))
  println(eval("1 * 2 + 3"))
  println(eval("1 - 2 + 3"))
  println(eval("1 - 2 * 3"))
  println(eval("1 * 2 - 3"))




}
