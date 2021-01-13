package expressions
object Expressions {
  def evaluate[A](expression: String, converter: String => A, operatorTable: Map[String, (A, A) => A], order: List[List[String]]): A = {
    var PresidenceMap: Map[String, Int] = Map()
    var tokenExp: String = ""
    val stack = new Stack[String]
    val stackEvaluation = new Stack[String]
    val queue = new Queue[String]
    var expressionLst: Array[String]= Array()
    tokenExp = expression.replaceAll(" ", "")
    for(i <- order){
      for(s <- i){
        PresidenceMap += (s ->  order.indexOf(i))
      }
    }
    for(i<- operatorTable.keys){
      tokenExp = tokenExp.replace(i, "_"+i+"_")
    }
    tokenExp = tokenExp.replace("(", "_(_")
    tokenExp = tokenExp.replace(")", "_)_")
    expressionLst = tokenExp.split("_")
    for(i <- expressionLst){
      if(operatorTable.contains(i)){
        if(stack.top != null){
          if (operatorTable.contains(stack.top.value)) {
            while(stack.top != null&&stack.top.value != "("&&stack.top.value != "("&&PresidenceMap(stack.top.value) <= PresidenceMap(i)) {
              queue.enqueue(stack.top.value)
              stack.pop()
            }
            stack.push(i)
          }
          else{stack.push(i)}
        }
        else{stack.push(i)}
      }
      else if(i == "("){stack.push(i)}
      else if(i ==")"){
        while(stack.top.value != "(") {
          queue.enqueue(stack.top.value)
          stack.pop()
        }
        stack.pop()
      }
      else if(i == ""){}//(i.startsWith("0")|i.startsWith("1")|i.startsWith("2")|i.startsWith("3")|i.startsWith("4")|i.startsWith("5")|i.startsWith("6")|i.startsWith("7")|i.startsWith("8")|i.startsWith("9")|i.startsWith("true")|i.startsWith("false")){queue.enqueue(i)}
      else{queue.enqueue(i)}
    }
    while(stack.top != null ){
      queue.enqueue(stack.top.value)
      stack.pop()
    }
    while(queue.front != null){
      if(operatorTable.contains(queue.front.value)){
        var result: String = ""
        result = operatorTable(queue.front.value)(converter(stackEvaluation.top.next.value), converter(stackEvaluation.top.value)).toString
        stackEvaluation.pop()
        stackEvaluation.pop()
        stackEvaluation.push(result)
      }
      else{
        stackEvaluation.push(queue.front.value)
      }
      queue.dequeue()
    }
    converter(stackEvaluation.top.value)
  }
}