import scala.io.StdIn.readLine

object Quec02 {

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be positive and not exceed total possible marks."))
    } else {
      (true, None)
    }
  }

  // Function to get student info with retry until valid input is provided
  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var validInput = false
    var studentInfo: (String, Int, Int, Double, Char) = null

    while (!validInput) {
      val name = readLine("Enter student name: ")
      val marks = readLine("Enter marks obtained: ").toInt
      val totalMarks = readLine("Enter total possible marks: ").toInt

      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      if (isValid) {
        studentInfo = getStudentInfo(name, marks, totalMarks)
        validInput = true
      } else {
        println(s"Invalid input: ${errorMessage.get}")
      }
    }

    studentInfo
  }

  // Function to get student info
  def getStudentInfo(name: String, marks: Int, totalMarks: Int): (String, Int, Int, Double, Char) = {
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (name, marks, totalMarks, percentage, grade)
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    println(s"Student Name: ${record._1}")
    println(s"Marks Obtained: ${record._2}")
    println(s"Total Possible Marks: ${record._3}")
    println(s"Percentage: ${record._4} %")
    println(s"Grade: ${record._5}")
  }

  // Main method to run the application
  def main(args: Array[String]): Unit = {
    val studentRecord = getStudentInfoWithRetry()
    printStudentRecord(studentRecord)
  }
}


