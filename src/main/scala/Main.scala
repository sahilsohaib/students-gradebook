import scala.collection.mutable.{Map, ListBuffer}

case class Student(name: String, scores: List[Int])

object GradebookManager {
  def main(args: Array[String]): Unit = {
    val gradebook = Map[String, ListBuffer[Int]]()

    while (true) {
      println("\nGradebook Menu:")
      println("1. Add Student Record")
      println("2. Calculate Average Grade")
      println("3. List All Students")
      println("4. Quit")
      print("Enter your choice: ")

      val choice = scala.io.StdIn.readInt()

      choice match {
        case 1 =>
          // Add Student Record
          addStudentRecord(gradebook)
        case 2 =>
          // Calculate Average Grade
          calculateAverageGrade(gradebook)
        case 3 =>
          // List All Students
          listAllStudents(gradebook)
        case 4 =>
          // Quit
          println("Exiting the Gradebook Manager.")
          sys.exit(0)
        case _ =>
          println("Invalid choice. Please try again.")
      }
    }
  }

  def addStudentRecord(gradebook: Map[String, ListBuffer[Int]]): Unit = {
    print("Enter student name: ")
    val name = scala.io.StdIn.readLine()

    val scores = ListBuffer[Int]()
    while (true) {
      print("Enter a test score (or 'q' to finish): ")
      val input = scala.io.StdIn.readLine()
      if (input == "q") {
        if (scores.isEmpty) {
          println("No scores entered. Student record not added.")
        } else {
          gradebook(name) = scores
          println(s"Student '$name' record added.")
        }
        return
      }
      val score = try {
        input.toInt
      } catch {
        case _: NumberFormatException => -1
      }
      if (score >= 0 && score <= 100) {
        scores += score
      } else {
        println("Invalid score. Please enter a score between 0 and 100.")
      }
    }
  }

  def calculateAverageGrade(gradebook: Map[String, ListBuffer[Int]]): Unit = {
    print("Enter student name to calculate average grade: ")
    val name = scala.io.StdIn.readLine()
    gradebook.get(name) match {
      case Some(scores) if scores.nonEmpty =>
        val averageGrade = scores.sum.toDouble / scores.length
        println(s"Average grade for '$name' is: $averageGrade")
      case Some(_) => println(s"No scores found for student '$name'.")
      case None => println(s"Student '$name' not found in the gradebook.")
    }
  }

  def listAllStudents(gradebook: Map[String, ListBuffer[Int]]): Unit = {
    if (gradebook.isEmpty) {
      println("The gradebook is empty.")
    } else {
      println("Student Records:")
      for ((name, scores) <- gradebook) {
        val averageGrade = if (scores.nonEmpty) scores.sum.toDouble / scores.length else 0.0
        println(s"Student: $name, Scores: ${scores.mkString(", ")}, Average Grade: $averageGrade")
      }
    }
  }
}
