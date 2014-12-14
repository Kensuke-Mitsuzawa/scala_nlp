package ml_scala
import scala.io.Source
import scala.collection.mutable.Map

object lesson_zero {

  def main(args: Array[String]): Unit = {
    // 単語の出現頻度をカウントするだけ
    
    val file_path = "./lesson0/input.txt"
      val word_map = Map[String, Int]()
      
      for (line <- Source.fromFile(file_path).getLines) {
    	  println(line.length + " " + line)
    	  val ss = line.split(" ")
    	  for (word_item <- ss) {
    	    // println(word_item)
    	    if (word_map.get(word_item) == None){  //　この行は word_map.contains()でも可
    	    	word_map += (word_item -> 1)  
    	    } else {
    	      var word_freq = word_map(word_item)
    	      word_freq = word_freq + 1
    	      word_map(word_item) = word_freq
    	    } 
    	  } 
      }
    println(word_map)
  }
}