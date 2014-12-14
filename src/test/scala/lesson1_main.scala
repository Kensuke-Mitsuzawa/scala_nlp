package ml_scala

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.math

// uni-gram言語モデルを作成するクラス
class TrainModel(file_path: String, out_path: String){
  	def CountWordFreq(file_path:String) = {
	  // 単語の頻度をカウントする
	  var total_count:Int = 0  // 全単語数を保存しておく  
	  val counts = collection.mutable.Map[String, Int]() // キーをstring　値をfloatにしてマップの生成 mutable Map 
	  
	  for (line <- Source.fromFile(file_path).getLines){
	    val array_of_words:Array[String] = line.split(" ")  // 文字分割
	    val list_of_words = array_of_words.toList  // Arrayは要素の変更不可なので、listにする
	    val list_of_words_plus = list_of_words :+ "</s>"  // listの末尾に新しい要素を追加して、新しいリストを生成する
	    
	    for (word <- list_of_words_plus){
	      total_count += 1
	      if (counts.get(word) == None){
	        counts += (word -> 1)
	      } else {
	        var word_freq:Int = counts(word)
	        word_freq += word_freq + 1
	        counts(word) = word_freq
	      }
	    }
	  }
	  val return_tuple = (counts, total_count)
	  return_tuple
	}
  
def ConstructProbMap(word_freq_map:Map[String, Int], total_count:Int) = {
  //  Unigram言語モデルを構築
  val word_prob_map = collection.mutable.Map[String, Float]()
  for ((key_word, freq) <- word_freq_map){
    // println("key:%s, value:%s".format(key_word, freq))
    val uni_prob = freq.toFloat / total_count.toFloat
    word_prob_map += key_word -> uni_prob  // keyが単語 valueがunigram確率
  }
  word_prob_map
}

def TrainMain():Unit = {
      
    val word_freq_tuple = CountWordFreq(file_path)  // 単語の頻度カウントの処理がかけた
    val word_freq_map = word_freq_tuple._1  // 頻度カウントしたマップ
    val total_count = word_freq_tuple._2  //　全単語数
	  val imm_word_freq_map = Map.empty ++ word_freq_map  // 関数にmutable mapを渡す方法がわからないので、一度immutable mapにしておく
    val word_prob_map = ConstructProbMap(imm_word_freq_map, total_count)


	  val outfile = new PrintWriter(new File(out_path))
    for ((key_word, uni_prob) <- word_prob_map) {
			outfile.write("%s\t%s\n".format(key_word, uni_prob))
    }
	outfile.close
}
}


class EvalModel(model_path: String, testfile_path: String){

	def LoadModel(model_path: String) = {
		// モデルファイルを読み込む
		val unigram_map = scala.collection.mutable.Map[String, Float]()

		val source_ins = Source.fromFile(model_path, "UTF-8")
		source_ins.getLines().foreach({
			line =>
				val result: Array[String] = line.split("\t")
				val key_string = result(0)
				val prob = result(1).toFloat
				unigram_map(key_string) = prob
		})
		unigram_map
	}


	def LoadTestFileIntoArray(testfile_path: String) = {
		// テストファイルを読み込んでArrayにする
		val word_list = scala.collection.mutable.ListBuffer[String]()
		val testfile_source = Source.fromFile(testfile_path, "UTF-8")
		testfile_source.getLines().foreach({
			line =>
				val result: Array[String] = line.split(" ")
				result.foreach({item: String => word_list += item})
		})
		word_list += "</s>"

		val list_word_list = word_list.toList
		list_word_list
	}

	def EvalUnigraModel(unigram_map: scala.collection.mutable.Map[String, Float],
											test_word_list: List[String]) = {
		// モデルの評価を行う

		// params
		val lambda_1 = 0.95
		var lambda_unk = 1 - lambda_1
		var V = 1000000
		var W = 0
		var H = 0.000
		var P = 0.000

		for(item <- test_word_list){
			W += 1
			P = lambda_unk / V
			if (unigram_map.contains(item)){
				P += lambda_1 * unigram_map(item)
			} else {
				lambda_unk += 1
			}
			H += scala.math.log(P)
		}
		val entropy = H/W
		val coverage = (W-lambda_unk)/W
		println(s"entropy=$entropy")
		println(s"coverage=$coverage")
	}


	def EvalMain(): Unit = {

		val unigram_map = LoadModel(model_path)
		val test_word_list = LoadTestFileIntoArray(testfile_path)
		// TODO
		// 評価部分の記述と内容理解
		// 評価部分のクラス化
		EvalUnigraModel(unigram_map, test_word_list)
	}

}


object lesson1_main {

  def main(args: Array[String]): Unit = {  // メインの定義

		val file_path:String = "src/test/resources/test/01-train-input.txt" // 相対パスの現在地点はプロジェクトのルートパス
    val out_path:String = "src/test/scala/unigram_model.tsv"
    val train_ins = new TrainModel(file_path, out_path)
    train_ins.TrainMain()
      
    val model_path = "src/test/scala/unigram_model.tsv"
    val testfile_path = "src/test/resources/test/01-test-input.txt"
		val eval_ins = new EvalModel(model_path, testfile_path)
		eval_ins.EvalMain()
	}
  
}