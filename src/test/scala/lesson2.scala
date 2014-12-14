/**
 * Created by kensuke-mi on 2014/12/14.
 */

import scala.io.Source
import java.io.{File, PrintWriter}

class TrainBigramModel(trainfile_path: String, modelfile_path: String) {

  def AddWordFreq(word_freq_map: scala.collection.mutable.Map[String, Int], word_key: String) = {
    // 単語辞書に頻度を記録する
    if (word_freq_map.contains(word_key)) {
      word_freq_map(word_key) = word_freq_map(word_key) + 1
    } else {
      word_freq_map += (word_key -> 1)
    }
    word_freq_map
  }


  def ConstructTrainFile(trainfile_path: String) = {
    // 訓練ファイルを読みこんで、単語モデルを構築する
    var map_counts = scala.collection.mutable.Map[String, Int]()
    var map_context_counts = scala.collection.mutable.Map[String, Int]()

    val trainfile_ins = Source.fromFile(trainfile_path, "UTF-8")
    trainfile_ins.getLines().foreach({
      line =>
        val split_word: Array[String] = line.split(" ")
        val arraybuffer_word = split_word.toBuffer
        val arraybuffer_sentence = "<s>" +: arraybuffer_word :+ "</s>"
        for (word_index <- 1 until (arraybuffer_sentence.length - 1)) {
          val word_index_i_minus_1 = arraybuffer_sentence(word_index - 1)
          val word_index_i = arraybuffer_sentence(word_index)

          val counts_seq_key = s"$word_index_i_minus_1 $word_index_i"
          map_counts = AddWordFreq(map_counts, counts_seq_key)
          val counts_uni_key = s"$word_index_i"
          map_counts = AddWordFreq(map_counts, counts_uni_key)

          val context_counts_key = s"$word_index_i_minus_1"
          map_context_counts = AddWordFreq(map_context_counts, context_counts_key)
          val context_counts_uni_key = ""
          map_context_counts = AddWordFreq(map_context_counts, context_counts_uni_key)
        }
    })
    val return_tuple = (map_counts, map_context_counts)
    return_tuple
  }


  def ConstructModel(map_counts: scala.collection.mutable.Map[String, Int],
                     map_context_counts: scala.collection.mutable.Map[String, Int]) = {
    // bi-gramモデルを構築する
    var map_bigram_prob = scala.collection.mutable.Map[String, Float]()
    map_counts.foreach({
      case (ngram, count) =>
        val array_word_in_ngram = ngram.split(" ")

        var word_index_i_minus_1: String = ""
        var word_index_i: String = ""

        if (array_word_in_ngram.length == 2) {
          word_index_i_minus_1 = array_word_in_ngram(0)
          word_index_i = array_word_in_ngram(1)
        } else if (array_word_in_ngram.length == 1) {
          word_index_i_minus_1 = ""
          word_index_i = array_word_in_ngram(0)
        } else {
          word_index_i_minus_1 = ""
          word_index_i = ""
        }

        val probability: Float = (map_counts(ngram).toFloat) / map_context_counts(word_index_i_minus_1)
        map_bigram_prob += (ngram -> probability)
    })
    map_bigram_prob
  }

  def SaveModelFile(modelfile_path: String,
                    map_bigram_model: scala.collection.mutable.Map[String, Float]): Unit = {
    // モデルファイルをtsvで書きだす
    val outfile_ins = new PrintWriter(new File(modelfile_path))
    for ((ngram_key, prob) <- map_bigram_model) {
      outfile_ins.write(s"$ngram_key\t$prob\n")
    }
    outfile_ins.close()

  }

  def TrainMain(): Unit = {

    // bigramモデル構築のMain
    val tuple_word_freq = ConstructTrainFile(trainfile_path)
    val map_counts = tuple_word_freq._1
    val map_context_counts = tuple_word_freq._2
    val map_bigram_prob = ConstructModel(map_counts, map_context_counts)

    SaveModelFile(modelfile_path, map_bigram_prob)
  }
}


class EvalModel(testfile_path: String, modelfile_path: String) {
  // params
  val lambda_1: Float = 0.95f
  val lambda_2: Float = 0.90f
  var V: Int = 1000000
  var W: Int = 0
  var H: Float = 0f

  def LoadModel(modelfile_path: String): scala.collection.mutable.Map[String, Float] = {
    // モデルファイルを読み込む
    var map_model = scala.collection.mutable.Map[String, Float]()
    val ins_modelfile = Source.fromFile(modelfile_path, "UTF-8")
    ins_modelfile.getLines().foreach({
      line =>
        val array_key_value = line.stripSuffix("\n").split("\t")
        val key_string = array_key_value(0)
        val value_float = array_key_value(1).toFloat
        map_model += (key_string -> value_float)
    })
    map_model
  }

  def EvalTestFile(testfile_path: String,
                   map_bigram_model: scala.collection.mutable.Map[String, Float]) = {
    // テストファイルの準備をする
    val ins_testfile = Source.fromFile(testfile_path, "UTF-8")
    ins_testfile.getLines().foreach({
      line =>
        val array_testfile = line.stripSuffix("\n").split(" ")
        val arraybuffer_testfile = array_testfile.toBuffer
        val arraybuffer_sentence = "<s>" +: arraybuffer_testfile :+ "</s>"

        // ここからエントロピーの計算を行う
        for (word_index <- 1 until (arraybuffer_sentence.length - 1)) {
          val word_i: String = arraybuffer_sentence(word_index)
          val word_i_minus_1: String = arraybuffer_sentence(word_index - 1)

          val P1: Float = lambda_1 * map_bigram_model(word_i) + (1 - lambda_1) / V
          val key_for_P2: String = s"$word_i_minus_1 $word_i"
          val P2: Float = lambda_2 * map_bigram_model(key_for_P2) + (1 - lambda_2) * P1
          H += -1f * (scala.math.log(P2).toFloat)
          W += 1
        }
    })
    val tuple_return = (H, W)
    tuple_return
  }

  def EvalMain() = {
    val map_bigram_model = LoadModel(modelfile_path)
    val tuple_result = EvalTestFile(testfile_path, map_bigram_model)
    val hentoropy: Float = tuple_result._1
    val word_freq: Int = tuple_result._2

    val final_hentoropy = hentoropy / word_freq
    println(s"entropy = $final_hentoropy")
  }
}


object lesson2 {

  def main(args: Array[String]): Unit = {
    // メインの定義
    val trainfile_path: String = "src/test/resources/test/02-train-input.txt"
    val modelfile_path: String = "src/test/scala/bigram_model.tsv"
    val ins_train_bigram = new TrainBigramModel(trainfile_path, modelfile_path)
    ins_train_bigram.TrainMain()

    val testfile_path: String = "src/test/resources/test/02-train-input.txt"
    val ins_eval_model = new EvalModel(testfile_path, modelfile_path)
    ins_eval_model.EvalMain()
  }
}
