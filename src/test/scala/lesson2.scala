/**
 * Created by kensuke-mi on 2014/12/14.
 */

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source
import java.io.{File, PrintWriter}


class TrainBigramModel(trainfile_path: String, modelfile_path: String,
                       unique_type_file_path: String, word_freq_file_path: String) {

  def AddWordFreq(word_freq_map: scala.collection.mutable.Map[String, Int], word_key: String) = {
    // 単語辞書に頻度を記録する
    if (word_freq_map.contains(word_key)) {
      word_freq_map(word_key) = word_freq_map(word_key) + 1
    } else {
      word_freq_map += (word_key -> 1)
    }
    word_freq_map
  }


  def AddWordType(word_freq_map: mutable.Map[String, mutable.ListBuffer[String]],
                  word_key: String,
                  word_in_position_i_plus_1: String) = {
    // 単語辞書に、直後に出現した単語を記録する
    if (!word_freq_map.contains(word_key)) {
      word_freq_map += (word_key -> mutable.ListBuffer(word_in_position_i_plus_1))
    } else if(!word_freq_map.contains(word_key)){
      word_freq_map(word_key) += word_in_position_i_plus_1
    }
    word_freq_map
  }


  def ConstructTrainFile(trainfile_path: String) = {
    // 訓練ファイルを読みこんで、単語モデルを構築する
    // 確率化したファイルと、頻度をカウントしたファイルの２種類を書き出す
    // 頻度カウントのファイルはwordの後(word_index + 1)に続くtype数とwordの出現頻度の２種類を書き出す
    var map_counts = scala.collection.mutable.Map[String, Int]()
    var map_context_counts = scala.collection.mutable.Map[String, Int]()
    var map_n_unique_after = scala.collection.mutable.Map[String, mutable.ListBuffer[String]]()
    var map_n_freq_word = scala.collection.mutable.Map[String, Int]()

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

          // witten_bell smoothing用にtype数と頻度のカウントとる
          val word_index_i_plus_1 = arraybuffer_sentence(word_index + 1)
          map_n_unique_after = AddWordType(
            map_n_unique_after, word_index_i, word_index_i_plus_1)

          map_n_freq_word = AddWordFreq(map_n_freq_word, word_index_i)
        }
    })
    val return_tuple = (map_counts, map_context_counts, map_n_unique_after, map_n_freq_word)
    return_tuple
  }


  def ConstructModel(map_counts: scala.collection.mutable.Map[String, Int],
                     map_context_counts: scala.collection.mutable.Map[String, Int]) = {
    // bi-gramモデルを構築する
    // もっともシンプルなモデル
    // bigramの確率は freq(w_i_minus_1, w_i) / freq(w_i) で確率化される

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
                    map_bigram_model: mutable.Map[String,_]): Unit = {
    // モデルファイルをtsvで書きだす
    val outfile_ins = new PrintWriter(new File(modelfile_path))
    for ((ngram_key, prob) <- map_bigram_model) {
      outfile_ins.write(s"$ngram_key\t$prob\n")
    }
    outfile_ins.close()

  }

  def CountUpUniqueWord(map_n_unique_after: mutable.Map[String, ListBuffer[String]]) = {
    val map_unique_word_counted = mutable.Map[String, Int]()
    map_n_unique_after.foreach({ case (key, value) =>
        val n_unique_word_after = value.length
      map_unique_word_counted += (key -> n_unique_word_after)
    })
    map_unique_word_counted
  }


  def TrainMain(): Unit = {

    // bigramモデル構築のMain
    val tuple_word_freq = ConstructTrainFile(trainfile_path)
    val map_counts = tuple_word_freq._1
    val map_context_counts = tuple_word_freq._2
    val map_bigram_prob = ConstructModel(map_counts, map_context_counts)
    SaveModelFile(modelfile_path, map_bigram_prob)

    val map_n_unique_after = tuple_word_freq._3
    val map_unique_word_counted = CountUpUniqueWord(map_n_unique_after)
    SaveModelFile(unique_type_file_path, map_unique_word_counted)

    val map_n_freq_word = tuple_word_freq._4
    SaveModelFile(word_freq_file_path, map_n_freq_word)
  }
}


class EvalModelWittenBell(testfile_path: String,
                          modelfile_path: String,
                          n_uniqueword_after_path: String,
                          n_wordfreq_path: String){
  // モデルファイルの評価を行なうただし、スムージングはwitten-bellで行なう
  // params
  var H = 0f
  var W = 0
  val V = 10000 // The number of possible unknown words in the vocabulary
  val lambda_unk = 0.1f // lambda2 if word_i not in model file

  def PrepareTestFile(testfile_path: String) = {
    // テストファイルの準備をする
    // ２重のarraybufferを返す
    val arraybuffer_test_sentence = new scala.collection.mutable.ArrayBuffer[mutable.Buffer[String]]()

    val testfile_ins = Source.fromFile(testfile_path, "UTF-8")
    testfile_ins.getLines().foreach({
      line =>
        val split_word: Array[String] = line.split(" ")
        val arraybuffer_word = split_word.toBuffer
        val arraybuffer_sentence = "<s>" +: arraybuffer_word :+ "</s>"

        arraybuffer_test_sentence += arraybuffer_sentence
    })
    arraybuffer_test_sentence
  }


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


  def Witten_Bell_Smoothing(word_in_index: String,
                            map_n_uniqueword_after: mutable.Map[String, Float],
                            map_n_wordfreq: mutable.Map[String, Float]) = {

    if(map_n_uniqueword_after.contains(word_in_index)) {
      val n_uniqword_freq = map_n_uniqueword_after(word_in_index)
      val n_word_freq = map_n_wordfreq(word_in_index)
      val lambda_witten_bel = (n_uniqword_freq / (n_uniqword_freq + n_word_freq))
      lambda_witten_bel
    } else {
      val lambda_witten_bel = lambda_unk
      lambda_witten_bel
    }
  }


  def Calc_Interpolated_Prob(map_model: mutable.Map[String, Float],
                             key_bigram: String,
                             key_unigram: String,
                             lambda_1: Float,
                             lambda_2: Float) = {
    // 線形補完によるbigramの確率を計算する
    val p_ml_bigram = map_model(key_bigram)
    val p_ml_unigram = map_model(key_unigram)
    val prob_unigram = ((1 - lambda_2) * p_ml_unigram) + (1 - lambda_1) / V
    val prob_bigram = lambda_2 * p_ml_bigram
    val prob_interpolated = prob_unigram + prob_bigram

    prob_interpolated
  }


  def EvalModel(map_model: mutable.Map[String, Float],
                map_n_uniqueword_after: mutable.Map[String,Float],
                map_n_wordfreq: mutable.Map[String,Float],
                buffer_test_sentence: mutable.ArrayBuffer[mutable.Buffer[String]]) = {
    // Witten-Bellスムージングでlambdaパラメータを求めて、線形補完でモデルの評価を行なう
    /*
    線形補完式は
     P(w_i|w_{i-1}) = lambda_{w_i-1} * P_ml(w_i|w_{w_{i-1}}) + (1 - lambda_{w_i-1})*P(w_i)
     lambdaパラメタはWitten-Bellにより求められる
     W_B(w_{i-1}) = Unique(w_{i-1}) / (Unique(w_{i-1}) + Freq(w_{i-1}))
     */
    for (arraybuffer_sentence <- buffer_test_sentence){
      for(word_index <- 1 until (arraybuffer_sentence.length - 1)){
        val word_in_index = arraybuffer_sentence(word_index)  // w_i
        val word_in_index_minus_1 = arraybuffer_sentence((word_index - 1)) // w_{i-1}

        // witten-bell smoothingによるlambda param
        val lambda_2 = Witten_Bell_Smoothing(
            word_in_index,
            map_n_uniqueword_after, map_n_wordfreq)
        val lambda_1 = 1f - lambda_2

        val key_bigram = s"$word_in_index_minus_1 $word_in_index"
        val key_unigram = word_in_index

        // 線形補完によるbigram確率の計算
        val prob_interpolated = Calc_Interpolated_Prob(
          map_model, key_bigram,
          key_unigram, lambda_1, lambda_2)

        H += -1f * scala.math.log(prob_interpolated).toFloat
        W += 1
      }
      println(s"sentence: $arraybuffer_sentence")
      println(s"H is $H")
    }
    val entoropy = H / W
  }


  def EvalMain():Unit = {
    val buffer_test_sentence = PrepareTestFile(testfile_path)
    val map_model = LoadModel(modelfile_path)

    val map_n_unique_word = LoadModel(n_uniqueword_after_path)
    val map_n_word_freq = LoadModel(n_wordfreq_path)
    EvalModel(
      map_model, map_n_unique_word,
      map_n_word_freq, buffer_test_sentence)

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
    val unique_word_file_path: String = "src/test/scala/n_unique_word_after.tsv"
    val word_freq_file_path: String = "src/test/scala/n_word_freq.tsv"
    val ins_train_bigram = new TrainBigramModel(trainfile_path,
      modelfile_path,
      unique_word_file_path,
      word_freq_file_path)

    ins_train_bigram.TrainMain()


    val testfile_path: String = "src/test/resources/test/02-train-input.txt"
    // val ins_eval_model = new EvalModel(testfile_path, modelfile_path)
    // ins_eval_model.EvalMain()
    // val ins_eval_wittenbell_model = new EvalModelWittenBell(testfile_path, modelfile_path)
    // ins_eval_wittenbell_model.EvalMain()
    val ins_eval_model_witten_bell = new EvalModelWittenBell(
      testfile_path = testfile_path,
      modelfile_path = modelfile_path,
      n_uniqueword_after_path = unique_word_file_path,
      n_wordfreq_path = word_freq_file_path)

    ins_eval_model_witten_bell.EvalMain()

  }
}
