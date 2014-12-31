/**
 * Created by kensuke-mi on 2014/12/30.
 */

import collection.mutable.{ListBuffer, ArrayBuffer}
import scala.collection.{mutable, immutable}


class MODEL_IO(path_to_file: String, separator: String){

  import java.io.{File, PrintWriter}
  import scala.io.Source

  def write_out_model(map_model: mutable.Map[_,_]): Unit = {
    /*
    mapで保存されたモデルに関して、指定したseparatorで書き出す
    key(separator)value
    */
    val outfile_ins = new PrintWriter(new File(path_to_file))
    map_model.foreach({
      case(key, value) =>
        outfile_ins.write(s"$key$separator$value\n")
    })
    outfile_ins.close()
  }


  def load_model(): mutable.Map[_,_] = {
    /*
    モデルをファイルに書き出す
    separatorで指定した形式でkeyとvalueを分割する
    valueはfloat型で読み込む
     */
    var map_model = mutable.Map[String, Float]()
    val source_ins = Source.fromFile(path_to_file, "UTF-8")
    source_ins.getLines().foreach({
      line =>
        val array_key_value = line.stripSuffix("\n").split(separator)
        val key_string = array_key_value(0)
        val value_float = array_key_value(1).toFloat
        map_model += (key_string -> value_float)
    })
    return map_model
  }
}


trait LOAD_FILE_FORMAT{
  /*
  ファイル形式を読み込んで、データ構造にもつ
   */
  import scala.io.Source


  def load_from_raw(path_to_file: String):
  mutable.ArrayBuffer[Tuple2[Float, Array[String]]] ={
    /*
    label付きでないファイルからデータ構造を起こす
     */
    val arraybuffer_word_in_sentence =
      new scala.collection.mutable.ArrayBuffer[Tuple2[Float, Array[String]]]()

    val sourcefile_ins = Source.fromFile(path_to_file)
    sourcefile_ins.getLines().foreach({
      line =>
        val label: Float = 0f // 正解ラベル
        val array_string_of_sentence: Array[String] = line.stripSuffix("\n").split(" ") // 訓練文
        val tuple_answer_label_training_sentence =
        Tuple2(label, array_string_of_sentence)  // 訓練ラベルと訓練文はタプルで管理する

        arraybuffer_word_in_sentence += tuple_answer_label_training_sentence
    })
    arraybuffer_word_in_sentence
  }


  def load_from_tsv(path_to_train_file: String):
  mutable.ArrayBuffer[Tuple2[Float, Array[String]]] = {

    val arraybuffer_word_in_sentence =
      new scala.collection.mutable.ArrayBuffer[Tuple2[Float, Array[String]]]()

    val sourcefile_ins = Source.fromFile(path_to_train_file)
    sourcefile_ins.getLines().foreach({
      line =>
        val array_instance_info: Array[String] = line.stripSuffix("\n").split("\t")
        val answer_label: Float = array_instance_info(0).toFloat // 正解ラベル
      val array_string_of_sentence: Array[String] = array_instance_info(1).split(" ") // 訓練文
      val tuple_answer_label_training_sentence =
        Tuple2(answer_label, array_string_of_sentence)  // 訓練ラベルと訓練文はタプルで管理する

        arraybuffer_word_in_sentence += tuple_answer_label_training_sentence
    })
    arraybuffer_word_in_sentence
  }
}


trait FEATURE_FUNCTIONS{

  def unigram_feature(array_string_input: Array[String]): mutable.Map[String, Int] = {
    /*
    素性関数　素性の種類はunigramのみ
    LOAD_FILE_FORMATを継承する
    */
    val feature_map = mutable.Map[String, Int]()

    for (word <- array_string_input){
      if(!feature_map.contains(word)) {
        feature_map += (word -> 1)
      }
    }
    feature_map
  }
}


trait PREDICT_AND_UPDATE{
  /*
  パーセプトロンのtrainingとupdateを行なう
   */
  def PREDICT_ONE(weight_map: mutable.Map[String, Float],
                  feature_map: mutable.Map[String, Int]): Float = {
    /*
    重み関数と素性関数を使ってスコアを求める
     */
    var score: Float = 0f
    feature_map.foreach({
      case (key, value) =>
        if (weight_map.contains(key)) {
          score += weight_map(key)
        }
    })
    if (score >= 0) {
      return 1
    } else {
      return -1
    }
  }

  def initialize_model(weight_map: mutable.Map[String, Float],
                       feature_map: mutable.Map[String, Int]): mutable.Map[String, Float] = {
    /*
    モデルの初期化をする
    feature_mapに登録されていないキーをモデルに新規登録する
     */
    feature_map.foreach({
      case (key, value) =>
        if(!weight_map.contains(key)){
          weight_map += (key -> 0f)
        }
    })
    return weight_map
  }


  def UPDATE_WEIGHT(weight_map: mutable.Map[String, Float],
                    feature_map: mutable.Map[String, Int],
                    answer_label: Float): mutable.Map[String, Float] = {
    /*
    正解ラベルと予測ラベルが異なってた場合のみに、重みの更新を行なう
     */

    feature_map.foreach({
      case (name, value) =>
        val weight_old: Float = weight_map(name)
        val weight_new: Float = weight_old + (value * answer_label)
        weight_map += (name -> weight_new)
    })
    return weight_map
  }
}


class TRAIN_MODEL(path_to_train_file:String)
  extends PREDICT_AND_UPDATE with FEATURE_FUNCTIONS with LOAD_FILE_FORMAT{


  def TRAIN_MODEL(): mutable.Map[String, Float] = {
    /*
    モデルの訓練をする
     */
    val arraybuffer_word_in_sentence = load_from_tsv(path_to_train_file)

    var weight_map = mutable.Map[String, Float]()
    for (tuple_instance <- arraybuffer_word_in_sentence) {
      val y = tuple_instance._1
      val array_training_sentence = tuple_instance._2

      val phi = unigram_feature(array_training_sentence)
      weight_map = initialize_model(weight_map, phi)
      val y_dash = PREDICT_ONE(weight_map, phi)
      if (y != y_dash) {
        weight_map = UPDATE_WEIGHT(weight_map, phi, y)
        val string_of_sentence = array_training_sentence.mkString(" ")
        println(s"model is updated for correct_label:$y and sentence: $string_of_sentence")
      }
    }
    return weight_map
  }

}


class MODEL_PREDICT(path_to_testfile: String, map_model: mutable.Map[String, Float])
  extends PREDICT_AND_UPDATE with FEATURE_FUNCTIONS with LOAD_FILE_FORMAT{
  /*
  モデルで予測をする
  ラベル付きでないファイルに対して予測を行なう
   */
  def predict(): Unit = {
    val arraybuffer_word_in_sentence = load_from_raw(path_to_testfile)

    for (tuple_sentence <- arraybuffer_word_in_sentence) {
      val correct_label = tuple_sentence._1
      val arraybuffer_word_in_sentence = tuple_sentence._2
      val map_feature = unigram_feature(arraybuffer_word_in_sentence)
      val predicted_label = PREDICT_ONE(map_model, map_feature)
      println(s"correct label is $correct_label and predicted label is $predicted_label")
    }
  }


  def predict_for_labeled_data(): Unit = {
    val arraybuffer_word_in_sentence = load_from_tsv(path_to_testfile)

    for (tuple_sentence <- arraybuffer_word_in_sentence) {
      val correct_label = tuple_sentence._1
      val arraybuffer_word_in_sentence = tuple_sentence._2
      val map_feature = unigram_feature(arraybuffer_word_in_sentence)
      val predicted_label = PREDICT_ONE(map_model, map_feature)
      println(s"correct label is $correct_label and predicted label is $predicted_label")
    }

  }
}


object lesson_3 {
  def main(args: Array[String]): Unit = {
    //val path_to_train_file: String = "src/test/resources/test/03-train-input.txt"
    val path_to_train_file: String = "src/resources/data/titles-en-train.labeled"

    val training_ins = new TRAIN_MODEL(path_to_train_file)
    val weight_map = training_ins.TRAIN_MODEL()

    val model_path: String = "src/test/scala/lesson3_perceptron_model.tsv"
    val io_ins = new MODEL_IO(model_path, separator = "\t")
    io_ins.write_out_model(weight_map)

    // テストファイルのパスを指定して、ラベルを予測できるようにする
    val path_to_testfile = "src/resources/data/titles-en-test.word"
    val prediction_ins = new MODEL_PREDICT(path_to_testfile, weight_map)
    prediction_ins.predict()
  }

}
