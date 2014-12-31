/**
 * Created by kensuke-mi on 2014/12/30.
 */

import collection.mutable.{ListBuffer, ArrayBuffer}
import scala.collection.{mutable, immutable}
import scala.io.Source


trait LOAD_FILE_FORMAT{
  /*
  ファイル形式を読み込んで、データ構造にもつ
   */
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
      if(feature_map.contains(word)) {
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
          score += value + weight_map(key)
        }
    })
    if (score >= 0) {
      return 1
    } else {
      return -1
    }
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


  def TRAIN_MODEL() = {
    /*
    モデルの訓練をする
     */
    val arraybuffer_word_in_sentence = load_from_tsv(path_to_train_file)

    var weight_map = mutable.Map[String, Float]()
    for (tuple_instance <- arraybuffer_word_in_sentence) {
      val y = tuple_instance._1
      val array_training_sentence = tuple_instance._2

      val phi = unigram_feature(array_training_sentence)
      val y_dash = PREDICT_ONE(weight_map, phi)
      if (y != y_dash) {
        weight_map = UPDATE_WEIGHT(weight_map, phi, y)
      }
      println(y)
    }
  }

}


class PREDICT_ALL(path_to_train_file: String){
  /*
  パーセプトロンのtrainingを行なう
   */

  def FEATURE_FUNCTION(array_string_input: Array[String]): mutable.Map[String, Int] = {
    /*
    素性関数　素性の種類はunigramのみ
     */
    val feature_map = mutable.Map[String, Int]()

    for (word <- array_string_input){
      if(feature_map.contains(word)) {
        feature_map += (word -> 1)
      }
    }
    feature_map
  }


  def CREATE_SENTENCE_INSTANCE(path_to_train_file: String):
  mutable.ArrayBuffer[Tuple2[Float, Array[String]]] = {
    /*
    訓練用のインスタンスを作成する
     */
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


  def PREDICT_ONE(weight_map: mutable.Map[String, Float],
                  feature_map: mutable.Map[String, Int]): Float = {
    /*
    重み関数と素性関数を使ってスコアを求める
     */
    var score: Float = 0f
    feature_map.foreach({
      case (key, value) =>
        if(weight_map.contains(key)){
          score += value + weight_map(key)
        }
    })
    if(score >= 0){
      return 1
    } else {
      return -1
    }
  }


  def UPDATE_WEIGHT(weight_map: mutable.Map[String, Float],
                    feature_map: mutable.Map[String, Int],
                    answer_label: Float): mutable.Map[String, Float] = {
    /*
    正解ラベルと予測ラベルが異なってた場合のみに、重みの更新を行なう
     */
    feature_map.foreach({
      case(name, value) =>
        val weight_old:Float = weight_map(name)
        val weight_new:Float = weight_old + (value * answer_label)
        weight_map += (name -> weight_new)
    })
    return weight_map
  }


  def TRAIN_MAIN() = {
    val arraybuffer_training_sentence = CREATE_SENTENCE_INSTANCE(path_to_train_file)

    var weight_map = mutable.Map[String, Float]()
    for(tuple_instance <- arraybuffer_training_sentence){
      val y = tuple_instance._1
      val array_training_sentence = tuple_instance._2

      val phi = FEATURE_FUNCTION(array_training_sentence)
      val y_dash = PREDICT_ONE(weight_map, phi)
      if(y != y_dash){
        weight_map = UPDATE_WEIGHT(weight_map, phi, y)
      }
      println(y)
    }
  }

}


object lesson_3 {
  def main(args: Array[String]): Unit = {
    val path_to_train_file: String = "src/test/resources/test/03-train-input.txt"

    //val predict_all_ins = new PREDICT_ALL(path_to_train_file)
    //predict_all_ins.TRAIN_MAIN()

    val training_ins = new TRAIN_MODEL(path_to_train_file)
    training_ins.TRAIN_MODEL()

  }

}
