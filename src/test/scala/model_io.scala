/**
 * Created by kensuke-mi on 2015/01/02.
 */

package model_io

import scala.collection.mutable
import java.io.{File, PrintWriter}
import scala.io.Source



class LOAD_RAW_FILE{


  def load_raw_file(path_to_train_file: String): mutable.ArrayBuffer[List[String]] = {
    // 言語モデル訓練用のファイルを読み込む
    val arraybuffer_unigram_training = mutable.ArrayBuffer[List[String]]()

    val source_file_ins = Source.fromFile(path_to_train_file)
    source_file_ins.getLines().foreach({
      line =>
        val array_word_in_sentence = line.stripSuffix("\n").split(" ")
        val list_word_in_setence = array_word_in_sentence.toList
        // arrayの先頭に<s>, arrayの最後に</s>を挿入する
        val list_of_sentence = list_word_in_setence :+ "</s>"

        arraybuffer_unigram_training += list_of_sentence
    })
    return arraybuffer_unigram_training
  }


}


class MODEL_FILE_IO(path_to_file: String, separator: String) extends LOAD_RAW_FILE{


  def write_out_model(map_model: mutable.Map[_, _]): Unit = {
    /*
    mapで保存されたモデルに関して、指定したseparatorで書き出す
    key(separator)value
    */
    val outfile_ins = new PrintWriter(new File(path_to_file))
    map_model.foreach({
      case (key, value) =>
        outfile_ins.write(s"$key$separator$value\n")
    })
    outfile_ins.close()
  }


  def load_model(): mutable.Map[String, Float] = {
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
