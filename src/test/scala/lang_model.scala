/**
 * Created by kensuke-mi on 2015/01/02.
 */

package lang_model

import scala.collection.mutable
import scala.io.Source

trait LOAD_RAW_FILE{


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


class TRAIN_UNIGRAM_MODLE(path_to_train_file: String) extends LOAD_RAW_FILE{
  /*
  unigram言語モデルを構築する
   */
  def Calc_unigram_prob(map_word_freq: mutable.Map[String, Int], total_word: Float):
  mutable.Map[String, Float] = {
    /*
    unigram確率を求める
     */

    val map_unigram_lang_model = mutable.Map[String, Float]()

    map_word_freq.foreach({
      case(key, word_freq) =>
        val prob_word_unigram = word_freq / total_word
        map_unigram_lang_model += (key -> prob_word_unigram)
    })

    return map_unigram_lang_model
  }


  def train_unigram_model(arraybuffer_unigram_training: mutable.ArrayBuffer[List[String]]):
  mutable.Map[String, Float] = {
    /*
    訓練文からunigram確率モデルを構築する
     */
    val map_word_freq = mutable.Map[String, Int]()

    var total_word = 0f

    for(list_of_sentence <- arraybuffer_unigram_training){
      for(word_in_sentence <- list_of_sentence){
        total_word += 1
        if(map_word_freq.contains(word_in_sentence)) map_word_freq(word_in_sentence) += 1
        else map_word_freq += (word_in_sentence -> 1)
      }
    }
    val map_unigram_lang_model = Calc_unigram_prob(map_word_freq, total_word)

    return map_unigram_lang_model
  }


  def construct_lang_model(path_to_trainfile: String): mutable.Map[String, Float] = {


    val arraybuffer_unigram_training = load_raw_file(path_to_trainfile)
    val map_unigram_lang_model = train_unigram_model(arraybuffer_unigram_training)


    return map_unigram_lang_model
  }


  def get_uniram_prob(word_unigram: String,
                       map_unigram_lang_model: mutable.Map[String, Float],
                       lambda_unk: Float = 0.05f, total_word_size: Float = 1000000f): Float = {
    /*
    unigram確率を求める
    線形補完で確率値を求める

    モデルに単語がある場合は
    p(word_unigram) = (lambda_one * P_ml(word_unigram)) + ((1 - lambda_one) * (1 / total_word_size))

    モデルに単語がない場合は
    p(word_unigram) = lambda_unk
     */

    val lambda_one = 1 - lambda_unk
    if(map_unigram_lang_model.contains(word_unigram)){
      val prob_from_model = map_unigram_lang_model(word_unigram)
      val word_prob = (lambda_one * prob_from_model) + ((1 - lambda_one) * (1 / total_word_size))

      return word_prob
    } else {
      val word_prob = lambda_unk

      return word_prob
    }
  }


}
