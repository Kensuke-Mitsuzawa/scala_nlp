/**
 * Created by kensuke-mi on 2015/01/01.
 */

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, immutable}
import scala.io.Source
import model_io.MODEL_FILE_IO
import lang_model.UNIGRAM_MODLE


class viterbi_character_split(unigram_lang_ins: UNIGRAM_MODLE,
                              map_unigram_lang_model: mutable.Map[String, Float]){
  /*
  unigram確率とviterbiアルゴリズムで文字列分割を行なう
   */
  def viterbi_backward_step(arraybuffer_best_edge: mutable.ArrayBuffer[Tuple2[Int, Int]],
                            array_sentence: Array[String]): mutable.ListBuffer[String] = {
    /*
    backwardのアルゴリズムを実施する
     */
    var listbuffer_words = mutable.ListBuffer[String]()

    val next_edge_index = arraybuffer_best_edge.length - 1
    var tuple_next_edge = arraybuffer_best_edge(next_edge_index)
    while (tuple_next_edge != Tuple2(0, 0)){
      val arraybuffer_word = array_sentence.slice(tuple_next_edge._1, tuple_next_edge._2)
      val word = arraybuffer_word.mkString("")
      listbuffer_words += word
      tuple_next_edge = arraybuffer_best_edge(tuple_next_edge._1)
    }
    val listbuffer_backward_result = listbuffer_words.reverse

    return listbuffer_backward_result
  }


  def viterbi_forward_step(unigram_lang_ins: UNIGRAM_MODLE,
                           array_sentence: Array[String],
                           map_unigram_lang_model: mutable.Map[String, Float]):
  Tuple2[mutable.ArrayBuffer[Tuple2[Int, Int]], mutable.ArrayBuffer[Float]] = {

    val length_of_input_array = array_sentence.length + 1
    val arraybuffer_best_edge = mutable.ArrayBuffer.fill(length_of_input_array)(Tuple2(-1, -1))
    val arraybuffer_best_score = mutable.ArrayBuffer.fill(length_of_input_array)(0f)

    arraybuffer_best_edge(0) = Tuple2(0, 0)  // 最初のインデックスは(0,0)で始める
    arraybuffer_best_score(0) = 0f

    for(word_end_index <- 1 to array_sentence.length){

      arraybuffer_best_score(word_end_index) = 1.0E10f  // 巨大な数を追加しておく
      for(word_begin_index <- 0 to word_end_index - 1){
        val array_sub_word = array_sentence.slice(word_begin_index, word_end_index)
        val word = array_sub_word.mkString("")
        if(map_unigram_lang_model.contains(word) || array_sub_word.length==1){
          /*
          モデルに単語が存在するか、長さが１の場合のみに実行
           */
          val prob_unigram: Float =
            unigram_lang_ins.get_uniram_prob(word, map_unigram_lang_model)
          val my_score_double =
            arraybuffer_best_score(word_begin_index) + (-1f * math.log10(prob_unigram))
          val my_score = my_score_double.toFloat

          if(my_score < arraybuffer_best_score(word_end_index)){
            arraybuffer_best_score(word_end_index) = my_score
            arraybuffer_best_edge(word_end_index) = Tuple2(word_begin_index, word_end_index)
          }
        }
      }
    }

    val tuple_return = Tuple2(arraybuffer_best_edge, arraybuffer_best_score)
    return tuple_return
  }


  def string_into_array(path_to_train_file: String): ArrayBuffer[Array[String]] = {
    /*
    文字を分割してリストに格納する
     */
    val arraybuffer_of_sentence = mutable.ArrayBuffer[Array[String]]()

    val source_file_ins = Source.fromFile(path_to_train_file)
    source_file_ins.getLines().foreach({
      line =>
        val array_word_in_sentence = line.stripSuffix("\n").split("")
        val array_word_sentence = array_word_in_sentence.slice(1, array_word_in_sentence.length)

        arraybuffer_of_sentence += array_word_sentence
    })
    return arraybuffer_of_sentence
  }


  def viterbi_main(arraybuffer_input_file: mutable.ArrayBuffer[Array[String]]) = {
    val listbuffer_splited_sentence = mutable.ListBuffer[mutable.ListBuffer[String]]()

    arraybuffer_input_file.foreach({
      array_sentence =>
        val tuple_forward_result =
          viterbi_forward_step(unigram_lang_ins, array_sentence, map_unigram_lang_model)
        val arraybuffer_best_edge = tuple_forward_result._1
        val arraybugger_best_score = tuple_forward_result._2
        val listbuffer_backward_result = viterbi_backward_step(arraybuffer_best_edge, array_sentence)
        listbuffer_splited_sentence += listbuffer_backward_result
    })
    listbuffer_splited_sentence
  }


}



object lesson4_word_segmentation{


  def main(args: Array[String]): Unit = {
    // 本番用のunigram training code
    /*
    val path_to_trainfile: String = "src/test/resources/test/01-train-input.txt"
    val train_unigram_lang_ins = new TRAIN_UNIGRAM_MODLE(path_to_trainfile)
    val map_unigram_model = train_unigram_lang_ins.construct_lang_model(path_to_trainfile)

    val path_to_unigram_model: String = "src/test/scala/unigram_model.tsv"
    val model_io_ins = new MODEL_IO(path_to_unigram_model, separator = "\t")
    model_io_ins.write_out_model(map_unigram_model)
    */
    // unigram model読み込み
    val path_to_unigram_model: String = "src/test/resources/test/04-model.txt"
    val model_io_ins = new MODEL_FILE_IO(path_to_unigram_model, separator = "\t")
    val map_unigram_lang_model = model_io_ins.load_model()

    // unigram確率クラスのインスタンスを読み込み
    val unigram_lang_ins = new UNIGRAM_MODLE(path_to_unigram_model)


    val viterbi_training_ins =
      new viterbi_character_split(unigram_lang_ins, map_unigram_lang_model)

    // inputファイルの読み込み
    val test_file_input: String = "src/test/resources/test/04-input.txt"
    val arraybuffer_input_file = viterbi_training_ins.string_into_array(test_file_input)
    val listbuffer_splited_sentence = viterbi_training_ins.viterbi_main(arraybuffer_input_file)
    println(listbuffer_splited_sentence)
  }


}
