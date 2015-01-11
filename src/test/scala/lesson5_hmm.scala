/**
 * Created by kensuke-mi on 2015/01/11.
 */

import model_io.MODEL_FILE_IO
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class TrainTransitionEmissionProbs(PathToTrainFile: String,
                                   PathToModelProbMap: String){
  /*
  HMMによるPOS予測のために、訓練ファイルからTransition Prob. と Emission Prob.を作成する。
  訓練ファイルの形式は以下
  Word1_POS1 Word2_POS2 Word3_POS3 ...
  Word1_POS1 Word2_POS2 Word3_POS3 ...
   */
  def LoadDataFromFile(PathToTrainFile: String):
  mutable.ArrayBuffer[mutable.ArrayBuffer[Tuple2[String, String]]] = {
    /*
    データをファイルから読み込んで、tupleのarraybufferにして返す
    arraybufferは１次元目がdata全体、２次元目が１訓練文
     */
    val ArrayBufferTrainData = mutable.ArrayBuffer[mutable.ArrayBuffer[Tuple2[String, String]]]()

    val SourceFileIns = Source.fromFile(PathToTrainFile)
    SourceFileIns.getLines().foreach({
      line =>
        val ArrayBufferOfSentenceData = mutable.ArrayBuffer[Tuple2[String, String]]()

        val ArrayTrainInfoOfSentence = line.stripSuffix("\n").split(" ")
        ArrayTrainInfoOfSentence.foreach({
          TokenPosInfo =>
            val ArrayOfTokenPos = TokenPosInfo.split("_")
            val Token: String = ArrayOfTokenPos(0)
            val Pos: String = ArrayOfTokenPos(1)
            val TupleToken = Tuple2(Token, Pos)
            ArrayBufferOfSentenceData += TupleToken
        })
        ArrayBufferTrainData += ArrayBufferOfSentenceData
    })
    return ArrayBufferTrainData
  }


  def UpdataFreqMap(FreqMap: mutable.Map[String, Int], key: String) = {
    if(FreqMap.contains(key)){
      FreqMap(key) = FreqMap(key) + 1
    } else {
      FreqMap(key) = 1
    }
    FreqMap
  }


  def TrainMaps(ArrayBufferTrainData:
                mutable.ArrayBuffer[mutable.ArrayBuffer[Tuple2[String, String]]]):
  Tuple2[mutable.Map[String, Float], mutable.Map[String, Float]] = {
    /*
    POS訓練コーパスの頻度を数えて、確率化したMapを返す
     */
    var MapTransitionFreq = mutable.Map[String, Int]()
    var MapEmissionFreq = mutable.Map[String, Int]()
    var MapContext = mutable.Map[String, Int]()

    for(arraybufferofsentence <- ArrayBufferTrainData){
      var previous: String = "<s>"
      MapContext = UpdataFreqMap(MapContext, previous)
      for(tupleoftokenpos <- arraybufferofsentence){
        val token = tupleoftokenpos._1
        val pos = tupleoftokenpos._2
        val TransitionTag = s"$previous $pos"
        val EmissionTag = s"$pos $token"
        MapTransitionFreq = UpdataFreqMap(MapTransitionFreq, TransitionTag)
        MapEmissionFreq = UpdataFreqMap(MapEmissionFreq, EmissionTag)
        MapContext = UpdataFreqMap(MapContext, pos)
        previous = pos
      }
      val FinalTag = s"$previous </s>"
      MapTransitionFreq = UpdataFreqMap(MapTransitionFreq, FinalTag)
    }
    val MapTransitionProb = CalcProbs(MapTransitionFreq, MapContext, "T")
    val MapEmissionProb = CalcProbs(MapEmissionFreq, MapContext, "E")

    return Tuple2(MapTransitionProb, MapEmissionProb)
  }


  def CalcProbs(MapOfNumerator: mutable.Map[String, Int],
                MapOfDenominator: mutable.Map[String, Int],
                ProbType: String): mutable.Map[String, Float] = {
    val MapResultProb = mutable.Map[String, Float]()
    MapOfNumerator.foreach({
      case (key, value) =>
        val PosPrevious = key.split(" ")(0)
        val KeyProb = (value.toFloat / MapOfDenominator(PosPrevious))
        val KeyOfProbMap = s"$ProbType $key"
        MapResultProb(KeyOfProbMap) = KeyProb
    })
    return MapResultProb
  }



  def TrainMain() = {
    val ArrayBufferTrainData = LoadDataFromFile(PathToTrainFile)
    val TupleResultMaps = TrainMaps(ArrayBufferTrainData)
    val MapTransitionProb = TupleResultMaps._1
    val MapEmissionProb = TupleResultMaps._2

    MapTransitionProb ++= MapEmissionProb

    val InsTransitionMapIO =
      new MODEL_IO(path_to_file = PathToModelProbMap, separator = "\t")
    InsTransitionMapIO.write_out_model(MapTransitionProb)
  }
}



object lesson5_hmm {

  def LoadTestFile(path_to_train_file: String):
  mutable.ArrayBuffer[List[String]] = {
    /*
    TODO 後で前処理用のパッケージを作ってまとめて突っ込んでおくこと
     */
    val arraybuffer_unigram_training = mutable.ArrayBuffer[List[String]]()

    val source_file_ins = Source.fromFile(path_to_train_file)
    source_file_ins.getLines().foreach({
      line =>
        val array_word_in_sentence = line.stripSuffix("\n").split(" ")
        val list_word_in_setence = array_word_in_sentence.toList
        // arrayの先頭に<s>, arrayの最後に</s>を挿入する
        val list_of_sentence = "<s>" +: list_word_in_setence :+ "</s>"

        arraybuffer_unigram_training += list_of_sentence
    })
    return arraybuffer_unigram_training
  }


  def LoadHmmModel(PathToModelProbMap: String):
  Tuple3[mutable.Map[String, Float], mutable.Map[String, Float], mutable.Map[String, Int]] = {
    /*
    モデルファイルを読み込む
    モデルファイルはtransition probとemission probの２つにわけて返す
     */
    val MapTransitionProb = mutable.Map[String, Float]()
    val MapEmissionProb = mutable.Map[String, Float]()
    val MapPossibleTags = mutable.Map[String, Int]()

    val InsTransitionMapIO =
      new MODEL_IO(path_to_file = PathToModelProbMap, separator = "\t")
    val MapHmmModel = InsTransitionMapIO.load_model()
    MapHmmModel.foreach({
      case (key, value) =>
        val keyString = key.toString
        val valueFloat = value.toString.toFloat
        val TypeTag = keyString.split(" ")(0)
        val PosTag = keyString.split(" ")(1)
        MapPossibleTags(PosTag) = 1

        val ContextWord: String = keyString.split(" ").slice(1,3).mkString(" ") // 返すmapのキーを作成する
        if(TypeTag=="T") MapTransitionProb(ContextWord) = valueFloat
        else if(TypeTag=="E") MapEmissionProb(ContextWord) = valueFloat
    })
    return Tuple3(MapTransitionProb, MapEmissionProb, MapPossibleTags)
  }


  def HmmPosPredicting(ArrayBufferTestFile: mutable.ArrayBuffer[List[String]],
                       MapTransitionProb: mutable.Map[String, Float],
                       MapEmissionProb: mutable.Map[String, Float],
                       MapPossibleTags: mutable.Map[String, Int]) = {

    HmmViterbiForward(ArrayBufferTestFile, MapTransitionProb, MapEmissionProb, MapPossibleTags)
    // HmmViterbiBackward
  }


  def HmmViterbiForward(ArrayBufferTestFile: mutable.ArrayBuffer[List[String]],
                        MapTransitionProb: mutable.Map[String, Float],
                        MapEmissionProb: mutable.Map[String, Float],
                        MapPossibleTags: mutable.Map[String, Int]) = {

    val MapBestScore = mutable.Map[String, Float]()
    val MapBestEdge = mutable.Map[String, String]()

    val StartTag = "0 <s>"
    MapBestScore(StartTag) = 0f
    MapBestEdge(StartTag) = "NULL"

    // TODO forward stepの続きを書いていくことq
  }


  def HmmViterbiBackward() = {

  }
  // ------------------------------------------------


  def TrainModel():Unit = {
    val PathToTrainFile: String = "src/test/resources/test/05-train-input.txt"
    val PathToModelProbMap: String = "src/test/scala/hmm_model_prob.tsv"
    val InsTrainProbs = new TrainTransitionEmissionProbs(
      PathToTrainFile,
      PathToModelProbMap)
    InsTrainProbs.TrainMain()
  }

  def main(args: Array[String]): Unit = {
    //TrainModel()

    val PathToModelProbMap: String = "src/test/scala/hmm_model_prob.tsv"
    val PathToTestFile: String = "src/test/resources/test/05-test-input.txt"
    val ArrayBufferTestFile = LoadTestFile(PathToTestFile)

    val TupleModelMaps = LoadHmmModel(PathToModelProbMap)
    val MapTransitionProb = TupleModelMaps._1
    val MapEmissionProb = TupleModelMaps._2
    val MapPossibleTags = TupleModelMaps._3
    HmmPosPredicting(ArrayBufferTestFile, MapTransitionProb, MapEmissionProb, MapPossibleTags)

  }

}
