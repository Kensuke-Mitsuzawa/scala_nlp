/**
 * Created by kensuke-mi on 2015/01/11.
 */

import model_io.MODEL_FILE_IO
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
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
        val list_of_sentence = list_word_in_setence

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


  def HmmPosPredicting(ListInputSentence: List[String],
                       MapTransitionProb: mutable.Map[String, Float],
                       MapEmissionProb: mutable.Map[String, Float],
                       MapPossibleTags: mutable.Map[String, Int]): ListBuffer[String] = {
    /*
    入力の１文に対して、POSの予測をする
     */

    val TupleMapBestScoreEdge =
      HmmViterbiForward(ListInputSentence, MapTransitionProb, MapEmissionProb, MapPossibleTags)
    val MapBestEdge = TupleMapBestScoreEdge._1
    val MapBestScore = TupleMapBestScoreEdge._2

    val ArrayBufferOrderedPosTag = HmmViterbiBackward(MapBestEdge, ListInputSentence)

    return ArrayBufferOrderedPosTag
  }


  def HmmViterbiForward(ListInputSentence: List[String],
                        MapTransitionProb: mutable.Map[String, Float],
                        MapEmissionProb: mutable.Map[String, Float],
                        MapPossibleTags: mutable.Map[String, Int],
                        smoothing:Boolean=true):
  Tuple2[mutable.Map[String, String], mutable.Map[String, Float]] = {
    /*
    Viterbiのforward stepによるベストパスとスコアの探索を行なう
     */
    val MapBestScore = mutable.Map[String, Float]()
    var MapBestEdge = mutable.Map[String, String]()
    var ScoreEmissionProb = 0f

    var NextNodeKey = ""
    val LenghtOfSentence = ListInputSentence.length

    val StartTag = "0 <s>"
    MapBestScore(StartTag) = 0f
    MapBestEdge(StartTag) = "NULL"


    for(i <- 0 to LenghtOfSentence - 1){
      for(prev <- MapPossibleTags.keys){
        for(next <- MapPossibleTags.keys){
          val WordAtIndex = ListInputSentence(i)
          val BestscoreKey = s"$i $prev"
          val TransitionKey = s"$prev $next"
          val EmissionKey = s"$next $WordAtIndex"

          if(MapBestScore.contains(BestscoreKey) & MapTransitionProb.contains(TransitionKey)){
            val ScoreBestScore = MapBestScore(BestscoreKey)
            val ScoreTransitionProb = -1.0f * math.log10(MapTransitionProb(TransitionKey)).toFloat
            if(smoothing == true) {
              // スム〜ジングする
              ScoreEmissionProb = ProbSmooting(MapEmissionProb, EmissionKey)
            } else {
              ScoreEmissionProb = MapEmissionProb(EmissionKey)
            }

            val NewScore = ScoreBestScore + ScoreTransitionProb + ScoreEmissionProb

            val NextIndex = i + 1
            NextNodeKey = s"$NextIndex $next"

            // MapBestScoreが次のKeyを持っている時と、そうでない時で処理をわける
            if(MapBestScore.contains(NextNodeKey) == false){
              MapBestEdge(NextNodeKey) = s"$i $prev"
              MapBestScore(NextNodeKey) = NewScore
            } else if(MapBestScore.contains(NextNodeKey)){
              val OldScore = MapBestScore(NextNodeKey)
              if(OldScore > NewScore){
                MapBestEdge(NextNodeKey) = s"$i $prev"
                MapBestScore(NextNodeKey) = NewScore
              }
            }
          }
        }
      }
    }
    // 最後のIndexから</s>への処理を行なう
    MapBestEdge = SelectLastEdge(MapBestScore, MapBestEdge, LenghtOfSentence)

    return Tuple2(MapBestEdge, MapBestScore)
  }


  def ProbSmooting(MapEmissionProb: mutable.Map[String, Float], EmissionKey: String): Float = {
    // スムージングを行なう
    // ハイパーパラメータ
    val UnknownLambda = 0.0001f
    val NallWord = 1000000

    var ScoreEmissionProb = 0f
    if(MapEmissionProb.contains(EmissionKey)){
      val EmissionProb = MapEmissionProb(EmissionKey).toFloat
      val SmoothedProb = UnknownLambda * EmissionProb + (1 - UnknownLambda) * (1/NallWord)
      ScoreEmissionProb = -1.0f * math.log10(SmoothedProb).toFloat
    } else {
      val SmoothedProb = UnknownLambda
      ScoreEmissionProb = -1.0f * math.log10(SmoothedProb).toFloat
    }
    return ScoreEmissionProb
  }


  def SelectLastEdge(MapBestScore: mutable.Map[String, Float],
                     MapBestEdge: mutable.Map[String, String],
                     LenghtOfSentence: Int): mutable.Map[String, String] = {
    /*
    MapBestScoreを参照して、minのキーを指定する
     */
    var LastEdge = ""
    var ValueOfLastEdge = 100000000f
    MapBestScore.foreach({
      case(key, value) =>
        val ArrayOfIndexTag = key.split(" ")
        val Index = ArrayOfIndexTag(0)
        if(Index == LenghtOfSentence.toString){
          if(value < ValueOfLastEdge){
            LastEdge = key
            ValueOfLastEdge = value
          }
        }
    })
    val LastIndex = LenghtOfSentence + 1
    val EndTag = s"$LastIndex </s>"
    MapBestEdge(EndTag) = LastEdge

    return MapBestEdge
  }



  def HmmViterbiBackward(MapBestEdge: mutable.Map[String, String],
                         ListInputSentence: List[String]): mutable.ListBuffer[String] = {


    val ListBufferTags = mutable.ListBuffer[String]()

    var LengthOfSentence  = ListInputSentence.length
    val LastIndexForSentenceTag = LengthOfSentence + 1
    val NextEdgeKey = s"$LastIndexForSentenceTag </s>"
    var NextEdge = MapBestEdge(NextEdgeKey)

    while(NextEdge!="0 <s>"){
      val ArrayOfPositionTag = NextEdge.split(" ")
      val Position = ArrayOfPositionTag(0)
      val Tag = ArrayOfPositionTag(1)

      ListBufferTags += Tag
      NextEdge = MapBestEdge(NextEdge)
    }
    val ListBufferOrdered = ListBufferTags.reverse

    return ListBufferOrdered
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

    ArrayBufferTestFile.foreach({
      ListOfSentence =>
        println(ListOfSentence)
        println(HmmPosPredicting(ListOfSentence, MapTransitionProb, MapEmissionProb, MapPossibleTags))
    })
  }
}
