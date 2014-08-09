package AppStore

import chalk.text.LanguagePack.English._
import chalk.text.tokenize.SimpleEnglishTokenizer
import java.io.PrintWriter
import scala.io.Source

trait WordsProcessor {

  // NLP Preprocessing

  val stopWords: Set[String] =
    Set("[]","]","[","()","(",")",").",";",":","...",",",".","!","?","a","able","about","across","after","all","almost","also","always","am","among","an","and","any","are","as","at","be","because","been", "before", "best","better","but","by","can","cannot","check","content","could","current","contents","dear","did","do","does", "each","either","else","even","ever","every","experience","etc","experiences","for","from","get","give","gave","given", "go","good","got","great","had","has","have","he","her","hers","high","him","his","how","however","i","if","in","inside","into","is","it","its","just", "keep", "keeping", "know", "knows", "now", "latest", "least","left","let","like","likely", "made", "major","make","makes","made","may","me","might", "more","most","move","much","must","my","neither", "new","nice" ,"no","nor","not","of","off","often","on", "one", "ones", "only","or","other","our", "out", "over","own","please","quality","rather","right","said","say","says", "see", "seen", "sees","she","should","since","size","so","some", "take", "takes", "taken", "than","that","the","their","them","then","there","these","they","this", "through", "time", "times", "tis","to","too","twas", "up","until","unless", "us", "use", "used", "view", "views","want","wanted", "wants","was","we","were","what","when","where","which","while","who","whom","why","will","willing","with","within","without","well","would","yet","you","your","yours")

  val commonUnigrams: Set[String] =
    Set("app", "ipod", "way", "includes", "choose", "very", "provides", "learn", "create", "day", "many", "using", "need", "screen", "simple", "different", "first", "last", "enjoy", "version", "designed","mode", "friends","" ,"apps", "interest", "interesting" ,"account", "confirm" ,"subscription","allow","renew", "new", "itunes", "street","select", "end", "hour", "indicate", "push", "notification", "fun", "list","set" , "connect", "facebook", "twitter", "map", "internet", "features", "include", "email","help", "provide", "application", "world", "wait", "iphone", "find", "easy", "information", "ipad", "free", "touch", "full", "play", "game", "share", "around", "support", "level", "available", "record", "design", "download", "friend", "follow")

  def removeStopWords(sentence:List[String]):List[String] =
    sentence.toList.map(_.toLowerCase).filterNot(stopWords)

  def removeCommonUnigrams(sentence:List[String]):List[String] =
    sentence.toList.map(_.toLowerCase).filterNot(commonUnigrams)

  def ngram(n:Int, sentence: List[String]): List[String] = {
    val s: List[String] = sentence.filterNot(_=="")
    if(s.length>=n) s.sliding(n).map(_.mkString("_")).toList
    else Nil
  }

  def fullClean(txt:String) =
    sentenceSegmenter(txt).toList.map(
      sentence =>
        removeCommonUnigrams(
          removeStopWords(
            SimpleEnglishTokenizer.apply()(sentence).toList
          )//.map(PorterStemmer.apply())
        )
    ).filterNot(_=="")


  // Words counting

  def mapOccurrences[T](countingWhat:String, whoFor:String)(wordsSet: List[T]):List[(T,Int)] = {
    println("Counting " + countingWhat + " for " + whoFor)
    wordsSet.foldLeft(Map.empty[T, Int]){
      (count, word) => count + ( word -> (count.getOrElse(word, 0) + 1) )
    }.toList.sortBy(- _._2)
  }
  /*apps.
  flatMap{ app => app.wordsCleaned.flatten}.
  map(word => (word,1)).
  reduceByKey((a,b)=>a+b).
  collect().
  sortBy(- _._2)*/


  // Other useful functions

  // Convert a dense array to a sparse list(positionNonZero,nonZeroValue)
  def denseToSparse(denseArray: Array[Double]): Seq[(Int,Double)] = {
    val nonZeros = denseArray.filterNot(_==0.0)
    def filterReturnIndices[T](l:List[T], lAcc:List[Int], n:Int, predicate:T=>Boolean): List[Int] = {
      l match {
        case Nil=> lAcc.reverse
        case x::xs => if(predicate(x)) filterReturnIndices(xs, n::lAcc, n+1, predicate) else filterReturnIndices(xs, lAcc, n+1, predicate)
      }
    }
    val nonZerosIndices = filterReturnIndices[Double](denseArray.toList, Nil, 0, _!=0.0)
    (nonZerosIndices zip nonZeros).toSeq

  }

  // Write an array of (words, count) to a file
  def writeToFile(l:Array[(String, Int)], fName:String) = {
    val writer = new PrintWriter(fName)
    l.foreach{case (s,c) => writer.write(s + " " + c + System.getProperty("line.separator"))}
    writer.close()
  }

  // From a file containing info about apps to an apps array
  def fileToAppsMap(p:String): Array[App] = {
    Source.fromFile(p).getLines().map(_.split("!#!", -1).toList).map {
      case List(appId, appName, appDesc) => App(appId.toInt, appName, appDesc)
      case List(appId, appName) => App(appId.toInt, appName, "")
    }
  }.toArray
  /*def fileToAppsMap(p:String): RDD[App] = {
    sc.textFile(p).map(_.split("!#!", -1).toList).map {
      case List(appId, appName, appDesc) => App(appId.toInt, appName, appDesc)
      case List(appId, appName) => App(appId.toInt, appName, "")
    }
  }*/



}
