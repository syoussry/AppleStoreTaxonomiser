package AppStore

//import chalk.text.analyze.PorterStemmer

case class App(id: Int, name:String, description: String) extends java.io.Serializable with WordsProcessor {

    // DATA CLEANED, stored by sentences
    // list of list of words == list of sentences where each sentence is a list of words
    val wordsCleaned: List[List[String]] = fullClean(description)

    // DATA flattened
    lazy val flatWords: Array[String] = wordsCleaned.flatten.toArray
    lazy val flatBigrams: Array[String] = wordsCleaned.map(ngram(2,_)).filterNot(_.isEmpty).flatten.toArray
    lazy val flatTrigrams: Array[String] = wordsCleaned.map(ngram(3,_)).filterNot(_.isEmpty).flatten.toArray

    // 3rd step: for this app, output the features array (of unigrams, bigrams and trigrams count)
    def features(featUni: Array[String], featBi: Array[String], featTri: Array[String]): Array[Double] = {
      val fUni = featUni.map(feature => flatWords.foldLeft[Double](0.0)( (count,word) => if(word==feature) count+1 else count ))
      val fBi = featBi.map(feature => flatBigrams.foldLeft[Double](0.0)( (count,word) => if(word==feature) count+1 else count ))
      val fTri = featTri.map(feature => flatTrigrams.foldLeft[Double](0.0)( (count,word) => if(word==feature) count+1 else count ))
      fUni union fBi union fTri
    }



}