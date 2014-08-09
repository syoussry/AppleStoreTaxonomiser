package AppStore


case class AppSubCategory(id:Int, apps: List[App]) extends java.io.Serializable with WordsProcessor{

  val names: List[List[String]] = apps.map( app => fullClean(app.name).flatten )

  val unigramsCount: List[(String, Int)] = mapOccurrences[String]("unigrams", "sub:" + id)(apps.map(_.flatWords).flatten)
  val bigramsCount: List[(String, Int)] = mapOccurrences[String]("bigrams", "sub:" + id)(apps.map(_.flatBigrams).flatten)
  val trigramsCount: List[(String, Int)] = mapOccurrences[String]("trigrams", "sub:" + id)(apps.map(_.flatTrigrams).flatten)

  val nameWordsCount = mapOccurrences[String]("words", "sub:" + id + " apps names")(names.flatten.toList)

}
