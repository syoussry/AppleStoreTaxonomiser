package htmlParser

import com.cybozu.labs.langdetect.{LangDetectException, Detector, DetectorFactory}
import java.io.{PrintWriter, File}
import scala.io.Source

object languageDetect {

  // i copied the directory "profiles" of the jar file into my folder repository
  //have to find a cleaner way for production
  DetectorFactory.loadProfile("lang-profiles")

  def detectText(txt:String): String = {
    val detector: Detector = DetectorFactory.create()
    detector.append(txt)
    val lang:String = detector.detect()
    //val langlist: ArrayList[Language] = detector.getProbabilities()
    lang
  }

  def cleanTextFromBadCharacters(txt:String):String = {
    //patterns
    val newLine: String = "\\n"
    val unicode: String = "\\u"
    val pattern: String = "[^(a-z|A-Z| |.|!|?|-|_)]"

    txt.replace(newLine,"").replace(unicode,"").replaceAll(pattern,"")
  }

  def launchDetection(category: String):Unit = {

    val path:String = "crawlData/" + category + "/megafiles/"
    val fileName:String = category + "Data"

    val writer = new PrintWriter(new File(path + fileName + "-en.csv"))
    val writerException = new PrintWriter(new File(path+fileName + "-exceptions.csv"))

    val sep: String = "!#!"

    Source.fromFile(path + fileName + ".csv").getLines().foreach(
      line => {
        val splitted = line.split("!#!")
        if(splitted.size>2)
          try {
            if(detectText(splitted(2)) == "en"){
              writer.write(
                splitted(0) + sep + splitted(1) + sep + cleanTextFromBadCharacters(splitted(2))
                  + System.getProperty("line.separator")
              )
            }
          } catch {
            case e:LangDetectException => writerException.write(line)
          }
      }
    )

    writer.close()
    writerException.close()

  }


}