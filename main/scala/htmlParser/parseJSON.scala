package htmlParser

// useful links
// https://github.com/json4s/json4s

import org.json4s._
import org.json4s.native.JsonMethods._
import scala.io.Source

class parseJSON(id: String) {

  val url:String = "https://itunes.apple.com/lookup?id=" + id

  val lines:String = Source.fromURL(url).getLines().mkString

  val json: _root_.org.json4s.JValue = parse(lines)

  val exists: Boolean = if(json \ "resultCount" != JNothing) compact(render(json \ "resultCount")) != "0" else false

  val englishLanguage:Boolean =
    if(exists)
      if(json \\ "languageCodesISO2A" != JNothing) compact(render(json \\ "languageCodesISO2A")).contains("EN")
      else false
    else false

  val description:String =
    if(exists)
      if(json \\ "results" \ "description" != JNothing) compact(render(json \\ "results" \ "description"))
      else ""
    else ""

}
