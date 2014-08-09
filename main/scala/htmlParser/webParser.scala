package htmlParser

//useful links
//http://alvinalexander.com/scala/scala-html-parsing
//http://htmlcleaner.sourceforge.net/
//https://www.apple.com/itunes/affiliates/resources/documentation/itunes-store-web-service-search-api.html

import org.htmlcleaner.{TagNode, HtmlCleaner}
import java.net.URL
import java.io._
import scala.io.Source

object webParser {

  //Parse the App store main page
  def parseMainPage: Map[String, String] = {

    //fixed app store main page url
    val url:String = "https://itunes.apple.com/gb/genre/ios/id36?mt=8"

    //starts an html cleaner instance
    val cleaner: HtmlCleaner = new HtmlCleaner
    val rootNode: TagNode = cleaner.clean(new URL(url))

    //all the categories are listed among top-level-genre <a> tags
    val topLevelsLinks: Array[TagNode] = rootNode.getElementsByAttValue("class","top-level-genre",true,true)

    topLevelsLinks.map(link => link.getText.toString -> link.getAttributes.get("href")).toMap

  }

  //Parse the App store main page
  def parse148Apps: List[Long] = {

    //fixed app store main page url
    val url:String = "http://feeds.feedburner.com/148apps_newest?format=xml"
    val newAppsXML = scala.xml.XML.load(url)

    (newAppsXML \\ "guid").map(_.text.split("/").last.toLong).toList

  }

  //from a top category + first letter + page number, gets all the urls
  def parseAppsGivenCatLetterId(url: String): (Map[String, String],Boolean) = {

    def extractIDFromAppUrl(url: String): String = {
      val arr = url.replace("?mt=8","").split("/")
      arr(arr.length-1).replace("id","")
    }

    //starts an html cleaner instance
    val cleaner: HtmlCleaner = new HtmlCleaner
    val rootNode = cleaner.clean(new URL(url))

    //all the categories are listed among top-level-genre <a> tags
    val appsDiv = rootNode.getElementsByAttValue("id","selectedcontent",true,true).head //there should be only one, as id is unique
    val appsLinks: Array[TagNode] = appsDiv.getElementsByName("a",true)

    //will store our apps in a map appName -> url
    val apps :Map[String, String] =
      appsLinks.map(link => link.getText.toString -> extractIDFromAppUrl(link.getAttributes.get("href"))).toMap

    //thereIsANextPage checks whether a "Next" link exists
    val thereIsANextPage:Boolean = !rootNode.getElementsByAttValue("class","paginate-more",true,true).isEmpty

    (apps.toMap,thereIsANextPage)

  }

  //takes a map and writes it nicely to a .csv file, with separator !#!
  def writeMapToFile(fileName:String, dataMap:Map[String,String]) = {

    val writer = new PrintWriter(new File(fileName))

    for ((key,value) <- dataMap){
      writer.write(List(key,value).mkString("!#!") + System.getProperty("line.separator"))
    }
    writer.close()

  }

  // MAIN function that gets the Apps ids, STILL NOT LEGAL COZ' NOT USING API
  def crawlAppsIDs(cat:String) = {

    //letters of the alphabet
    val letters:List[String] = List("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","#")

    val crawlFolder:String = "crawlData/"

    //main page categories
    val categoryToCrawl: Set[String] = Set(cat)

   val mainPage:Map[String,String] = parseMainPage.filterKeys(categoryToCrawl.contains)

    //writes the main categories in a file
    writeMapToFile(crawlFolder + "categories.csv",mainPage)

    //for each category
    for( (category, categoryURL) <- mainPage){

      //for each letter in this category
      letters.foreach{

        letter => {

          //for this letter, which page id are we crawling?
          var id:Int =1

          //prints out to the console to know where we are
          println("letter: " + letter)

          var goNext:Boolean = true
          //for each id (1,2...10...) for the given letter, get the application urls
          while (goNext){
            val categoryLetterIdURL: String = categoryURL + "&letter=" + letter + "&page=" + id
            val (apps:Map[String,String],next:Boolean) = parseAppsGivenCatLetterId(categoryLetterIdURL)
            writeMapToFile(crawlFolder + category + "--APPSLIST--" + letter + "--" + id + ".csv",apps)
            id = id + 1
            goNext = next
          }

          println(category + letter + "done")

        }

      }

    }

  }

  // takes a megafile with all the ids, within a category, and parses the XML
  // using app store API
  def fromIDToData(category: String, nToDrop:Int = 0) = {

    val fileName:String = "crawlData/" + category + "/megafiles/" + category + ".csv"
    val megaFileName:String = "crawlData/" + category + "/megafiles/" + category + "Data.csv"
    val writer = new PrintWriter(new File(megaFileName))
    val sep:String = "!#!"

    var nTreated: Int = 0

    // loop over the file lines
    Source.fromFile(fileName).getLines().drop(nToDrop).foreach(line => {
        val splitted = line.split("!#!")
        val id = splitted(1)
        val name = splitted(0)
        val jsonParser = new parseJSON(id)
        if (jsonParser.exists){
          writer.write(id + sep +
            name + sep
            + jsonParser.description
            + System.getProperty("line.separator"))
        }else{
          writer.write(id + sep +
            name + sep
            + "NO_MATCHING"
            + System.getProperty("line.separator"))
        }

      // print the number of parsed applications so far
      nTreated = nTreated+1
      if(nTreated % 100==0) println(nTreated + " parsed applications")

      }
    )

    writer.close()

  }


}