package com.adbrain

import org.specs2.mutable._
import htmlParser._

class languageDetectSpecs extends Specification {

  // Language detection module
  "Language detection" should {
    val l = languageDetect
    "Recognize English " in {
      l.detectText("This is a proper English sentence, isn't it?") must_== "en"
      l.detectText("This cool app gives you hundreds of fun and interesting facts about.Paste your favorite key facts into Facebook or Twitter by tapping the clipboard! \n\nEnjoy! ") mustEqual "en"
    }
    "Recognize non-English sentences " in {
      "下午好！/ 晚上好！" must_!= "en"
      "Salut mec ça va bien? Cela te dirait d'aller au cinéma" must_!= "en"
    }

  }

}
