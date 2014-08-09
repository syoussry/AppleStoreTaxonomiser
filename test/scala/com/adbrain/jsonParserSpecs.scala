package com.adbrain

import org.specs2.mutable._
import htmlParser.parseJSON

class jsonParserSpecs extends Specification {

  "JSON Parser" should {

    val j = new parseJSON("334325516")
    val j2 = new parseJSON("334325516333323233")

    "Extract right info" in {
      j.description must contain("Come explore with NASA")
      j.englishLanguage must_== true
      j.exists must_== true
      j.url must_== "https://itunes.apple.com/lookup?id=334325516"
    }

    "Find when an app ID does not exist yet" in {
      j2.exists must_== false
    }


  }

}
