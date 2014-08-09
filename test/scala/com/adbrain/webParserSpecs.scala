package com.adbrain

import org.specs2.mutable._
import htmlParser._

class webParserSpecs extends Specification {

  "Web parser" should {

    val w = webParser

    "Find main categories from iTunes main page" in {
      val categories: Set[String] = w.parseMainPage.keys.toSet
      categories.size must_== 23
      categories.contains("Sports") must beTrue
      categories.contains("Utilities") must beTrue
    }

    "update from 148Apps" in {
      w.parse148Apps.size > 0 must beTrue
    }



  }


}
