package com.adbrain

import org.specs2.mutable._
import AppStore._

class appSpecs extends Specification {

  "An App object" should {

    val app = App(10, "AppTest", "My favourite football and tennis app description, yes it is really what? I mean that sincerely.")
    "Contain right cleaned ngrams" in {
      app.wordsCleaned must_== List(List("favourite", "football", "tennis", "description", "yes", "really"), List("mean", "sincerely"))
      app.flatWords must_== Array("favourite", "football", "tennis", "description", "yes", "really","mean", "sincerely")
      app.flatBigrams must_== Array("favourite_football", "football_tennis", "tennis_description", "description_yes", "yes_really","mean_sincerely")
      app.flatTrigrams must_== Array("favourite_football_tennis", "football_tennis_description", "tennis_description_yes", "description_yes_really")
    }

    "Output correct features" in {
      app.features(app.flatWords, Array(), Array()) must_== Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
      app.features(Array(), app.flatBigrams, app.flatTrigrams) must_== Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    }


  }

}