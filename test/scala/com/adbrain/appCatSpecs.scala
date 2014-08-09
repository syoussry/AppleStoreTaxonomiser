package com.adbrain

import org.specs2.mutable._
import AppStore.{AppCat, App}

class appCatSpecs extends Specification {

  "An Apps category" should {

    val app1 = App(123, "AppTest1", "Horror movies and science fiction. Horror movies and science fiction. Horror movies and science fiction.")
    val app2 = App(456, "AppTest2", "Horror movies. Horror movies. Science fiction. Horror movies and science.")
    val cat = AppCat(Array(app1,app2), 0.0, "testCat", 2, 2, 2, null)

    "Find correct n-grams count" in {
      cat.featuresUnigrams must_== Array(("horror",6), ("movies",6))
      cat.featuresBigrams must_== Array(("horror_movies",6), ("movies_science",4))
      cat.featuresTrigrams must_==  Array(("horror_movies_science",4), ("movies_science_fiction",3))
    }

    "Assign correct features counts to apps" in {
      app1.features(cat.featuresOnlyUnigrams, cat.featuresOnlyBigrams, cat.featuresOnlyTrigrams) must_== Array(3.0, 3.0, 3.0, 3.0, 3.0, 3.0)
      app1.features(cat.featuresOnlyUnigrams, cat.featuresOnlyBigrams, cat.featuresOnlyTrigrams) must_== Array(3.0, 3.0, 3.0, 1.0, 1.0, 0.0)
    }

  }

}
