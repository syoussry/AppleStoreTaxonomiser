package test

import com.mongodb.casbah.Imports._
import scala.collection.mutable
import com.mongodb.casbah
import com.mongodb.casbah.commons.Imports

object mongodb {

  val mongoClient: casbah.MongoClient = MongoClient("localhost", 27017)

  val db: casbah.MongoDB = mongoClient("test")
  val colNames: mutable.Set[String] = db.collectionNames
  val coll: casbah.MongoCollection = db("test")

  val a: Imports.DBObject = MongoDBObject("hello" -> "world")
  val b: Imports.DBObject = MongoDBObject("language" -> "scala")

  coll.insert( a )
  coll.insert( b )

  coll.count()

  val allDocs: coll.type#CursorType = coll.find()
  println( allDocs )
  for(doc: casbah.Imports.DBObject <- allDocs) println( doc )



}
