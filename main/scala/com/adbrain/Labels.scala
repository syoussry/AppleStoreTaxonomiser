package com.adbrain

// TODO Consider using enumerations
object Labels {
  // Edge
  val logRelationLabel: String = "logRelation"
  val profileDBRelationLabel: String = "profileDBRelation"

  // Vertex
  val adbrainIdLabel: String = "adbrainId" // Get's it's own label
  val idFieldLabel: String = "idField"
  val commonFieldLabel: String = "commonField"
  val conversionFieldLabel: String = "conversionField"
  val clickImpFieldLabel: String = "clickOrImpField"
  val clickFieldLabel: String = "clickField"
  val bidFieldLabel: String = "bidField"
  val impressionFieldLabel: String = "impressionField"

  val fieldsLabel: String = "Fields"
  val fieldLabel: String = "Field"
  val datasetLabel: String = "dataSet"
}

object PropertyKeys {
  // Edge keys
  val logRelationTypeKey: String = "logRelationType"
  val multiplicityKey: String = "multiplicity"
  val dbRelationKey: String = "dbRelation"

  val nameKey: String = "name"
  val incomingFrequencyKey: String = "incomingFrequency"
  val changeRateKey: String = "changeRate"
  val fieldTypeKey: String = "type"
  val descriptionKey: String = "description"
  val exampleKey: String = "example"
  val noKey: String = "fieldNo"

  val labelProperty: String = "label"
}

object FieldTypes {
  val str: String = "String"
}