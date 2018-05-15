package com.github.thankyo

import play.api.libs.json._

package object wappalyzer {

  type AppId = String
  type CategoryId = Int

  def seqObjFormat[T](
    field: String,
    format: OFormat[T],
    converter: String => JsValue = Json.toJson(_),
  ): OFormat[Seq[T]] = {
    OFormat(objAsSeqReader(field, converter)(format), seqAsObjWriter(field)(format))
  }

  def seqAsObjWriter[T](field: String)(implicit format: OWrites[T]): OWrites[Seq[T]] = (o: Seq[T]) => {
    JsObject(
      o.map(el => {
        val jsObj = Json.toJsObject(el)
        val jsProp = jsObj - field
        val id = (jsObj \ field).as[JsValue].toString()
        id -> jsProp
      })
    )
  }

  def objAsSeqReader[T](field: String, converter: String => JsValue)(implicit format: Reads[T]): Reads[Seq[T]] = {
    case obj: JsObject =>
      val cats = obj.value.map({ case (id, catJson) => catJson.asOpt[JsObject].map(_ + (field -> converter(id))).flatMap(_.asOpt[T]) })
      JsSuccess(cats.flatten.toList)
    case _ => JsError()
  }

}
