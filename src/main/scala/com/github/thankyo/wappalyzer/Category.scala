package com.github.thankyo.wappalyzer

import play.api.libs.json._

case class Category(id: CategoryId, name: String, priority: Int)

object Category {

  implicit val listFormat: OFormat[Seq[Category]] = seqObjFormat(
    "id",
    Json.format[Category],
    (id) => JsNumber(id.toInt),
  )

}