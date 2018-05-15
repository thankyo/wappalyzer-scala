package com.github.thankyo.wappalyzer

import play.api.libs.json.Json

case class WappalyzerConf(
  apps: Seq[App],
  categories: Seq[Category]
)

object WappalyzerConf {

  implicit val jsonFormat = Json.format[WappalyzerConf]

}
