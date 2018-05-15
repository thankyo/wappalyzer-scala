package com.github.thankyo.wappalyzer

import play.api.libs.json.Json

case class WappalyzerConf(
  apps: Seq[App],
  categories: Seq[Category]
) {

  def getHintsFor(appId: AppId): Seq[AppHint] = {
    apps.filter(app => app.implies.contains(appId) || app.id == appId).
      map(_.hint).
      filter(_.isValid())
  }

  def isValid: Boolean = {
    apps.flatMap(_.implies.map(_.appId)).forall(appId => apps.exists(_.id == appId)) &&
    apps.flatMap(_.cats).forall(cat => categories.exists(_.id == cat))
  }

}

object WappalyzerConf {

  implicit val jsonFormat = Json.format[WappalyzerConf]

}
