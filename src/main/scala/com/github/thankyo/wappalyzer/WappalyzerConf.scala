package com.github.thankyo.wappalyzer

import play.api.libs.json.Json
import play.api.libs.ws.StandaloneWSResponse

case class WappalyzerConf(
  apps: Seq[App],
  categories: Seq[Category]
) {

  def analyze(res: StandaloneWSResponse): Seq[App] = {
    val relApps = apps.filter(_.analyze(res).nonEmpty)
    val implied = relApps.flatMap(_.implies).flatMap(impl => apps.find(_.id == impl.appId))
    relApps ++ implied
  }

  def analyzeByApp(appId: AppId, res: StandaloneWSResponse): Boolean = {
    val appConf = apps.find(_.id == appId).get
    val impliesApp = apps.filter(_.implies.contains(appId))
    val relHints = (appConf :: impliesApp.toList).flatMap(_.analyze(res))
    relHints.nonEmpty
  }

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
