package com.github.thankyo.wappalyzer

import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, Json}

import scala.io.Source
import scala.util.matching.Regex

class WappalyzerConfSpec extends Specification {

  val jsonStr = Source.fromURL(getClass.getResource("/testApps.json")).mkString
  val wapJson = Json.parse(jsonStr)

  def getApp(appId: AppId): Option[App] = {
    val appsJson = (wapJson \ "apps").as[JsObject]
    val apps = appsJson.as[Seq[App]]

    apps.find(_.id == appId)
  }

  "Read specs" in {
    val wapConf = wapJson.asOpt[WappalyzerConf]
    wapConf shouldNotEqual None
  }

  "Read categories" in {
    val catJson = (wapJson \ "categories").as[JsObject]
    val categories = catJson.as[Seq[Category]]

    categories.size shouldEqual catJson.value.size
    categories.size shouldEqual 57
  }

  "Read apps" in {
    val appsJson = (wapJson \ "apps").as[JsObject]
    val apps = appsJson.as[Seq[App]]

    apps.size shouldNotEqual 0
    apps.size shouldEqual appsJson.value.size
    apps.size shouldEqual 1061
  }

  "Check MaxCDN" in {
    val maxCDNOpt = getApp("MaxCDN")
    maxCDNOpt shouldNotEqual None

    val maxCDN = maxCDNOpt.get
    maxCDN.cats shouldEqual List(31)
    maxCDN.icon shouldEqual Some("MaxCDN.png")
    maxCDN.website shouldEqual "http://www.maxcdn.com"

    maxCDN.hint.headers.toString() shouldEqual Seq(
      Hint("Server", RegexHint(Some(new Regex("^NetDNA")), None, None)),
      Hint("X-CDN-Forward", RegexHint(Some(new Regex("^maxcdn$")), None, None))
    ).toString()
  }

  "Check Acquia Cloud" in {
    val aquiaCloudOpt = getApp("Acquia Cloud")
    aquiaCloudOpt shouldNotEqual None

    val aquiaCloud = aquiaCloudOpt.get
    aquiaCloud.implies shouldEqual Seq(
      AppImply("Drupal", Some(95), None)
    )
  }

  "Check Livefyre" in {
    val livefyreOpt = getApp("Livefyre")
    livefyreOpt shouldNotEqual None

    val livefyre = livefyreOpt.get

    val jsHintOpt = livefyre.hint.js.find(_.name == "L.version")
    jsHintOpt shouldNotEqual None

    val jsHint = jsHintOpt.get
    jsHint.value.version shouldEqual Some("1")
    jsHint.value.confidence shouldEqual Some(0)
  }

  "Parsed valid" in {
    val wapConf = wapJson.as[WappalyzerConf]

    val invalidCat = wapConf.apps.flatMap(_.cats).filterNot(cat => wapConf.categories.exists(_.id == cat))
    invalidCat shouldEqual List.empty

    val invalidAppId = wapConf.apps.flatMap(_.implies.map(_.appId)).filterNot(appId => wapConf.apps.exists(_.id == appId))
    invalidAppId shouldEqual List.empty

    wapConf.isValid shouldEqual true
  }

}
