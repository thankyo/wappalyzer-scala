package com.github.thankyo.wappalyzer

import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, Json}

import scala.io.{Source}

class WappalyzerConfSpec extends Specification {

  val jsonStr = Source.fromURL(getClass.getResource("/testApps.json")).mkString
  val wapJson = Json.parse(jsonStr)

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
    apps.size shouldEqual 1062
  }

  "Check MaxCDN" in {
    val appsJson = (wapJson \ "apps").as[JsObject]
    val apps = appsJson.as[Seq[App]]

    val maxCDNOpt = apps.find(_.id == "MaxCDN")
    maxCDNOpt shouldNotEqual None

    val maxCDN = maxCDNOpt.get
    maxCDN.cats shouldEqual List(31)
    maxCDN.icon shouldEqual Some("MaxCDN.png")
    maxCDN.website shouldEqual "http://www.maxcdn.com"

    maxCDN.hint.headers shouldEqual Seq(
      Hint("Server", Some("^NetDNA")),
      Hint("X-CDN-Forward", Some("^maxcdn$"))
    )
  }

  "All apps are valid" in {
    val appsJson = (wapJson \ "apps").as[JsObject]
    val apps = appsJson.as[Seq[App]]

    val invalidApps = apps.filter(!_.isValid).map(_.id)

    invalidApps shouldEqual Seq.empty
  }

}
