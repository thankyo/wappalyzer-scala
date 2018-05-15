package com.github.thankyo.wappalyzer

import com.github.thankyo.wappalyzer.ConfidenceAware.{parseConfidence, parseVersion}
import play.api.libs.json._
import play.api.libs.ws.StandaloneWSResponse

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

case class RegexHint(
  value: Option[Regex],
  confidence: Option[Int],
  version: Option[String]
) extends ConfidenceAware {

  def matches(str: String): Boolean = value.forall(_.findAllMatchIn(str).nonEmpty)

}

object RegexHint {

  implicit val jsonFormat: Format[RegexHint] = new Format[RegexHint] {

    override def writes(o: RegexHint): JsValue = {
      JsString(
        o.value.getOrElse("") +
          o.confidence.map(c => s"\\\\;confidence:$c").getOrElse("") +
          o.version.map(v => s"\\\\;version:$v").getOrElse("")
      )
    }

    override def reads(json: JsValue): JsResult[RegexHint] = json match {
      case JsString(str) =>
        val regexPart = str.split("\\\\;")(0)
        val regexOpt = Some(regexPart).filterNot(_.trim.length == 0).flatMap(str => {
          val normStr = str.replace("/", "\\/")
          Try({
            new Regex(normStr)
          }) match {
            case Success(regex) =>
              Some(regex)
            case Failure(_) =>
              println(s"Failed to parse ${normStr}")
              None
          }
        })
        JsSuccess(RegexHint(regexOpt, parseConfidence(str), parseVersion(str)))
    }

  }

}

case class Hint(
  name: String,
  value: RegexHint
)

case class AppHint(
  appId: AppId,
  url: Option[RegexHint],
  js: Seq[Hint],
  cookies: Seq[Hint],
  headers: Seq[Hint],
  meta: Seq[Hint],
  html: List[RegexHint],
  script: List[RegexHint],
) {

  def isValid(): Boolean = {
    url.nonEmpty ||
      js.nonEmpty ||
      cookies.nonEmpty ||
      headers.nonEmpty ||
      meta.nonEmpty ||
      html.nonEmpty ||
      script.nonEmpty
  }

  private def analyzeCookies(res: StandaloneWSResponse): Seq[RegexHint] = {
    val allCookies = res.cookies
    val matchedHints = cookies.filter(hint => {
      val relCookie = allCookies.find(cookie => cookie.name.toLowerCase.trim == hint.name.toLowerCase.trim)
      relCookie.exists({
        case cookie if hint.value.matches(cookie.toString) => true
      })
    })
    matchedHints.map(_.value)
  }

  private def analyzeHeaders(res: StandaloneWSResponse): Seq[RegexHint] = {
    val allHeaders = res.headers
    val matchedHints = headers.filter(hint => {
      val relHeaders = allHeaders.collect({ case (name, hdrs) if name.trim.toLowerCase == hint.name.trim.toLowerCase => hdrs })
      relHeaders.exists(_.exists(hint.value.matches))
    })
    matchedHints.map(_.value)
  }

  private def analyzeUrl(url: StandaloneWSResponse): Seq[RegexHint] = {
    Seq.empty
  }

  private def analyzeMeta(res: StandaloneWSResponse): Seq[RegexHint] = {
    Seq.empty
  }

  private def analyzeJs(res: StandaloneWSResponse): Seq[RegexHint] = {
    Seq.empty
  }

  private def analyzeHtml(res: StandaloneWSResponse): Seq[RegexHint] = {
    html.filter(_.matches(res.body))
  }

  private def analyzeScript(res: StandaloneWSResponse): Seq[RegexHint] = {
    script.filter(_.matches(res.body))
  }

  def analyze(res: StandaloneWSResponse): Seq[RegexHint] = {
    analyzeCookies(res) ++
    analyzeHeaders(res) ++
    analyzeMeta(res) ++
    analyzeHtml(res) ++
    analyzeUrl(res) ++
    analyzeScript(res) ++
    analyzeJs(res)
  }

}
