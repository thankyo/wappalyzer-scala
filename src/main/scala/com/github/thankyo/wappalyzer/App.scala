package com.github.thankyo.wappalyzer

import com.github.thankyo.wappalyzer.WithConfidenceConfig.{parseConfidence, parseVersion}
import play.api.libs.json._
import play.api.libs.ws.StandaloneWSResponse

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

trait WithConfidenceConfig {
  val confidence: Option[Int]
  val version: Option[String]
}

object WithConfidenceConfig {

  def parseConfidence(strOpt: Option[String]): Option[Int] = strOpt.flatMap(parseConfidence)

  def parseConfidence(str: String): Option[Int] = {
    str.split("\\\\;")
      .map(_.split(":"))
      .find(parts => parts.length > 0 && parts(0).trim.equalsIgnoreCase("confidence"))
      .map(parts => parts(1).replaceAllLiterally("\\", "").toInt)
  }

  def parseVersion(strOpt: Option[String]): Option[String] = strOpt.flatMap(parseVersion)

  def parseVersion(str: String): Option[String] = {
    str.split("\\\\;")
      .map(_.split(":"))
      .find(parts => parts.length > 0 && parts(0).trim.equalsIgnoreCase("version"))
      .map(parts => parts(1).replaceAllLiterally("\\", ""))
  }

}

case class RegexHint(
  value: Option[Regex],
  confidence: Option[Int],
  version: Option[String]
) extends WithConfidenceConfig {

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
      val relHeader = allHeaders.find({ case (header, _) if (header.trim.toLowerCase == hint.name.trim.toLowerCase) => true })
      relHeader.exists({
        case (_, relHeaders) if relHeaders.exists(hint.value.matches) => true
      })
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

case class AppImply(
  appId: AppId,
  confidence: Option[Int],
  version: Option[String]
) extends WithConfidenceConfig


case class App(
  id: AppId,
  website: String,
  icon: Option[String],
  cats: List[CategoryId],
  excludes: List[AppId],
  implies: List[AppImply],

  hint: AppHint
) {

  def isValid: Boolean = hint.isValid()

}

case class AppOpt(
  id: AppId,
  website: String,
  icon: Option[String],
  url: Option[RegexHint],
  cats: Option[List[CategoryId]],
  js: Option[Seq[Hint]],
  cookies: Option[Seq[Hint]],
  headers: Option[Seq[Hint]],
  meta: Option[Seq[Hint]],
  html: Option[Either[RegexHint, List[RegexHint]]],
  script: Option[Either[RegexHint, List[RegexHint]]],
  excludes: Option[Either[AppId, List[AppId]]],
  implies: Option[Either[AppId, List[String]]]
) {

  import WithConfidenceConfig._

  def asList[T](eitherOpt: Option[Either[T, List[T]]]): List[T] = {
    eitherOpt match {
      case Some(Left(str)) => List(str)
      case Some(Right(arr)) => arr
      case None => List.empty[T]
    }
  }

  def parseHints(hintOpt: Option[Seq[Hint]]): Seq[Hint] = hintOpt match {
    case Some(hints) => hints
    case None => Seq.empty[Hint]
  }

  def asAppHint(): AppHint = {
    AppHint(
      appId = id,
      url = url,
      js = parseHints(js),
      headers = parseHints(headers),
      meta = parseHints(meta),
      cookies = parseHints(cookies),
      html = asList(html),
      script = asList(script)
    )
  }

  def asAppImplies(): List[AppImply] = {
    asList(implies).map(appImply => {
      val conf = appImply.replaceAllLiterally("\\", "").split(";")
      AppImply(conf(0), parseConfidence(appImply), parseVersion(appImply))
    })
  }

  def asApp(): App = {
    App(
      id = id,
      website = website,
      icon = icon,
      cats = cats.getOrElse(List.empty[CategoryId]),
      hint = asAppHint(),
      excludes = asList(excludes),
      implies = asAppImplies()
    )
  }
}

object App {

  implicit def eitherFormat[T]()(implicit format: Format[T]): Format[Either[T, List[T]]] = new Format[Either[T, List[T]]] {
    override def writes(o: Either[T, List[T]]): JsValue = o match {
      case Left(str) => format.writes(str)
      case Right(arr) => JsArray(arr.map(format.writes))
    }

    override def reads(json: JsValue): JsResult[Either[T, List[T]]] = json match {
      case JsArray(values) => JsSuccess(Right(values.map(json => format.reads(json).get).toList))
      case otherJson => format.reads(otherJson).map(Left(_))
    }
  }

  implicit val eitherFormatForString: Format[Either[String, List[String]]] = eitherFormat[String]()
  implicit val eitherFormatForRegexHint: Format[Either[RegexHint, List[RegexHint]]] = eitherFormat[RegexHint]()

  implicit val hintFormat: OFormat[Seq[Hint]] = new OFormat[Seq[Hint]] {
    override def writes(o: Seq[Hint]): JsObject = {
      JsObject(o.map({ case Hint(name, regexHint) => name -> Json.toJson(regexHint) }))
    }

    override def reads(json: JsValue): JsResult[Seq[Hint]] = json match {
      case obj: JsObject =>
        val hints = obj.value.map({ case (field, jsVal) => Hint(field, jsVal.as[RegexHint]) })
        JsSuccess(hints.toSeq)
    }
  }

  implicit val appOptJsonFormat: OFormat[Seq[AppOpt]] = seqObjFormat[AppOpt](
    "id",
    Json.format[AppOpt]
  )

  implicit val appListJsonFormat: Reads[Seq[App]] = appOptJsonFormat.map(_.map(_.asApp))

  implicit val appHintFormat = Json.format[AppHint]

  implicit val appImplyFormat = Json.format[AppImply]

  implicit val appWrites: Writes[App] = Json.format[App]

}
