package com.github.thankyo.wappalyzer

import play.api.libs.json._
import play.api.libs.ws.StandaloneWSResponse

case class AppImply(
  appId: AppId,
  confidence: Option[Int],
  version: Option[String]
) extends ConfidenceAware

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

  def analyze(res: StandaloneWSResponse): Seq[RegexHint] = {
    hint.analyze(res)
  }

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

  import ConfidenceAware._

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
