package com.github.thankyo.wappalyzer

import play.api.libs.json._

case class Hint(
  name: String,
  value: Option[String] = None,
  confidence: Option[Int],
  version: Option[String]
) extends WithConfidenceConfig

trait WithConfidenceConfig {
  val confidence: Option[Int]
  val version: Option[String]
}

object WithConfidenceConfig {

  def parseConfidence(strOpt: Option[String]): Option[Int] = strOpt.flatMap(parseConfidence)

  def parseConfidence(str: String): Option[Int] = {
    str.split(";")
      .map(_.split(":"))
      .find(parts => parts.length > 0 && parts(0).trim.equalsIgnoreCase("confidence"))
      .map(parts => parts(1).replaceAllLiterally("\\","").toInt)
  }

  def parseVersion(strOpt: Option[String]): Option[String] = strOpt.flatMap(parseVersion)

  def parseVersion(str: String): Option[String] = {
    str.split(";")
      .map(_.split(":"))
      .find(parts => parts.length > 0 && parts(0).trim.equalsIgnoreCase("version"))
      .map(parts => parts(1).replaceAllLiterally("\\",""))
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
  url: Option[String],
  cats: Option[List[CategoryId]],
  js: Option[Seq[Hint]],
  cookies: Option[Seq[Hint]],
  headers: Option[Seq[Hint]],
  meta: Option[Seq[Hint]],
  html: Option[Either[String, List[String]]],
  script: Option[Either[String, List[String]]],
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

  import WithConfidenceConfig._

  implicit val eitherFormat: Format[Either[String, List[String]]] = new Format[Either[String, List[String]]] {
    override def writes(o: Either[String, List[String]]): JsValue = o match {
      case Left(str) => JsString(str)
      case Right(arr) => JsArray(arr.map(JsString))
    }

    override def reads(json: JsValue): JsResult[Either[String, List[String]]] = json match {
      case JsString(str) => JsSuccess(Left(str))
      case JsArray(values) => JsSuccess(Right(values.map(_.as[String]).toList))
    }
  }

  implicit val hintFormat: OFormat[Seq[Hint]] = new OFormat[Seq[Hint]] {
    override def writes(o: Seq[Hint]): JsObject = {
      JsObject(o.map({ case Hint(name, value, confidence, version) => name -> JsString(
        value.getOrElse("") +
        confidence.map(c => s";confidence:$c").getOrElse("") +
        version.map(v => s";version:$v").getOrElse("")
      ) }))
    }

    override def reads(json: JsValue): JsResult[Seq[Hint]] = json match {
      case obj: JsObject =>
        val hints = obj.value.map({ case (field, jsVal) => {
          val str: Option[String] = jsVal.asOpt[String].filterNot(_.trim.isEmpty)
          Hint(field, str, parseConfidence(str), parseVersion(str))
        } })
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
