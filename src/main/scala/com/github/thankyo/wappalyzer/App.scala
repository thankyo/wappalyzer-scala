package com.github.thankyo.wappalyzer

import play.api.libs.json._

case class Hint(name: String, value: Option[String] = None) {
  def normalize: Hint = {
    value match {
      case Some(str) if (str.trim.isEmpty) => Hint(name)
      case _ => this
    }
  }
}

case class App(
  id: AppId,
  website: String,
  icon: Option[String],
  url: Option[String],
  cats: List[CategoryId],
  excludes: List[AppId],
  implies: List[AppId],

  hint: AppHint
) {

  def isValid: Boolean = hint.isValid()

}

case class AppHint(
  js: Seq[Hint],
  cookies: Seq[Hint],
  headers: Seq[Hint],
  meta: Seq[Hint],
  html: List[String],
  script: List[String],
) {

  def isValid(): Boolean = js.nonEmpty ||
    cookies.nonEmpty ||
    headers.nonEmpty ||
    meta.nonEmpty ||
    html.nonEmpty ||
    script.nonEmpty
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
  implies: Option[Either[AppId, List[AppId]]]
) {

  def asList[T](eitherOpt: Option[Either[T, List[T]]]): List[T] = {
    eitherOpt match {
      case Some(Left(str)) => List(str)
      case Some(Right(arr)) => arr
      case None => List.empty[T]
    }
  }

  def parseHints(hintOpt: Option[Seq[Hint]]): Seq[Hint] = hintOpt match {
    case Some(hints) => hints.map(_.normalize)
    case None => Seq.empty[Hint]
  }

  def asAppHint(): AppHint = {
    AppHint(
      js = parseHints(js),
      headers = parseHints(headers),
      meta = parseHints(meta),
      cookies = parseHints(cookies),
      html = asList(html),
      script = asList(script)
    )
  }

  def asApp(): App = {
    App(
      id = id,
      website = website,
      icon = icon,
      url = url,
      cats = cats.getOrElse(List.empty[CategoryId]),
      hint = asAppHint(),
      excludes = asList(excludes),
      implies = asList(implies)
    )
  }
}

object App {

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
      JsObject(o.map({ case Hint(name, value) => name -> JsString(value.getOrElse("")) }))
    }

    override def reads(json: JsValue): JsResult[Seq[Hint]] = json match {
      case obj: JsObject =>
        val hints = obj.value.map({ case (field, jsVal) => Hint(field, jsVal.asOpt[String]) })
        JsSuccess(hints.toSeq)
    }
  }

  implicit val appOptJsonFormat: OFormat[Seq[AppOpt]] = seqObjFormat[AppOpt](
    "id",
    Json.format[AppOpt]
  )

  implicit val appListJsonFormat: Reads[Seq[App]] = appOptJsonFormat.map(_.map(_.asApp))

  implicit val appHintFormat = Json.format[AppHint]

  implicit val appWrites: Writes[App] = Json.format[App]

}
