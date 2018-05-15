package com.github.thankyo.wappalyzer

trait ConfidenceAware {
  val confidence: Option[Int]
  val version: Option[String]
}

object ConfidenceAware {

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