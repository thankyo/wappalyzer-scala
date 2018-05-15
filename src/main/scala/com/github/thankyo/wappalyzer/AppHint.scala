package com.github.thankyo.wappalyzer

import scala.util.matching.Regex

case class AppHint(
  appId: AppId,
  url: Option[String],
  js: Seq[Hint],
  cookies: Seq[Hint],
  headers: Seq[Hint],
  meta: Seq[Hint],
  html: List[String],
  script: List[String],
) {

  def isValid(): Boolean =
    url.nonEmpty ||
    js.nonEmpty ||
    cookies.nonEmpty ||
    headers.nonEmpty ||
    meta.nonEmpty ||
    html.nonEmpty ||
    script.nonEmpty
}

case class Check(string: String, regex: Regex)


object AppHint {

//  def parsePatterns(pattern: String) = {
//    pattern.split("\\;").map(attr => {
//      attr.split(":")
//    })
////        .map(attr => {
////            if ( i ) {
////              // Key value pairs
////              attr = attr.split(':');
////
////              if ( attr.length > 1 ) {
////                attrs[attr.shift()] = attr.join(':');
////              }
////            } else {
////              attrs.string = attr;
////
////              try {
////                attrs.regex = new RegExp(attr.replace('/', '\/'), 'i'); // Escape slashes in regular expression
////              } catch (e) {
////                attrs.regex = new RegExp();
////
////                this.log(e + ': ' + attr, 'error', 'core');
////              }
////            }
////          });
//    }
//
//  }


}