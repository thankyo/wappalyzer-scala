package com.github.thankyo.wappalyzer

import play.api.libs.ws.StandaloneWSClient

import scala.concurrent.{ExecutionContext, Future}

trait WappalyzerService {

  def analyze(url: String): Future[Seq[App]]

}

case class SimpleWappalyzerService(client: StandaloneWSClient, config: WappalyzerConf)(implicit ec: ExecutionContext) extends WappalyzerService {

  override def analyze(url: String): Future[Seq[App]] = {
    client.url(url).get().map(config.analyze(_))
  }

}
