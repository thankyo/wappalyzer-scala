package com.github.thankyo.wappalyzer

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import play.api.libs.json.Json
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.concurrent.duration._

class WappalyzerServiceSpec extends Specification {

  val conf: WappalyzerConf = {
    Json.parse(
      Source.fromURL(getClass.getResource("/testApps.json")).mkString
    ).as[WappalyzerConf]
  }

  def withClient(test: WappalyzerService => Fragment) = {
    // Create Akka system for thread and streaming management
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val service = SimpleWappalyzerService(StandaloneAhcWSClient(), conf)

    val res = test(service)

    system.terminate()
    materializer.shutdown()

    res
  }

  withClient(service => {
    "Test wordpress" in {
      val res = Await.result(service.analyze("https://wordpress.com/"), 1 minute)
      res.map(_.id) shouldEqual List.empty
    }
  })

}
