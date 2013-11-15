package com.mahjong

import com.twitter.finatra.{FileService, AppService, Config, Controller, FinatraServer}
import com.twitter.finagle.tracing.{NullTracer, Tracer}
import com.twitter.ostrich.admin.{ServiceTracker, RuntimeEnvironment}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{RichHttp, Http, Response, Request}
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.conversions.storage._
import java.net.InetSocketAddress
import scala.util.Properties

object App {
  def main(args: Array[String]) = {
    val app = new App
    app.start()
  }
}

class MahjongController extends Controller {
  post("/hand") { request =>
    val handStr = request.params.get("hand").get
    val combinations = Mahjong.handToCombinations(handStr)

    val hand = new Hand(combinations)
    val sc = hand.winningScoringHands

    val lis = sc.map { case(h, s) =>
      """<div class="row"><div class="col-md-10">%s</div><div class="col-md-2 score">%d</div></div>""".format(h.name, s)
    }.mkString

    val score = sc.map(_._2).sum

    val scoreClass = if (score < 8) "lost" else "win"
    val scoreStr =
      """<div class="row %s"><div class="col-md-10">Final score</div><div class="col-md-2 score">%d</div></div>""".format(scoreClass, score)

    val body = """<div class="response">""" + lis + scoreStr + "</div>"

    render.html(body).toFuture
  }


}

class App extends FinatraServer {
  register(new MahjongController)

  override def start(
             tracer:     Tracer              = NullTracer,
             runtimeEnv: RuntimeEnvironment  = new RuntimeEnvironment(this)) {

    ServiceTracker.register(this)

    initLogger()

    val appService  = new AppService(controllers)
    val fileService = new FileService

    addFilter(fileService)

    val port = Properties.envOrElse("PORT", Config.get("port")).toInt
    val service: Service[Request, Response] = allFilters(appService)
    val http = Http().maxRequestSize(Config.getInt("max_request_megabytes").megabyte)

    val codec = new RichHttp[Request](http)

    val server = ServerBuilder()
      .codec(codec)
      .bindTo(new InetSocketAddress(port))
      .tracer(tracer)
      .name(Config.get("name"))
      .build(service)

    logger.info("process %s started on %s", pid, port)

    println("finatra process " + pid + " started on port: " + port.toString)
    println("config args:")
    Config.printConfig()

  }

}
