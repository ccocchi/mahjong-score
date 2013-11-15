package com.mahjong

import com.twitter.finatra.{Controller, FinatraServer}

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
}
