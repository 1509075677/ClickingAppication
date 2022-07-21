package clicker.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import clicker.game.GameActor
import clicker.{BuyEquipment, Click, GameState, Update, UpdateGames}
import com.corundumstudio.socketio.listener.DataListener
import com.corundumstudio.socketio.{AckRequest, Configuration, SocketIOClient, SocketIOServer}

import scala.io.Source

class ClickerServer(val configuration: String) extends Actor {

  var uTOs: Map[ActorRef, SocketIOClient] = Map()
  var sTOu: Map[SocketIOClient, ActorRef] = Map()

  val config: Configuration = new Configuration {
    setHostname("localhost")
    setPort(8080)
  }

  val server: SocketIOServer = new SocketIOServer(config)
  server.start()

  server.addEventListener("startGame",classOf[String],new RegisterListener(this))
  server.addEventListener("click",classOf[Nothing], new RegisterListener2(this))
  server.addEventListener("buy",classOf[String], new RegisterListener3(this))

  override def receive: Receive = {
    case UpdateGames=>
      for (u<-uTOs.toList) {
        u._1 ! Update
      }
    case g:GameState=>
      uTOs(sender).sendEvent("gameState",g.gameState)

  }

  override def postStop(): Unit = {
    println("stopping server")
    server.stop()
  }
}

class RegisterListener(server:ClickerServer) extends DataListener[String] {
  override def onData(socket: SocketIOClient, username: String, ackRequest: AckRequest): Unit = {
    val actor=server.context.actorOf(Props(classOf[GameActor],username,server.configuration))
    server.uTOs=server.uTOs + (actor -> socket)
    server.sTOu=server.sTOu + (socket -> actor)
    socket.sendEvent("initialize",server.configuration)
  }
}

class RegisterListener2(server:ClickerServer) extends DataListener[Nothing] {
  override def onData(socket: SocketIOClient, username: Nothing, ackRequest: AckRequest): Unit = {
    server.sTOu(socket) ! Click
  }
}
class RegisterListener3(server:ClickerServer) extends DataListener[String] {
  override def onData(socket: SocketIOClient, username: String, ackRequest: AckRequest): Unit = {
    server.sTOu(socket) ! BuyEquipment(username)
  }
}

object ClickerServer {

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher

    import scala.concurrent.duration._

    val configuration: String = Source.fromFile("codeConfig.json").mkString

    val server = actorSystem.actorOf(Props(classOf[ClickerServer], configuration))

    actorSystem.scheduler.schedule(0.milliseconds, 100.milliseconds, server, UpdateGames)
  }
}
