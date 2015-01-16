package feh.util

import akka.actor.ActorSystem

object Tests {
  lazy val asys = ActorSystem.create("Tests")
}
