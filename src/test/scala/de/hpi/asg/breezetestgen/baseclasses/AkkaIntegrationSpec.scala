package de.hpi.asg.breezetestgen.baseclasses

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}

import concurrent.duration._


abstract class AkkaIntegrationSpec(_system: ActorSystem)
  extends TestKit(_system) with FlatSpecLike with ImplicitSender with BeforeAndAfterAll {
  def this(name: String) = this(ActorSystem(name))

  implicit val timeout = Timeout(7 seconds)

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
}
