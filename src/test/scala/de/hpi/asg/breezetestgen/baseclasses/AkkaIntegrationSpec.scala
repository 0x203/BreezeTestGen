package de.hpi.asg.breezetestgen.baseclasses

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}


abstract class AkkaIntegrationSpec(_system: ActorSystem)
  extends TestKit(_system) with FlatSpecLike with ImplicitSender with BeforeAndAfterAll {
  def this(name: String) = this(ActorSystem(name))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
}
