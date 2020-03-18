//#full-example
package com.example


import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello

//This QuickStart shows three ways you can implement a Typed Actor

/**
 *  Simple Greeter Actor  which implements Behaviour of actor using receive method
 *  It Receives commands of type [[Greeter.Greet]] and implements Behavior in apply method
 **/
object Greeter {

  //commands
  sealed trait Greet {val whom: String}
  final case class GreetWithBot(override val whom: String, replyTo: ActorRef[Greeted]) extends Greet
  final case class SimpleGreet(override val whom: String) extends Greet

  //response
  final case class Greeted(whom: String, from: ActorRef[Greet])

  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>
    message match {
      case GreetWithBot(whom: String, replyTo: ActorRef[Greeted]) =>
        //Invokes the GreeterBot
        replyTo ! Greeted(message.whom, context.self)
        Behaviors.same

      case SimpleGreet(whom: String) =>
        context.log.info("Simple Greeting Hello {}!", whom)
        Behaviors.same
    }
  }
}

/**
 *  Greeter Bot receives [[Greeter.Greeted]] Message and Prints Greeting message to log max number of times
 **/
object GreeterBot {

  def apply(max: Int): Behavior[Greeter.Greeted] = {
    bot(0, max)
  }

  private def bot(greetingCounter: Int, max: Int): Behavior[Greeter.Greeted] =
    Behaviors.receive { (context, message) =>
      val n = greetingCounter + 1
      context.log.info("Automated Bot Greeting {} for {}", n, message.whom)
      if (n == max) {
        Behaviors.stopped
      } else {
        message.from ! Greeter.GreetWithBot(message.whom, context.self)
        bot(n, max)
      }
    }
}

/**
 *  This is a Main actors which receives [[GreeterMain.SayHello]] request
 *  It does all the necessary setup in [[Behaviors.setup]] like creation of [[Greeter]] actor
 *  And starts receiving message using [[Behaviors.receiveMessage]] method
 **/
object GreeterMain {

  final case class SayHello(name: String)

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      //#create-actors
      val greeter = context.spawn(Greeter(), "greeter")
      //#create-actors

      Behaviors.receiveMessage { message =>

        //#create-actors for automated bot greeting max number of times
        val replyTo = context.spawn(GreeterBot(max = 3), message.name)
        //#create-actors
        greeter ! Greeter.GreetWithBot(message.name, replyTo)

        //simple greeting
        greeter ! Greeter.SimpleGreet(message.name)
        Behaviors.same
      }
    }
}

/**
 *  Creates an ActorSystem of Type [[GreeterMain.SayHello]]
 *  And sends [[GreeterMain.SayHello]] message to it
 **/
object AkkaQuickstart extends App {
  //#actor-system
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")
  //#actor-system

  //#main-send-messages
  greeterMain ! SayHello("Charles")
  //#main-send-messages
}
