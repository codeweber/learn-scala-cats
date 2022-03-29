package com.rockthejvm.cats.abstractMath

import cats.data.EitherT
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object MonadTransformers:

    
    // Execution context
    given ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(5))

    // Exercise

    val bandwidths = Map[String, Int](
        "1" -> 50,
        "2" -> 300,
        "3" -> 170
    )

    type AsyncResponse[T] = EitherT[Future, String, T]

    def getBandwidth(s: String): AsyncResponse[Int] =
        bandwidths.get(s) match 
            case None => EitherT(Future(Left("Server not found")))
            case Some(b) => EitherT(Future(Right(b)))

    def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
        for
            b1 <- getBandwidth(s1)
            b2 <- getBandwidth(s2)
        yield
            b1 + b2 > 250

    def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =

        val failStringPrefix = s"Servers $s1 and $s2 cannot cope with the incoming spike: "
        canWithstandSurge(s1,s2).transform {
            case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike")
            case Left(reason) => Left(failStringPrefix + reason)
            case Right(false) => Left(failStringPrefix + "Insufficient bandwidth")
        }


    @main def TryMonadTransformers1() =

        generateTrafficSpikeReport("1", "3").value.foreach(println)
        generateTrafficSpikeReport("1", "2").value.foreach(println)
        generateTrafficSpikeReport("1", "5").value.foreach(println)
    
        
