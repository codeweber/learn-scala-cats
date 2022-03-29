package com.rockthejvm.cats.intro

import scala.util.{Try, Success, Failure}

object Essentials:

    val theUnit: Unit = ()

    val anAttempt = Try {
        val x = 1
        1 / 0 
    }
    

    @main def TryEssentials() =

        anAttempt match 
            case Success(a) => println("Success!")
            case Failure(e) => println("Failure! Reason: " + e.getMessage)
