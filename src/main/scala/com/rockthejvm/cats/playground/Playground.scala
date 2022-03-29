package com.rockthejvm.cats.playground

import cats.Eval

object Playground:

    val meaningOfLife = Eval.later {
        println("The meaning of life")
        42
    }

    @main def TryPlayground() = 
        println(meaningOfLife.value)