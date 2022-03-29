package com.rockthejvm.cats.dataManipulation

import cats.data.Reader

object Readers:
  
    // Reader[A,B] is defined by a function A => B
    // the type [B] =>> Reader[A,B] is a Monad, with pure, flatMap and map

    // Example
    case class Config(dbUsername: String, emailReplyTo: String)
    val conf = Config(dbUsername = "dean", emailReplyTo = "donotreply@foo.bar")

    case class DbConn(name: String):
        def getOrderStatus(orderId: Long): String = if orderId % 2 == 0 then "dispatched" else "ready for dispatch"
        def getLastOrderId(username: String): Long = username.length.toLong

    
    val dbReader = Reader[Config, DbConn]( conf => DbConn(conf.dbUsername))

    def getLastOrderStatus(username: String): String = 
        
        val statusReader = 
            for
                orderid <- dbReader.map(_.getLastOrderId(username))
                status  <- dbReader.map(_.getOrderStatus(orderid))
            yield status

        statusReader.run(conf)


    @main def TryReader() =

        println(getLastOrderStatus("monkey"))
        println(getLastOrderStatus("olive"))
    


    // Exercise
    case class EmailService(emailReplyTo: String):
        def sendMail(address: String, contents: String): String = s"From: $emailReplyTo; To: $address >>> $contents"

    def emailUser(username: String, userEmail: String) =
        // fetch last order status
        // email with your order has the status...
        val emailServiceReader = Reader[Config, EmailService](conf => EmailService(conf.emailReplyTo))

        val emailReader =
            for
                orderid <- dbReader.map(_.getLastOrderId(username))
                status  <- dbReader.map(_.getOrderStatus(orderid))
                emailService <- emailServiceReader
            yield emailService.sendMail(userEmail, s"Your last order (ID: $orderid) has the status: $status")

        emailReader.run(conf)
