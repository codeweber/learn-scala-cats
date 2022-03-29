package com.rockthejvm.cats.abstractMath

object UsingMonads:

    import cats.Monad 

    val monadList = Monad[List]

    val aSimpleList = monadList.pure(4)
    val anExtendedList = monadList.flatMap(aSimpleList)( x => List(x,x+1) )


    val aManualEither: Either[String, Int] = Right(42)

    type LoadingOr = [T] =>> Either[String, T]
    val loadingMonad = Monad[LoadingOr]

    val anEither = loadingMonad.pure(45)

    //Exercise 1
    // Inputs below
    case class Connection(host: String, port: String)

    trait HttpService[M[_]]:
        def getConnection(cfg: Map[String, String]): M[Connection]
        def issueRequest(connection: Connection, payload: String): M[String]

    // TODO
    // provide a real implementation of HttpService for some monadic type, e.g. Option, Try, Future, etc
    object HttpServiceOption extends HttpService[Option]:
        override def getConnection(cfg: Map[String, String]): Option[Connection] =
            for 
                host <- cfg.get("host")
                port <- cfg.get("port")
            yield Connection(host, port)

        override def issueRequest(connection: Connection, payload: String): Option[String] =
            if payload.length < 20 then
                Some("Payload accepted")
            else 
                None
    
    object HttpServiceLoadingOr extends HttpService[LoadingOr]:

        override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = 
            val connOption = 
                for 
                    host <- cfg.get("host")
                    port <- cfg.get("port")
                yield Connection(host, port)

            connOption match
                case Some(conn) => Right(conn) 
                case None => Left("Config details incorrect")

        override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
            if payload.length < 20 then 
                Right("Payload accepted")
            else
                Left("Payload too long")


    val config = Map(
        "host" -> "localhost",
        "port" -> "4040"
    )

    // define a function to get a response

    import cats.syntax.flatMap.* 
    import cats.syntax.functor.*

    def getResponse[M[_]:Monad](service: HttpService[M], payload: String): M[String] =
        for 
            conn <- service.getConnection(config)
            response <- service.issueRequest(conn, payload)
        yield response


    @main def TryUsingMonads() =

        println(getResponse(HttpServiceOption, "test short"))
        
