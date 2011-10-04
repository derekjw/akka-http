package net.fyrie.http

import akka.actor._
import akka.util.ByteString

class HttpServer(port: Int) extends Actor {

  val state = IO.IterateeRef.Map[IO.Handle]()

  override def preStart {
    IO listen (IOManager.global, port)
  }

  def receive = {

    case IO.NewClient(server) ⇒
      val socket = server.accept()
      state(socket) flatMap { _ ⇒
        IO repeat {
          for {
            request ← readRequest
          } yield {
            val keepAlive = request.headers.exists { case Header(n, v) ⇒ n.toLowerCase == "connection" && v.toLowerCase == "keep-alive" }
            val msg = ByteString("Hello World! Current connections: %04d" format state.size)
            socket write (ByteString("HTTP/1.1 200 OK") ++ CRLF ++
              ByteString("Content-Type: text/html; charset=utf-8") ++ CRLF ++
              ByteString("Cache-Control: no-cache") ++ CRLF ++
              ByteString("Date: " + (new java.util.Date().toString)) ++ CRLF ++
              ByteString("Server: Fyrie") ++ CRLF ++
              ByteString("Content-Length: " + msg.length) ++ CRLF ++
              (if (keepAlive) (ByteString("Connection: Keep-Alive") ++ CRLF ++ CRLF) else CRLF) ++ msg)
            if (!keepAlive) socket.close()
          }
        }
      }

    case IO.Read(socket, bytes) ⇒
      state(socket)(IO Chunk bytes)

    case IO.Closed(socket, cause) ⇒
      state(socket)(IO EOF None)
      state -= socket

  }

  val SP = ByteString(" ")
  val CR = ByteString("\r")
  val LF = ByteString("\n")
  val CRLF = ByteString("\r\n")
  val COLON = ByteString(":")

  def readRequest =
    for {
      requestLine ← readRequestLine
      (meth, uri, httpver) = requestLine
      headers ← readHeaders
      body ← readBody(headers)
    } yield Request(meth, uri, httpver, headers, body)

  def readBody(headers: List[Header]) =
    if (headers.exists(header ⇒ header.name == "Content-Length" || header.name == "Transfer-Encoding"))
      IO.takeAll map (Some(_))
    else
      IO Done None

  def readHeaders = {
    def step(found: List[Header]): IO.Iteratee[List[Header]] = {
      IO peek 2 flatMap {
        case CRLF ⇒ IO takeUntil CRLF flatMap (_ ⇒ IO Done found)
        case _    ⇒ readHeader flatMap (header ⇒ step(header :: found))
      }
    }
    step(Nil)
  }

  def readHeader =
    for {
      name ← IO takeUntil COLON
      value ← IO takeUntil CRLF
    } yield Header(name.utf8String.trim, value.utf8String.trim)

  def readRequestLine =
    for {
      meth ← IO takeUntil SP
      uri ← IO takeUntil SP
      httpver ← IO takeUntil CRLF
    } yield (meth.utf8String, uri.utf8String, httpver.utf8String)

}

case class Request(meth: String, uri: String, httpver: String, headers: List[Header], body: Option[ByteString])
case class Header(name: String, value: String)

object Main extends App {
  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  val server = Actor.actorOf(new HttpServer(port)).start
}
