package net.fyrie.http

import akka.actor._
import akka.util.ByteString

class HttpServer(port: Int) extends Actor {
  import HttpIteratees._

  val state = IO.IterateeRef.Map.async[IO.Handle]()

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
            val rsp = OKResponse(ByteString("<p>Hello World!</p>") /* <p>Current connections: %4d</p>" format state.size) */ ,
              request.headers.exists { case Header(n, v) ⇒ n.toLowerCase == "connection" && v.toLowerCase == "keep-alive" })
            socket write OKResponse.bytes(rsp)
            if (!rsp.keepAlive) socket.close()
          }
        }
      }

    case IO.Read(socket, bytes) ⇒
      state(socket)(IO Chunk bytes)

    case IO.Closed(socket, cause) ⇒
      state(socket)(IO EOF None)
      state -= socket

  }

}

object HttpIteratees {

  val SP = ByteString(" ")
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

  val readHeader =
    for {
      name ← IO takeUntil COLON
      value ← IO takeUntil CRLF
    } yield Header(name.utf8String.trim, value.utf8String.trim)

  val readRequestLine =
    for {
      meth ← IO takeUntil SP
      uri ← IO takeUntil SP
      httpver ← IO takeUntil CRLF
    } yield (meth.utf8String, uri.utf8String, httpver.utf8String)

}
object OKResponse {
  import HttpIteratees.CRLF

  def bytes(rsp: OKResponse) = {
    okStatus ++ CRLF ++
      contentType ++ CRLF ++
      cacheControl ++ CRLF ++
      date ++ ByteString(new java.util.Date().toString) ++ CRLF ++
      server ++ CRLF ++
      contentLength ++ ByteString(rsp.body.length.toString) ++ CRLF ++
      connection ++ (if (rsp.keepAlive) keepAlive else close) ++ CRLF ++ CRLF ++ rsp.body
  }

  val okStatus = ByteString("HTTP/1.1 200 OK")
  val contentType = ByteString("Content-Type: text/html; charset=utf-8")
  val cacheControl = ByteString("Cache-Control: no-cache")
  val date = ByteString("Date: ")
  val server = ByteString("Server: Fyrie")
  val contentLength = ByteString("Content-Length: ")
  val connection = ByteString("Connection: ")
  val keepAlive = ByteString("Keep-Alive")
  val close = ByteString("Close")

}
case class OKResponse(body: ByteString, keepAlive: Boolean)
case class Request(meth: String, uri: String, httpver: String, headers: List[Header], body: Option[ByteString])
case class Header(name: String, value: String)

object Main extends App {
  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  val server = Actor.actorOf(new HttpServer(port)).start
}
