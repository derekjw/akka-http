package net.fyrie.http

import akka.actor._
import akka.util.{ ByteString, ByteStringBuilder }
import java.net.InetSocketAddress

class HttpServer(port: Int) extends Actor {
  import HttpIteratees._

  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.system)

  override def preStart {
    IO listen new InetSocketAddress(port)
  }

  def receive = {

    case IO.NewClient(server) ⇒
      val socket = server.accept()
      state(socket) flatMap { _ ⇒
        IO repeat {
          for {
            request ← readRequest
          } yield {
            val rsp = request match {
              case Request("GET", "ping" :: Nil, _, headers, _) ⇒
                OKResponse(ByteString("<p>pong</p>"),
                  request.headers.exists { case Header(n, v) ⇒ n.toLowerCase == "connection" && v.toLowerCase == "keep-alive" })
              case req ⇒
                OKResponse(ByteString("<p>" + req.toString + "</p>"),
                  request.headers.exists { case Header(n, v) ⇒ n.toLowerCase == "connection" && v.toLowerCase == "keep-alive" })
            }
            socket write OKResponse.bytes(rsp).compact
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
  val HT = ByteString("\t")
  val CRLF = ByteString("\r\n")
  val COLON = ByteString(":")
  val PERCENT = ByteString("%")
  val PATH = ByteString("/")
  val QUERY = ByteString("?")

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

  def decodeHeader(bytes: ByteString): String = bytes.decodeString("US-ASCII").trim

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
      value ← IO takeUntil CRLF flatMap readMultiLineValue
    } yield Header(decodeHeader(name), decodeHeader(value))

  def readMultiLineValue(initial: ByteString): IO.Iteratee[ByteString] = IO peek 1 flatMap {
    case SP ⇒ IO takeUntil CRLF flatMap (bytes ⇒ readMultiLineValue(initial ++ bytes))
    case _  ⇒ IO Done initial
  }

  val readRequestLine =
    for {
      meth ← IO takeUntil SP
      uri ← readRequestURI
      httpver ← IO takeUntil CRLF
    } yield (decodeHeader(meth), uri, decodeHeader(httpver))

  val readRequestURI = IO peek 1 flatMap {
    case PATH ⇒ readPath
    case _    ⇒ sys.error("Not Implemented")
  }

  // TODO: Optimize path reading
  def readPath = {
    def step(segments: List[String]): IO.Iteratee[List[String]] = IO peek 1 flatMap {
      case PATH ⇒ IO take 1 flatMap (_ ⇒ readPathSegment flatMap (segment ⇒ step(segment :: segments)))
      case _ ⇒ segments match {
        case "" :: rest ⇒ IO Done rest.reverse
        case _          ⇒ IO Done segments.reverse
      }
    }
    step(Nil)
  }

  val alpha = Set.empty ++ ('a' to 'z') ++ ('A' to 'Z') map (_.toByte)
  val digit = Set.empty ++ ('0' to '9') map (_.toByte)
  val hexdigit = digit ++ (Set.empty ++ ('a' to 'f') ++ ('A' to 'F') map (_.toByte))
  val subdelim = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=') map (_.toByte)
  val pathchar = alpha ++ digit ++ subdelim ++ (Set(':', '@') map (_.toByte))

  def readPathSegment: IO.Iteratee[String] = for {
    str ← IO takeWhile pathchar map (_.utf8String)
    pchar ← IO peek 1 map (_ == PERCENT)
    segment ← if (pchar) readPChar flatMap (ch ⇒ readPathSegment map (str + ch + _)) else IO Done str
  } yield segment

  def readPChar = IO take 3 map {
    case Seq('%', rest @ _*) if rest forall hexdigit ⇒
      java.lang.Integer.parseInt(rest map (_.toChar) mkString, 16).toChar
  }

}
object OKResponse {
  import HttpIteratees.CRLF

  def bytes(rsp: OKResponse) = {
    new ByteStringBuilder ++=
      okStatus ++= CRLF ++=
      contentType ++= CRLF ++=
      cacheControl ++= CRLF ++=
      date ++= ByteString(new java.util.Date().toString) ++= CRLF ++=
      server ++= CRLF ++=
      contentLength ++= ByteString(rsp.body.length.toString) ++= CRLF ++=
      connection ++= (if (rsp.keepAlive) keepAlive else close) ++= CRLF ++= CRLF ++= rsp.body result
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
case class Request(meth: String, uri: List[String], httpver: String, headers: List[Header], body: Option[ByteString])
case class Header(name: String, value: String)

object Main extends App {
  val port = Option(System.getenv("PORT")) map (_.toInt) getOrElse 8080
  val config = com.typesafe.config.ConfigFactory.parseString("""
      akka {
        actor {
          default-dispatcher {
            core-pool-size-min = 4
            core-pool-size-factor = 0.5
            throughput = 2147483647
          }
        }
      }
      """)

  val system = ActorSystem("TestSystem", config)
  val server = system.actorOf(Props(new HttpServer(port)))
}
