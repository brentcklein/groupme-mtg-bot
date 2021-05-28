package docs.http.scaladsl

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import scala.concurrent.Future
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.Http
import scala.util.Random

case class MessageIn(text: String)
case class MessageOut(text: String, bot_id: String)

case class ScryfallResponseImageUris(normal: String)
case class ScryfallSuccessResponse(image_uris: ScryfallResponseImageUris)
case class ScryfallNotFoundResponse()
case class ScryfallFailureResponse()

object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val messageInFormat = jsonFormat1(MessageIn)
  implicit val messageOutFormat = jsonFormat2(MessageOut)

  implicit val scryfallResponseImageUrisFormat = jsonFormat1(ScryfallResponseImageUris)
  implicit val scryfallResponseFormat = jsonFormat1(ScryfallSuccessResponse)
}

object Main {

  def postMessage(message: String): Future[HttpResponse] = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    import JsonSupport._

    val botId = sys.env.get("BOT_ID") filter { _.length != 0} getOrElse("")
    
    val responseMessage = MessageOut(message, botId)
    Marshal(responseMessage).to[RequestEntity].flatMap { entity =>
      Http().singleRequest(  
        HttpRequest(
          uri = "https://api.groupme.com/v3/bots/post",
          method = HttpMethods.POST,
          entity = entity
        )
      )
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    import JsonSupport._

    val route =
      pathEndOrSingleSlash {
        post {
          entity(as[MessageIn]) { message => // will unmarshal JSON to Message
            val text = message.text
            // check to see whether there are double square brackets
            val cardTags = raw".*\[\[(.+)\]\].*".r
            // if so, grab the text inside
            text match {
              case cardTags(cardSearch) => {
                // TODO: add support for multiple card tags
                // val matches = cardTags.findAllMatchIn(text)
                // call scryfall api and get image link using name
                var query: Map[String, String] = Map()
                val setTag = raw".*\[(.+)\]".r
                cardSearch match {
                  case setTag(setName) => {
                    query += ("set" -> setName)
                    val cardTag = raw"(.*)\[.+\]".r
                    cardSearch match {
                      case cardTag(cardName) => {
                        query += ("fuzzy" -> cardName)
                      }
                    }
                  }
                  case _ => {
                    query += ("fuzzy" -> cardSearch)
                  }
                }
                val f: Future[HttpResponse] = 
                Http().singleRequest(
                  HttpRequest(
                    uri = Uri("https://api.scryfall.com/cards/named").withQuery(Uri.Query(query)),
                  )
                ).flatMap{ response =>
                  response.status match {
                    case StatusCodes.NotFound => {
                      // send snarky message
                      val snarks = List(
                        "That's not a card, dumb dumb.",
                        "You wanna check your spelling on that one?",
                        "https://i.pinimg.com/474x/ee/ac/46/eeac460d3ed617cbcca56cc69903134e.jpg",
                        "I'm pretty sure you made that one up.",
                        "I'm a bot, not a magician. Check your spelling."
                      )
                      val responseMessage = snarks(Random.nextInt(snarks.length))
                      postMessage(responseMessage)
                    }
                    case StatusCodes.OK => {
                      Unmarshal(response).to[ScryfallSuccessResponse].flatMap(marshalledResponse => {
                        // TODO: add multiple links to message, including full scryfall listing and gatherer
                        val responseMessage = marshalledResponse.image_uris.normal
                        postMessage(responseMessage)
                      })
                    }
                    case _ => {
                      // something went wrong
                      Future{HttpResponse(StatusCodes.InternalServerError)}
                    }
                  }
                }
                // return whatever response we got from posting message
                complete(f)
              }
              case _ => {
                // message did not contain card tags
                complete("No tags detected")
              }
            }
          }
        } ~ get {
          complete("Welcome to MtgBot")
        }
      }

    val port: Int = sys.env.getOrElse("PORT", "8080").toInt
    val bindingFuture = Http().newServerAt("0.0.0.0", port).bind(route)

    println(s"Server online at http://0.0.0.0:$port/")
    // StdIn.readLine() // let it run until user presses return
    // bindingFuture
    //   .flatMap(_.unbind()) // trigger unbinding from the port
    //   .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
