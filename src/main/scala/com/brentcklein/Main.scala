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
import scala.concurrent.ExecutionContext

case class MessageIn(text: String)
case class MessageOut(text: String, bot_id: String)

case class ScryfallResponseImageUris(normal: String)
case class ScryfallSuccessResponse(scryfall_uri: String, multiverse_ids: List[Int], image_uris: ScryfallResponseImageUris)
case class ScryfallNotFoundResponse()
case class ScryfallFailureResponse()

object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val messageInFormat = jsonFormat1(MessageIn)
  implicit val messageOutFormat = jsonFormat2(MessageOut)

  implicit val scryfallResponseImageUrisFormat = jsonFormat1(ScryfallResponseImageUris)
  implicit val scryfallResponseFormat = jsonFormat3(ScryfallSuccessResponse)
}

object Main {
  import JsonSupport._

  def postMessages(messages: List[String])(implicit actorSystem: ActorSystem[_], executionContext: ExecutionContext): Future[List[HttpResponse]] = {
    Future.sequence(messages.map(postMessage))
  }

  def postMessage(message: String)(implicit actorSystem: ActorSystem[_], executionContext: ExecutionContext): Future[HttpResponse] = {
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

  def getCardSearchString(message: String): Option[(String, String)] = {
    PartialFunction.condOpt(message) {
      case s"${_}[[$searchString]]${remainingText}" => (searchString, remainingText)
    }
  }

  def getAllCardSearchStrings(message: String): Seq[String] = {
    getCardSearchString(message) match {
      case None => Seq()
      case Some((searchString, remainingText)) => Seq(searchString) ++ getAllCardSearchStrings(remainingText)
    }
  }

  def getCardNameAndOptionalSet(searchString: String): (String, Option[String]) = {
    searchString match {
      case s"${cardName}/${setName}" => (cardName.trim(), Some(setName.trim()))
      case cardName => (cardName, None)
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
            for (searchString <- getAllCardSearchStrings(message.text)) {
              // TODO: add support for multiple card tags
              // val matches = cardTags.findAllMatchIn(text)
              // call scryfall api and get image link using name
              val (cardName, setName) = getCardNameAndOptionalSet(searchString)
              val query = Map("fuzzy" -> cardName) ++ setName.map(("set" -> _))

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
                      postMessage(marshalledResponse.image_uris.normal)
                    })
                  }
                  case _ => {
                    // something went wrong
                    Future{HttpResponse(StatusCodes.InternalServerError)}
                  }
                }
              }
            }
            // return whatever response we got from posting message
            complete(StatusCodes.NoContent)
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
