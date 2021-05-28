package docs.http.scaladsl

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import scala.concurrent.Future
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.unmarshalling.Unmarshal

case class MessageIn(text: String)
case class MessageOut(text: String, bot_id: String)

case class ScryfallResponseImageUris(normal: String)
case class ScryfallResponse(image_uris: ScryfallResponseImageUris)

object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val messageInFormat = jsonFormat1(MessageIn)
  implicit val messageOutFormat = jsonFormat2(MessageOut)

  implicit val scryfallResponseImageUrisFormat = jsonFormat1(ScryfallResponseImageUris)
  implicit val scryfallResponseFormat = jsonFormat1(ScryfallResponse)
}

object Main {

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
            // if so, grab the text inside
            // call scryfall api and get image link using name
            val f: Future[HttpResponse] = 
            Http().singleRequest(
              HttpRequest(
                uri = Uri("https://api.scryfall.com/cards/named").withQuery(Uri.Query(("fuzzy" -> text))),
              )
            )
            .flatMap{ response =>
              Unmarshal(response).to[ScryfallResponse]
            }.flatMap{scryfallResponse =>
              // post link text to thread with card name
              val responseMessage = MessageOut(scryfallResponse.image_uris.normal, "c05d3805b3d555a1837209bab8")
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

            // if no card found on scryfall, post snarky message to thread
            complete(f)
          }
        } ~ get {
          complete("hello")
        }
      }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
