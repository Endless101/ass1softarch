import akka.*
import akka.stream.*
import akka.stream.scaladsl.*
import akka.actor.ActorSystem
import akka.util.ByteString

import java.nio.file.StandardOpenOption.*
import ApiRequest.{Package, PackageResponse, apiRequest, processApiResponse}
import scala.concurrent.{Await, ExecutionContext, Future}
import java.io.File
import java.nio.file.Paths
import scala.concurrent.duration.*

object Assigment extends App{
  // Setup code
  implicit val actorSystem: ActorSystem = ActorSystem("Assigment")
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher
  val filePath = "src/main/resources" 
  // First Source 
  val source = FileIO.fromPath(Paths.get(s"$filePath/packages.txt.gz"))
  // Unzip the file and put in on a flow
  val flowExtractPackageNames: Flow[ByteString, String, NotUsed] = Compression.gunzip().mapConcat(_.utf8String.split("\n"))
  // Two Flows that makes an API request and puts the relevant information in a scala object
  // Uses throttle and the backpressure overflow strategy.
  val apiBuffer = Flow[String].map {
    packageName =>
      Package(packageName)
  }.buffer(25, OverflowStrategy.backpressure).throttle(1,2.seconds)

  val apiFlow: Flow[Package, PackageResponse, NotUsed] = Flow[Package].map{
    pack =>
      processApiResponse(apiRequest(pack))

  }
  // Filter flow that will get duplicated later in a custom flow
    val packageFilterFlow: Flow[PackageResponse, PackageResponse, NotUsed] = Flow[PackageResponse]
    .filter(pack=> pack.githubMetrics.starsCount >20)
    .filter(pack=>pack.evaluationMetrics.tests > 0.5)
    .filter(pack=>pack.githubMetrics.contributor.map(contributor => contributor.commitCount).sortWith(_ > _).take(3).sum >=150)
    .filter(pack => {
      val version = pack.metadata.version
      val majorRelease = version.substring(1,version.indexOf(".")).toInt
      majorRelease >=2
    })
    // Custom Flow that processes and filters packages in parallel.
    // Broadcasts message to three independent filters.
  val parallelFilter =
    Flow.fromGraph(GraphDSL.create() {
    implicit builder =>
      import GraphDSL.Implicits._
aln
      val broadcaster = builder.add(Broadcast[PackageResponse](3))
      val merge = builder.add(Merge[PackageResponse](3))
      val pipe1, pipe2, pipe3 = builder.add(packageFilterFlow)

      balancer.out(0) ~> pipe1 ~> merge.in(0)
      balancer.out(1) ~> pipe2 ~> merge.in(1)
      balancer.out(2) ~> pipe3 ~> merge.in(2)
aln
      FlowShape(broadcaster.in,merge.out)

  // Flow that converts each package to a Bytestring
  val flowOut = Flow[PackageResponse].map(pack=>s"${pack.toString}\n" ).map(s=>ByteString(s))
  // Sinks that print to the console and write to a file.
  val sinkIO: Sink[ByteString ,Future[IOResult]] = FileIO.toPath(Paths.get(s"$filePath/filtered-packages.txt"), Set(CREATE,WRITE, APPEND))
  val sinkPrint = Sink.foreach[ByteString](bs=>print(bs.utf8String))
  val combinedSink = Sink.combine(sinkIO,sinkPrint)(Broadcast[ByteString](_))
  // Final combined graph
  val g =  source
    .via(flowExtractPackageNames)
    .via(apiBuffer)
    .via(apiFlow)
    .via(parallelFilter)
    .via(flowOut)
    .alsoToMat(combinedSink)(Keep.right)
    g.run().onComplete(_ => actorSystem.terminate())
    //TODO FIX WRITING TO FILE AND SHOWING TO SCREEN


}
