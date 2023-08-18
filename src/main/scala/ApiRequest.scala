// Requests a package to the API and returns the information about that package
// Change this code at your convenience

package scala

import scala.collection.mutable
object ApiRequest:
  // Helper classes to store data
  case class Contributor(username: String, commitCount: Int)
  case class Metadata(version: String)
  case class EvaluationMetrics(tests: Double) {
    override def toString: String = {
      s"Tests: $tests\n"
    }
  }
  case class GithubMetrics(starsCount: Int, forksCount: Int,subscriberCount: Int, contributor: List[Contributor]) {
    override def toString: String = {
      s"Star Count: ${starsCount.toString}\n Sum of top-3 contributors' commits: ${contributor.map(contributor=> contributor.commitCount)
        .sortWith(_ > _)
        .take(3)
        .sum}"
    }
  }

  // Main classes that represent packages

  case class Package(name: String)
  case class PackageResponse(name: String, githubMetrics: GithubMetrics, evaluationMetrics: EvaluationMetrics, metadata: Metadata) {
    override def toString: String = s"$name\n $githubMetrics\n $evaluationMetrics"
  }

  // Process an API request into a PackageResponse object which contains all the relevant information
  // for the filtering process 

  def processApiResponse(response: ujson.Value.Value): PackageResponse =
    val map = response("collected")
    val metadata = map("metadata")
    val name = metadata.obj("name").toString
    val metadataObject = Metadata(metadata.obj("version").toString)
    val githubMetrics = map("github")
    val contributors = githubMetrics("contributors").arr.map(contributor=> Contributor(contributor("username").toString,contributor("commitsCount").toString.toInt))
   // val starsCount: Int = githubMetrics("starscount")
    val githubMetricsObject = GithubMetrics(githubMetrics("starsCount").toString.toInt, githubMetrics("forksCount").toString.toInt, githubMetrics("subscribersCount").toString.toInt,contributors.toList)
    val evaluationMetrics = response("evaluation")
    val evaluationMetricsObject = EvaluationMetrics(
      evaluationMetrics("quality")("tests").toString.toDouble)
    PackageResponse(name, githubMetricsObject, evaluationMetricsObject, metadataObject)
    
    

  def apiRequest(packageObj: Package): ujson.Value.Value =
    val url = s"https://api.npms.io/v2/package/${packageObj.name}"
    val response = requests.get(url)
    val json = ujson.read(response.data.toString)
    json


