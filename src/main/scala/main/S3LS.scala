package main

import com.amazonaws.SDKGlobalConfiguration
import com.amazonaws.auth.AWSCredentials
import com.amazonaws.auth.profile.ProfileCredentialsProvider

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.amazonaws.services.s3.{AmazonS3Client, AmazonS3ClientBuilder}
import com.amazonaws.auth.SystemPropertiesCredentialsProvider

sealed trait Credential
case class AwsID(id: String) extends Credential
case class AwsKey(key: String) extends Credential
case class AwsRole(role: String) extends Credential

sealed trait AwsLogin
case class AwsCredential(role: AwsRole, id: AwsID, key: AwsKey) extends AwsLogin
case object InvalidLogin extends AwsLogin

object Main extends App {
  Credentials.config match {
    case Left(ex) => println(ex)
    case Right(creds) => println(creds)
  }
//  Credentials.setProfile(AwsCredential(...))
  S3Client.list
}

object S3Client {
  lazy val client = AmazonS3ClientBuilder.defaultClient()
  def list: Unit = println(client.listBuckets())
}

// Credentials is used for parsing the AWS Credentials file
// TODO: Allow it to parse other files, currently stuck at ~/.aws/credentials
object Credentials {
  val AwsAccessKeyID = "aws_access_key_id"
  val AwsSecretAccessKey = "aws_secret_access_key"

  def setProfile(creds: AwsCredential): Unit = {
    println(System.setProperty(SDKGlobalConfiguration.ACCESS_KEY_SYSTEM_PROPERTY, creds.id.id))
    println(System.setProperty(SDKGlobalConfiguration.SECRET_KEY_SYSTEM_PROPERTY, creds.key.key))
  }

  def config: Either[String, Seq[AwsLogin]] =
    fromFile match {
      case Success(file) => Right(parseInput(file))
      case Failure(ex) => Left(ex.toString)
    }

  def fromFile: Try[String] = {
    val home = System.getProperty("user.home")
    Try(scala.io.Source.fromFile(s"$home/.aws/credentials").mkString)
  }

  def parseInput(in: String): Seq[AwsLogin] =
    in.split("\n\n") filter { l =>
      l.contains(AwsAccessKeyID) && l.contains(AwsSecretAccessKey)
    } map parseLogin


  def parseLogin(in: String): AwsLogin =
    in.split("\n").foldLeft(AwsCredential(AwsRole(""), AwsID(""), AwsKey(""))) { (cred: AwsCredential, line: String) =>
      line match {
        case role: String if !role.contains("=") =>
          cred.copy(role = AwsRole(role))
        case key: String if key.contains(AwsSecretAccessKey) =>
          cred.copy(key = AwsKey(key.split(" = ")(1)))
        case id: String if id.contains(AwsAccessKeyID) =>
          cred.copy(id = AwsID(id.split(" = ")(1)))
        case _ => cred
      }
    }

}