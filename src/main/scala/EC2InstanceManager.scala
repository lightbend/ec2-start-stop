package com.typesafe.jenkins

import com.amazonaws.auth._
import com.amazonaws.services.ec2._
import com.amazonaws.services.ec2.model._
import com.amazonaws.regions._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

object EC2InstanceManager {
  def exponentialBackoff(timeoutMinutes: Int = 3, retries: Int = 3)(oneTry: () => Unit): Stream[Unit] = {
    def currentSeconds = System.nanoTime/1000000000L
    val endTimeSeconds = currentSeconds + (timeoutMinutes * 60L)
    // Had some fun and came up with this beautiful series: 1, 3, 9, 25, 68, 186, 506, 1376, 3742, ...
    def genSeconds = Stream.iterate(1)(i => i + 1).map(i => Math.pow(Math.E, i)).map(_.toLong/16).filter(_ > 0)
    val myTry =
      if (retries == 0) Stream.empty
      else genSeconds map { s => Math.min(s, endTimeSeconds - currentSeconds) * 1000 } takeWhile (_ > 0) map { Thread.sleep }

    Stream(oneTry()).append(myTry.append(exponentialBackoff(timeoutMinutes, retries - 1)(oneTry)))
  }

  /** Start named instance, wait for status checks to report it's up and running.
   * 
   * `false` if could not start within specified `timeoutMinutes`
   */
  def startByNameBlocking(name: String, timeoutMinutes: Int = 3, retries: Int = 3)(implicit logger: String => Unit): Boolean = {
    def doStart() = try {
      val instances = instanceIdsByName(name)
      logger(s"[EC2] Starting instance '$name'... Matching instances: ${instances.mkString(",")}")
      start(instances)
    } catch { case e@NonFatal(_) =>
      logger(s"[EC2] Failure while starting instance '$name':\n$e")
    }

    def done = try {
      val status = statusCheck(instanceIdsByName(name))
      logger(s"[EC2] Waiting... Current status check: ${status.mkString(",")}")
      status.exists(_.getStatus == "ok")
    } catch { case e@NonFatal(_) =>
      logger(s"[EC2] Failure while checking instance state of '$name':\n$e")
      false
    }

    exponentialBackoff(timeoutMinutes)(doStart).find(_ => done).nonEmpty
  }

  /** Stop named instance.
   * 
   * `false` if something went wrong
   */
  def stopByName(name: String, timeoutMinutes: Int = 15): Boolean =
    try {
      println(s"Stopping $name...")
      stop(instanceIdsByName(name)) // idempotent :-)
      println(s"Stopped $name")
      true
    } catch { case e@NonFatal(_) =>
      println(s"Failed to stop $name due to $e")
      false
    }
  
  def instanceIdsByName(name: String)(implicit logger: String => Unit = println): List[String] = instanceByName(name).map(_.getInstanceId).toList
 
  def statusCheck(instanceIds: List[String]): Seq[InstanceStatusSummary] =
   for (instStatus <- describeInstanceStatus(instanceIds).getInstanceStatuses.asScala)
     yield instStatus.getInstanceStatus

  def instanceState(instanceIds: List[String]): Seq[InstanceState] =
    for (instStatus <- describeInstanceStatus(instanceIds).getInstanceStatuses.asScala)
    yield instStatus.getInstanceState


  private val ec2 = {
    val _ec2 = new AmazonEC2Client(new InstanceProfileCredentialsProvider())
    _ec2.setEndpoint(Regions.getCurrentRegion.getServiceEndpoint(ServiceAbbreviations.EC2))
    _ec2
  }

  def instanceByName(name: String)(implicit logger: String => Unit = println): Option[Instance] = {
    val instances =
      for {
        reservation <- ec2.describeInstances.getReservations.asScala
        instance    <- reservation.getInstances.asScala
        //    if instance.getState.getName != "Terminated"
        tag <- instance.getTags.asScala
        if tag.getKey == "Name" && tag.getValue == name
      } yield instance

    if (instances.nonEmpty && instances.tail.nonEmpty) logger(s"Multiple instances found! $instances")
    instances.headOption
  }

  def start(instanceIds: List[String]) =
    ec2.startInstances(new StartInstancesRequest(instanceIds.asJava)).getStartingInstances.asScala

  def stop(instanceIds: List[String]) =
    ec2.stopInstances(new StopInstancesRequest(instanceIds.asJava))

  def describeInstanceStatus(instanceIds: List[String]) =
    ec2.describeInstanceStatus((new DescribeInstanceStatusRequest).withInstanceIds(instanceIds.asJava))
}
