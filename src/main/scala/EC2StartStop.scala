package com.typesafe.jenkins

import java.util.logging.Logger

import jenkins.model.Jenkins

import scala.collection.JavaConverters._

import hudson.model._
import hudson.slaves._

/**
 * A simple ComputerListener that launches an EC2 worker before a node is started, and stops the
 * worker when the Jenkins node is taken off-line.
 *
 * The EC2 instances for the workers must be in the same region as the Jenkins master.
 * The master must have an IAM instance profile that permits describing/starting/stopping instances.
 * To specify the EC2 worker name "worker-name", label the node as "ec2:worker-name".
 * If no tag prefixed by "ec2:" is found, the Jenkins node
 * name is used.
 */
// TODO: schedule a task every 45 min to stop EC2 instances of off-line workers (in case we didn't get the onOffline, or if it failed)
// TODO @hudson.Extension // Doesn't work due to https://issues.scala-lang.org/browse/SI-7041
class EC2StartStopComputerListener extends ComputerListener {
  // worker-to-node ratio must be 1:1 (or we might stop an EC2 worker that's being used by another jenkins node)
  def ec2NodeNameFor(computer: Computer) = computer.getName

  // block until worker comes online
  override def preLaunch(computer: Computer, listener: TaskListener): Unit = {
    listener.getLogger.println(s"[EC2] Starting worker ${ec2NodeNameFor(computer)}.")

    if (!EC2InstanceManager.startByNameBlocking(ec2NodeNameFor(computer))(listener.getLogger.println))
      throw new hudson.AbortException // TODO: refined (e.g., when node is still shutting down, this will fail to re-launch)

    listener.getLogger.println(s"[EC2] Started worker ${ec2NodeNameFor(computer)}!")
  }

  override def onOffline(computer: Computer, cause: OfflineCause): Unit = cause match {
    // don't ignore these -- seems to be cause when actual disconnects happen, despite the name
    // case _ : OfflineCause.LaunchFailed | _ : OfflineCause.ChannelTermination =>
    case _ if cause.toString == "relaunch" =>   // ignore for testing purposes
    case _ => // actually stop
      val nodeName = ec2NodeNameFor(computer)
      logger.info(s"Taking $nodeName offline!")

      if (!EC2InstanceManager.stopByName(nodeName))
        logger.warning(s"Could not take $nodeName offline!")
  }

  private lazy val logger = Logger.getLogger(classOf[EC2StartStopComputerListener].getName)
}
