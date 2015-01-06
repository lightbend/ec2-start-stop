package com.typesafe.jenkins

import scala.collection.JavaConverters._

import hudson.model._
import hudson.slaves._

// TODO: extension is not being picked up...
// go to $jenkins/script and run: (new com.typesafe.jenkins.EC2StartStopComputerListenerBase()).register()

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
@hudson.Extension
class EC2StartStopComputerListener extends ComputerListener {
  def ec2NodeNameFor(computer: Computer) =
    computer.getNode.getAssignedLabels.asScala.map(_.toString).filter(_.startsWith("ec2:")).map(_.drop(4)).headOption.getOrElse(computer.getName)

  // block until worker comes online
  override def preLaunch(computer: Computer, listener: TaskListener): Unit = {
    listener.getLogger.println(s"[EC2] Starting worker ${ec2NodeNameFor(computer)}.")

    if (!EC2InstanceManager.startByNameBlocking(ec2NodeNameFor(computer))(listener.getLogger.println))
      throw new hudson.AbortException

    listener.getLogger.println(s"[EC2] Started worker ${ec2NodeNameFor(computer)}!")
  }

  // TODO: either go back to 1-to-1 for EC2 worker and jenkins node, or only stop worker if all jenkins nodes that run on it are idle
  override def onOffline(computer: Computer, cause: OfflineCause): Unit =
    if (!EC2InstanceManager.stopByName(ec2NodeNameFor(computer)))
      throw new hudson.AbortException

}
