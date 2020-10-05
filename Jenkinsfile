#!/usr/bin/env groovy
import hudson.model.*
import hudson.EnvVars
import groovy.json.JsonSlurper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import java.net.URL

String ISPW_Application     = "MKS1"        // Change to your assigned application
String HCI_Token            = "PFHMKS0"     // Change to your assigned ID

node {
  stage ('Checkout')
  {
    // Get the code from the Git repository
    checkout scm
  }

  stage('Git to ISPW Synchronization')
  {
    gitToIspwIntegration app: "${ISPW_Application}",
    branchMapping: '''*master* => STG, per-branch'
    bug* => EMR, per-branch
    feature1* => QA1, per-branch
    feature2* => QA2, per-branch
    feature3* => QA3, per-branch''',
    //connectionId: '38e854b0-f7d3-4a8f-bf31-2d8bfac3dbd4', // CWC2
    connectionId: '4b4cf589-b835-4579-96ee-2aba6b818125', // TD-CWCC
    credentialsId: "${HCI_Token}",
    gitCredentialsId: 'ec3e192a-3e5f-4c5f-bb40-129b63ce6c23', // Mingh9999
    gitRepoUrl: 'https://github.com/msingh9999/GitTXXX.git',
    //runtimeConfig: 'isp8', // CWC2
    runtimeConfig: 'ispw', // CWCC
    stream: 'FTSDEMO'
  }

  stage('Build ISPW assignment')
  {
    //ispwOperation connectionId: '38e854b0-f7d3-4a8f-bf31-2d8bfac3dbd4', // CWC2
    ispwOperation connectionId: '4b4cf589-b835-4579-96ee-2aba6b818125', // TD-CWCC
    consoleLogResponseBody: false,
    //credentialsId: 'CWEZXXX-CES', // CWC2
    credentialsId: 'PFHMKS0-CES', // CWCC
    ispwAction: 'BuildTask',
    ispwRequestBody: '''buildautomatically = true'''
  }

  stage('Deploy to Testing')
  {
    sleep(10)
    println "Deploy successfull!"
  }

  stage('Run TTT Tests')
  {
    sleep(10)
    println "TTT Tests successfull!"
  }

  stage('Retrieve Code Coverage')
  {
    sleep(10)
    println "Retrieve code successfull!"
  }

  stage('Run Sonar Analysis')
  {
    sleep(10)
    println "Sonar analysis successfull!"
  }


}