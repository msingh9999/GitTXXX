#!/usr/bin/env groovy
import hudson.model.*
import hudson.EnvVars
import groovy.json.JsonSlurper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import java.net.URL

String ISPW_Application     = "TXXX"        // Change to your assigned application
String HCI_Token            = "CWEZXXX"     // Change to your assigned ID

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
    TXX1* => QA1, per-branch
    TXX2* => QA2, per-branch
    TXXX* => QA3, per-branch''', 
    connectionId: '38e854b0-f7d3-4a8f-bf31-2d8bfac3dbd4', // CWC2
    credentialsId: "${HCI_Token}",
    gitCredentialsId: 'ec3e192a-3e5f-4c5f-bb40-129b63ce6c23', // Manoj
    gitRepoUrl: 'https://github.com/msingh9999/GitTXXX.git', 
    runtimeConfig: 'isp8', // CWC2 
    //runtimeConfig: 'ispw', // CWCC
    stream: 'CWEZ'
  }

  stage('Build ISPW assignment')
  { 
    ispwOperation connectionId: '38e854b0-f7d3-4a8f-bf31-2d8bfac3dbd4', // CWC2
    consoleLogResponseBody: false, 
    credentialsId: 'CWEZXXX-CES', // CWC2
    //credentialsId: 'PFHMKS0-CES', // CWCC
    ispwAction: 'BuildTask',
    ispwRequestBody: '''buildautomatically = true'''
  }
  
  stage('Deploy')
  {
    println "Deploy successfull!"
  }
}