#!/usr/bin/env groovy
import hudson.model.*
import hudson.EnvVars
import groovy.json.JsonSlurper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import java.net.URL

String ISPW_Application     = "TXXX"        // Change to your assigned application
String HCI_Token            = "CWEZXXX"     // Change to your assigned Test Drive ID

node {
  stage ('Checkout') 
  {
    // Get the code from the Git repository
    checkout scm
  }

  stage('Git to ISPW Synchronization')
  {     
    gitToIspwIntegration app: "${ISPW_Application}", 
    branchMapping: '''*master => STG, per-branch'
    TXX1 => QA1, per-branch
    TXX2 => QA2, per-branch
    TXXX => QA3, per-branch''', 
    connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
    credentialsId: "${HCI_Token}",
    gitCredentialsId: 'de2894bf-c81a-4a4d-af99-18ab5c6f0e3b', 
    gitRepoUrl: 'http://192.168.96.156/Bonobo.Git.Server/IspwGitTXXXTest.git', 
    runtimeConfig: 'isp8', 
    stream: 'CWEZ'
  }

  stage('Build ISPW assignment')
  { 
    ispwOperation connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
    consoleLogResponseBody: false, 
    credentialsId: 'CWEZXXX-CES', 
    ispwAction: 'BuildTask',
    ispwRequestBody: '''buildautomatically = true'''
  }
}