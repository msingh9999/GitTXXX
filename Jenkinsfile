node {
  stage ('Checkout') 
  {
    // Get the code from the Git repository
    checkout scm
  }

  stage('Git to ISPW Synchronization')
  { 	
	gitToIspwIntegration app: 'TXXX', 
	branchMapping: '*TXXX* => DEV3, per-branch', 
	connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
	credentialsId: 'CWEZXXX', 
	gitCredentialsId: 'de2894bf-c81a-4a4d-af99-18ab5c6f0e3b', 
	gitRepoUrl: 'http://192.168.96.156/Bonobo.Git.Server/IspwGitTXXXTest.git', 
	runtimeConfig: 'isp8', 
	stream: 'CWEZ'
  }

  stage('Build ISPW assignment')
  {
	ispwOperation connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
	credentialsId: 'CWEZXXX-CES', 
	ispwAction: 'BuildAssignment', 
	ispwRequestBody: '''assignmentId=PLAY003145 
	level=DEV3'''
  }
}