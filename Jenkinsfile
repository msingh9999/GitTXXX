node {
  stage ('Checkout') 
  {
    // Get the code from the Git repository
    checkout scm
  }

  stage('Git to ISPW Synchronization')
  { 
	/* Original
	gitToIspwIntegration app: 'PLAY', 
	branchMapping: '''*Play* => DEV1, per-branch''', 
	connectionId: '9079999f-ab78-4047-8366-00eb8aa4f173', 
	credentialsId: 'ce986ae7-1b4d-4f0d-8f9b-a0b022182124', 
	gitCredentialsId: 'loginToGit3',
	gitRepoUrl: 'https://evolve.compuware.com:8443/scm/~kathy.turetzky_compuware.com/ispwgitplaytest.git', 
	runtimeConfig: 'TPZP', 
	stream: 'PLAY'
	*/

	gitToIspwIntegration app: 'TXXX', 
	branchMapping: '*TXXX* => DEV1, per-branch', 
	connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
	credentialsId: 'CWEZXXX', 
	gitCredentialsId: 'de2894bf-c81a-4a4d-af99-18ab5c6f0e3b', 
	gitRepoUrl: 'http://192.168.96.156/Bonobo.Git.Server/IspwGitTXXXTest.git', 
	runtimeConfig: 'isp8', 
	stream: 'CWEZ'
  }

  stage('Build ISPW assignment')
  {
	/* Original
	ispwOperation connectionId: '9079999f-ab78-4047-8366-00eb8aa4f173', 
	consoleLogResponseBody: true, 
	credentialsId: 'CES20.1Conn', 
	ispwAction: 'BuildAssignment', 
	ispwRequestBody: '''assignmentId=PLAY003145
	level=DEV1
	'''
	*/

	ispwOperation connectionId: '5520f4ea-7300-4387-aea2-182136258d31', 
	credentialsId: 'CWEZXXX-CES', 
	ispwAction: 'BuildAssignment', 
	ispwRequestBody: '''assignmentId=PLAY003145 level=DEV3'''
  }
}