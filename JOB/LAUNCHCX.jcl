//LAUNCHCX JOB (),'XPED ECLIPSE CX',NOTIFY=&USERID,
//         CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),TIME=(0,09)
//*
//*  USE THIS JCL IN AN XPEDITER/ECLIPSE LAUNCH CONFIGURATION
//*  TO EXECUTE THE CWKTCOBX PROGRAM.
//*
//CWKTCOBX EXEC PGM=CWKTCOBX,PARM=00005
//*
//STEPLIB DD DISP=SHR,DSN=CWEZ.TXXX.PRD.LOAD
//*
//*********************************************************************
//*  UPDATE THE EMPFILE DD BELOW WITH THE ECC 17 SLCXCNTL DATASET     *
//*  NAME FOR YOUR SYSTEM                                             *
//*********************************************************************
//EMPFILE  DD DISP=SHR,DSN=SYS2.CW.&CWGAXT..SLCXCNTL(TTTDATH)
//RPTFILE  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//