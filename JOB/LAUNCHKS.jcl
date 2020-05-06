//LAUNCHKS JOB (),'XPED ECLIPSE KS',NOTIFY=&USERID,
//         CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),TIME=(0,09)
//*
//*  USE THIS JCL IN AN XPEDITER/ECLIPSE LAUNCH CONFIGURATION
//*  TO EXECUTE THE CWKTVSKS PROGRAM.
//*
//CWKTVSKS EXEC PGM=CWKTVSKS,PARM=00001
//STEPLIB  DD DISP=SHR,DSN=CWEZ.TXXX.PRD.LOAD
//*
//*********************************************************************
//*  UPDATE THE EMPFILE DATASET NAME BELOW                            *
//*********************************************************************
//EMPFILE  DD DISP=SHR,DSN=COMPWARE.VSAM.KSDS.HRLY
//RPTFILE  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//