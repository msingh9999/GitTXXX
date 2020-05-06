//CWEZXXXA JOB  ('OAABAS9.0.4QC',822),'90 CWXTJCLC 09/16',
//             CLASS=L,MSGCLASS=X,NOTIFY=&USERID
/*JOBPARM  L=9999,S=CWCC
//*
//*********************************************************************
//*                                                                   *
//*  MEMBER = CWXTJCLC                                                *
//*                                                                   *
//*  THIS IS THE JCL TO EXECUTE COBOL PROGRAM CWXTCOB                 *
//*                                                                   *
//*********************************************************************
//*
//*   EXECUTE CWXTCOB IN BATCH
//*
//CWXTCOB  EXEC PGM=CWXTCOB,PARM=00003
//*
//STEPLIB  DD  DISP=SHR,DSN=CWEZ.#APP.PRD.LOAD
//*
//EMPFILE  DD  DISP=SHR,DSN=SYS2.CW.&CWGAXT..SLXTSAMP(CWXTDATA)
//RPTFILE  DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//