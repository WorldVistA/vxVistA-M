VFDTIU ;DSS/SGM - TIU OBJECT FRONT DOOR ; 02/3/2013 13:45
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**6,28**;16 Aug 2013;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine should be the entry point for all TIU Objects.  To save
 ; space in this routine, any extensive TIU Object documentation will
 ; be in the called routine
 ;
SS(DFN) ;current smoking status
 Q $$SS^VFDTIU1($G(DFN))
 ;
CLABS(DFN,SDATE,EDATE) ;Last 24 hrs critical labs
 Q $$CLABS^VFDTIU1($G(DFN),$G(SDATE),$G(EDATE))
 ;
RLABS(DFN,EDATE) ;LABS - LAST 7 DAYS (EXCLUDE MI & AP)
 Q $$RLABS^VFDTIU1(DFN,EDATE)
 ;
RXBRIEF(DFN) ; returns a brief listing of active medications
 Q $$RXBRIEFA^VFDTIU1(DFN,"OP")
