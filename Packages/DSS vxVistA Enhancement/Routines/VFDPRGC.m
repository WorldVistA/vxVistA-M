VFDPRGC ;DSS/SMP - VFD Program Codes ; 01/22/2015 15:20
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**25**;28 Jan 2013;Build 16
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
  ;=====================================================================
 ;                   R e m o t e   P r o c e d u r e s 
 ;=====================================================================
 ;
GET(RET,INPUT) ; RPC - [VFD PRGM CODE GET]
 ; INPUT(n) - req where INPUT(n) = code ^ value for n=1,2,3,...
 ; 
 ; CODE  REQ VALUE
 ; ----- --- -----------------------------------------------------------
 ; LOC    Y  HOSPITAL LOCATION (#44) ien
 ; NUM    N  Number of results to return.  Defaults to 40
 ; FROM   N  starting Fileman index value based on last record returned
 ;             from previous RPC call
 ; DIR    N  Direction to traverse results.  Defaults to forwards
 ;              1 = forwards
 ;             -1 = backwards
 ; OUTPUT: 
 ;   RET(I) = IEN ^ Name ^ Abbreviation
 ;
 N I,DIR,FROM,LOC,NUM
 F I=1:1 Q:'$D(INPUT(I))  S @$P(INPUT(I),U)=$P(INPUT(I),U,2)
 ; set defualts
 I '$G(NUM) S NUM=40
 S FROM=$G(FROM),LOC=$G(LOC),DIR=$G(DIR,1)
 G GET^VFDPRGC1
 Q
 ;
 ;=====================================================================
 ;                                A P I s 
 ;=====================================================================
ACT4LOC(LOC,PRGC) ; Is Program Code Active for a Given Location
 ; INPUTS:
 ;   LOC   - req - Institution IEN
 ;   PRGC  - req - VFD Program Code IEN
 ;
 ; OUTPUT:
 ;   IEN in LOCATION multiple if active
 ;   0   if not active
 ;
 N VFD,VFDERR,VFDSCR
 S LOC=$$GET1^DIQ(4,LOC,.01) Q:LOC="" 0
 S VFDSCR="I $$ACTIVE^VFDPRGC($P(^(0),U,2),$P(^(0),U,3),DT,1)"
 S VFD=$$FIND1^DIC(21630.0051,","_PRGC_",","X",LOC,,VFDSCR,"VFDERR")
 Q +VFD
 ;
FUT4LOC(LOC,PRGC) ; Is Program Code Scheduled in the future for Loc
 ; INPUTS:
 ;   LOC   - req - Institution IEN
 ;   PRGC  - req - VFD Program Code IEN
 ;
 ; OUTPUT:
 ;   IEN in LOCATION multiple if found
 ;   0   if not found
 N VFD,VFDERR,VFDSCR
 S LOC=$$GET1^DIQ(4,LOC,.01) Q:LOC="" 0
 S VFDSCR="I $P(^(0),U,2)>DT"
 S VFD=$$FIND1^DIC(21630.0051,","_PRGC_",",,LOC,,VFDSCR,"VFDERR")
 Q +VFD
 ;
 ;=====================================================================
 ;                    C o m p u t e d   F i e l d
 ;=====================================================================
 ;
ACTIVE(START,STOP,NOW,ALL) ;
 I '$G(ALL) Q 0
 S:$G(NOW)="" NOW=$$NOW^XLFDT
 I '$G(STOP),START'>NOW Q 1
 I START'>NOW,$G(STOP)'<NOW Q 1
 Q 0
 ;
 ;=====================================================================
 ;                   I n p u t   T r a n s f o r m
 ;=====================================================================
 ;
 ;
 ;=====================================================================
 ;                             X - R E F
 ;=====================================================================
 ;
SET(PRGC,LOC,START,STOP) ;
 ; Called from:    ASDT x-ref on LOCATION START DATE
 ;                 ASTOP x-ref on LOCATION STOP DATE
 ;
 ;  PRGC - req - Program Code IEN
 ;   LOC - req - Institution
 ; START - opt - Start Date
 ;  STOP - opt - Stop Date
 ;
 ; Start or Stop date has to be valued
 ; Has to be one or the other, not both
 ;
 ;
 D EN1^VFDPRGC2(PRGC,LOC,$G(START),$G(STOP))
 Q
 ;
KILL(PRGC,LOC) ; Delete Stop Date
 D EN2^VFDPRGC2(PRGC,LOC)
 ;
