VFDPRGC2 ;DSS/SMP - VFD Program Codes - Cross References ; 04/16/2015 19:50
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**25**;28 Jan 2013;Build 16
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
EN1(PRGC,LOC,START,STOP) ; Called from SET^VFDPRGC
 ; Update all facilities under a region or metro
 ;      - > FACILITY TYPE (field 13) must be RGR or RGM
 ;
 ; Input:
 ;  PRGC - req - Program Code IEN
 ;   LOC - req - Institution Pointer
 ; START - opt - START DATE
 ;  STOP - opt - STOP DATE
 ;
 ;
 N X,Y,Z,I,DA,VFD,VFDERR
 I 'STOP,'START Q
 Q:'$$REGION(LOC)
 S VFDSCR="I $P($G(^(99)),U,6)="_LOC
 D LIST^DIC(4,,"@","P",,,,,VFDSCR,,"VFD","VFDERR")
 F I=1:1:+VFD("DILIST",0) D UPDATE(PRGC,VFD("DILIST",I,0),START,STOP)
 Q
 ;
EN2(PRGC,LOC) ; Called from KILL^VFDPRGC
 ; Remove Stop Date for all facilities under a region
 N X,Y,Z,I,VFD,VFDERR
 Q:'$$REGION(LOC)
 S VFDSCR="I $P($G(^(99)),U,6)="_LOC
 D LIST^DIC(4,,"@","P",,,,,VFDSCR,,"VFD","VFDERR")
 F I=1:1:+VFD("DILIST",0) D UPDATE(PRGC,VFD("DILIST",I,0),"","@")
 Q
 ;
UPDATE(PRGC,LOC,START,STOP) ; 
 N X,Y,Z,IEN,VFDERR,VFDFDA
 S IEN=$$EXISTS(LOC,PRGC)
 I 'IEN S IEN=$$ADD^VFDPRGCLK(LOC,PRGC,0,START)
 S:START VFDFDA(21630.0051,IEN_","_PRGC_",",.02)=START
 S:STOP'="" VFDFDA(21630.0051,IEN_","_PRGC_",",.03)=STOP
 D FILE(,.VFDFDA,.VFDERR)
 Q
 ;
EXISTS(LOC,PRGC) ; Is there an existing entry?
 N RET
 S RET=$$ACT4LOC^VFDPRGC(LOC,PRGC)
 I 'RET S RET=$$FUT4LOC^VFDPRGC(LOC,PRGC)
 Q RET
 ;
FILE(FLAGS,FDA,ERR) ;
 D FILE^DIE($G(FLAGS),"FDA","ERR")
 Q
 ;
REGION(LOC) ; Is this a region or metro?
 Q "^RGR^RGM^"[(U_$$GET1^DIQ(4,LOC,13,,,"VFDERR")_U)
 ;
