VFDPRGC1 ;DSS/SMP - VFD Program Codes ; 01/22/2015 15:20
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**25**;28 Jan 2013;Build 16
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine can only be invoked by VFDPRGC
 ;
GET ;
 ;
 ; INPUTS:
 ;   LOC  - req - HOSPITAL LOCATION (#44) ien
 ;
 ; OUTPUT: 
 ;   RET(I) = IEN ^ Name ^ Abbreviation
 ;   Sorted Alphabetically
 ;
 ; LOC,NUM,FROM,DIR newed in VFDPTCHR
 ;
 N X,Y,INST,NA,START,STOP,VFD,VFDERR,VFDLIST
 S NA=$NA(^TMP("VFDPRGC",$J)) K @NA
 S RET=NA,INST=$$GET1^DIQ(44,LOC,3,"I")
 S @NA@(1)="-1^No matches found"
 D GETALL(NA) I $D(VFDERR) D ERR Q
 I INST D GETLOC(NA,INST) I $D(VFDERR) D ERR Q
 D RESULTS(NA,NUM,FROM,DIR)
 Q
 ;
 ;=====================================================================
 ;                P R I V A T E   S U B R O U T I N E S
 ;=====================================================================
 ;
ERR ;
 K @RET S @RET(1)="-1^Error encountered on lookup"
 Q
 ;
GETALL(NA) ; Get Program Codes that are active for all locations
 N I,X,GREF,VFDSCR
 S GREF=$NA(@NA@("ALL")),VFDSCR="I $$GET1^DIQ(21630.005,Y,5)"
 D LIST^DIC(21630.005,"","1.01","P",,,,,VFDSCR,,GREF,"VFDERR")
 Q:$D(VFDERR)
 F I=1:1:+@GREF@("DILIST",0) S X=@GREF@("DILIST",I,0) D
 .S @NA@("B",$P(X,U,2))=X
 S @NA=+@GREF@("DILIST",0)
 K @GREF
 Q
 ;
GETLOC(NA,VFDLOC) ; Get Program Codes that are active for given loc
 N I,X,VFD,VFDSCR
 S VFDSCR="I $$GETLOC1^VFDPRGC1(Y)"
 D FIND^DIC(21630.005,,1.01,"PQX",VFDLOC,,"LOC",VFDSCR,,"VFD","VFDERR")
 Q:$D(VFDERR)
 F I=1:1:+VFD("DILIST",0) S X=VFD("DILIST",I,0) D
 .S @NA@("B",$P(X,U,2))=X
 S @NA=$G(@NA)+VFD("DILIST",0)
 Q
 ;
GETLOC1(IEN) ; Screen for GETLOC
 N ERR,OUT,SCR
 S SCR="I $$GET1^DIQ(21630.0051,Y1,5)"
 D LIST^DIC(21630.0051,","_IEN_",",,"P",,,,,SCR,,"OUT","ERR")
 I $D(ERR) Q 0
 Q +$G(OUT("DILIST",0))
 ;
RESULTS(NA,NUM,FROM,DIR) ; Filter the results
 N I,X,LIST
 S X=FROM F I=1:1:NUM S X=$O(@NA@("B",X),DIR) Q:X=""  D
 .S LIST(X)=@NA@("B",X)
 S X="" F I=1:1 S X=$O(LIST(X)) Q:X=""  S @NA@(I)=LIST(X)
 K @NA@("B")
 Q
 ;
