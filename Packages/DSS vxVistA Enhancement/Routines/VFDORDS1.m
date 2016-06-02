VFDORDS1 ;DSS/SMP - DATA SEGREGATION ;Feb 13, 2014
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**25**;;Build 5
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ; *** This routine should only be invoked via VFDORDS ***
 ;
 Q
 ;
AUTH ;  RPC [VFD IS AUTHORIZED]
 ;
 ; Patient Location - Pointer to file 44 - HOSPITAL LOCATOIN
 ; In file 44:
 ;    Field 3 INSTITUTION points to file 4 - INSTITUTION
 ;
 N X,Y,Z,LIST
 D GETDIV(.LIST,DUZ)
 S X=$$GET1^DIQ(44,LOC,3,"I")
 I X S VFDRET=$D(LIST(X))
 Q
 ;
ADDJUST ; RPC [VFD CREATE JUSTIFICATION]
 ; .01       ID NUMBER (RNJ14,0), [0;1] - HAVE
 ; .02       PROVIDER (RP200'), [0;2]   - HAVE
 ; .03       PATIENT (RP2'), [0;3]      - NEED
 ; .04       LOCATION (RP44'), [0;4]    - NEED
 ; .05       START DATE/TIME (D), [0;5] - HAVE
 ; .06       END DATE/TIME (D), [0;6]   - HAVE
 ; 1         JUSTIFICATION (Multiple-21620.0001), [1;0] - NEED
 ;         .01  JUSTIFICATION (Wx), [0;1]
 ;
 ;
 N X,Y,Z,NOW,VFDFDA,VFDIEN,VFDERR
 ; verify data integrity
 I $G(DFN)=""!('$D(^DPT($G(DFN)))) S VFDRET="-1^Invalid or Missing Patient" Q
 I $G(LOC)=""!('$D(^SC($G(LOC)))) S VFDRET="-1^Invalid or Missing Location" Q
 S VFDRET="-1^Invalid or Missing Justification" D
 .S X=0 F  S X=$O(JUST(X)) Q:'X  I JUST(X)'?." " S VFDRET=1 Q
 Q:+VFDRET=-1
 ;
 S NOW=$$NOW^XLFDT,NOW(1)=$$FMADD^XLFDT(NOW,1)
 S VFDFDA(21620,"+1,",.01)=$$GETNXT ;    .01 - ID Number
 S VFDFDA(21620,"+1,",.02)=DUZ ;         .02 - Provider
 S VFDFDA(21620,"+1,",.03)=DFN ;         .03 - Patient
 S VFDFDA(21620,"+1,",.04)= LOC ;        .04 - Location
 S VFDFDA(21620,"+1,",.05)=NOW ;         .05 - Start Date/Time
 S VFDFDA(21620,"+1,",.06)=NOW(1) ;      .06 - End Date/Time
 D UPDATE^DIE(,"VFDFDA","VFDIEN","VFDERR")
 I $D(VFDERR) S VFDRET="-1^Failed to add Justification" Q
 D WP^DIE(21620,VFDIEN(1)_",",1,,"JUST","VFDERR")
 I $D(VFDERR) S VFDRET="-1^Failed to add Justification" Q
 S VFDRET="1^"_VFDIEN(1)
 Q
 ;
ENDJUST ; RPC [VFD END JUSTIFICATION]
 ; set end date/time
 N X,Y,VFDFDA,VFDERR
 S VFDFDA(21620,JUST_",",.06)=$$NOW^XLFDT
 D FILE^DIE(,"VFDFDA","VFDERR")
 I $D(VFDERR) S VFDRET="-1^Failed to update Justification"
 Q
 ;
 ;=====================================================================
 ;             P R I V A T E   S U B R O U T I N E S
 ;=====================================================================
 ;
GETDIV(RET,PROV) ; Return an array of the user's authorized divisions
 N X,Y,Z,ERR,LIST,TODAY
 S TODAY=$$DT^XLFDT
 D GETS^DIQ(200,PROV_",","16*;21604*","IN","LIST","ERR")
 S X="" F  S X=$O(LIST(200.02,X)) Q:X=""  D
 .N LOC S LOC=$G(LIST(200.02,X,.01,"I")) Q:'LOC
 .S RET(LOC)=""
 S X="" F  S X=$O(LIST(200.021604,X)) Q:X=""  D
 .N LOC,SDT,EDT
 .S LOC=$G(LIST(200.021604,X,.01,"I")) Q:'LOC
 .S SDT=$G(LIST(200.021604,X,.02,"I")) I SDT,SDT>TODAY Q
 .S EDT=$G(LIST(200.021604,X,.03,"I")) I EDT,EDT<TODAY Q
 .S RET(LOC)=""
 Q
 ;
GETNXT() ;  Return the next ID number
 N X,LIST,ERR
 D LIST^DIC(21620,,".01","BP",1,,,,,,"LIST","ERR")
 S X=1+$G(LIST("DILIST",1,0))
 Q X
