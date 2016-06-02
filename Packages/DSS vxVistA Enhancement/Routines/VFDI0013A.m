VFDI0013A ;DSS/SMP - ENV/PRE/POST VFD*2013.0*17 ; 06/10/2014 15:00
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;**17**;11 Jun 2013;Build 6
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ; ICR#  SUPPORTED DESCRIPTION
 ;-----  ---------------------------------------------------------
 ;       ^DID: FILE
 ;       ^DILFD: $$VFIELD, $$VFILE
 ;       EN^DIU2
 ;       MES^XPDUTL
 ;       Reference the %ZOSF global
 Q
 ;
ENV ; environmental check for VFD*2013.0*17
 Q
 ;
PREX ; [Pre-transport] - KIDS Pre-Transport Routine for VFD*2013.0*17
 Q:'$L($G(XPDGREF))  ;Sanity check
 N VFDI,VFDR,VFDX
 S VFDI=0,VFDR=$NA(@XPDGREF@("VFD")),VFDX=$NA(^TMP("VFD",$J))
 M @VFDR=@VFDX
 Q
 ;
PRE ; pre-install for VFD*2013.0*17
 ; Check for DECLINED TO ANSWER in the LANGUAGE file
 ; If found, quit
 ; Else, add entry to file
 ;
 N VFDIEN,VFDFDA,VFDERR
 S VFDIEN=$$FIND1^DIC(.85,,"O","DECLINED TO ANSWER","C",,"VFDERR")
 I VFDIEN D MSG(1) Q
 K VFDIEN S VFDIEN(1)=21600999
 S VFDFDA(.85,"+1,",.01)=21600999
 S VFDFDA(.85,"+1,",1)="DECLINED TO ANSWER"
 S VFDFDA(.85,"+1,",.03)="ZZZ"
 D UPDATE^DIE(,"VFDFDA","VFDIEN","VFDERR")
 I $D(VFDERR) D MSG(2) Q
 D MSG(3)
 Q
 ;
 ;
 ;=====================================================================
 ;
 ;
POST ; post-install for VFD*2013.0*17
 Q:'$L($G(XPDGREF))  ;Sanity check
 N X,Y,Z,I,T,NA,VFDFDA,TEMP,CNT
 S (CNT("UP"),CNT("ADD"))=0
 S NA=$NA(@XPDGREF@("VFD")),T=$C(9) Q:'$D(@NA)
 F I=2:1 Q:'$D(@NA@(I))  S X=$$UP^XLFSTR(@NA@(I)) D
 .N ISO2,ISO3,LANG,SYN
 .S LANG=$P(X,T,3),ISO3=$P(X,T),ISO2=$P(X,T,2),SYN=$P(X,T,4)
 .D FIND(LANG,ISO3,ISO2,SYN)
 D MSG2(.CNT)
 Q
 ;
FIND(LANG,ISO3,ISO2,SYN) ; Find entry in LANGUAGE file
 N VFDIEN,VFDFDA,VFDERR,SCRN
 S SCRN="I Y>21600000"
 S VFDIEN=$$FIND1^DIC(.85,,"O",$E(LANG,1,30),"C",SCRN,"VFDERR")
 I 'VFDIEN S VFDIEN=$$FIND1^DIC(.85,,"O",ISO3,"D",SCRN,"VFDERR")
 I VFDIEN D FILE(VFDIEN,LANG,ISO3,ISO2,SYN) Q
 D UPDATE(LANG,ISO3,ISO2,SYN)
 Q
 ;
FILE(IEN,LANG,ISO3,ISO2,SYN) ; Update existing entry
 N I,IENS,VFDCNT,VFDFDA,VFDERR S IENS=IEN_","
 S VFDFDA(.85,IENS,.03)=ISO3
 S VFDFDA(.85,IENS,.04)=ISO2
 S VFDFDA(.85,IENS,1)=LANG
 D FILE^DIE(,"VFDFDA","VFDERR")
 I $D(VFDERR) D MSG(4,"Error when updating "_LANG) Q
 S CNT("UP")=CNT("UP")+1
 Q:'$L(SYN)  K VFDFDA S VFDCNT=1
 I $L(SYN) F I=1:1:$L(SYN,";") D
 .Q:$$FIND1^DIC(.850216,","_IENS,"X",$P(SYN,";",I),,,"VFDERR")
 .S VFDFDA(.850216,"+"_VFDCNT_","_IENS,.01)=$P(SYN,";",I),VFDCNT=VFDCNT+1
 D:$D(VFDFDA) UPDATE^DIE(,"VFDFDA",,"VFDERR")
 I $D(VFDERR) D MSG(5,"Error when updating "_LANG) Q
 Q
 ;
UPDATE(LANG,ISO3,ISO2,SYN) ; Add new entry
 N I,IENS,VFDFDA,VFDERR,VFDIENS
 S VFDIENS(1)=$$NEXT
 S VFDFDA(.85,"+1,",.01)=$$NEXT
 S VFDFDA(.85,"+1,",.03)=ISO3
 S VFDFDA(.85,"+1,",.04)=ISO2
 S VFDFDA(.85,"+1,",1)=LANG
 I $L(SYN) F I=1:1:$L(SYN,";") D
 .S VFDFDA(.850216,"+"_(I+1)_",+1,",.01)=$P(SYN,";",I)
 D UPDATE^DIE(,"VFDFDA","VFDIENS","VFDERR")
 I $D(VFDERR) D MSG(5,"Error when updating "_LANG) Q
 S CNT("ADD")=CNT("ADD")+1
 Q
 ;
NEXT() ; Get next ID number
 Q 1+$O(^DI(.85,"B",21600999),-1)
 ;
 ;
 ;=====================================================================
 ;
 ;
MSG(LINE,TXT) ;
 ;;DECLINED TO ANSWER already exists in the LANGUAGE file
 ;;*** ERROR adding DECLINED TO ANSWER to the LANGUAGE file ***
 ;;DECLINED TO ANSWER successfully added to the LANGUAGE file
 ;;*** ERROR updating entry in the LANGUAGE file ***
 ;;*** ERROR adding entry to the LANGUAGE file ***
 ;;*** ERROR adding entry to the SYNONYM multiple ***
 ;;
 N I,J,X,Y,Z,VFDM
 S (VFDM(1),VFDM(3))=" "
 S VFDM(2)=$P($T(MSG+LINE),";;",2)
 I $G(TXT)'="" S VFDM(4)=TXT,VFDM(5)=" "
 D MES^XPDUTL(.VFDM)
 Q
 ;
MSG2(IN) ;
 N VFDM
 S (VFDM(1),VFDM(4))=" "
 S VFDM(2)="  "_$$RJ^XLFSTR(IN("UP"),4)_" LANGUAGES updated."
 S VFDM(3)="  "_$$RJ^XLFSTR(IN("ADD"),4)_" LANGUAGES added."
 D MES^XPDUTL(.VFDM)
 Q
 ;
