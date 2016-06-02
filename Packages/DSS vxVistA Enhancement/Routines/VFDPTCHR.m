VFDPTCHR ;DSS/SMP - VFD Patient Characteristics ; 01/08/2015 15:40
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**22**;28 Jan 2013;Build 7
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;=====================================================================
 ;                   R e m o t e   P r o c e d u r e s 
 ;=====================================================================
 ;
GET(RET,DFN,SORT) ; RPC - [VFD PT CHAR GET]
 ;
 ; This RPC returns a list of Patient Characteristics depending on
 ; inputs.  If a DFN is passed in, the result will be a list of the
 ; active Patient Characteristics for the given patient.  If no DFN
 ; is passed in, the result will be a list of all the active Patient
 ; Characteristics available.
 ;
 ; INPUTS:
 ;   DFN  - opt - Patient IEN
 ;   SORT - opt - Boolean, default to 0 - if SORT then return result
 ;                sorted by IEN, if 'SORT, return result by PRIORITY
 ;
 ; OUTPUT: 
 ;   RET(I) = IEN ^ Name ^ Abbreviation
 ;
 G GET^VFDPTCHR1
 Q
 ;
SAVE(RET,DFN,DUZ,VST,CHAR) ; RPC - [VFD PT CHAR SAVE]
 ; This RPC is used to add/edit the PATIENT CHARACTERISTICS multiple
 ; within the PATIENT file.
 ; 
 ; Example:
 ; 
 ; Patient currently has PATIENT CHARACTERISTICS of A and C.
 ; This RPC is called with B, C and D.
 ; 
 ; CHARACTERISTIC A will be marked inactive with end date of Visit DT.
 ; CHARACTERISTIC B will be added with start date of Visit DT.
 ; CHARACTERISTIC C will not change.
 ; CHARACTERISTIC D will be added with start date of Visit DT.
 ;
 ;
 ; INPUTS:
 ;   DFN   - req - Patient IEN
 ;   DUZ   - req - New Person IEN
 ;   VISIT - req - Visit Date/Time
 ;   CHAR  - opt - List of Patient Characteristics.  CHAR(I) = IEN
 ;
 ; OUTPUT:
 ;   1 if add/edit is successful
 ;  -1^error message
 ;
 G SAVE^VFDPTCHR1
 Q
 ;
 ;=====================================================================
 ;                                A P I s 
 ;=====================================================================
 ;
HAS(DFN,CHAR) ; Patient has characteristic
 ; INPUTS:
 ;   DFN   - req - Patient IEN
 ;   CHAR  - req - VFD Patient Characteristic IEN
 ;
 ; OUTPUT:
 ;   1 if patient has given characteristic
 ;   0 if patient does not have given characteristic
 ;
 N VFD,VFDERR,VFDSCR
 S CHAR=$$GET1^DIQ(21602.5,CHAR,.01) Q:CHAR="" 0
 S VFDSCR="I $$ACTIVE^VFDPTCHR($P(^(0),U,2),$P(^(0),U,4))"
 S VFD=$$FIND1^DIC(2.021605,","_DFN_",",,CHAR,,VFDSCR,"VFDERR")
 Q VFD>0
 ;
 ;=====================================================================
 ;                    C o m p u t e d   F i e l d
 ;=====================================================================
 ;
ACTIVE(START,STOP,NOW) ;
 S:$G(NOW)="" NOW=$$NOW^XLFDT
 I '$G(STOP),START'>NOW Q 1
 I START'>NOW,$G(STOP)'<NOW Q 1
 Q 0
 ;
 ;=====================================================================
 ;                   I n p u t   T r a n s f o r m
 ;=====================================================================
 ;
PRIORITY(PNUM) ;
 ; PRIORITY has to be unique among Characteristics
 N VFD,VFDERR,CT,PRI,I,PRIC,DIR,X,Y,IPT
 I PNUM'?1.15N Q 0
 D LIST^DIC(21602.5,,"@;.01;1.06","P",,,,,,,"VFD","VFDERR")
 ; VFD("DILIST",I,0) = IEN ^ NAME ^ PRIORITY
 S CT=$NA(VFD) F  S CT=$Q(@CT) Q:CT=""  D
 .Q:$QS(CT,2)<1!(+$P(@CT,U,3)<PNUM)
 .S PRI($P(@CT,U,3))=$P(@CT,U,1,2)
 ; LET'S TIGHTEN UP
 S CT=0 F I=(PNUM+1):1 S CT=$O(PRI(CT)) Q:'CT  S PRIC(I)=PRI(CT)
 I $D(PRI(PNUM)) D  Q:'Y 0
 .W !!,"**This priority has already been assigned to...",!
 .W "   ",$P(PRI(PNUM),U,2),!!
 .S DIR("A")="Would you like to shift all priorities"
 .S DIR(0)="Y",DIR("B")="NO" D ^DIR I Y D
 ..S CT="" F  S CT=$O(PRIC(CT),-1) Q:CT=""  D
 ...S IPT(21602.5,+PRIC(CT)_",",1.06)=CT D FILE^DIE(,"IPT") K IPT
 Q 1
 ;
 ;=====================================================================
 ;                        X - R E F  2 1 6 0 2 . 5
 ;=====================================================================
 ;
STOP ; From the VFD PATIENT CHARACTERISTIC file to 2
 N OUT,STP,CHR
 S STP=$S(X'="":X_.235959,1:"")
 S OUT=$NA(^TMP($J,"VFDPTCHR")),CHR=$G(D0,DA) K @OUT
 D LIST^DIC(2,,,"P",,,,"VFDPTCHR",,"D:DIVALUE=$G(D0,DA) STPS^VFDPTCHR(Y)")
 K ^TMP("DILIST",$J),@OUT
 Q
 ;
STPS(DFN) ;
 Q:$D(@OUT@(DFN))  S @OUT@(DFN)=""
 N VFDPTCHR,GET,CT,FLG,IPT,STR
 S VFDPTCHR=1 ;PATIENT FILE CHECK TO NOT SET INACTIVED BY
 D GETS^DIQ(2,DFN_",","21605*","I","GET")
 S CT=$NA(GET) F  S CT=$Q(@CT) Q:CT=""  D
 .I $QS(CT,3)=.01,@CT=CHR S FLG=1 Q
 .I $QS(CT,3)=.05 D  K FLG,IPT
 ..Q:'$D(FLG)!(@CT'="")
 ..S STR=$$GET1^DIQ(2.021605,$QS(CT,2),.02,"I")
 ..I STP,STR>STP D  Q
 ...S IPT(2.021605,$QS(CT,2),.04)=STR D UPDATE^DIE(,"IPT")
 ..S IPT(2.021605,$QS(CT,2),.04)=STP D UPDATE^DIE(,"IPT")
 Q
 ;
STPP(CHR,IENS) ; From the Patient Characteristic multiple from 2
 N STP,IPT
 ; Check the INACTIVATED BY field to protect from re-index
 Q:$$GET1^DIQ(2.021605,IENS,.05)'=""
 S STP=$$GET1^DIQ(21602.5,$G(CHR)_",",1.04,"I") S:STP STP=STP_".235959"
 S IPT(2.021605,IENS,.04)=STP D UPDATE^DIE(,"IPT") K IPT
 Q
 ;
 ;=====================================================================
 ;           C H A R  F O R  P A T I E N T  I N Q U I R Y
 ;=====================================================================
 ;
HDR ; CALLED FROM HDR^DGRPD1 (DFN SET THIS ROUTINE)
 N OUT,CT,NAM,PR,PRI,TOT
 D GET^VFDPTCHR(.OUT,DFN) Q:+$G(OUT(1))<1
 S CT=$NA(OUT) F  S CT=$Q(@CT) Q:CT=""  D
 .S NAM=$P(@CT,U,2),PR=$QS(CT,1)
 .S PRI(PR)="   **"_$$GET1^DIQ(21602.5,+@CT_",",1.05)_"-"_$P(@CT,U,3)_": "
 .S TOT=($L(PRI(PR))+$L(NAM)) I (TOT-78)>0 S NAM=$E(NAM,1,($L(NAM)-(TOT-75)))_"..."
 .W !,PRI(PR)_NAM
 Q
 ;
 ;=====================================================================
 ;                        R E P O R T I N G
 ;=====================================================================
 ;
RBROW ; REPORT BROWSER - SHOW ACTIVE CHARACTERISTIC LIST
 D RBROW^VFDPTCHR1
 Q
