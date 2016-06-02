VFDI0022 ;DSS/SMP - ENV/PRE/POST for VFD*2013.1*49 ; 06/22/2015 15:00
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**49**;16 Aug 2013;Build 7
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR # SUPPORTED DESCRIPTION
 ;----- --------------------------------------------
 ;
 ;
 Q
 ;
ENV ; environmental check for VFD*2013.1*49
 Q
 ;
PRE ; pre-install for VFD*2013.1*49
 Q
 ;
POST ; post-install for VFD*2013.1*49
 D CLEAN
 D XREF,PROC,RPC
 D CLEAN(1)
 Q
 ;
 ;----------------------- PRIVATE SUBROUTINES -------------------------
 ;
 ;=====================================================================
 ;                  D A T A   V E R I F I C A T I O N
 ;=====================================================================
 ;
DRUG(IEN,SCAN,NDC) ; Lookup in DRUG file
 N X
 I 'SCAN Q 0
 I '$D(^PSDRUG(SCAN)) Q 0
 S X=$O(^PSB(53.79,IEN,.5,"B",SCAN,0))
 I 'X Q 0
 Q $$FILE(X_","_IEN_",",SCAN,NDC)
 ;
NDC(IEN,SCAN,NDC) ; Lookup DRUG based on NDC
 N X,DRG,NDCX,RET S RET=0
 I NDC="" Q 0
 S DRG=0 F  S DRG=$O(^TMP($J,"NDC",NDC,DRG)) Q:'DRG  D
 .S X=$O(^PSB(53.79,IEN,.5,"B",DRG,0)) Q:'X
 .S RET=$$FILE(X_","_IEN_",",SCAN,NDC)
 I 'RET S NDCX=$TR(NDC,"-") I $D(^TMP($J,"NDC",NDCX)) D
 .S DRG=0 F  S DRG=$O(^TMP($J,"NDC",NDCX,DRG)) Q:'DRG  D
 ..S X=$O(^PSB(53.79,IEN,.5,"B",DRG,0)) Q:'X
 ..S RET=$$FILE(X_","_IEN_",",SCAN,NDC)
 Q RET
 ;
SYN(IEN,SYN,NDC) ; Lookup DRUG based on SYNONYM
 N X,DRG,RET S RET=0
 I '$L(SYN) Q 0
 I '$D(^TMP($J,"SYN",SYN)) Q 0
 S DRG=0 F  S DRG=$O(^TMP($J,"SYN",SYN,DRG)) Q:'DRG  D
 .S X=$O(^PSB(53.79,IEN,.5,"B",DRG,0)) Q:'X
 .S RET=$$FILE(X_","_IEN_",",SYN,NDC)
 Q RET
 ;
 ;=====================================================================
 ;                     E R R O R   P R O C E S S I N G
 ;=====================================================================
ERR1(IENS,MSG) ; Record Error Filing Data
 N IEN S IEN(1)=$P(IENS,",",2),IEN(2)=$P(IENS,",")
 D XTMPINIT
 S ^XTMP("VFDI022","ERR1",IEN(1),IEN(2))=$G(MSG)
 Q
 ;
ERR2(IEN,SUB,MSG) ; No match found error
 D XTMPINIT
 S ^XTMP("VFDI0022","ERR2",IEN,SUB)=$G(MSG)
 Q
 ;
 ;
 ;=====================================================================
 ;                     E R R O R   R E P O R T I N G
 ;=====================================================================
FAIL ; Error Report
 I $D(^XTMP("VFDI0022","ERR1")) D
 .W !,"The following entries in the DISPENSED DRUG multiple failed to update:",!!
 .D FAIL1("ERR1")
 I $D(^XTMP("VFDI0022","ERR2")) D
 .W !,"Corresponding entires in the DISPENED DRUG multiple could not be found",!
 .W "for these entries in the AUDIT LOG multiple:",!!
 .D FAIL1("ERR2")
 Q
 ;
FAIL1(NODE) ; Error Report Helper
 N X,Y,Z,TXT
 S TXT=$S($E(NODE,$L(NODE))=1:"DISPENSED DRUG",1:"AUDIT LOG")
 S X=0 F  S X=$O(^XTMP("VFDI0022",NODE,X)) Q:'X  D
 .S Y=0 F  S Y=$O(^XTMP("VFDI0022",NODE,X,Y)) Q:'Y  S Z=^(Y) D
 ..W $$LJ("IEN: "_X,40),TXT_" ENTRY: "_Y,!
 ..W $$LJ("ASSOCIATED SCAN: "_$P(Z,U),40)_"NDC FOR SCAN: "_$P(Z,U,2),!!
 Q
 ;=====================================================================
 ;                  G E N E R A L   P R O C E S S I N G
 ;=====================================================================
CLEAN(XTMP) ; Clean up GLOBALS
 K ^TMP($J,"NDC"),^TMP($J,"SYN")
 Q:$G(XTMP)  ; PRESERVE XTMP
 K ^XTMP("VFDI0022")
 Q
 ;
FILE(IENS,SCAN,NDC) ; File Data in the DISPENSE DRUG MULTIPLE
 N X,Y,Z,VFDFDA,VFDERR
 S VFDFDA(53.795,IENS,21600.01)=SCAN
 S VFDFDA(53.795,IENS,21600.02)=NDC
 D FILE^DIE(,"VFDFDA","VFDERR")
 I $D(VFDERR) D ERR1(IENS,SCAN_U_NDC)
 Q '$D(VFDERR)
 ;
PROC ; Main DRIVER
 N X,Y,Z,I,CNT,FAIL,SUCCESS S (CNT,FAIL,SUCCESS)=0
 S X=0 F  S X=$O(^PSB(53.79,X)) Q:'X  D
 .S Y=0 F  S Y=$O(^PSB(53.79,X,.9,Y)) Q:'Y  D
 ..Q:'$D(^PSB(53.79,X,.9,Y,21600))
 ..S CNT=CNT+1 I CNT#1000=0 W "."
 ..F I=1,2 S Z(I)=$P($G(^PSB(53.79,X,.9,Y,21600)),U,I)
 ..I '$$PROC1(X,Z(1),Z(2)) S FAIL=FAIL+1 D ERR2(X,Y,Z(1)_U_Z(2))
 W !!,SUCCESS_" records updated successfully.",!
 W FAIL_" records failed to update.",!
 I FAIL!$D(^XTMP("VFDI0022","ERR1")) D
 .W !!,"To run the FAILED UPDATE REPORT enter 'D FAIL^VFDI0022' at",!
 .W "the programmer's prompt.",!!
 Q
 ;
PROC1(IEN,SCAN,NDC) ; Process the data
 I $$DRUG(IEN,SCAN,NDC) S SUCCESS=SUCCESS+1 Q 1
 I $$NDC(IEN,SCAN,NDC) S SUCCESS=SUCCESS+1 Q 1
 I $$SYN(IEN,SCAN,NDC) S SUCCESS=SUCCESS+1 Q 1
 Q 0
 ;
XREF ; Create temporary X-REF on NDC and SYNONYMS
 N X,Y,Z
 S X=0 F  S X=$O(^PSDRUG(X)) Q:'X  D
 .S Z=$P($G(^PSDRUG(X,2)),U,4) I $L(Z) S ^TMP($J,"NDC",Z,X)=""
 .S Y=0 F  S Y=$O(^PSDRUG(X,1,Y)) Q:'Y  S Z(0)=^(Y,0)  D
 ..S Z=$P(Z(0),U),^TMP($J,"SYN",Z,X,Y)=""
 ..S Z=$P(Z(0),U,2) I $L(Z) S ^TMP($J,"NDC",Z,X,Y)=""
 Q
 ;
XTMPINIT ; Initialize the ^XTMP Global
 S ^XTMP("VFDI0022",0)=$$FMADD^XLFDT(DT,14)_U_$$NOW^XLFDT
 Q
 ;
 ;=====================================================================
 ;                    R P C   R E G I S T R A T I O N
 ;=====================================================================
RPC ; Register RPC to the vxBCMA Menu Context
 N I,J,X,Y,RPC,SUP,VFDATA,VFDMES,VXS
 S VFDATA("PSB GUI CONTEXT - USER","XWB GET VARIABLE VALUE")=""
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
 ;=====================================================================
 ;                           U T I L I T I E S
 ;=====================================================================
LJ(STR,LEN) ; Left Justify
 Q $$LJ^XLFSTR(STR,LEN)
 ;
