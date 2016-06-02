VFDI0034 ;DSS/SMP - VFD*15.0*26 ENV/PRE/POST ; 10/05/2015 13:40
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**26**;11 Jun 2013;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
ENV ; environmental check
 Q
 ;
 ;=====================================================================
 ;                      P R E - T R A N S P O R T
 ;=====================================================================
 ;
PREX ; [Pre-transport] - KIDS Pre-Transport Routine for VFD*15.0*XXX
 Q:'$L($G(XPDGREF))  ;Sanity check
 N VFDI,VFDR,VFDX
 S VFDR=$NA(@XPDGREF@("VFD"))
 S VFDX=0 F  S VFDX=$O(^VFDV(21611,VFDX)) Q:'VFDX  D
 .N NAME
 .Q:$$GET1^DIQ(21611,VFDX,.05)'="WHO"
 .S NAME=$$GET1^DIQ(21611,VFDX,.01)
 .M @VFDR@(VFDX)=^VFDV(21611,VFDX)
 .S @VFDR@("B",NAME,VFDX)=""
 Q
 ;
 ;=====================================================================
 ;                        P R E - I N S T A L L
 ;=====================================================================
 ;
PRE ; pre-install - Remove data from VFD TABLE Entries with SOURCE=WHO
 Q:'$L($G(XPDGREF))  ;Sanity check
 N FLAG,MSG,VFDR,VFDX S VFDR=$NA(@XPDGREF@("VFD"))
 S VFDX=0 F  S VFDX=$O(^VFDV(21611,VFDX)) Q:'VFDX  D
 .N NAME
 .Q:$$GET1^DIQ(21611,VFDX,.05)'="WHO"
 .S NAME=$$GET1^DIQ(21611,VFDX,.01)
 .Q:'$D(@VFDR@("B",NAME))
 .D DELETE^VFDATA(VFDX) S FLAG=1
 S:$G(FLAG) MSG(2)="Old WHO Data cleared from VFD TABLE File",MSG(1)=""
 D:$D(MSG) BMES^XPDUTL(.MSG)
 Q
 ;
 ;=====================================================================
 ;                       P O S T - I N S T A L L
 ;=====================================================================
 ;
POST ; post-install - Update Package File entry
 Q:'$L($G(XPDGREF))  ;Sanity check
 N FLAG,MSG,VFDR,VFDX S VFDR=$NA(@XPDGREF@("VFD"))
 S VFDX="" F  S VFDX=$O(@VFDR@("B",VFDX)) Q:VFDX=""  D
 .N RET D LOOKUP^VFDATA(.RET,VFDX) Q:'$D(RET)
 .F VFDI=1:1 Q:'$D(RET(VFDI))  I $P(RET(VFDI),U,6)="WHO" D
 ..N IEN,TMP S IEN=+RET(VFDI),TMP=$O(@VFDR@("B",VFDX,""))
 ..D MERGE(IEN,TMP)
 Q
 ;
 ;=====================================================================
 ;                               M I S C
 ;=====================================================================
 ;
MERGE(TO,FRM) ;
 ; TO  - IEN in VFD TABLE FILE - ^VFDV(21611,TO,
 ; FRM - "IEN" in global @XPDGREF@("VFD")
 Q:'$L($G(XPDGREF))  ;Sanity check
 N VFDR,VFDX S VFDR=$NA(@XPDGREF@("VFD"))
 F VFDX=2,3,4 I $D(@VFDR@(FRM,VFDX)) D
 .M ^VFDV(21611,TO,VFDX)=@VFDR@(FRM,VFDX)
 Q
