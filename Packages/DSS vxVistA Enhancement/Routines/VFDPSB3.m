VFDPSB3 ;DSS/SMP - vxBCMA Utilities ; 10/14/2015
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**20**;01 Dec 2009;Build 2
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
 ;=====================================================================
 ;         R P C   -   V F D   B C M A   N D C   S C A N
 ;=====================================================================
SCAN(RET,INPUT) ; RPC - [VFD BCMA NDC SCAN]
 N X,Y,CNT,DFN,ERROR,REC,VFD S CNT=0
 S X="" F  S X=$O(INPUT(X)) Q:X=""  S Y=INPUT(X) D
 .I $P(Y,U)="REC" S CNT=CNT+1,REC(CNT)=$P(Y,U,2,999) Q
 .S @$P(Y,U)=$P(Y,U,2)
 I '$G(DFN)!'$D(REC) S RET=0 Q
 F I=1:1:CNT S VFD=REC(I) D
 .N I,LEN,OI,VALUE S LEN=$L(VFD,U)/3,OI=$$GET1^DIQ(50,+VFD,2.1,"I")
 .S IEN=$O(^PSB(53.79,"AOIP",DFN,OI,$P(VFD,U,2),"")) I 'IEN S ERROR=1 Q
 .F J=1:1:LEN D PROCESS(IEN,$P(VFD,U,1,3)) S VFD=$P(VFD,U,4,999)
 S RET='$D(ERROR)
 Q
 ;
PROCESS(IEN,DATA) ;
 ; IEN  - IEN in BCMA MEDICATION LOG file (#53.79)
 ; DATA - DRUG IEN ^ Date/Time ^ Scan Value
 ;
 W IEN,*9,DATA,!
 N I,DD,FDA,VFDERR
 S DD=$$FIND1^DIC(53.795,","_IEN_",","Q",+DATA,,,"VFDERR")
 I $D(VFDERR) S ERROR=1 Q
 S FDA(53.795,DD_","_IEN_",",21600.01)=$P(DATA,U,3)
 S FDA(53.795,DD_","_IEN_",",21600.02)=$$NDC^VFDPSB(+DATA,$P(DATA,U,3))
 D FILE^DIE(,"FDA","VFDERR")
 I $D(VFDERR) S ERROR=1 Q
 Q
 ;
 ;=====================================================================
 ;      R P C   -   V F D   B C M A   I N J   S I T E   R E Q 
 ;=====================================================================
INJSITE(RET,VFDOR) ; RPC - [VFD BCMA INJ SITE REQ]
 N X,ROU,VFD S VFDOR=+VFDOR
 I 'VFDOR!('$D(^OR(100,+VFDOR))) S RET="-1^Invalid Order" Q
 S X=$O(^OR(100,VFDOR,4.5,"ID","ROUTE",""))
 I 'X S RET="-1^No ROUTE associated with Order" Q
 S ROU=$G(^OR(100,VFDOR,4.5,X,1))
 I 'ROU S RET="-1^No ROUTE associated with Order" Q
 S RET=+$$GET1^DIQ(51.2,ROU,8,"I")
 Q
