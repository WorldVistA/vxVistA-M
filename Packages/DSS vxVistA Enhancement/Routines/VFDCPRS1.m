VFDCPRS1 ;DSS/SMP - Miscellaneous vxCPRS support ;September 9, 2009
 ;;2011.1.2;DSS,INC VXVISTA OPEN SOURCE;**20**;28 Jan 2013;Build 4
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ; *** This routine can only be invoked via the routine VFDCPRS ***
 ;
 Q
 ;
TABS ; [RPC] VFD CPRS TABS SHOW
 ;;P^PROBLEMS
 ;;M^MEDS
 ;;O^ORDERS
 ;;N^NOTES
 ;;C^CONSULTS
 ;;S^SURGERY
 ;;D^D/CSUMMARY
 ;;L^LABS
 ;;R^REPORTS
 ;
 ; *** If the Meds tab is shown, the Orders tabs MUST also be shown. ***
 ;
 N I,X,ENT,TABS,VFD
 F I=1:1:9 D
 .S TABS(I)=$P($T(TABS+I),";",3),TABS("B",$P(TABS(I),U))=I
 .S VFDRET(I)=TABS(I)_U_1_U_"YES"
 F ENT="SYS","DIV","USR" K VFD D
 .K VFD D GET^VFDCXPR(.VFD,ENT_"~VFD CPRS TABS SHOW"),SET(.VFD)
 ; If Meds tab is shown, the Orders tab must also be shown
 I $P(VFDRET(2),U,3)=1 S $P(VFDRET(3),U,3,4)="1^YES"
 Q
 ;
SET(ARR) ;
 N I,X,Y
 Q:ARR(1)="-1^No data found"
 F I=1:1 Q:'$D(ARR(I))  D
 .S X=TABS("B",$P(ARR(I),U))
 .S VFDRET(X)=ARR(I)
 Q
