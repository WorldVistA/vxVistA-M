VFDPSORL ;DSS/AMC - RPC entry point to finish and release an outpatient order in file 100
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**45,57**;29 May 2015;Build 4
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 Q
RELEASE(AXY,VFDORD,VFDSCAN) ;Finish and release an RX without backdoor pharmacy
 ;Input:
 ;    VFDORD  - Order File Entry
 ;    VFDSCAN - User Scanned NDC Code
 I '$G(VFDORD) S AXY="-1^Missing Order Number!" Q
 I '$G(VFDSCAN)!($G(VFDSCAN)'?1.N) S AXY="-1^Invalid Scan Value" Q
 N VFDPU,VFDPUI,VFDISSUE,VFDMSG,VFDPEND
 S VFDPEND=$$GET1^DIQ(100,+VFDORD,5)="PENDING"
 S VFDPU="W",VFDISSUE=1 D RELEASE^VFDPSOR
 S AXY=$S($G(VFDMSG)]"":VFDMSG,1:1)
 Q
DRGEPT(AXY,ORD) ;RPC - VFDPSO IS DRUG EPT
 ;Input Parameter
 ;    ORD - IEN to file 100 ORDERS
 ;
 ;Return Value
 ;    -1 ^ Order File Entry Not Found!
 ;    -1 ^ Unable to find the Drug associated with the Order Entry!
 ;     0 = Drug not EPT
 ;     1 = Drug is EPT
 ;
 N CNT,DRG,IENS,ORIT
 K ^TMP($J,"VFDPSORL")
 S IENS=$P(ORD,";",2)_","_+ORD_","
 S ORIT=$$GET1^DIQ(100.001,IENS,.01,"I",,"VFDERR")
 I $D(VFDERR)!('$G(ORIT)) S AXY="-1^Unable to find ORDERABLE ITEM" Q
 S ORIT("ID")=$$GET1^DIQ(101.43,ORIT_",",2,,,"VFDERR")
 I $D(VFDERR)!($P(ORIT("ID"),";",2)'="99PSP") D  Q
 .S AXY="-1^ORDERABLE ITEM is not a PHARMACY ORDERABLE ITEM"
 D ASP^PSS50(+ORIT("ID"),"","","VFDPSORL")
 S CNT=+^TMP($J,"VFDPSORL",0)
 I CNT=-1 S AXY="-1^Unable to find DRUG" G EXIT
 I CNT=1 D  G EXIT
 .S DRG=$O(^TMP($J,"VFDPSORL",0))
 .S AXY=+$$GET1^DIQ(50,DRG,21600.01,"I")
 I CNT>1 S AXY=0
EXIT ;
 K ^TMP($J,"VFDPSORL")
 Q
