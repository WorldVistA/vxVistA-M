VFDORNDC ;DSS/RBN - CPRS RPC NDC-ORD ID MATCH ; 10/31/11 2:21pm
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**50**;11 Jun 2013;Build 24
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 Q
NDC(VFDRET,VFDORID,VFDNDC) ; CPRS RPC
 ; VFORID=[Required] File 55 ORDER NUMBER
 ; VFDNDC=[Required] NDC OF DRUG
 ; VFDRET=Return value (passed by reference)
 ; 
 ; Returns:
 ;        1 - If the NDC is found for the order id.
 ;        0 - If NDC is not found for the order id.
 ;       -1 - If an error occurs.  Format is "-1^error message"
 ; 
 N DRUG,NDC,VFD,VFDLIST
 S VFDRET=0,DRUG=$O(^OR(100,+VFDORID,4.5,"ID","DRUG",""))
 I 'DRUG S VFDRET="-1^Dispensed DRUG not found" Q
 I DRUG S DRUG=+$G(^OR(100,+VFDORID,4.5,DRUG,1))
 I 'DRUG!'$D(^PSDRUG(DRUG)) S VFDRET="-1^Dispensed DRUG not found" Q
 S NDC=$$GET1^DIQ(50,DRUG,31)
 I $$MATCH(VFDNDC,NDC) S VFDRET=1 Q
 D GETS^DIQ(50,DRUG_",","9*",,"VFDLIST","VFDERR")
 S VFD="" F  S VFD=$O(VFDLIST(50.1,VFD)) Q:VFD=""  D  Q:VFDRET
 .N NA S NA=$NA(VFDLIST(50.1,VFD))
 .I $$MATCH(VFDNDC,@NA@(.01)) S VFDRET=1 Q
 .I $$MATCH(VFDNDC,@NA@(2)) S VFDRET=1 Q
 Q
 ;
MATCH(X,Y) ;
 S X(1)=$TR(X,"-",""),Y(1)=$TR(Y,"-","")
 I X=Y!(X=Y(1))!(X(1)=Y)!(X(1)=Y(1)) Q 1
 Q 0
