VFDPSB1 ;DSS/LM - vxBCMA to vxPAMS interface ; 6/23/2009
 ;;2009.2;DSS,INC VXVISTA OPEN SOURCE;;01 Dec 2009;Build 3
 ;Copyright 1995-2009,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
EVENT(VFDRSLT,VFDIEN,VFDSCAN,VFDTAB) ;Implement RPC: VFD BCMA EVENT
 ; See EVENT^VFDPSB for documentation
 ;
 I $G(VFDIEN)>0
 E  S VFDRSLT="-1^Invalid required parameter: BCMA MEDICATION LOG IEN." Q
 I $L($G(VFDSCAN))
 E  S VFDRSLT="-1^Invalid required parameter: Scanned medication code." Q
 I $D(^PSB(53.79,VFDIEN)) S VFDTAB=$G(VFDTAB)
 E  S VFDRSLT="-1^Referenced BCMA MEDICATION LOG entry does not exist." Q
 S VFDRSLT=0 I VFDTAB="UDTAB" N VFD50IEN,VFDLIST,VFDNDC
 E  D OTHER(.VFDRSLT,.VFDIEN,.VFDSCAN,.VFDTAB) Q
 ; UNIT DOSE case - Resolve DRUG IEN -
 D SCANMED^PSBRPC2(.VFDLIST,VFDSCAN,VFDTAB)
 I $G(VFDLIST(0))<0 S VFDRSLT="-1^Dispense drug lookup failed." Q
 I $G(VFDLIST(0))>1 S VFDRSLT="-1^Unable to determine exact medication." Q
 S VFD50IEN=$P($G(VFDLIST(1)),U,2) I VFD50IEN>0
 E  S VFDRSLT="-1^Unexpected failure resolving unit dose dispense drug." Q
 ; DRUG IEN resolved.
 S VFDNDC=$$NDC(VFD50IEN,VFDSCAN)
 D FILE ;File AUDIT LOG entry (whether we have an NDC or not)
 I VFDNDC="" S VFDRSLT="-1^Contact Pharmacy/Nursing Management that Five Rights was used. Click OK to continue with documentation."
 ;
 D SCHEDULE
 ;
 Q
OTHER(VFDRSLT,VFDIEN,VFDSCAN,VFDTAB) ; Non-unit dose medications
 ; IV, Piggyback, ward stock, etc.
 ;
 S VFDRSLT="-1^Not implemented.  This RPC version supports 'unit dose' only." Q
 ; Remove the preceding line when non-unit dose interface is ready
 ;
 Q
NDC(VFDDIEN,VFDSCAN) ;[Private]   Match SYNONYM or DRUG to NDC
 ; VFDDIEN=[Required] File 50 (DRUG) IEN
 ; VFDSCAN=[Required] Raw scan code (See EVENT^VFDPSB documentation)
 ;
 ; Returns corresponding NDC or the empty string, if NDC cannot be resolved.
 ;
 ; DSS/SMH... fix NDC capture to get the real NDC associated with a scan.
 ; LM's code only got the main NDC on the drug file entry; which is incorrect.
 ;
 N OUTNDC S OUTNDC=""  ; default
 N SYNIEN S SYNIEN=$O(^PSDRUG("C",VFDSCAN,VFDDIEN,""))  ; synonym ien
 I 'SYNIEN QUIT ""
 N SYN0 S SYN0=^PSDRUG(VFDDIEN,1,SYNIEN,0) ; syn 0
 N NDC S NDC=$P(SYN0,U,2)
 I 'NDC QUIT ""
 S OUTNDC=$$NDCFMT^PSSNDCUT(NDC)  ; format NDC. If not valid, = ""
 Q OUTNDC  ; May be empty.
 ;
FILE ;[Private] File AUDIT LOG entry for scan data
 ; ZEXCEPT: VFDIEN,VFDSCAN,VFDNDC
 ; Variables VFDIEN, VFDSCAN, and VFDNDC are required.
 ;
 I $G(VFDIEN) S VFDSCAN=$G(VFDSCAN),VFDNDC=$G(VFDNDC)
 E  Q  ;Invalid context
 N VFDFDA,VFDR S VFDR=$NA(VFDFDA(53.799,"+1,"_VFDIEN_","))
 S @VFDR@(.01)=$$NOW^XLFDT
 S @VFDR@(.02)=$G(DUZ,.5)
 S @VFDR@(.03)="Data for vxPAMS"
 S @VFDR@(21600.01)=VFDSCAN
 S @VFDR@(21600.02)=VFDNDC
 ;
 N VFDIENR D UPDATE^DIE(,$NA(VFDFDA),$NA(VFDIENR)) ;File AUDIT LOG entry
 Q
SCHEDULE ;[Private] Schedule task to interface data to vxPAMS.
 ;
 N ZTDESC,ZTDTH,ZTIO,ZTRTN,ZTSAVE,ZTSK
 S ZTDESC="vxVistA to vxPAMS BCMA Interface"
 S ZTDTH=$H,ZTIO="",ZTRTN="DQ^VFDPSB2",ZTSAVE("VFD*")=""
 D ^%ZTLOAD
 Q
 ;
TNDC D EN^%ut($T(+0),1) ; Test NDC logic
 QUIT
T1 ; @TEST Test everything is okay
 N % S %=$$NDC(268,317478012023) ; Benztropine 1mg/ml inj 2ml
 D CHKEQ^%ut(%,"17478-0012-02")
 QUIT
T2 ; @TEST Bad IEN
 N % S %=$$NDC(694,305910658014) ; Buspirone hcl 10mg tab (wrong IEN)
 D CHKEQ^%ut(%,"")
 QUIT
T3 ; @TEST Bad NDC
 N % S %=$$NDC(322,305910658999)
 D CHKEQ^%ut(%,"")
 QUIT
T4 ; @TEST NDC as DIEN
 N % S %=$$NDC(322,322)
 D CHKEQ^%ut(%,"")
 QUIT
T5 ; cant TEST no NDC on barcode
 N % S %=$$NDC(657,361314014252)
 D CHKEQ^%ut(%,"")
 QUIT
 ;
