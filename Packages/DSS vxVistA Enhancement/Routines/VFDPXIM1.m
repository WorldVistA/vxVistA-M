VFDPXIM1 ;DSS/WLC/JM - MAIN ENTRY TO VFDPXIM ROUTINES ; 30 Dec 2015  3:15 pm
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**10,19,40**;11 Jun 2013;Build 3
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;All Integration Agreements for VFDPXIM*
 ;DBIA#  Supported Reference
 ;-----  ----------------------------------------------------
 ;APP    AGET^ORWORR
 ;2056   GET1^DIQ,GETS^DIQ
 ;10103  NOW^XLFDT
 ;1889   $$DATA2PCE^PXAPI
 ;1894   ENCEVENT^PXKENC
 ;       $$FIND1^DIC
 ;       FILE^DIE
 ;
GETIMM(VFDIM)  ;  RPC:  VFD PXIM GET ORDERABLE
 N CNT,I,ITEM,J,K,X,Y,Z
 S (CNT,X)=0 F  S X=$O(^PSDRUG("AOC",X)) Q:'X  D
 .S Y="" F  S Y=$O(^PSDRUG("AOC",X,Y)) Q:Y=""  D
 ..I Y'="IM100",Y'="IM105",Y'="IM109" Q  ; Immunizations only
 ..S Z=0 F  S Z=$O(^PSDRUG("AOC",X,Y,Z)) Q:'Z  D
 ...Q:$D(^PSDRUG(Z,"I"))  ; Inactive item
 ...S I=+$G(^PSDRUG(Z,2)) Q:'I
 ...S ITEM=$O(^ORD(101.43,"ID",I_";99PSP",0)) Q:+$G(^ORD(101.43,ITEM,.1)) 
 ...S VFDIM(CNT)=ITEM_U_$P(^ORD(101.43,ITEM,0),U,1) S CNT=CNT+1
 ..S VFDIM(0)=CNT-1
 Q
 ;
GETORD(VFDIM,DFN)  ;  RPC:  VFD PXIM GET IMM ORDERS
 ; RPC to retrieve active Immunization orders for patient
 ; INPUT:
 ;    DFN = Pointer to PATIENT (#2) file.
 ; OUTPUT:
 ;   VFDIM(n) where:
 ;    VFDIM(0)=Total number of active Immunization orders
 ;    VFDIM(n)=Order # ^ Orderable Item description
 ;
 N ACT,ARR,CNT,MAX,OR,ORN,PDRG,VFDIM1,TYP,X,Y,Z
 S DFN=$G(DFN) K VFDIM I '$D(^DPT(DFN)) S VFDIM(0)="-1^Invalid DFN sent" Q
 D AGET^ORWORR(.VFDIM1,DFN,1)  ; get list of all orders
 S CNT=1
 S X="" F  S X=$O(^TMP("ORR",$J,X)) Q:X=""  S MAX=$P(^TMP("ORR",$J,X,.1),U,1) D
 .F Y=1:1:MAX S ORN=+^TMP("ORR",$J,X,Y) D
 ..I $$ISIMM(ORN) D
 ...D GETS^DIQ(100,ORN_",","**",,"ARR")
 ...S VFDIM(CNT)=ORN_U_ARR(100.001,"1,"_ORN_",",.01),CNT=CNT+1
 .S VFDIM(0)=CNT-1
 Q
 ;
ISIMM(ORN) ; Is this order an Immunization Order?
 N X,Y,Z,ACT,OI,PDRG,TYP,RET
 S RET=0
 F Z=1:1 Q:'$D(^OR(100,ORN,4.5,Z))  D
 .I $P(^OR(100,ORN,4.5,Z,0),U,4)="ORDERABLE" S OI=+^OR(100,ORN,4.5,Z,1)
 I $G(OI) Q $$ISIMMOI(OI)
 Q RET
 ;
ISIMMOI(VFDOI) ; Is this orderable item an immunization?
 N RET,PDRG,TYP
 S RET=0 D
 .S TYP="",PDRG=+$$GET1^DIQ(101.43,VFDOI_",",2),TYP=$O(^PSDRUG("AOC",PDRG,TYP))
 .I TYP'="IM100",TYP'="IM105",TYP'="IM109" Q  ; not immunization
 .S RET=1
 Q RET
 ;
GETLOTS(VFDIM,ORN)  
 ; RPC to return associated LOT #'S FOR Vaccines
 ; Each lot number indicates a separate quantity of vaccine.
 ; These could be by Vial, ml, mcl, etc.
 ; INPUT:
 ;    ORN = Internal CPRS Order number
 ; OUTPUT:
 ;    LIST(n) where:
 ;    LIST(0) = Count of Active Lots
 ;    LIST(n) = IEN # ^ LOT NUMBER ^ MANUFACTURER ^ CVX CODE ^ MVX CODE ^EXPIRATION DATE ^ QUANTITY AVAIL ^
 ;
 ; The scheme for looking up lots goes like this:
 ; The Order record points to a list orderable items
 ; Each Orderable item is then looked up in the DRUG file to get the Pharmacy Orderable Item
 ; The Pharmacy Orderable Item has the pointer to the IMMUNIZATION file
 N I,J,X,Y,CNT,CVX,ERR,FLE,ID,IMM,IMMREC,LOT,LOTAR,MVX,ORBLE,PHID,VFDIEN
 S ORN=$G(ORN),VFDIM(0)=0 I 'ORN S VFDIM(0)="-1^Invalid Order number sent." Q
 I '$D(^OR(100,ORN)) S VFDIM(0)="-1^Order NOT on file." Q
 S CNT=0 F I=1:1 Q:'$D(^OR(100,ORN,.1,I))  S X=+^OR(100,ORN,.1,I,0) D
 . S PHID=+$P(^ORD(101.43,X,0),U,2) ; Pharmacy Orderable Item IEN
 . S ID="",ID=$O(^PSDRUG("AOC",PHID,ID)) Q:$E(ID,1,2)'="IM"  ; Check that its an Immunization
 . S ORBLE(X)=PHID ; Save the IDs for later
 ; 
 S VFDIM(0)="0^No lot numbers found"  ;Default return value
 S X=0 F  S X=$O(ORBLE(X)) Q:'X  D
 . I '$D(^PS(50.7,ORBLE(X),"IMM")) S VFDIM("PHID")=ORBLE(X),VFDIM(0)="-1^Orderable Item "_ORBLE(X)_" is not linked to an immunization" Q
 . S IMM=$P(^PS(50.7,ORBLE(X),"IMM"),U) I IMM=""!'$D(^AUTTIMM(IMM,0)) S VFDIM("IMM")=IMM,VFDIM(0)="-1^Pharmacy Orderable Item "_ORBLE(X)_" links to an invalid immunization: "_IMM Q
 . S IMMREC=$G(^AUTTIMM(IMM,0)) I IMMREC="" S VFDIM("IMM")=IMM,VFDIM(0)="-1^Immunization: "_IMM_" is corrupt" Q  ; jm 10/9/15 add additional data check
 . ; GET THE CVX CODE FOR THIS IMMUNIZATION
 . S CVX=$P(IMMREC,U,3) ;save the immunization CVX code for later
 . ; FIND THE LOTS AVAILABLE FOR THIS IMMUNIZATION
 . S LOT=0 F  S LOT=$O(^AUTTIML("C",IMM,LOT)) Q:+LOT'>0  D
 . . S VFDIEN=LOT_","
 . . D GETS^DIQ(9999999.41,VFDIEN,"*","IE","LOTAR","ERR")
 . . N FLE S FLE=$NA(LOTAR(9999999.41,VFDIEN))
 . . Q:$G(@FLE@(.03,"I"))       ;0 IS ACTIVE - QUIT IF NON-ZERO
 . . Q:$G(@FLE@(.09,"I"))<DT    ;EXPIRED
 . . Q:'$G(@FLE@(.12,"I"))      ;UNUSED DOSES = 0
 . . D LOTADD
 . S VFDIM(0)=CNT_U_IMM
 Q
 ;
LOTADD  ; ADD A LOT NUMBER RECORD TO THE GETLOTS RESULT ARRAY
 S CNT=CNT+1,VFDIM(CNT)=+LOT_U ; IEN
 F I=.01,.02,.09,.12,.18 D   ; LOT#,MFG,EXP DT,DOSES UNUSED,NDC CODE,  removedSTART COUNT,.11
 . ; IEN # ^ LOT NUMBER ^ MANUFACTURER ^ CVX CODE ^ MVX CODE ^ EXPIRATION DATE ^ DOSES UNSED ^ NDC CODE
 . S VFDIM(CNT)=VFDIM(CNT)_@FLE@(I,"E")_U    ;,"p","50","LOTAR","ERR")
 . I I=.02 D  ;GET THE MFG'S MVX #
 . . S MVX=$P($G(^AUTTIMAN(@FLE@(.02,"I"),0)),U,2)
 . . S MVX=$S($G(MVX):MVX,1:"")
 . . S VFDIM(CNT)=VFDIM(CNT)_CVX_U_MVX_U ; ADD CVX # AND MVX # TO OUTPUT
 Q
 ;
STORLOT(VFDIM,VFDRAY)  ; RPC:  VFD PXIM STORE LOT
 ; This RPC will set a drug order as administered by setting the STOP or
 ; EXPIRATION DATE to NOW.  This will in effect expire the order when
 ; Outpatient or Inpatient Pharmacy normally does so.
 ;  INPUT:
 ;    ARRAY is a LIST Variable of values and pointers to store, where
 ;    ARRAY(0)= "A" for Add, "E" for Edit ^ IEN to edit (blank for ADD)
 ;    ARRAY(1-n)=Field # ^ VALUE  (i.e.  ARRAY(1) = ".02^32-A-456" to 
 ;               store lot #32-A-456
 ;  OUTPUT:
 ;    VFDIM=-1^Error Text
 ;    or
 ;    VFDIM=1^Success
 ;
 N ADDED,I,IEN,J,FDA,ERR
 S VFDRAY=$G(VFDRAY) I '$O(VFDRAY(0)) S VFDIM="-1^No data sent." Q
 S ADDED=$S(VFDRAY(0)="A":1,1:0),IEN=$P(VFDRAY(0),U,2)
 S I=0 F  S I=$O(VFDRAY(I)) Q:'I  D
 .S J=VFDRAY(I),FDA(21630.01,$S(ADDED="A":"+1,",1:IEN_","),$P(J,U,1))=$P(J,U,2)
 D UPDATE^DIE(,"FDA",,"ERR")
 I $D(ERR) S VFDIM="-1^"_$G(ERR("DIERR",1,"TEXT",1))
 E  S VFDIM="1^Success"
 Q
 ;
ERROR(VFDARR) ; RPC: VFD PXIM ENTERED IN ERROR
 ;  This RPC will mark an entry in the V IMMUNIZATION file as being
 ;  ENTERED IN ERROR.  It will record the date/time ($$NOW) and the 
 ;  current user (DUZ), as well as a REASON as to why this is being
 ;  done.
 ;
 ;  Input:
 ;  VFDARR is an array
 ;  VFDARR(n) = TAG^DATA
 ;
 ;  Supported Tags:
 ;    IMM - req - IEN in the V IMMUNIZATION file
 ;    RSN - req - REASON why this record is being entered in error
 ;
 ;  Output:
 ;    If successful:  1
 ;    If FAILURE:    -1^Error Message
 ;
 ;  Updates the fields:  File 9000010.11
 ;    21601.01  ENTERED IN ERROR (D), [21601;1]
 ;    21601.02  ENTERED IN ERROR BY (P200'), [21601;2]
 ;    21601.03  ENTERED IN ERROR REASON (F), [21601;3]
 ;
 N X,Y,IMM,RSN,VFDERR,VFDFDA
 S X="" F  S X=$O(VFDARR(X)) Q:X=""  S Y=VFDARR(X),@$P(Y,U)=$P(Y,U,2)
 I '$G(IMM) Q "-1^No V IMMUNIZATION record passed in"
 I '$D(^AUPNVIMM(IMM)) Q "-1^Invalid V IMMUNIZATION record"
 I $L($G(RSN))<10 Q "-1^Invalid REASON"
 S IMM=IMM_","
 S VFDFDA(9000010.11,IMM,21601.01)=$$NOW^XLFDT
 S VFDFDA(9000010.11,IMM,21601.02)=DUZ
 S VFDFDA(9000010.11,IMM,21601.03)=RSN
 D FILE^DIE(,"VFDFDA","VFDERR")
 I $D(VFDERR) Q "-1^Error updating V IMMUNIZATION file"
 Q 1
 ;
ROUTES(VFDIM) ;RPC: VFD PXIM ROUTES
 N VIMMI,RTE
 S RTE="",VIMMI=0
 F  S RTE=$O(^PXV(920.2,"B",RTE)) Q:RTE=""  D
 . S IDX=0 F  S IDX=$O(^PXV(920.2,"B",RTE,IDX)) Q:IDX'>0  D 
 . . S VIMMI=VIMMI+1,VFDIM(VIMMI)=IDX_U_RTE
 S VFDIM(0)=VIMMI
 Q
 ;  
SITES(VFDIM) ;RPC: VFD PXIM SITES
 N IDX,SITE,VIMMI
 S SITE="",VIMMI=0
 F  S SITE=$O(^PXV(920.3,"B",SITE)) Q:SITE=""  D
 . S IDX=0 F  S IDX=$O(^PXV(920.3,"B",SITE,IDX)) Q:IDX'>0  D 
 . . I ^PXV(920.3,"B",SITE,IDX)'=1 S VIMMI=VIMMI+1,VFDIM(VIMMI)=IDX_U_SITE ;FILTER OUT HL7 CODES IN THE "B" INDEX (!)
 S VFDIM(0)=VIMMI
 Q
 ;  
SOURCES(VFDIM) ;RPC: VFD PXIM SOURCES - May not be needed but, here it is if we do!
 N SRC,VIMMI
 S SRC="",VIMMI=0
 F  S SRC=$O(^PXV(920.1,"B",SRC)) Q:SRC=""  D
 . S IDX=0 F  S IDX=$O(^PXV(920.1,"B",SRC,IDX)) Q:IDX'>0  D 
 . . S VIMMI=VIMMI+1,VFDIM(VIMMI)=IDX_U_SRC
 S VFDIM(0)=VIMMI
 Q
 ;  
VIS(VFDIM,IMM) ;RPC: VFD PXIM VIS
 ; RETRIEVE LIST OF VACCINE INFORMATION STATEMENTS FOR THE GIVEN IMMUNIZATION
 ; IMM = IEN OF VACCINE IN QUESTION
 ; Returns VFDIM(0..N) where:
 ;      VFDIM(0) = Count of records returned
 ;      VFDIM(N) = VIS IEN# ^ VIS NAME (Language) EDITION DATE
 ; as of patch 40 this RPC returns only the latest dated record
 ;
 N CNT,FLD,IMMVIS,VFDVIS,VISIEN,VISDATA,VISNM,VISDT,VISL,LASTDT
 K VFDVIS
 I '$G(IMM) S VFDIM(0)="-1^Invalid Immunization IEN" Q
 I '$D(^AUTTIMM(IMM)) S VFDIM(0)="-1^Invalid IMMUNIZATION record" Q
 I '$D(^AUTTIMM(IMM,4,0)) S VFDIM(0)="0^No VIS records" Q  ;Not all immunizations have VIS records
 S CNT=0,IMMVIS=0
 F  S IMMVIS=$O(^AUTTIMM(IMM,4,IMMVIS)) Q:IMMVIS'>0  D
 . S VISIEN=$P(^AUTTIMM(IMM,4,IMMVIS,0),U,1)
 . K VISAR
 . D GETS^DIQ(920,VISIEN_",",".01;.04;.02;.03","IE","VISAR","VISERR")
 . S VISDATA=$NA(VISAR(920,VISIEN_",")),VISNM=@VISDATA@(.01,"E"),VISDT=@VISDATA@(.02,"I"),VISL=@VISDATA@(.04,"I")
 . I @VISDATA@(.03,"I")="C" D
 . . S LASTDT=$G(VFDVIS(VISNM,VISL))
 . . I LASTDT<VISDT S VFDVIS(VISNM,VISL)=VISDT,VFDVIS(VISNM,VISL,"IEN")=VISIEN
 S VISNM=""
 F  S VISNM=$O(VFDVIS(VISNM)) Q:VISNM=""  D
 . S VISL=""
 . F  S VISL=$O(VFDVIS(VISNM,VISL)) Q:VISL=""  D
 . . S VFDLANG=$$TOCAMEL($$GET1^DIQ(.85,VISL_",","1",,,"VFDMSGS"))
 . . S VISDT=$G(VFDVIS(VISNM,VISL))
 . . S VISIEN=$G(VFDVIS(VISNM,VISL,"IEN"))
 . . S CNT=CNT+1,VFDIM(CNT)=+VISIEN_U_VISNM_" ("_VFDLANG_") "_$$FMTE^XLFDT(VISDT)
 S VFDIM(0)=CNT
 Q
 ;
PRODUCT(VFDIM,IMM) ; rpc - VFD PXIM PRODUCT NAMES
 ; RETRIEVE LIST OF PRODUCT NAMES FOR A GIVEN IMMUNIZATION
 ; IMM = IEN OF VACCINE IN QUESTION
 ; Returns VFDIM(0..N) where:
 ;      VFDIM(0) = Count of records returned
 ;      VFDIM(1..N) = PRODUCT NAMES
 N CNT,PROD,VIMMI
 I '$D(^AUTTIMM(IMM)) S VFDIM(0)="-1^Invalid IMMUNIZATION record" Q
 I '$D(^AUTTIMM(IMM,5,0)) S VFDIM(0)="0^No Product Names for this Immunization" Q  ;Not all immunizations have Product Name records
 S (CNT,VIMMI,VFDIM(0))=0
 F  S VIMMI=$O(^AUTTIMM(IMM,5,VIMMI)) Q:VIMMI'>0  D
 . S CNT=CNT+1,VFDIM(CNT)=^AUTTIMM(IMM,5,VIMMI,0)
 S VFDIM(0)=CNT
 Q
 ;
TOCAMEL(VFDSTR) ; Utility to change XYYYYYY to Xyyyyyy (not really camel case...)
 Q $E(VFDSTR,1,1)_$TR($E(VFDSTR,2,99),"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz")
 ;
