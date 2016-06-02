VFDPXIM2 ;DSS/WLC/JM - MAIN ENTRY TO VFDPXIM ROUTINES ; 8 Jan 2016  9:32 PM
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**10,19,45**;11 Jun 2013;Build 5
 ;Copyright 1995-2016,Document Storage Systems Inc. All Rights Reserved
 ;
 ;All Integration Agreements for VFDPXIM*
 ;DBIA#  Supported Reference
 ;-----  ----------------------------------------------------
 ;1889   $$DATA2PCE^PXAPI
 ;1894   ENCEVENT^PXKENC
 ;2056   GET1^DIQ
 ;       $$FIND1^DIC
 ;       FILE^DIE
 ;       UPDATE^DIE
 ;       NOW^XLFDT
 ;
 ; This RPC will set a drug order as administered by setting the STOP or
 ; EXPIRATION DATE to NOW.  This will in effect expire the order when
 ; Outpatient or Inpatient Pharmacy normally does so.  It also creates
 ; a Visit and V Immunization records using $$DATA2PCE^PXAPI
 ;
 ; See RPC:  VFD PXIM GET IMM ORDERS - this validates that the order
 ; is an immunization order.
 ;  INPUT STRING:
 ;    1-DOSE = Dose administered
 ;    2-UNIT = Unit of dose administered
 ;    3-WHO = Who did the administration, Pointer to New Persons (#200)
 ;    4-START = Start Date/Time of administration 
 ;    5-END = End Date/Time of administration
 ;    6-ORN = Internal CPRS Order number pointer (#100)
 ;    7-LOC = Location, pointer to Hospital Location (#44)
 ;    >>>8-VFD IMM = pointer to VFD immunization (#21630.01) <<<VIMM 2.0 No longer used
 ; VIMM 2.0
 ;    8-IMM - Immunization IEN
 ;    9-LOT NUMBER = IEN of Lot Number administered
 ;   10-ROUTE = Route used to administer the vaccination
 ;   11-SITE = Site of administration (Body area)
 ; Patch 19 - Add Reaction, Contraindications, Reason for refusal placeholders
 ;   12-REACTION = Reaction Code (0=None;1=Fever;2=Irritability;3=Local reaction or swelling;4=Vomiting;5=Rash or itching;6=Lethargy;7=Convulsions
 ;                                8=Arthritis or arthralgias;9=Anaphylaxis or collapse;10=Respiratory distress;11=Other)
 ;   13-CONTRAINDICATION = Contraindication code (0|1|null)
 ;   14-REASON FOR REFUSAL (13 and 14 are for future use)
 ; Patch 19 - End of changes
 ;   15-VIS = Vaccine Info Stmt info (Format: COUNT|VISIEN1:VISDATE1;VISIEN2:VISDATE2;...;VISIENCOUNT:VISDATECOUNT
 ;   16-RMKS = Remarks/comments
 ; VIMM 2.0 P10 End of changes
 ;  OUTPUT:
 ;    VFDIM=-1^Error Text
 ;    or
 ;    VFDIM=1^Success updated
 ;
ADMIN(VFDIM,VFDRAY)  ;RPC: VFD PXIM ADMIN - Entry point
 N DR,DIE,DA,PSOIEN,VFDORD,VFDSTA
 D DATA Q:VFDIM["-1"
 S VFDIM="1^Administration Successful."
 S VFDORD=$P(VFDRAY,U,6),VFDSTA="",VFDSTA=$$FIND1^DIC(100.01,,,"COMPLETE")
 I VFDSTA D STATUS^ORCSAVE2(VFDORD,VFDSTA)
 ;Set Prescription Status to 10 ("DONE")
 S PSOIEN=+$G(^OR(100,VFDORD,4))
 S DR="100///10",DIE="^PSRX(",DA=PSOIEN D ^DIE
 Q
 ;
 ;------------------------ PRIVATE SUBROUTINES ------------------------
 ;
DATA ;Get and Validate data
 N CONTRA,DFN,DFNR,DOSE,END,IMM,LOC,ORN,OPROV,PKG,PREF,RMKS,UNIT,VFDAY
 N LOTNUM,REACTION,REFUSAL,ROUTE,SITE,VIS,VISCNT,VISI ;VIMM 2.0
 N VFDIMM,WHO,WHEN
 S VFDAY=$G(VFDRAY)
 I '$D(VFDAY)!(VFDAY="") S VFDIM="-1^No data passed" Q
 S DOSE=$P(VFDAY,U,1),UNIT=$P(VFDAY,U,2),WHO=$P(VFDAY,U,3),WHEN=$P(VFDAY,U,4)
 S END=$P(VFDAY,U,5),ORN=$P(VFDAY,U,6),LOC=$P(VFDAY,U,7),IMM=$P(VFDAY,U,8)
 S LOTNUM=$P(VFDAY,U,9),ROUTE=$P(VFDAY,U,10),SITE=$P(VFDAY,U,11)
 S REACTION=$P(VFDAY,U,12),CONTRA=$P(VFDAY,U,13),REFUSAL=$P(VFDAY,U,14) ; S25568 Add Reaction,contra and refusal inputs fields
 S VIS=$P(VFDAY,U,15),RMKS=$P(VFDAY,U,16) ; VIMM 2.0 
 I 'ORN S VFDIM="-1^No Order number sent" Q
 I '$D(^OR(100,ORN)) S VFDIM="-1^Invalid Order number." Q
 S PREF=$G(^OR(100,ORN,4))
 S DFNR=$P(^OR(100,ORN,0),U,2) I DFNR["DPT" S DFN=+DFNR
 I '$D(DFN)!(DFN="") S VFDIM="-1^No DFN for order" Q
 I PREF["P" S VFDIM="-1^Order in PENDING status." Q
 I DOSE="" S VFDIM="-1^Invalid data for Dose" Q
 I UNIT="" S VFDIM="-1^Invalid data for Unit" Q
 I WHO="" S WHO=DUZ ;Use current login
 I LOC="" S VFDIM="-1^Invalid data for Location" Q
 ;I VFDIMM="" S VFDIM="-1^Invalid VFD Immunization" Q
 ;S IMM=$P($G(^VFD(21630.01,VFDIMM,3)),U,3) ;Pointer to 9999999.14
 I IMM="" S VFDIM="-1^Invalid VFD Immunization pointer" Q
 ;S OPROV=$P(^OR(100,ORN,0),U,4) ;*45 changed to use Fileman call
 S OPROV=$$ORDPRV(ORN)
 I OPROV="" S OPROV=$$GET1^DIQ(100,ORN_",","1","I") ;*45
 ; VIMM 2.0
 I LOTNUM="" S VFDIM="-1^No LOT NUMBER specified"
 I '$D(^AUTTIML(LOTNUM,0)) S VFDIM="-1^Invalid LOT NUMBER" Q
 I ROUTE="" S VFDIM="-1^ROUTE not specified" Q
 I '$D(^PXV(920.2,ROUTE)) S VFDIM="-1^Invalid ROUTE" Q
 I SITE="" S VFDIM="-1^SITE not specified" Q
 I '$D(^PXV(920.3,SITE)) S VFDIM="-1^Invalid SITE" Q
 ; The VIS piece will look like:
 ; ^COUNT|VISIEN1:VISDATE1;VISIEN2:VISDATE2;...;VISIENCOUNT:VISDATECOUNT^
 I $D(VIS) S VISCNT=$P(VIS,"|"),VIS=$P(VIS,"|",2) F VISI=1:1:VISCNT S VIS(VISI)=$P(VIS,";",VISI)
 ; END VIMM 2.0
 ;
 ; Get dialog
 N DLG,DLGN
 S DLG=$P(^OR(100,ORN,0),U,5),DLGN=$$GET1^DIQ(101.41,DLG_",",.01)
 ; Get first 8 diagnosis
 N DIAG,DIAGN,I
 S DIAGN=0,I=1
 F  S DIAGN=$O(^OR(100,ORN,5.1,DIAGN)) Q:'DIAGN!(I>7)  D
 .S DIAG(I)=^OR(100,ORN,5.1,DIAGN,0)
 .S I=1+1
 S PKG="",PKG=$O(^DIC(9.4,"C","VFD",PKG))
 S:'WHEN!(WHEN="") WHEN=$E($$NOW^XLFDT,1,12)
 S:'END!(END="") END=$E($$NOW^XLFDT,1,12)
 S VFDIM="1^All data is good"
 ;
PCEDATA ;Create Visit and V Immunization (#9000010.11) record
 N VFDRA,I,OK,VISITIEN,VFDSCR
 K VISITIEN
 S VFDRA("ENCOUNTER",1,"ENC D/T")=WHEN
 S VFDRA("ENCOUNTER",1,"PATIENT")=DFN
 S VFDRA("ENCOUNTER",1,"HOS LOC")=LOC
 S VFDRA("ENCOUNTER",1,"SERVICE CATEGORY")="X" ;Ancillary Package Daily Data
 S VFDRA("ENCOUNTER",1,"ENCOUNTER TYPE")="A" ;Ancillary
 S VFDRA("ENCOUNTER",1,"CHECKOUT D/T")=END
 S VFDSCR="VFD IMMUNOLOGY"
 S VFDRA("IMMUNIZATION",1,"IMMUN")=IMM ;Pointer to 9999999.14 
 S I=1 F I=$O(DIAG(I)) Q:'I  D
 .S:I=1 VFDRA("IMMUNIZATION",1,"DIAGNOSIS")=DIAG(I)
 .S:I>1 VFDRA("IMMUNIZATION",1,"DIAGNOSIS "_I)=DIAG(I)
 S VFDRA("IMMUNIZATION",1,"ENC PROVIDER")=WHO
 S VFDRA("IMMUNIZATION",1,"EVENT D/T")=WHEN
 ;--Add new fields for VIMM 2.0 - PX*1*209
 S VFDRA("IMMUNIZATION",1,"LOT NUM")=LOTNUM
 S VFDRA("IMMUNIZATION",1,"ADMIN ROUTE")=ROUTE
 S VFDRA("IMMUNIZATION",1,"ANATOMIC LOC")=SITE
 S VFDRA("IMMUNIZATION",1,"REACTION")=REACTION       ;*19
 S VFDRA("IMMUNIZATION",1,"COMMENT")=RMKS
 ;--End VIMM 2.0 Change
 S VFDRA("IMMUNIZATION",1,"DOSAGE")=DOSE ; This bit was already being passed down from the GUI but was not used(?)
 S VFDSCR="VFD IMMUNOLOGY"
 ;--VIMM 2.0 - Vaccine Information Sheet(s)
 ;BUG in IMM^PXAIIMM will not allow multiple VIS statemnts to be recorded yet (a/o patch 209)
 ;F VISI=1:1:VISCNT S VFDRA("IMMUNIZATION",1,"VIS",VISI)=VIS(VISI)
 ;use the first one
 ;I VISCNT>0 S VFDRA("IMMUNIZATION",1,"VIS",1)=VIS(1) ;*19 Commented out and added fourth level index (=1) to VFDRA
 ;--End VIMM 2.0 Change
 S OK=$$DATA2PCE^PXAPI("VFDRA",PKG,VFDSCR,.VISITIEN)
 I OK<0 S VFDIM="-1^Problem adding PCE data." Q
 I '$D(VISITIEN)!(VISITIEN="") S VFDIM="-1^No visit returned from DATA2PCE" Q
 I '$D(^AUPNVSIT(VISITIEN,0)) S VFDIM="-1^No Visit record" Q
 ; VIMM 2.0
 ; Following code is the workaround to file new Immunization data
 ; Should not be needed after Patch 210 is released by VA and instaled in vxVistA
 D ENCEVENT^PXKENC(VISITIEN)
 ; Get V IMM record from visit
 N MM,NODE,VFDVIM,VIM,VISIT,VREC,ICNT
 S (VIM,VISIT,VFDVIM)="",ICNT=0
 F  S VIM=$O(^TMP("PXKENC",$J,VISITIEN,"IMM",VIM)) Q:'VIM!(VFDIM["-1")  D
 .S VREC=^AUPNVIMM(VIM,0)
 .I VREC="" S VFDIM="-1^No V-Immunization record" Q
 .I $P(VREC,U,1)=IMM D
 ..N VFDA,VFDERR
 ..S VFDIEN=VIM_","
 ..S VFDA(9000010.11,VFDIEN,.05)=LOTNUM
 ..S VFDA(9000010.11,VFDIEN,.06)=REACTION  ;*19
 ..S VFDA(9000010.11,VFDIEN,1202)=OPROV  ;*45
 ..S VFDA(9000010.11,VFDIEN,1207)=LOTNUM
 ..S VFDA(9000010.11,VFDIEN,1301)="1" ;Event info source = new immunization *45
 ..S VFDA(9000010.11,VFDIEN,1302)=ROUTE
 ..S VFDA(9000010.11,VFDIEN,1303)=SITE
 ..S VFDA(9000010.11,VFDIEN,1312)=DOSE_" "_UNIT
 ..D FILE^DIE(,"VFDA","VFDERR")
 ..I $G(VFDERR) S VFDIM="-1"_VFDERR
 ..K VFDA
 ..S VISI=0 F  S VISI=$O(VIS(VISI)) Q:VISI'>0  D
 ...S VFDA(9000010.112,"+"_VISI_","_VIM_",",.01)=$P(VIS(VISI),":",1)
 ...S VFDA(9000010.112,"+"_VISI_","_VIM_",",.02)=WHEN ;JM 10/3/15 SET TO EVENT DATE FOR NOW $P(VIS(VISI),":",2)
 ..D UPDATE^DIE(,"VFDA","VFDIENS","VFDERR") ; *45
 ..I $G(VFDERR) S VFDIM="-1"_VFDERR
 K ^TMP("PXKENC",$J) ;Clean up ^TMP from ENCEVENT^PXKENC
 ; End Patch 210 workaround JM 10/2/15
 ;
PSDATA ;Update inpatient pharmacy order with expiration date
 N REC
 I DLGN["PSJ" D
 .S REC=+PREF
 .N DIERR,VFDA,VFDIEN
 .S VFDIEN=DFN_",",VFDA(55.06,VFDIEN,34)=$E($$NOW^XLFDT,1,12)
 .D FILE^DIE(,"VFDA")
 .S:'$D(DIERR) VFDIM="1^Inpatient Order Successfully updated."
 .S:$D(DIERR) VFDIM="-1^Inpatient Order Unsuccess."
 ; Update outpatient pharmacy with expiration date
 I DLGN["PSO" D
 .N DIERR,VFDA,VFDIEN
 .S VFDIEN=PREF_",",VFDA(52,VFDIEN,26)=$E($$NOW^XLFDT,1,12)
 .D FILE^DIE(,"VFDA")
 .S:'$D(DIERR) VFDIM="1^Outpatient Order Successfully updated."
 .S:$D(DIERR) VFDIM="-1^Outpatient Order Unsuccessful."
 Q
 ;
ORDPRV(ORN) ; Retrieve the ordering provider
 ; Track down the NW order action in ORDER ACTIONS sub-file (.8)
 N VFDACT,VFDREC,VFDRSLT
 S VFDRSLT="",VFDACT=0
 F  S VFDACT=$O(^OR(100,ORN,8,VFDACT)) Q:(+VFDACT=0)!(VFDRSLT]"")  D
 . S VFDREC=$G(^OR(100,ORN,8,VFDACT,0))
 . I $P(VFDREC,U,2)="NW" S VFDRSLT=$P(VFDREC,U,3)
 Q VFDRSLT
