TIUPXAP1 ; SLC/JER - Interface w/PCE/Visit Tracking ; 1/8/09 2:20pm
 ;;1.0;TEXT INTEGRATION UTILITIES ;**15,29,20,89,82,107,117,126,124,149,179,205**;Jun 20, 1997;Build 153
QUE ; Use a RESOURCE to post visit tracking information in background
 N ZTDTH,ZTIO,ZTSAVE,ZTSK,ZTRTN,ZTDESC
 ; if there is already a visit, and no workload data quit
 I +$P($G(TIUDPRM(0)),U,16),(+$P($G(^TIU(8925,$S(+$G(TIUDA):+$G(TIUDA),1:+$G(DA)),0)),U,11)=0),+$$WORKOK($S(+$G(TIUDA):+$G(TIUDA),1:+$G(DA))) D  Q:'$$BROKER^XWBLIB
 . D DEFER($S(+$G(TIUDA):+$G(TIUDA),1:+$G(DA)))
 I +$G(TIU("VISIT")),($D(CPT)'>9) Q
 I +$P($G(^TIU(8925,$S(+$G(TIUDA):+$G(TIUDA),1:+$G(DA)),0)),U,3),($D(CPT)'>9) Q
 ;DSS/LM - Begin Mod - Detect value of VFDV TIU-VISIT FOREGROUND parameter
 I $$GET^XPAR("ALL","VFDV TIU-VISIT FOREGROUND") D ENQ Q
 ;DSS/LM - Mod End
 S (ZTSAVE("TIU("),ZTSAVE("DFN"),ZTSAVE("TIUDA"),ZTSAVE("DA"))=""
 S (ZTSAVE("DUZ("),ZTSAVE("ICD("),ZTSAVE("CPT("),ZTSAVE("SC("))=""
 S (ZTSAVE("TIUPRLST("),ZTSAVE("XWBOS"))=""
 S ZTDTH=$H,ZTIO="TIU/PXAPI RESOURCE",ZTRTN="ENQ^TIUPXAP1"
 S ZTDESC="TIU/PCE/AmbCare API Call" D ^%ZTLOAD
 I '$D(ZTSK) D ENQ ; If can't get Resource, Run PXAPI call in foreground
 Q
WORKOK(DA) ; Evaluate whether workload collection is appropriate
 N TIUD0 S TIUD0=$G(^TIU(8925,DA,0))
 Q $S($P(TIUD0,U,13)="A":1,$P(TIUD0,U,13)="I":1,$P(TIUD0,U,13)="T":1,1:0)
ENQ ; Entry point for Resource
 I $D(ZTQUEUED) S ZTREQ="@"
 D POST(.TIU,DFN,$S(+$G(TIUDA):+$G(TIUDA),1:$G(DA)),.ICD,.CPT,.SC)
 Q
POST(TIUX,DFN,TIUDA,ICD,CPT,SC) ; Call on commitment to post data to PCE/AmbCare
 N TIULOC,TIUVDT,TIUSTOP,TIUVCAT,TIUVSIT,TIUD0
 S TIULOC=$P($G(TIUX("VSTR")),";"),TIUVDT=+$P($G(TIUX("VSTR")),";",2)
 S TIUVCAT=$P($G(TIUX("VSTR")),";",3),TIUSTOP=$P($G(^SC(+TIULOC,0)),U,7)
 D PXAPI(.TIUVSIT,DFN,TIULOC,TIUVDT,TIUVCAT,TIUSTOP,.ICD,.CPT,.SC,TIUDA)
 I +$G(TIUVSIT)>0,+$G(TIUDA)>0,$D(^TIU(8925,+TIUDA,0)) S TIUD0=^(0) D
 . I $P(TIUD0,U,2)=DFN,$P(TIUD0,U,7)=TIUVDT,$P($G(^TIU(8925,+TIUDA,12)),U,11)=TIULOC D
 . . N DIE,DR,DA S DA=+$G(TIUDA)
 . . S DIE=8925,DR=".03////^S X="_+$G(TIUVSIT)
 . . D ^DIE
 . . S TIUX("VISIT")=+$G(TIUVSIT)_U_TIUVDT
 Q
PXAPI(TIUVSIT,DFN,VLOC,VDT,VCAT,VSTOP,ICD,CPT,SC,TIUDA) ; Build input root
 N TIUPXAPI,SUCCESS,TIUI,TIUPROV,DA,TIUAUTH
 K ^TMP("TIUPXAPI",$J) S TIUPXAPI=$NA(^TMP("TIUPXAPI",$J))
 S:+$G(VDT) @TIUPXAPI@("ENCOUNTER",1,"ENC D/T")=+$G(VDT)
 S:+$G(DFN) @TIUPXAPI@("ENCOUNTER",1,"PATIENT")=+$G(DFN)
 S:+$G(VLOC) @TIUPXAPI@("ENCOUNTER",1,"HOS LOC")=+$G(VLOC)
 I $D(SC)>9 D
 . S @TIUPXAPI@("ENCOUNTER",1,"SC")=$P($G(SC("SC")),U)
 . I $G(SC("AO"))]"" S @TIUPXAPI@("ENCOUNTER",1,"AO")=$P($G(SC("AO")),U)
 . I $G(SC("IR"))]"" S @TIUPXAPI@("ENCOUNTER",1,"IR")=$P($G(SC("IR")),U)
 . I $G(SC("EC"))]"" S @TIUPXAPI@("ENCOUNTER",1,"EC")=$P($G(SC("EC")),U)
 . I $G(SC("MST"))]"" S @TIUPXAPI@("ENCOUNTER",1,"MST")=$P($G(SC("MST")),U)
 . I $G(SC("HNC"))]"" S @TIUPXAPI@("ENCOUNTER",1,"HNC")=$P($G(SC("HNC")),U)
 I $D(CPT) S @TIUPXAPI@("ENCOUNTER",1,"CHECKOUT D/T")=$E($$NOW^XLFDT,1,12)
 S:$G(VCAT)]"" @TIUPXAPI@("ENCOUNTER",1,"SERVICE CATEGORY")=$G(VCAT)
 S:+$G(VSTOP) @TIUPXAPI@("ENCOUNTER",1,"DSS ID")=+$G(VSTOP)
 S @TIUPXAPI@("ENCOUNTER",1,"APPT")=9
 S @TIUPXAPI@("ENCOUNTER",1,"ENCOUNTER TYPE")="P"
 I $D(TIUPRLST) D
 . M @TIUPXAPI@("PROVIDER")=TIUPRLST
 . S TIUPROV=$S($G(TIUPRLST(1,"PRIMARY")):$G(TIUPRLST(1,"NAME")),1:$G(TIUPRLST(2,"NAME")))
 E  D
 . N TIUDDOC
 . I +$G(TIUDA) D
 . . S TIUAUTH=$P($G(^TIU(8925,+$G(TIUDA),12)),U,2) Q:+TIUAUTH'>0
 . . I +$$PROVIDER(TIUAUTH,$G(VDT))'>0 S TIUAUTH=0
 . S TIUDDOC=+$$DFLTDOC^TIUPXAPI(+$G(VLOC))
 . D:'$D(TIUPRM0) SETPARM^TIULE
 . S TIUPROV=$S(+$G(TIUAUTH):+$G(TIUAUTH),+$$PROVIDER(DUZ,$G(VDT)):+$G(DUZ),1:"")
 . I +TIUPROV>0,'$D(XWBOS) D
 . . I +TIUDDOC'=+TIUPROV,(+$P(TIUPRM0,U,8)=1),+TIUDDOC>0 D
 . . . ; Get Provider information from Encounter.
 . . . ; If ENC has no provicders then add default provider as primary. 
 . . . ; Add TIUPROV unless already primary provider for encounter.
 . . . N TIUPRIME,TIUPVCNT,TIUTVST,TIUTPRV,TIUPDATA
 . . . S TIUPRIME="",TIUPVCNT=1
 . . . D GETENC^PXAPI($G(DFN),$G(VDT),$G(VLOC))
 . . . S TIUTVST=""
 . . . F  S TIUTVST=$O(^TMP("PXKENC",$J,TIUTVST)) Q:TIUTVST=""  D
 . . . . S TIUTPRV=""
 . . . . F  S TIUTPRV=$O(^TMP("PXKENC",$J,TIUTVST,"PRV",TIUTPRV)) Q:TIUTPRV=""  D
 . . . . . S TIUPDATA=$G(^TMP("PXKENC",$J,TIUTVST,"PRV",TIUTPRV,0))
 . . . . . I $P(TIUPDATA,"^",4)="P" S TIUPRIME=+TIUPDATA
 . . . I 'TIUPRIME D
 . . . . S @TIUPXAPI@("PROVIDER",TIUPVCNT,"NAME")=+TIUDDOC
 . . . . S @TIUPXAPI@("PROVIDER",TIUPVCNT,"PRIMARY")=1
 . . . . S TIUPVCNT=TIUPVCNT+1
 . . . S @TIUPXAPI@("PROVIDER",TIUPVCNT,"NAME")=+TIUPROV
 . . E  D
 . . . S @TIUPXAPI@("PROVIDER",1,"NAME")=+TIUPROV
 I $D(ICD)>9 S TIUI=0 F  S TIUI=$O(ICD(TIUI)) Q:+TIUI'>0  D
 . S @TIUPXAPI@("DX/PL",TIUI,"ENC PROVIDER")=$G(TIUPROV)
 . S @TIUPXAPI@("DX/PL",TIUI,"DIAGNOSIS")=$P(ICD(TIUI),U)
 . S @TIUPXAPI@("DX/PL",TIUI,"NARRATIVE")=$P(ICD(TIUI),U,3)
 . S:$P(ICD(TIUI),U,4)]"" @TIUPXAPI@("DX/PL",TIUI,"CATEGORY")=$P(ICD(TIUI),U,4)
 . S:+$G(ICD(TIUI,"PRIMARY")) @TIUPXAPI@("DX/PL",TIUI,"PRIMARY")=$G(ICD(TIUI,"PRIMARY"))
 I $D(CPT)>9 S TIUI=0 F  S TIUI=$O(CPT(TIUI)) Q:+TIUI'>0  D
 . S @TIUPXAPI@("PROCEDURE",TIUI,"PROCEDURE")=$P(CPT(TIUI),U)
 . S @TIUPXAPI@("PROCEDURE",TIUI,"QTY")=$S(+$G(CPT(TIUI,"QTY")):+$G(CPT(TIUI,"QTY")),1:1)
 . ;Set CPT Modifiers in Array for PCE
 . N MODCNT,MODATA
 . S MODCNT=0
 . F  S MODCNT=$O(CPT(TIUI,"MOD",MODCNT)) Q:'MODCNT  D
 . . S MODATA=$G(CPT(TIUI,"MOD",MODCNT))
 . . S:$P(MODATA,U,2)'="" @TIUPXAPI@("PROCEDURE",TIUI,"MODIFIERS",$P(MODATA,U,2))=""
 . S @TIUPXAPI@("PROCEDURE",TIUI,"ENC PROVIDER")=$G(TIUPROV)
 . S @TIUPXAPI@("PROCEDURE",TIUI,"NARRATIVE")=$P(CPT(TIUI),U,2)
 . S:$P(CPT(TIUI),U,3)]"" @TIUPXAPI@("PROCEDURE",TIUI,"CATEGORY")=$P(CPT(TIUI),U,3)
 S SUCCESS=$$DATA2PCE^PXAPI(TIUPXAPI,"TIU","TEXT INTEGRATION UTILITIES",.TIUVSIT,DUZ,0)
 K @TIUPXAPI
 Q
PROVIDER(USER,DATE) ; Was USER a PROVIDER on DATE?
 N TIUY,PRVCL,EXCL S TIUY=0
 S EXCL="V0802V0805V0806V0808V0809V0812V0813"
 I +$$ISA^USRLM(USER,"PROVIDER") S TIUY=1 G PROVIDEX
 S PRVCL=$$PRVCLASS^PXAPI(USER,DATE) I PRVCL'>0 G PROVIDEX
 I $P(PRVCL,U,7)]"",(EXCL'[$E($P(PRVCL,U,7),1,5)) S TIUY=1
PROVIDEX Q TIUY
DEFER(DA) ; Mark record to defer workload collection
 N DIE,DR,TIUVSIT
 I +$P($G(^TIU(8925,$S(+$G(TIUDA):+$G(TIUDA),1:+$G(DA)),0)),U,11)=1 Q
 S DIE=8925,DR=".11////1" D ^DIE
 ;If not called via the broker try to link document to an existing visit
 I '$$BROKER^XWBLIB,$$LNKVST^TIUPXAP3(+DA,.TIUVSIT)
 Q
