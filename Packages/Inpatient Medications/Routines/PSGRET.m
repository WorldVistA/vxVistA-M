PSGRET ;BIR/CML3-ENTER RETURNS ; 27OCT2014
 ;;5.0; INPATIENT MEDICATIONS ;**31**;16 DEC 97;Build 11
 ;
 ; Reference to ^PS(50.7 is supported by DBIA# 2180
 ; Reference to ^PS(51.2 is supported by DBIA #2178
 ; Reference to ^PS(55 is supported by DBIA# 2191
 ;
 N PSJNEW,PSGPTMP,PPAGE,PSGEFN S PSJNEW=1
 D ENCV^PSGSETU Q:$D(XQUIT)  S (PSGONNV,PSGRETF)=1 K PSGPRP
 ;
GP ;
 D ENDPT^PSGP G:PSGP'>0 DONE I '$O(^PS(55,PSGP,5,"AUS",+PSJPAD)) W $C(7),!,"(Patient has NO active or old orders.)" G GP
 D ENL^PSGOU G:"^N"[PSGOL GP S PSGPTMP=0,PPAGE=1 D ^PSGO G:'PSGON GP S PSGLMT=PSGON,(PSGONC,PSGONR)=0
 ;DSS/AMC/SMH - Start mods Capture NDC
 ; Collect drugs using a custom routine if we want the NDC's. We call R as a callback down there.
 ; R gets the variable VFDNDC which is sent to PSGAMSA via the symbol table to be filed in the dispense multiple (node 11)
 I $T(^VFDPSNDC)]"",$T(^VFDPSJND)]"",$$IPON^VFDPSNDC() D  G GP  ; If NDC Capture is on (Fill on request or ALL) do scan
 . F  D IPSCAN^VFDPSJND($T(+0)) Q:X=U  ; Quit if ^
 ;DSS/AMC/SMH - End mods
 F  W !!,"Select ORDER",$E("S",PSGON>1)," 1-",PSGON,": " R X:DTIME W:'$T $C(7) S:'$T X="^" Q:"^"[X  D:X?1."?" H I X'?1."?" D ENCHK^PSGON W:'$D(X) $C(7),"  ??" Q:$D(X)
 G:"^"[X GP F PSGRET=1:1:PSGODDD F PSGRET1=1:1 S PSGRET2=$P(PSGODDD(PSGRET),",",PSGRET1) Q:'PSGRET2  K DA S (PSGORD)=^TMP("PSJON",$J,PSGRET2) D R G:Y GP
 G GP
 ;
DONE ;
 D ENKV^PSGSETU K ^TMP("PSJON",$J),DO,DRG,MR,OD,PSGLMT,PSGODDD,PSGOL,PSGON,PSGONC,PSGONR,PSGONV,PSGONNV,PSGORD,PSGRET,PSGRET1,PSGRET2,PSGRETF,SCH,Y1,Z Q
 ;
R ;
 ; DSS/SMH BEGIN MODS -- This code contains 2 bugs. I had to rewrite it for NDC capture.
 I $$IPON^VFDPSNDC() D RVFD QUIT
 ; DSS/SMH END MODS
 S MR=$P($G(^PS(55,PSGP,5,+PSGORD,0)),"^",3),Y=$G(^(.2)),SCH=$P($G(^(2)),"^"),DO=$P(Y,"^",2),DRG=$P(Y,"^"),DRG=$S(DRG'=+DRG:"NOT FOUND",'$D(^PS(50.7,DRG,0)):DRG,$P(^(0),"^")]"":$P(^(0),"^"),1:DRG_";PS(50.7,")
 S:MR]"" MR=$S(MR'=MR:MR,'$D(^PS(51.2,MR,0)):MR,$P(^(0),"^",3)]"":$P(^(0),"^",3),$P(^(0),"^")]"":$P(^(0),"^"),1:MR_";PS(51.2,") W !!,"----------------------------------------",!,DRG,!,"Give: ",DO," ",MR," ",SCH
 I '$O(^PS(55,PSGP,5,+PSGORD,1,0)) D  Q
 .W !!,"No Dispense drugs have been entered for this order. At least one Dispense drugs",!,"must be associated with an order before dispensing information may be entered.",!!
 .N DIR S DIR(0)="E" D ^DIR S Y=$S(Y:0,1:1)
 S Y=$O(^PS(55,PSGP,5,+PSGORD,1,0)) I '$O(^(Y)),$D(^(Y,0)) S DRG=$P(^(0),"^"),UD=$P(^(0),"^",2),DRG=$$ENDDN^PSGMI(DRG)
 I  W !!,"Dispense drug: ",DRG,"  (U/D: ",$S('UD:1,1:UD),")"
 E  K DA,DIC S DA(1)=PSGP,DA(2)=+PSGORD,DA=+PSGORD,DIC="^PS(55,"_PSGP_",5,"_+PSGORD_",1,",DIC(0)="AEQM" W ! D ^DIC K DIC Q:Y'>0
 K DA,DR S DA=+Y,DA(2)=PSGP,DA(1)=+PSGORD,DIE="^PS(55,"_PSGP_",5,"_+PSGORD_",1,",DR=.08 S:$P($G(^PS(55,PSGP,5,+PSGORD,1,DA,0)),"^",8) $P(^(0),"^",8)="" W ! D ^DIE S Y=$D(DUOUT)!$D(DTOUT)
 Q
 ;
 ; DSS/SMH - BEGIN MODS R without the bugs; and it does some extra stuff too.
RVFD ;
 ; ZEXCEPT: VFDNDCBYDRUG
 ; Stanza 1
 I '$O(^PS(55,PSGP,5,+PSGORD,1,0)) D  Q
 .W !!,"No Dispense drugs have been entered for this order. At least one Dispense drug",!,"must be associated with an order before dispensing information may be entered.",!!
 .N DIR S DIR(0)="E" D ^DIR S Y=$S(Y:0,1:1)
 ;
 ; Stanza 2
 ; This part of the original code is okay. No problems here.
 ; prints drug information and dosage
 S MR=$P($G(^PS(55,PSGP,5,+PSGORD,0)),"^",3),Y=$G(^(.2)),SCH=$P($G(^(2)),"^"),DO=$P(Y,"^",2),DRG=$P(Y,"^"),DRG=$S(DRG'=+DRG:"NOT FOUND",'$D(^PS(50.7,DRG,0)):DRG,$P(^(0),"^")]"":$P(^(0),"^"),1:DRG_";PS(50.7,")
 S:MR]"" MR=$S(MR'=MR:MR,'$D(^PS(51.2,MR,0)):MR,$P(^(0),"^",3)]"":$P(^(0),"^",3),$P(^(0),"^")]"":$P(^(0),"^"),1:MR_";PS(51.2,") W !!,"----------------------------------------",!,DRG,!,"Give: ",DO," ",MR," ",SCH
 ;
 ; Stanza 3
 ; For each drug, capture quantity and returned NDC. Original code has two bugs
 ; in this code: 1. It lets user add a drug (!), but the Stanza 1 already
 ; prevents the user from doing that. 2. Only the first drug is gets filed;
 ; not the next one.
 N DRG F DRG=0:0 S DRG=$O(^PS(55,PSGP,5,+PSGORD,1,DRG)) Q:'DRG  D
 . ;
 . ; This substanza is confusing. If the user scanned something in the scanner
 . ; code that calls this [VFDPSJND], and this is not the drug which has been
 . ; scanned, skip this one and try the next one. This should never be an issue
 . ; for single drug orders. Once one is found, VFDPSJNDCBYDRUG stays in the
 . ; ST so that the next drug (if found) won't be prompted. This way we ensure
 . ; that a scan only matches one drug and the user will exit after a single
 . ; drug, whether or not an order has multiple drug components.
 . I $D(VFDNDCBYDRUG),'$D(VFDNDCBYDRUG(+PSGORD,DRG)) QUIT
 . ;
 . N DRGIEN,UD,DRGNM S DRGIEN=$P(^PS(55,PSGP,5,+PSGORD,1,DRG,0),U),UD=$P(^(0),U,2),DRGNM=$$ENDDN^PSGMI(DRGIEN)
 . N DU S DU=$$GET1^DIQ(50,DRGIEN,14.5)
 . W !!,"Dispense drug: ",DRGNM," (",DU,") ","  (U/D: ",$S('UD:1,1:UD),")"
 . N VFDNDC,POP
 . I $D(VFDNDCBYDRUG(+PSGORD,DRG)) S VFDNDC(+PSGORD)=VFDNDCBYDRUG(+PSGORD,DRG)
 . E  D GRABNDC^VFDPSJND(+PSGORD,DRG,.VFDNDC,.POP)
 . I $G(POP) QUIT  ; This quits us so that we can do the next drug.
 . ;
 . N NDC S NDC=$P(VFDNDC(+PSGORD),U)
 . N OUT S OUT=0
 . I '$D(^PS(55,PSGP,5,"AVFDNDC",NDC,+PSGORD)) DO  QUIT:OUT
 . . W !,"Patient doesn't have this NDC on file for that specific Order."
 . . N DIR,X,Y S DIR(0)="Y",DIR("A")="Do you want to continue" D ^DIR
 . . I Y>0 QUIT  ; We can proceed
 . . ELSE  S OUT=1 QUIT  ; Stop && get out.
 . K DA,DR S DA(2)=PSGP,DA(1)=+PSGORD,DA=DRG,DIE="^PS(55,"_PSGP_",5,"_+PSGORD_",1,",DR=.08 S:$P($G(^PS(55,PSGP,5,+PSGORD,1,DA,0)),"^",8) $P(^(0),"^",8)="" W ! D ^DIE S Y=$D(DUOUT)!$D(DTOUT)
 QUIT
 ; DSS/SMH - END MODS
 ;
H ;
 W !!?2,"Select the orders (by number) for which you want to enter returns." D:X'="?" H2^PSGON Q
