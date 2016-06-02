VFDPSJND ;DSS/SMH - Inpatient NDC Capture Utilities;2/2/2015
 ;;2013.2;DSS INC VXVISTA OPEN SOURCE;;;Build 11
 ;
 ; This routine handles inpatient NDC capture requests from:
 ; - PSGEUD
 ; - PSGPEN
 ; - PSGRET
 ;
IPSCAN(INVOKER) ; [Called by PSGEUD and PSGRET] Grab NDC. Loop entry point.
 ; Input: INVOKER. Routine name where we got invoked. We use that as a call back for Help and R (data and filing)
 ; Purpose in Life: get PSGORD and pass to R. R collects NDCs.
 ;
 ; Exit returned variables:
 ; X (only important for ^)
 ; ZEXCEPT: X
 ;
 ; ZEXCEPT: PSGP ; Patient DFN
 ; ZEXCEPT: PSGON ; Delivered to us by PSGO as the number of orders (do we need this???)
 ;
 N RDDONE S RDDONE=0
 F  D  Q:RDDONE
 . W !!,"Scan Barcode, Enter NDC Or Select ORDER",$E("S",PSGON>1)," 1-",PSGON,": "  ; prompt
 . R X:DTIME
 . E  W $C(7) S X=U,RDDONE=1 QUIT  ; read or quit if timeout
 . I X[U S X=U,RDDONE=1 QUIT      ; bye
 . I X["?" D H^@INVOKER QUIT  ; Do help and come back
 . S RDDONE=1
 ;
 I X="" S X="^" QUIT  ; quit no return
 Q:X="^"  ; ditto, no return
 ;
 ; Stanza for processing selection of order by number; number,number; number-number; ALL
 ; Note that we have a 5 char limit to distinguish it from scans.
 ; ZEXCEPT: PSGODDD
 I $L(X)<5,X'?1."?" DO  QUIT  ; If an order from the list, then...
 . S X=$TR(X,"al","AL") ; Make all ALL
 . D ENCHK^PSGON  ; Analyze X and build ^TMP("PSJON") and PSGODDD
 . I '$D(X) W $C(7)," ??" S X="" QUIT  ; Invalid selection. Try again. (SET X="" to prevent crash undef in PSGEUD.)
 . I '$G(PSGODDD),X'[",",X'["-",X'="ALL" W !!,"Unable To Find Order." QUIT  ; Wrong selection. Try again.
 . ;
 . ; Loop: DDD top node = # of entries; RET1 = $P counter; RET2 actual piece
 . N PSGRET,PSGRET1,PSGRET2 F PSGRET=1:1:PSGODDD F PSGRET1=1:1 S PSGRET2=$P(PSGODDD(PSGRET),",",PSGRET1) Q:'PSGRET2  D  ; Loop
 . . ; Get order, and call callback (unfortunately callback calls us at GRABNDC again).
 . . N PSGORD S PSGORD=^TMP("PSJON",$J,PSGRET2) D   ; <--- Nothing below this dot level. Unwinds back to top.
 . . . ;
 . . . ; No drugs on order? If so, quit and process next order.
 . . . I '$O(^PS(55,PSGP,5,+PSGORD,1,0)) D  QUIT  ; <--- this exits
 . . . . W !!,"No Dispense drugs have been entered for this order. At least one Dispense drugs"
 . . . . W !,"must be associated with an order before dispensing information may be entered.",!!
 . . . . N DIR S DIR(0)="E" D ^DIR
 . . . ;
 . . . D R^@INVOKER  ; <--- Last call in this block. Unwind back up. This does all drugs inside the order.
 . . . S X="^"  ; This should exit us completely from the option. We are now completely done.
 ;
 ;
 ; Stanza for processing Barcodes (supposedly)
 ; ZEXCEPT: PSGODDD
 E  D  ; Barcode or NDC ; notice the IF way way up top
 . N MATCH S MATCH=$$FINDDRG(X)  ; Match = PSGORD^DRG^NDC^IEN in ^TMP("PSJON") (IEN goes into PSGODDD(sub))
 . I 'MATCH W "    This barcode is not valid",! QUIT  ; try again
 . N PSGORD,DRG,NDC,UI ; UI = User Input
 . S PSGORD=$P(MATCH,U),DRG=$P(MATCH,U,2),NDC=$P(MATCH,U,3),PSGODDD=1,PSGODDD(1)=$P(MATCH,U,4),UI=$P(MATCH,U,5)
 . N VFDNDCBYDRUG  ; passed in ST to R^@INVOKER. Ensures scanned drug is what is operated on.
 . S VFDNDCBYDRUG(PSGORD,DRG)=NDC_U_UI
 . D R^@INVOKER  ; ditto; <-- Last call
 . S X="^"              ; <-- exit option
 QUIT
 ;
GRABNDC(ORD,DRG,VFDNDC,POP) ; [Called by PSGEUD,PSGRET,PSGPEN] Grab NDC and put in VFDNDC
 ; ORD: Order IEN
 ; DRG: IEN of dispense drug under order
 ; .VFDNDC: return in format VFDNDC(PSGORD)=NDC^USER INPUT
 ; .POP: User requested ^.
 ;
 ; ZEXCEPT: PSGP ; Patient DFN
 ;
 N DRGIEN S DRGIEN=+^PS(55,PSGP,5,ORD,1,DRG,0)
 W !,"Getting NDC and Scan code for "_$P(^PSDRUG(DRGIEN,0),U),!
 N NDCUI ; NDC ^ UI
 F  D  Q:$D(POP)  Q:NDCUI  ; quit for if ^ or we find an NDC
 . N ERR
 . S NDCUI=$$ASKNDC^VFDPSNDC(DRGIEN,,.ERR)
 . I NDCUI QUIT  ; Got NDC... no need to continue
 . I $E($G(ERR))=U S POP=1 QUIT  ; User ^'ed
 . E  W !,"   ",ERR,!  ; error message. print and ask again.
 Q:$D(POP)  ; We quit the loop, now quit the subroutine
 W "  ",$P(NDCUI,U)  ; Write a confirmation message to the user of the actual resolved NDC
 S VFDNDC(ORD)=NDCUI  ; store
 QUIT
 ;
FINDDRG(CODE) ; [Internal $$] Find drug
 ; ZEXCEPT: PSGP ; Patient IEN
 ; ZEXCEPT: PSGORD ; Inpatient order number for this patient
 ; Returns: Order number, dispense drug, NDC, order number in ^TMP("PSJON",$J), scan code in variable MATCH
 Q:$G(CODE)="" 0
 ;
 ; See if we match ANY drug at all
 N DRGIEN S DRGIEN=$O(^PSDRUG("C",CODE,""))
 I 'DRGIEN Q 0  ; prints "this barcode is invalid" in the caller
 ;
 ; If we have any match
 ; DDRUG = Dispense drug ien in the 5,x,1 multiple
 ; ORDI = Order number under the 5 multiple
 N DDRUG,ORDI ; variables used inside the loop
 N ORD S ORD=0  ; looper through ^TMP
 N MATCH S MATCH=0  ; walk all orders in list until we find a match
 F  S ORD=$O(^TMP("PSJON",$J,ORD)) Q:'ORD  S ORDI=+^(ORD),DDRUG=0 D:ORDI  Q:MATCH
 .F  S DDRUG=$O(^PS(55,PSGP,5,ORDI,1,DDRUG)) Q:'DDRUG  I $P(^(DDRUG,0),U)=DRGIEN S MATCH=1 Q
 ;
 I 'MATCH D  QUIT 0
 . W !,"This number matches "_$P(^PSDRUG(DRGIEN,0),U)_", but I can't find this drug on the list.",!
 ;
 ; Format NDC
 N ERR
 N NDC S NDC=$$LOOKNDC^VFDPSNDC(CODE,DRGIEN,.ERR) ; Simple NDC lookup
 I 'NDC W "   ",ERR,! QUIT 0
 ;
 S MATCH=ORDI_U_DDRUG_U_NDC_U_ORD_U_CODE  ; ORD is order number in the ^TMP("PSJON",$J) jumped forward by the loop
 ; S PSGORD=^TMP("PSJON",$J,ORD)
 W !,"Found ",$P(^PSDRUG(DRGIEN,0),U)," for NDC/Scan Code: ",CODE," as NDC ",NDC,!
 Q MATCH
 ;
PLNDC ; [Public] Enter NDC's for drugs on the pick list
 ; ZEXCEPT: PSGPLG: Pick List IEN
 ; Start logging
 N % S:($T(^XTMLOG)]"") %=$$FILEINIT^XTMLOG("PSGPL")
 ;
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(53.5,"AC",PSGPLG)))
 ;
 ; TM = Team
 ; WDN = Ward Name
 ; PN = Patient Name (truncated)^DFN
 ; DRG = Drug Name (truncated)^Order IEN
 ; DDRG = Drug Name (truncated)^Drug node in the pick list (the bottom)
 N TM,WDN,RB,PN,DRG,DDRG S (TM,WDN,RB,PN,DRG,DDRG)=""
 N EXITALL,EXITWARD S (EXITALL,EXITWARD)=0
 N ST S ST="A"
 F  S TM=$O(^PS(53.5,"AC",PSGPLG,TM)) Q:TM=""  D  Q:EXITALL
 . F  S WDN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN)) Q:WDN=""  D  Q:EXITALL
 .. W !!,"TEAM: ",$S(TM'["zz":TM,1:"* N/F *"),?40,"WARD: ",$S(WDN'["zz":WDN,1:"* N/F *"),!
 .. F  S RB=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB)) Q:RB=""  D  Q:EXITWARD  Q:EXITALL
 ... F  S PN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN)) Q:PN=""  D  Q:EXITWARD  Q:EXITALL
 .... N PSGP S PSGP=$P(PN,"^",2) D W2(PSGPLG,PSGP)
 .... F  S ST=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST)) Q:ST["Z"  Q:ST=""  D  Q:EXITWARD  Q:EXITALL
 ..... F  S DRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG)) Q:DRG=""  D  Q:EXITWARD  Q:EXITALL
 ...... F  S DDRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG,DDRG)) Q:DDRG=""  D  Q:EXITWARD  Q:EXITALL
 ....... D FILLPL(PSGPLG,TM,WDN,RB,PN,ST,DRG,DDRG,.EXITWARD,.EXITALL)
 ;
 I EXITALL QUIT
 ;
 N %,DIE,DA,DR F  W !!,"MAY I FILE THE DATA IN THIS PICK LIST AWAY" S %=2 D YN^DICN Q:%  D FIMSG^PSGPLDPH
 I %=1 S DIE="^PS(53.5,",DA=PSGPLG,DR=".05////1" D ^DIE K DIE
 D:$T(^XTMLOG)]"" ENDLOG^XTMLOG("PSGPL")
 QUIT
 ;
FILLPL(PSGPLG,TM,WDN,RB,PN,ST,DRG,DDRG,EXITWARD,EXITALL) ; [Internal] Fill Pick List
 N PSGP S PSGP=$P(PN,"^",2) ; pt DFN
 N PSGORD S PSGORD=$P(DRG,"^",2) ; order number in pick list
 N PSJJORD S PSJJORD=+$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,0)) ; order number in 55/5
 N SCH S SCH=$P($G(^PS(55,PSGP,5,PSJJORD,2)),"^") ; schedule
 N DN S DN=+$P(DDRG,"^",2),DN=$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DN,0)) ; drug node
 Q:DN["DI"  ; ??? don't know, but that's there
 N PDD S PDD=$P(DN,"^",3) ; Dispensed
 N PDN S PDN=$P(DN,"^",2) ; Needed
 N DR S DR=$P($G(^PS(55,PSGP,5,PSJJORD,1,+DN,0)),"^"),DR=$$ENDDN^PSGMI(DR) ; Drug Name
 N DU ; Drug Unit
 N DRGIEN S DRGIEN=$P($G(^PS(55,PSGP,5,PSJJORD,1,+DN,0)),"^")  ; Drug IEN
 S DU=$$GET1^DIQ(50,DRGIEN,14.5),DU=" ("_DU_") "  ; Drug Units
 ;
 ; Don't ask for these
 ; NB: Original code captures dispense data for non-verified orders
 I PDN="WS" W !,$E(DR_$G(DU),1,30),?32,"WARD STOCK" QUIT
 I PDN="NV" W !,$E(DR_$G(DU),1,30),?32,"NOT VERIFIED" QUIT
 I PDN=0 W !,$E(DR_$G(DU),1,30),?32,"Need:0 NO DOSES NEEDED" QUIT
 ;
 N DONEASKING S DONEASKING=0
 F  D  Q:EXITWARD  Q:EXITALL  Q:DONEASKING
 . ;
 . N DEFAULTNDC S DEFAULTNDC=$P($G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,$P(DDRG,U,2),21600)),U)   ; Use as default in case it was entered already.
 . N ERR
 . N Y S Y=$$ASKNDCPL(DEFAULTNDC) ; "inline" function. Didn't want to put the whole thing here
 . ;
 . ; These may come out of $$ASKNDCPL()
 . I EXITALL QUIT  ; User entered ^^
 . I EXITWARD QUIT  ; User entered ^
 . I $G(ERR)]"" W !,ERR,! QUIT  ; If there is an error, write it out, and quit
 . ;
 . ; user just hit enter and accepted the default NDC. Quit the asking loop and move to next item
 . I DEFAULTNDC=$P(Y,U) S DONEASKING=1 QUIT
 . ;
 . I Y="" QUIT  ; This *shouldn't* happen. but it's here just in case.
 . ;
 . ; User asked for quantity. Capture, stuff, and then loop back (for loop) and ask for NDC again.
 . I Y="#" D  QUIT
 .. N DIR,DA,DIRUT,DIROUT,DTOUT,DUOUT,X,Y
 .. S DIR("A")="Enter quantity"
 .. I $$111^VFDPSJND() D  ; OMH only.
 ... S DIR("?")="Enter quantity for the drug. Up to 3 decimals are allowed."
 ... S DIR(0)="NO^0:9999:3"
 .. E  D
 ... S DIR("?")="Enter quantity for the drug."
 ... S DIR(0)="NO^0:1000"
 .. D ^DIR
 .. I $D(DUOUT)!$D(DTOUT) S EXITWARD=1 QUIT
 .. S PDD=Y
 .. I PDD=0 S DONEASKING=1
 .. I +PDD'=PDN S $P(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,$P(DDRG,"^",2),0),"^",3)=PDD  ; stuff only if different from needed.
 . ; E  I Y="" QUIT  ; User errored in the NDC thing and we are looping back to ask again. This happens when a bad NDC is scanned
 . E  D
 .. S ^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,$P(DDRG,U,2),21600)=Y  ; Stuff NDC ^ User Input
 . S DONEASKING=1  ; and say we are done
 ;
 Q:EXITWARD  Q:EXITALL  ; if we are asked to quit
 ;
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD)))
 ;
 QUIT
 ;
ASKNDCPL(DEFAULT) ; [Internal, inline] Wrapper to ASK NDC
 ; ZEXCEPT: PSGPLG,PSGP,PSGORD,DDRG,DU,PDD,PDN,VFDIEN,EXITALL,EXITWARD,DR,DRGIEN,ERR
 ;
 ; Returns ^, #, NDC ^ User Input or "".
 ; ^ user wants to exit
 ; # user wants to enter a quantity
 ; NDC ^ User Input -> capture successful
 ; "" -> capture failure
 ;
 ;
 N DIR,DA,DIRUT,DIROUT,DTOUT,X,Y,DUOUT
 S DIR("A")=$E(DR_$G(DU),1,30)_"  Need/Dispensed: "_PDN_"/"_$S(PDD="":"#",1:PDD)_"  NDC" ; ask user for NDC by default
 S DIR("?")="^D NDCHLP^VFDPSNDC"
 S DIR("??")="^D NDCHLP2^VFDPSNDC"
 N DRG S DRG=DRGIEN ; NDCHLP relies on this variable; eww eww eww.
 I $G(DEFAULT) S DIR("B")=DEFAULT
 S DIR(0)="FO"
 D ^DIR
 I Y=DEFAULT Q DEFAULT
 I $D(DIROUT) S EXITALL=1 QUIT U ; ^^
 I $D(DUOUT) S EXITWARD=1 QUIT U ; ^
 I $D(DTOUT) S EXITALL=1 QUIT U  ; Timeout
 I $E(Y)="#" QUIT "#"  ; if # entered, user wants quantity
 I Y?1.A S ERR="Entries containing letters are not allowed." QUIT ""
 I Y="" Q ""
 N NDC S NDC=$$LOOKNDC^VFDPSNDC(Y,DRGIEN,.ERR) ; Simple NDC lookup
 I $G(ERR)'="" QUIT ""
 E  W !,"   Dispensed ",NDC," with quantity of: "_$S(PDD:PDD,1:PDN) Q NDC_U_Y
 QUIT ""
 ;
W2(PSGPLG,DFN) ; [Internal] Print patient header (copied slightly modified from PSGPLDP0)
 N VA,PID,PPN,VADM
 D DEM^VADPT
 S PID=$S($D(VA("MRN")):VA("MRN"),1:$G(VA("PID")))
 N PSSN S PSSN=PID
 S PPN=VADM("1")
 N PR S PR=$P(^PS(53.5,PSGPLG,1,DFN,0),"^",4)
 W ! N I F I=1:1:79 W "-"
 W !,$S(PR'["zz":PR,1:"* N/F *"),?30,PPN,"  (",PSSN,")"
 I '$D(^PS(53.5,PSGPLG,1,DFN,1)) W ?65,"(NO ORDERS)",!
 QUIT
 ; =====================================================
 ;
RPREUD ; [Public] Menu option [VFD PSJ EUD LBL REPRINT] - Reprints last extra unit dispensed label
 ;
 I '$$GET^XPAR("ALL","VFD PSJ EUD LBL PRINT") DO  QUIT
 . W !!,"Option is not enabled.",!
 . W "You can turn this on by enabling parameter VFD PSJ EUD LBL PRINT.",!
 . D OUT^XPDMENU("VFD PSJ EUD LBL REPRINT","Disabled by parameter VFD PSJ EUD LBL PRINT") ; MM disable
 ;
 I '$$GET^XPAR("ALL","VFD PSJ EUD LBL PRINT") QUIT
 ;
 ; We are pretty much following PSGEUD in structure.
 N PSJNEW,PSGPTMP,PPAGE,PSGEFN S PSJNEW=1
 D ENCV^PSGSETU Q:$D(XQUIT)  S (PSGONNV,PSGRETF)=1 K PSGPRP
 ;
GP ; [Public Fallthrough] Start
 S PSGP="",DFN=""
 D ENDPT^PSGP G:(PSGP'>0)&(DFN'>0)!('$D(PSJPAD)) DONE S:PSGP<1 PSGP=DFN I '$O(^PS(55,PSGP,5,"AUS",+PSJPAD)) W $C(7),!,"(Patient has NO active or old orders.)" G GP
 D ENL^PSGOU G:"^N"[PSGOL GP S PSGPTMP=0,PPAGE=1 D ^PSGO G:'PSGON GP S PSGLMT=PSGON,(PSGONC,PSGONR)=0
 F  W !!,"Select ORDER",$E("S",PSGON>1)," 1-",PSGON,": " R X:DTIME W:'$T $C(7) S:'$T X="^" Q:"^"[X  D:X?1."?" H I X'?1."?" D ENCHK^PSGON W:'$D(X) $C(7),"  ??" Q:$D(X)
 G:"^"[X GP F PSGRET=1:1:PSGODDD F PSGRET1=1:1 S PSGRET2=$P(PSGODDD(PSGRET),",",PSGRET1) Q:'PSGRET2  S PSGORD=^TMP("PSJON",$J,PSGRET2) D R G:$D(DTOUT) GP
 G GP
 ;
DONE ; [Internal] Done. Kill vars and exit
 D ENKV^PSGSETU K ^TMP("PSJON",$J),DO,DRGN,MR,OD,PSGLMT,PSGODDD,PSGEUD,PSGEUDA,PSGOL,PSGON,PSGONC,PSGONR,PSGONV,PSGONNV,PSGORD,PSGQ,PSGRET,PSGRET1,PSGRET2,PSGRETF,PSGSPD,SCH,WG,Z,CST Q
 ;
H ; [Internal Callback from GP] Help
 W !!?2,"Select the orders that you want to edit",! D:X'="?" H2^PSGON
 Q
 ;
R ; [Internal Callback from GP] Ask for label reprint
 N DRG F DRG=0:0 S DRG=$O(^PS(55,PSGP,5,+PSGORD,1,DRG)) Q:'DRG  D PRT^VFDPSJPX($NA(^PS(55,PSGP,5,+PSGORD,1,DRG,0)))
 QUIT
 ;
 ; =====================================================
 ;
POST ; [PEP for KIDS only]
 ; Turn on the parameter at system level for OMH
 ; I '$D(DT) N DIQUIET S DIQUIET=1 D DT^DICRW K DIQUIET ; set DT, needed by $$SITE^VASITE(), just for me from cmd line.
 ; I $$111() D  ; for OMH
 ; . D EN^XPAR("SYS","VFD PSJ PL ENTER UNITS PROGRAM",1,"N") ; NDC based capture for OMH.
 ; . D EN^XPAR("SYS","VFD PSJ EUD LBL PRINT",,"Y")           ; Print labels for EUD for OMH
 ;
 ; Remove old style B index for .01 is dispense date/time if it is present.
 N I F I=0:0 S I=$O(^DD(55.0611,.01,1,I)) Q:'I  I ^DD(55.0611,.01,1,I,0)="55.0611^B" K ^DD(55.0611,.01,1,I)
 ;
 QUIT
 ;
 ; =====================================================
 ;
111() ; [Public] Is this Site 111 (OMH)?
 N SITE S SITE=$P($$SITE^VASITE(),U,3)
 Q ($E(SITE,1,3)=111)
 ;
 ; -------------------------------------------------
 ;
