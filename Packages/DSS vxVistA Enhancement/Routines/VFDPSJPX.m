VFDPSJPX ;DSS/SMH - Edit Dispensed Doses' NDC && Pre-exchange new NDC entry && Print EUD Label ; 1MAY2015
 ;;2013.2;DSS INC VXVISTA OPEN SOURCE;;;Build 11
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
R ; [Internal Callback from GP] Prompt for Dispense and let user select then enter NDC
 ; Display dispenses
 N DA,DIC,X,Y,DLAYGO,DINUM,DTOUT,DUOUT ; DIC Input & Output
 S DIC(0)="AEQMV" S DIC=$$OREF^DILF($NA(^PS(55,PSGP,5,+PSGORD,11)))
 S DA(1)=PSGORD,DA(2)=PSGP
 S DIC("W")="D DICW^VFDPSJPX"
 D ^DIC
 ;
 Q:Y<1  ; nothing selected
 ;
 N % S:($T(^XTMLOG)]"") %=$$FILEINIT^XTMLOG("PSG")
 ;
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(55,PSGP,5,+PSGORD,11,+Y,21600)))
 ; Get drug and NDC (if it exists) (that's why the $GET)
 N DRG S DRG=$P(^PS(55,PSGP,5,+PSGORD,11,+Y,0),U,2)
 N OLDNDC S OLDNDC=$P($G(^PS(55,PSGP,5,+PSGORD,11,+Y,21600)),U)
 ;
 N NDCUI S NDCUI="" ; NDC + User Input; what we ask for
 F  D  Q:NDCUI]""
 . N Y  ; my precious
 . N ERR
 . S NDCUI=$$ASKNDC^VFDPSNDC(DRG,OLDNDC,.ERR)  ; ask for NDC, supplying default if necessary.
 . I $D(ERR)#2 D  QUIT
 .. I $E(ERR)=U S NDCUI=U QUIT
 .. I ERR]"" W "   "_ERR QUIT
 ;
 Q:NDCUI=U  ; oopsie. Do nothing
 ;
 ; Get out if it is the same
 I $P(NDCUI,U)=OLDNDC D  W "  nothing changed.",! QUIT
 . D:$T(^XTMLOG)]"" DEBUG^XTMLOG("User accepted default")
 ;
 W "   "_$P(NDCUI,U),!
 ;
 ; Unindex NDC entries, set, then reindex
 S DA=+Y,DA(1)=+PSGORD,DA(2)=PSGP
 S DIK=$$OREF^DILF($NA(^PS(55,PSGP,5,+PSGORD,11)))
 F DIK(1)=21600,21601 D EN2^DIK
 ;
 S ^PS(55,PSGP,5,+PSGORD,11,+Y,21600)=NDCUI
 ;
 F DIK(1)=21600,21601 D EN1^DIK
 ;
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(55,PSGP,5,+PSGORD,11,+Y,21600)))
 D:$T(^XTMLOG)]"" ENDLOG^XTMLOG
 QUIT
 ;
DICW ; [Keep Out!!!!]
 N R,Z,Z21600 S R=$NA(^(0)),Z=@R,Z21600=$G(^(21600)) ; Reference and Zero node and the 21600 node
 N AMOUNT S AMOUNT=$P(Z,U,3) ; self-explanatory, hopefully.
 N HOW S HOW=$$EXTERNAL^DILFD(55.0611,.05,,$P(Z,U,5)) ; Pick List, Pre-Exchange, Return, Extra
 N USER S USER=$$EXTERNAL^DILFD(55.0611,.06,,$P(Z,U,6))   ; User
 N NDC S NDC=$P(Z21600,U)
 ;
 ; Now write this crap
 ; http://www.hardhats.org/fileman/pm/cl_ddiol.htm
 N WSTR
 S WSTR(1)="Amount: "_AMOUNT,WSTR(1,"F")="?27"
 S WSTR(2)="How: "_HOW,WSTR(2,"F")="?45"
 S WSTR(3)=USER,WSTR(3,"F")="!?3"
 S WSTR(4)="NDC: "_NDC,WSTR(4,"F")="?45"
 ;
 N N S N=$O(WSTR(" "),-1)+1
 S WSTR(N)="",WSTR(N,"F")="!"
 ;
 D EN^DDIOL(.WSTR)
 ;
 I R ; Restore naked reference
 QUIT
 ;
 ; ==========================================================================
 ;
 ; The below is completely completely separate from the above.
 ;
 ;
PREX ; [Public] Pre-exchange NDC entry (soooo hard!); PEP for [VFD PSJ PRE-EX NDC FILL]
 ; ^TMP("PSG",29987,"A","C","AMOXICILLIN/CLAVULANATE (AUGMENTIN) TAB^37U")=""
 ; ^TMP("PSG",29987,"A","C","ARIPIPRAZOLE (ABILIFY) TAB^41U")=""
 ; ^TMP("PSG",29987,"A","C","FLUOXETINE (PROZAC) CAP,ORAL^38U")=""
 ; ^TMP("PSG",29987,"A","C","LISINOPRIL (PRINIVIL) TAB^32U")=""
 ; ^TMP("PSG",29987,"A","C","MIRTAZAPINE (REMERON) TAB^39U")=""
 ; ^TMP("PSG",29987,"A","C","SIMVASTATIN (ZOCOR) TAB^34U")=""
 ; ^TMP("PSG",29987,"A","C","TRAZODONE (DESYREL) TAB^36U")=""
 ; ^TMP("PSG",29987,"A","P","IBUPROFEN (MOTRIN) TAB^33U")=""
 ;
 I '$$IPON^VFDPSNDC() W !!,"NDC Capture is not enabled",! QUIT
 ;
 N % S:($T(^XTMLOG)]"") %=$$FILEINIT^XTMLOG("PSG")
 ;
 K ^TMP("PSG",$J),^TMP("PSGPXDS",$J)
 D ENCV^PSGSETU ; Env vars for IP Pharmacy
 D ENDPT^PSGP   ; Patient selection plus Adm date
 I $G(PSGP)<1 QUIT  ; ^
 N PSGOL S PSGOL="S" ; Short profile please!
 D ENGORD^PSGOU      ; Collect data into ^TMP("PSG",$J)
 ;
 D:$T(^XTMLOG)]"" INFO^XTMLOG("--- ^TMP(""PSG"") ---")
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^TMP("PSG",$J)))
 ;
 N DSDT S DSDT=$$FMADD^XLFDT($$NOW^XLFDT(),-1)  ; Start from 24 hours before.
 ;
 ; Loop through active orders ("A") and populate our fake file in ^TMP("PSGPXDS",$J) with the data
 N CNT
 N ST S ST="" F  S ST=$O(^TMP("PSG",$J,"A",ST)) Q:ST=""  D  ; By Status
 . N DRGON S DRGON="" F  S DRGON=$O(^TMP("PSG",$J,"A",ST,DRGON)) Q:DRGON=""  D  ; each order
 . . N ODNM S ODNM=$P(DRGON,U)  ; order drug name
 . . N PSGORD S PSGORD=+$P(DRGON,U,2)  ; Order number in 55,dfn,5
 . . N DSNODE S DSNODE=$NA(^PS(55,PSGP,5,PSGORD,11)) ; Dispense node
 . . N HASPE S HASPE=0 ; Has Pre-exchange
 . . N DS S DS=DSDT F  S DS=$O(@DSNODE@("B",DS)) Q:'DS  D  ; Dispense date, starting from 1 day before
 . . . N DSIEN F DSIEN=0:0 S DSIEN=$O(@DSNODE@("B",DS,DSIEN)) Q:'DSIEN  D  ; dispense ien
 . . . . N Z S Z=$G(@DSNODE@(DSIEN,0)) Q:Z=""  ; get zero node
 . . . . N DRG S DRG=$P(Z,U,2) ; drug
 . . . . N AMOUNT S AMOUNT=$P(Z,U,3) ; amount
 . . . . N HOW S HOW=$P(Z,U,5)  ; Pre-exchange, Pick list, return etc
 . . . . N NDC S NDC=$P($G(@DSNODE@(DSIEN,21600)),U) ; NDC if present
 . . . . I HOW=2,NDC="" D  ; No NDC, and a pre-exchange. This is what we are looking for.
 . . . . . S CNT=$G(CNT)+1
 . . . . . N DRGNM S DRGNM=$P(^PSDRUG(DRG,0),U)
 . . . . . S ^TMP("PSGPXDS",$J,CNT,0)=ODNM_U_AMOUNT_U_PSGORD_U_DSIEN_U_DRG
 . . . . . S ^TMP("PSGPXDS",$J,"B",DRGNM,CNT)=""
 . . . . . S ^TMP("PSGPXDS",$J,"E",DRGNM,CNT)=AMOUNT_U_PSGORD_U_DSIEN
 . . . . . S ^TMP("PSGPXDS",$J,"F",DRG,CNT)=PSGORD_U_DSIEN_U_DRGNM  ; Index!! Love indices
 ;
 D:$T(^XTMLOG)]"" INFO^XTMLOG("--- ^TMP(""PSGPXDS"") ---")
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^TMP("PSGPXDS",$J)))
 ;
 ; No orders? Print labels (if any) and quit.
 I '$D(^TMP("PSGPXDS",$J)) D  QUIT
 . W !,"There are no pre-exchange doses that need an NDC."
 . D PRINT
 ;
 ; Make this a pretend fileman file.
 S ^TMP("PSGPXDS",$J,0)="ATTRIBUTE"_U_1.01
 ;
 N EXIT S EXIT=0  ; Exit flag. If set (by ^) then user didn't finish.
 ; Quit if no data (before and after) (1st time of course 1st quit is non-sensical.
 F  Q:'$O(^TMP("PSGPXDS",$J,0))  D SEL(.EXIT) Q:EXIT  Q:'$O(^TMP("PSGPXDS",$J,0))
 ;
 I EXIT QUIT  ; User didn't finish entereing NDCs (^).
 ;
 K ^TMP("PSGPXDS",$J) ; we are totally done
 ;
 D PRINT
 ;
 QUIT
 ;
PRINT ; [Internal] Print Label; called from a couple of places in this routine
 ; ZEXCEPT: DFN
 ; Copied from HK^PS{G,J}OE. Label printing code.
 W !,"Printing MAR Labels from Pre-exchange (if any)..." ; Yes I know, same lines as above
 S DFN=PSGP ; just for sanity
 N PSGOP S PSGOP=PSGP ; Old patient (label code wants that)
 D INP^VADPT ; get inpatient vars
 N PSJPWD S PSJPWD=+VAIN(4) ; ward
 I PSJPWD D  ; ward
 . N PSJACPF  ; can't remember what's that for
 . S PSJACPF=10
 . D:$P(PSJSYSL,U,2)]"" ENQL^PSGLW ; if we are supposed to print, go ahead and print.
 QUIT
 ;
SEL(EXIT) ; [Internal] Display and select. Called by PREX.
 ; ZEXCEPT: IOF,IOM
 ; ZEXCEPT: PSGP
 ; ZEXCEPT: AB Xindex has trouble with ?.
 ;
 D PDRGLST  ; Print drug list
 ;
 N DIR,X,Y,DA,DIRUT S DIR(0)="F^1:40",DIR("A")="Scan NDC or type drug name"
 S DIR("?")="^D HLP1^VFDPSJPX"
 S DIR("??")="^D HLP2^VFDPSJPX"
 D ^DIR
 I $D(DIRUT) W ?60,"...Cancelling",! S EXIT=1 QUIT
 ;
 I $E(Y)=" " QUIT
 ;
 N INPUT S INPUT=Y
 ;
 N NDC,SCAN,DIEN,NDCSCAN  ; prep our variables
 ;
 ; If input is alpha, search for a drug (B index)
 N BADLOOK S BADLOOK=0  ; Bad look-up
 I $E(INPUT)?1A DO  Q:BADLOOK
 . N X,Y,DIC,DIOUT,DTOUT,DLAYGO,DTOUT,DUOUT
 . S X=INPUT S DIC(0)="MEQ",DIC=$$OREF^DILF($NA(^TMP("PSGPXDS",$J)))
 . D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(DIC))
 . D ^DIC  ; search
 . I Y<1 S BADLOOK=1 QUIT  ; bail if not selected or not found.
 . ;
 . S DIEN=$P(^TMP("PSGPXDS",$J,+Y,0),U,5) ; drug ien
 . F  D  Q:$G(NDCSCAN)]""
 .. N Y  ; my precious
 .. N ERR
 .. S NDCSCAN=$$ASKNDC^VFDPSNDC(DIEN,"",.ERR)  ; ask for NDC
 .. I $D(ERR)#2 D  QUIT
 ... I $E(ERR)=U S NDCSCAN=U QUIT
 ... I ERR]"" W "   "_ERR QUIT
 . I NDCSCAN=U S EXIT=1 QUIT  ; grr grr. Quit here and above
 . W "   "_$P(NDCSCAN,U)
 . S NDC=$P(NDCSCAN,U)
 . S SCAN=$P(NDCSCAN,U,2)
 . ;
 ;
 QUIT:$G(EXIT)
 ;
 ; Input is not alpha. Try scan code matching.
 ELSE  DO  QUIT:$G(NDC)=""
 . S SCAN=INPUT
 . S DIEN=$O(^PSDRUG("C",SCAN,""))
 . I 'DIEN W !,"No match for this scan",! H 1 QUIT
 . I DIEN,'$D(^TMP("PSGPXDS",$J,"F",DIEN)) D  H 2 QUIT
 . . W !,"I matched "_$P($G(^PSDRUG(DIEN,0)),U)_" but I can't see this drug on the list."
 . N SYNIEN S SYNIEN=$O(^PSDRUG("C",SCAN,DIEN,""))
 . I 'SYNIEN W "**INDEX ERROR**. Edit entry in drug file for that synonym and try again.",! H 2 QUIT
 . S NDC=$P(^PSDRUG(DIEN,1,SYNIEN,0),U,2) ; get syn NDC for barcode
 . S NDC=$$NDCFMT^PSSNDCUT(NDC) ; format it to decide if it's valid
 . I 'NDC W "**STORED NDC IS NOT VALID**",! H 2 QUIT
 ;
 ; S ^TMP("PSGPXDS",$J,CNT,0)=ODNM_U_AMOUNT_U_PSGORD_U_DSIEN_U_DRG
 ; S ^TMP("PSGPXDS",$J,"B",DRGNM,CNT)=""
 ; S ^TMP("PSGPXDS",$J,"E",DRGNM,CNT)=AMOUNT_U_PSGORD_U_DSIEN
 ; S ^TMP("PSGPXDS",$J,"F",DRG,CNT)=PSGORD_U_DSIEN_U_DRGNM  ; Index!! Love indices
 ;
 N IEN S IEN=$O(^TMP("PSGPXDS",$J,"F",DIEN,""))
 W !,"Filing NDC for "_$P(^(IEN),U,3)  ; **NAKED**
 N PSGORD,DSIEN S PSGORD=$P(^(IEN),U),DSIEN=$P(^(IEN),U,2)      ; **NAKED**
 ;
 D:$T(^XTMLOG)]"" INFO^XTMLOG("--- Dispense Log Before ---")
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(55,PSGP,5,PSGORD,11,DSIEN)))
 ;
 N FDA
 N C S C=","
 S FDA(55.0611,DSIEN_C_PSGORD_C_PSGP_C,21600)=NDC
 S FDA(55.0611,DSIEN_C_PSGORD_C_PSGP_C,21601)=SCAN
 ;
 ; ZEXCEPT: DIERR
 N ERR
 D FILE^DIE(,"FDA","ERR")
 I $D(DIERR) D APPERROR^%ZTER("Fileman failure") W "Fileman failure. See error log.",! H 2 QUIT
 ;
 W ?60,"...filed",!
 ;
 D:$T(^XTMLOG)]"" INFO^XTMLOG("--- Dispense Log After ---")
 D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^PS(55,PSGP,5,PSGORD,11,DSIEN)))
 ;
 K ^TMP("PSGPXDS",$J,IEN)
 K ^TMP("PSGPXDS",$J,"B",$P(^PSDRUG(DIEN,0),U),IEN)
 K ^TMP("PSGPXDS",$J,"E",$P(^PSDRUG(DIEN,0),U),IEN)
 K ^TMP("PSGPXDS",$J,"F",DIEN,IEN)
 ;
 QUIT
 ;
PDRGLST ; [Internal] Called by SEL and HLP2: Print drug list to be scanned/selected
 N DRNM S DRNM=""
 N PSGORD S PSGORD=0
 N DSIEN S DSIEN=0
 W @IOF,!!!
 W $$CJ^XLFSTR("Drugs needing Pre-Exchange NDCs",IOM,"-"),!!
 N COL S COL=0
 N DRNM S DRNM=""
 F  S DRNM=$O(^TMP("PSGPXDS",$J,"E",DRNM)) Q:DRNM=""  D
 . N SUB S SUB="" F  S SUB=$O(^TMP("PSGPXDS",$J,"E",DRNM,SUB)) Q:SUB=""  D
 .. N AMOUNT S AMOUNT=$P(^(SUB),U)  ; **NAKED**
 .. I COL=2 S COL=0 W !
 .. N TAB S TAB=38*COL+4
 .. W ?(38*COL),"[",AMOUNT,"] ",?TAB,$E(DRNM,1,36)
 .. S COL=COL+1
 ;
 W !!!
 QUIT
 ;
HLP1 ; [Internal] Called by SEL; DIR ? help
 ;
 N %
 S %(1)="Scan the NDC barcode on your drug; or enter the first few letters of a"
 S %(2)="drug name. If there are multiple drugs with similar names, you will be"
 S %(3)="asked for which one you want to use."
 ;
 N DIQUIET D EN^DDIOL(.%) ; Save off DIQUIET field if it already existed.
 QUIT
 ;
HLP2 ; [Internal] Called by SEL; DIR ?? help
 N DIQUIET
 ; K ^TMP("VFDPSJPX",$J)
 ; D CLEAN^DILF
 ;
 N DIR,DIRUT
 N %
 S %(1)="Scan the NDC barcode on your drug; or enter the first few letters of a"
 S %(2)="drug name. If there are multiple drugs with similar names, you will be"
 S %(3)="asked for which one you want to use."
 S %(4)=" "
 S %(5)="If you have set your ward parameters to print labels, they will be"
 S %(6)="printed after the completion of this option."
 S %(7)=" "
 D EN^DDIOL(.%)
 ;
 D W("Your Drugs are:")
 N I S I="" F  S I=$O(^TMP("PSGPXDS",$J,"B",I)) Q:I=""  D W(I)
 D W(" ")
 S DIR(0)="E" D ^DIR
 ;
 D W("NDC and Barcode Information:")
 N I S I="" F  S I=$O(^TMP("PSGPXDS",$J,"B",I)) Q:I=""  D  Q:$G(DIRUT)
 . N DRGNM S DRGNM=I
 . N J S J=$O(^(I,""))
 . N Z S Z=^TMP("PSGPXDS",$J,J,0)
 . N DRG S DRG=$P(Z,U,5)
 . N I,J ; My precious
 . D W(" ")
 . D W(DRGNM_":")
 . D W(" ")
 . D NDCHLP2^VFDPSNDC
 . S DIR(0)="E" D ^DIR
 ;
 ; M ^TMP("VFDPSJPX",$J)=^TMP("DIMSG",$J)
 ;
 ; D BROWSE^DDBR($NA(^TMP("VFDPSJPX",$J)),"N","Pre-exchange NDC fill help")
 ;
 ; K ^TMP("VFDPSJPX",$J)
 ;
 D PDRGLST
 ;
 QUIT
 ;
W(X) D EN^DDIOL(X) QUIT
 ;
PRT(DRGREF) ; [Only from PSGEUD] Print EUD Label
 ; Input: REF like ^PS(55,D2,5,D1,1,D0,0) ; That's the units dispensed node.
 ;
 ; First, re-enable menu option to reprint
 D OUT^XPDMENU("VFD PSJ EUD LBL REPRINT","")
 ;
 ; unload the data
 N PSGP   S PSGP=$QS(DRGREF,2) ; pt
 N PSGORD S PSGORD=$QS(DRGREF,4) ; order
 N DA     S DA=$QS(DRGREF,6) ; drug ien in order
 N VFDPSGDRG S VFDPSGDRG=$P(^PS(55,PSGP,5,+PSGORD,1,DA,0),U)    ; drug
 N VFDPSGEUD S VFDPSGEUD=$P(^PS(55,PSGP,5,+PSGORD,1,DA,0),U,11) ; EUD quantity LAST dispensed
 ;
 ; Tell user what we are up to
 N DRGNM S DRGNM=$P(^PSDRUG(VFDPSGDRG,0),U)
 WRITE !!,"For drug: "_DRGNM,!
 ;
 I +VFDPSGEUD=0 W !!,"No previous dispense located. Quiting.",! QUIT
 ;
 ; Ask for label quantity
 N LBLQTY
 N DIR,X,Y,DA,DIRUT,DIROUT,DUOUT,DTOUT
 S DIR(0)="NO^1:99",DIR("A")="Enter number of labels",DIR("B")=1
 D ^DIR
 I $D(DIRUT) QUIT
 S LBLQTY=+$G(Y,1) ; + just in case Y is empty
 ;
 ; task print
 N %ZIS,IOP
 S %ZIS("A")="Label Printer: "
 S %ZIS("B")=$P(PSJSYSL,"^",2),%ZIS="NQ"
 D ^%ZIS
 I $G(POP) QUIT
 N ZTSK
 N ZTSAVE
 S ZTSAVE("VFDPSGDRG")=""
 S ZTSAVE("PSGORD")=""
 S ZTSAVE("PSGP")=""
 S ZTSAVE("VFDPSGEUD")=""
 S ZTSAVE("LBLQTY")=""
 N ZTRTN S ZTRTN="PRTDQ^VFDPSJPX"
 N ZTDESC S ZTDESC="Extra Doses Label"
 N ZTDTH S ZTDTH=$H
 N ZTIO,ZTUCI,ZTCPU,ZTPRI,ZTKIL  ; won't use these
 D ^%ZTLOAD
 W !
 I $D(ZTSK) W "Queued as task "_ZTSK,!
 E  W "Queuing failed! Contact your system manager.",!
 D ^%ZISC  ; Doesn't really close the device (we didn't open it); just resets IO.
 QUIT
 ;
PRTDQ ; Print Dequeue
 ; ZEXCEPT: LBLQTY,ZTREQ from Taskman
 USE IO ; not needed. $IO is already opened from taskman. Just in case you want to run this in foreground.
 N %,LBLCNT
 F LBLCNT=1:1:LBLQTY D
 . S %=$$EN^VFDPSPRT("PSGEUD")
 S ZTREQ="@" ; delete task when done.
 QUIT
 ;
POST ; [PEP for KIDS only] Turn on the parameter at system level for OMH
 I '$D(DT) N DIQUIET S DIQUIET=1 D DT^DICRW K DIQUIET ; set DT, needed by $$SITE^VASITE(), just for me from cmd line.
 I $$111() D EN^XPAR("SYS","VFD PSJ EUD LBL PRINT",,"Y")           ; Print labels for EUD for OMH
 QUIT
 ;
111() ; [Public] Is this Site 111 (OMH)?
 N SITE S SITE=$P($$SITE^VASITE(),U,3)
 Q ($E(SITE,1,3)=111)
 ;
