VFDPSJPM ;DSS/SMH - Capture patient dispense data for VFDPSJPL;12/22/2014
 ;;2013.2;DSS INC VXVISTA OPEN SOURCE;;;Build 11
 ;
UI(TM,WDN,PN,PSGP) ; [Private: permitted caller is VFDPSJPL] Draw the UI and point and shoot scanner
 ;
 D:($T(^XTMLOG)]"") INFO^XTMLOG("Data before Drawing")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^TMP("VFDPL",$J)))
 ;
 ; Don't need to do this as our caller does it
 ; D IOSETUP^VFDPSJPP
 ;
 D SCAN(TM,WDN,PN,PSGP)
 ;
 QUIT
 ;
DRAW(TM,WDN,PN,PSGP) ; Draw Header
 ; ZEXCEPT: DX,DY,IOF,IOINHI,IOINORM,IOM,IORVOFF,IORVON,IOUOFF,IOUON,IOXY
 W @IOF
 W ?0,IOINHI,IOUON,"Enter Units Dispensed via Pick List",IOINORM,IOUON
 N M S M=IOM/3\1  ; Divide into thirds and round down
 N %1 S %1="?"_(M+1) ; 1st col
 N %2 S %2="?"_(M*2+1) ; 2nd col
 W @%1,"TEAM: ",$S(TM'["zz":TM,1:"* N/F *"),@%2,"WARD: ",$S(WDN'["zz":WDN,1:"* N/F *")  ; write
 N % F %=$X:1:IOM-1 W " "  ; Fill the rest of the line
 ;
 W !!?0,IOINHI,IOUON,"Legend:",IOINORM
 W ?8,"[C]/[R]/[P]/[OC] is Continous, Fill on Request, PRN, or On Call",!
 W ?8,"##* stands for number of units dispensed and whether NDC was scanned",!
 W ?8,"00- means that the quantity requested is zero; and this drug may be skipped, but is still selectable",!
 W ?8,"WS-/NV- means that the drug is ward stock or not verified. Neither is selectable"
 ;
 ; Draw Patient Name/PID
 N PPN S PPN=$S('$D(^DPT(PSGP,0)):PN,$P(^(0),"^")]"":$P(^(0),"^"),1:PN)
 N DFN S DFN=PSGP
 N PID,VA
 D DEM^VADPT
 S PID=$S($D(VA("MRN")):VA("MRN"),1:$G(VA("PID")))
 N PSSN S PSSN=PID
 ;
 ; NBNBNB: Moving DY doesn't update $Y. So far we have just been doing LF's, so we still have the correct $Y.
 W !!
 S DY=$Y,DX=1 X IOXY W "Patient Name: "_IOINHI_PPN
 S DY=$Y,DX=M*2+1 X IOXY W IOINORM_IOUON_"MRN: "_PSSN
 ;
 W IOUOFF  ; remove underline
 ;
 N ROW S ROW=$Y+1                                    ; Move one down
 N COL S COL=0                                       ; First column will end up being LEN(0#3)\1+1 = 1
 N COLLEN S COLLEN=IOM/3-1                          ; Column length is a third of the screen. We are assured of IOM of at least 100
 N I S I="" F  S I=$O(^TMP("VFDPL",$J,"C",I)) Q:I=""  D  ; for each drug name.
 . N IEN F IEN=0:0 S IEN=$O(^TMP("VFDPL",$J,"C",I,IEN)) Q:'IEN  D  ; for each entry for the drug name (can have multiple entries for the same drug)
 . . S ($X,$Y)=0                                     ; We do this so we won't get artificial new lines
 . . I '(COL#3) S ROW=ROW+1                          ; Increment row every third column (drawing code)
 . . S DY=ROW,DX=COLLEN*(COL#3)\1+1 X IOXY   ; Move to Row and 1/3rd of screen length * column number + 1 (mod 3 makes sure we have exactly 3 columns)
 . . W $E($P(^TMP("VFDPL",$J,IEN,0),U,2),1,COLLEN-4) ; Write up to Column length - 4
 . . S ^TMP("VFDPL",$J,IEN,"IOXY")=DX_U_DY           ; Store location for drawing later
 . . S DX=COLLEN*(COL#3+1)\1-7 X IOXY                ; Move X (keep Y) to next column over (COL#3+1) from above and go back 7 spaces.
 . . N HASNDC S HASNDC=$L($G(^TMP("VFDPL",$J,IEN,"NDC"))) ; Is there already an NDC?
 . . S HASNDC=$S(HASNDC:"N",1:"*")                   ; Change HASNDC to N if present
 . . N QTY S QTY=^TMP("VFDPL",$J,IEN,"PDD")          ; Is there a dispensed quantity?
 . . S QTY=$S(QTY:QTY,QTY?2A:QTY,QTY=0:0,1:"##")     ; Change QTY to QTY or WS/SM/NV or 0 or ** if not filled yet
 . . S QTY=$J(QTY,2)                                 ; Make it take 2 spaces
 . . I ^TMP("VFDPL",$J,IEN,"PDN")?2A S QTY=^("PDN"),HASNDC="-"      ; Ward Stock etc; No NDC; and unselectable
 . . I QTY="##",^TMP("VFDPL",$J,IEN,"PDN")=0 S QTY="00",HASNDC="-"         ; Quantity is 0 (PRN, and Fill on Request)
 . . N ST S ST=^TMP("VFDPL",$J,IEN,"ST")             ; Order Status
 . . W IOUON_"["_ST_"]"_IOUOFF_IORVON_QTY_HASNDC_IORVOFF ; Write number holder + NDC holder
 . . S ^TMP("VFDPL",$J,IEN,"INDIOXY")=(DX+3)_U_DY        ; Store location for drawing later (+3 is to avoid the Status)
 . . S COL=COL+1                                     ; ++ for next drawing movement.
 ;
 ; Reset $Y
 S $Y=DY
 ;
 ; Draw last line
 W !
 N % F %=1:1:IOM W "="
 W !!
 ;
 QUIT
 ;
SCAN(TM,WDN,PN,PSGP) ; Scanning code
 ; ZEXCEPT: IORVOFF,IORVON
 ;
 ; NB: Right now, I made a bo bo with NDC and Scan fields sometimes being in
 ; the same variable (ndc^scan) called NDC.
 ;
 N DONE S DONE=0
 N X,Y
 ;
 ; In each loop, draw the drug grid. More expensive but programmatically easier.
 F  D  Q:DONE
 . D DRAW(TM,WDN,PN,PSGP)
 . ;
 . N SCANTOP S SCANTOP=$X_U_$Y
 . ;
 . ; The following code is only used by the HELP processor in ^DIR to restore
 . ; our location
 . ; S DX=$P(SCANTOP,U),DY=$P(SCANTOP,U,2) X IOXY ; Move cursor to where we will start scanning
 . ; W IOEDEOP  ; Erase from there to the end of the page
 . ;
 . N DIR,DA,DIRUT,DIOUT,DTOUT,DIRUT,DIROUT,DUOUT
 . S DIR(0)="FAO",DIR("A")="     Scan a barcode to capture a drug: "
 . S DIR("?")="^D HLP0^VFDPSJPM"
 . S DIR("??")="^D HLP1^VFDPSJPM"
 . D ^DIR
 . ;
 . I $D(DUOUT) S DONE=1 QUIT  ; ^
 . I $D(DIROUT) S DONE=1 QUIT  ; ^^
 . I $D(DTOUT) S DONE=1  ; Time out
 . I X="" W ?50,IORVON_"--- WUFF WUFF ---"_IORVOFF HANG 0.1 QUIT
 . ;
 . N SCAN S SCAN=Y
 . ;
 . N TIEN,NDC
 . ;
 . ; if statement is to capture scans
 . ; else statement is to capture alpha drugs
 . N TIEN,DIEN,SYNIEN
 . N SCANNOGO S SCANNOGO=0
 . ;
 . ; Using the user input (in SCAN), find drug in C index and then if you can
 . ; find a single drug, find this our drug array (that's TIEN). We then check
 . ; that TIEN is not ward stock and not non-verified.
 . ;
 . ; If any of this fails, we back out.
 . ;
 . I $O(^PSDRUG("C",SCAN,"")) D
 . . D
 . . . N %1,%2
 . . . S %1=$O(^PSDRUG("C",SCAN,""))
 . . . S %2=$O(^PSDRUG("C",SCAN,%1))
 . . . I %2 S SCANNOGO=1  QUIT  ; two items under sub... not a good syn
 . . . S DIEN=%1
 . . . S SYNIEN=$O(^PSDRUG("C",SCAN,DIEN,""))
 . . . S TIEN=$O(^TMP("VFDPL",$J,"B",DIEN,""))
 . . . I 'TIEN QUIT
 . . . S NDC=$P($G(^PSDRUG(DIEN,1,SYNIEN,0)),U,2)
 . . . I 'NDC QUIT  ; Redundant I know, but just in case I add more lines I won't get tripped.
 . . ;
 . . N DIR S DIR(0)="E"
 . . I '$G(DIEN) W !?5,"Cannot find drug associated with this scan code",! S SCANNOGO=1 D ^DIR QUIT
 . . I '$G(TIEN) D  S SCANNOGO=1 D ^DIR QUIT
 . . . W !?5,"Your scan matched "_$P(^PSDRUG(DIEN,0),U)_" but I don't see this drug on the above list",!
 . . I ^TMP("VFDPL",$J,TIEN,"PDN")?2A W !?5,"Cannot select a "_^("PDN")_" drug",! S SCANNOGO=1 D ^DIR QUIT
 . . I '$G(SYNIEN) W !?5,"Drug C index is corrupted. Call applicaton support!",! S SCANNOGO=1 D ^DIR QUIT
 . . I '$G(NDC) W !?5,"Barcode is not associated with an NDC code!",! S SCANNOGO=1 D ^DIR QUIT
 . . I '$$NDCFMT^PSSNDCUT(NDC) W !?5,"NDC "_NDC_" is not a valid NDC code",! S SCANNOGO=1 D ^DIR QUIT
 . . S NDC=$$NDCFMT^PSSNDCUT(NDC)
 . . W ?60,NDC,?80,$P(^PSDRUG(DIEN,0),U)
 . ;
 . QUIT:SCANNOGO  ; -- STOP --
 . ;
 . ; Select a drug from the list
 . ;
 . ;
 . N DRUGNOGO S DRUGNOGO=0
 . ELSE  DO                             ;===================
 . . N DIC,DLAYGO,DINUM,X,Y,DTOUT,DUOUT,D
 . . S X=SCAN
 . . S D="C"
 . . S DIC(0)="EQ",DIC="^TMP(""VFDPL"",$J,"
 . . S DIC("S")="I ^(""PDN"")'?2A" ; ** Don't allow Ward Stock selection **
 . . D IX^DIC
 . . I Y<1 S DRUGNOGO=1 W ?50,IORVON_"--- WUFF WUFF ---"_IORVOFF HANG 0.1 QUIT
 . . S TIEN=+Y
 . . S DIEN=$P(Y,U,2)
 . . S NDC=$P(^TMP("VFDPL",$J,TIEN,"NDC"),U) ; NDC only, no scan code. (may be empty)
 . . N RESULT S RESULT=""
 . . F  D  Q:$L(RESULT)
 . . . W !!,"For drug "_$P(^PSDRUG(DIEN,0),U)_":",!!
 . . . N Y  ; my precious
 . . . N ERR
 . . . S RESULT=$$ASKNDC^VFDPSNDC(DIEN,NDC,.ERR)  ; ask for NDC, supplying default if necessary.
 . . . I $L($G(ERR)),ERR'=U W !,ERR,! ; normal error ; loop around and ask again
 . . . I $E($G(ERR))=U S RESULT="^"
 . . I RESULT="^" S DRUGNOGO=1 QUIT
 . . S NDC=$P(RESULT,U),SCAN=$P(RESULT,U,2)
 . ;
 . QUIT:DRUGNOGO
 . ;
 . N DISPUNIT S DISPUNIT=$$GET1^DIQ(50,DIEN,14.5)
 . ; Store
 . N R S R=$NA(^TMP("VFDPL",$J,TIEN))
 . N DIR,DA,DIRUT,DIOUT,DTOUT,DIRUT,X,Y
 . S DIR(0)="N^"_-@R@("LMT")_":"_@R@("LMT")_":3",DIR("A")="     Enter Quantity ("_DISPUNIT_")"
 . S DIR("B")=$S(@R@("PDD"):@R@("PDD"),1:@R@("PDN"))  ; If actually dispensed exists, use that as the default; otherwise, use the expected amount.
 . S DIR("PRE")="I SCAN]"""",SCAN=X S X=DIR(""B"")" ; If we have a scan code and it's scanned again, take the default value as quantity.
 . S DIR("?")="     Enter a positive number for dispenses, negative for returns."
 . D ^DIR
 . ;
 . I $D(DIRUT) QUIT
 . ;
 . ; Okay. All done. Store data
 . S @R@("PDD")=Y
 . S @R@("NDC")=NDC_U_SCAN
 . ;
 . ; Remove item to indicate "done"
 . K ^TMP("VFDPL",$J,"D",TIEN)
 . D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($name(^TMP("VFDPL",$J,"D")))
 . ;
 . ; *** Store in actual pick list file ***
 . N PSGPLG S PSGPLG=^TMP("VFDPL",$J,"PSGPLG")
 . N PSGP S PSGP=^TMP("VFDPL",$J,"PSGP")
 . N DNNUM S DNNUM=^TMP("VFDPL",$J,TIEN,"DNNUM")
 . N PSGORD S PSGORD=^TMP("VFDPL",$J,TIEN,"ORD")
 . N PDD S PDD=^TMP("VFDPL",$J,TIEN,"PDD")
 . S $P(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DNNUM,0),"^",3)=PDD
 . S ^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DNNUM,21600)=^TMP("VFDPL",$J,TIEN,"NDC")
 . ;
 . ; Dump this patient's data so that we can watch it grow
 . D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^PS(53.5,PSGPLG,1,PSGP)))
 . ;
 . I '$D(^TMP("VFDPL",$J,"D")) S DONE=1  ; We are done if we processed all drugs
 QUIT
 ;
HLP0 ; Help for ?
 ; ZEXCEPT: SCANTOP Top of scanning region; used to draw
 ; ZEXCEPT: IOXY,DX,DY,IOEDEOP Screen drawing variables.
 W !
 W ?5,"Scan barcode or enter the first few letters of a drug on the list.",!
 W ?5,"     Type ?? for more help",!
 W !!
 N DIR,X,Y S DIR(0)="E" D ^DIR
 ;
 ; -1 b/c DIR adds a new line after help
 S DX=$P(SCANTOP,U),DY=$P(SCANTOP,U,2)-1 X IOXY ; Move cursor to where we will start scanning
 W IOEDEOP  ; Erase from there to the end of the page
 ;
 QUIT
 ;
HLP1 ; Help for ?? when scanning
 ; ZEXCEPT: SCANTOP Top of scanning region; used to draw
 ; ZEXCEPT: IOXY,DX,DY,IOEDEOP Screen drawing variables.
 W !
 W ?5,"Scan a barcode (and I will try to find it and tell you whether it matches)",!
 W ?5,"or type the first few letters of a drug name, and then pick an NDC.",!
 W ?5,"You will be then asked for quantity.",!!
 W ?5,"If you scanned a code already, you can scan it again at the quantity field",!
 W ?5,"to accept the default quantity."
 W !!
 N DIR,X,Y S DIR(0)="E" D ^DIR
 ;
 ; -1 b/c DIR adds a new line after help
 S DX=$P(SCANTOP,U),DY=$P(SCANTOP,U,2)-1 X IOXY ; Move cursor to where we will start scanning
 W IOEDEOP  ; Erase from there to the end of the page
 ;
 QUIT
 ;
ASSERT(X,%MSG) ; Primitive Assert
 I 'X S $EC=",ASSERTION-FAILED,"
 QUIT
