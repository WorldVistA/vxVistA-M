VFDPSJPL ; [Public] DSS/SMH - Duck Hunting Pick List;10/29/2014
 ;;2013.2;DSS INC VXVISTA OPEN SOURCE;;;Build 11
 ;
 ; Let's have fun!
 ;
 ; Resize the screen. Width has to be at least 100 chars, Height at least 24.
 N % S %=$$AUTOMARG^VFDPSJPP()
 N FAIL S FAIL=0
 I $P(%,U)<100!($P(%,U,2)<24) D  S FAIL=1
 . D HOME^%ZIS  ; size back to 80x24
 . W !!,"Cannot use hunting Pick List.",!
 . W "Terminal Width is less than 100 or Terminal Height is less than 24 characters.",!
 . W "The measured width is "_$P(%,U)_" and the height is "_$P(%,U,2)_"."
 ;
 N USEOLD S USEOLD=0  ; Use old pick list?
 I FAIL D
 . N DIR,DA,DTOUT,DUOUT,DIROUT,DIRUT,X,Y
 . S DIR(0)="Y",DIR("B")="Y",DIR("A")="Do you want to use the regular pick list" D ^DIR
 . I +$G(Y) S USEOLD=1
 ;
 I USEOLD DO PLNDC^VFDPSJND QUIT  ; Do the old one and exit
 I FAIL,'USEOLD QUIT         ; Okay we just have to quit now.
 ;
 ; ZEXCEPT: PSGPLG Pick List number from PSGPLDP
 N PSGPLTND S PSGPLTND=^PS(53.5,PSGPLG,0)  ; Zero Node
 I $P(PSGPLTND,"^",2),'$P(PSGPLTND,"^",9) D  ; If ward group but not run completion d/t, warn user:
 . N PSGOD S PSGOD=$$ENDTC^PSGMI($P(PSGPLTND,"^",11))
 . W $C(7),$C(7),!!?33,"*** WARNING ***",!,"THIS PICK LIST STARTED TO RUN ",PSGOD,", BUT HAS NOT RUN TO COMPLETION."
 ;
 D EN1
 ;
 QUIT
 ;
EN1 ; [Public] Main EP
 ; ZEXCEPT: IOF,IOINHI,IOINORM,IOM,IOUON,IOXY
 ;
 ; Start logging
 N % S:($T(^XTMLOG)]"") %=$$FILEINIT^XTMLOG("PSGPL")
 ;
 ; Set-up vars
 D IOSETUP^VFDPSJPP
 ;
 ; Ward drawing data structure (Put Teams and Ward Names by count of each ward)
 K ^TMP("VFDPL-WARD",$J)
 N CNT S CNT=0
 N TM,WDN S (TM,WDN)=""
 F  S TM=$O(^PS(53.5,"AC",PSGPLG,TM)) Q:TM=""  D
 . F  S WDN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN)) Q:WDN=""  D
 .. S CNT=CNT+1
 .. S ^TMP("VFDPL-WARD",$J,CNT,"WDN")=WDN
 .. S ^TMP("VFDPL-WARD",$J,CNT,"TM")=TM
 ;
 ; 0 node: Count ^ Highlighted entry
 S ^TMP("VFDPL-WARD",$J)=CNT_U_1  ; Count and initial highlighted (i.e. selected) ward
 ;
 ; Draw the wards
 D WARDDRAW
 ;
 ; Event loop
 S (TM,WDN)=""
 N EVENT S EVENT=""
 F  S TM=$O(^PS(53.5,"AC",PSGPLG,TM)) Q:TM=""  D  Q:(EVENT="QUIT")
 . S WDN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN)) Q:WDN=""  D  Q:(EVENT="QUIT")
 .. F  D WARD(.EVENT) Q:(EVENT="QUIT")  D:(EVENT'="") EPROC(EVENT) S EVENT=""
 ;
 ; Remove drawing data structures
 K ^TMP("VFDPL-WARD",$J)
 ;
 ; Done. Remove screen handling variables.
 D IOKILL^VFDPSJPP
 ;
 ; End logging
 D:$T(^XTMLOG)]"" ENDLOG^XTMLOG("PSGPL")
 ;
 QUIT
 ;
WARDDRAW ; [Private] Draw wards; uses ^TMP("VFDPL-WARD") set-up above
 ; Draw and addend the data structure with the $X and $Y locations of the wards so we can redraw.
 I '$D(IOINHI) D IOSETUP^VFDPSJPP
 ;
 ; Headers
 W @IOF
 W IOINHI,$$CJ^XLFSTR("Team/Ward Listing (use Page Up/Down to scroll)",IOM),IOINORM,!
 W $$CJ^XLFSTR("Key: Team Name (if present)/Ward Name "_IOINHI_"(unfilled orders)"_IOINORM,IOM+8),!
 ;
 N TOTALNUMY S TOTALNUMY=$Y ; Location where we put the total number (save for later when we need to draw this)
 ;
 W !
 S $X=0
 W IOUON
 F %=0:1:IOM-1 W " "
 W IOUOFF,!  ; Underline
 ;
 ; Each column is 30 wide. But once we find out how many we can fit, we expand
 ; it out to fit the screen.
 N MAXCOL S MAXCOL=IOM\30  ; # of 30 length columns on screen
 N COLWIDTH S COLWIDTH=IOM/MAXCOL\1+1  ; Expand it out (e.g. from 30 to 35)
 ;
 ; A few tracking variables
 N CNT S CNT=0
 N COL S COL=0
 N ROW S ROW=$Y
 N DX,DY  ; for IOXY
 ;
 N ORDCOUNT S ORDCOUNT=0 ; Total order count
 N HIWARD S HIWARD=$P(^TMP("VFDPL-WARD",$J),U,2) ; Highlighted ward
 ;
 ; Drawing
 N CNT F CNT=1:1:$P(^TMP("VFDPL-WARD",$J),U) D
 . S (DX,^TMP("VFDPL-WARD",$J,CNT,"$X"))=COLWIDTH*COL ; We keep these so we can know where to draw in events
 . S (DY,^TMP("VFDPL-WARD",$J,CNT,"$Y"))=ROW+1        ; ditto
 . X IOXY  ; move over
 . S WDN=^TMP("VFDPL-WARD",$J,CNT,"WDN")
 . S TM=^TMP("VFDPL-WARD",$J,CNT,"TM")
 . ;
 . W:HIWARD=CNT IORVON                ; If this ward is highlighted, start highlight
 . W $E($S(TM'="zz":TM_"/",1:"")_WDN,1,COLWIDTH-5) ; Don't print zz. Print $e(1-25)
 . N WARDCNT S WARDCNT=$$WARDCNT^VFDPSJPP(WDN)     ; each ward's count of orders
 . S ORDCOUNT=ORDCOUNT+WARDCNT        ; Keep track of total orders
 . W IOINHI_" ("_WARDCNT_") "_IOINORM ; Bold the count of orders
 . W:HIWARD=CNT IORVOFF               ; Turn off highlighting for selected ward
 . S COL=COL+1                        ; Next col
 . I COL#MAXCOL=0 S COL=1,ROW=ROW+1   ; Wrap columns and move down if we reach the max columns
 ;
 ; Draw total orders to complete (and whether to file)
 W IOSC  ; Save the cursor
 S DX=0,DY=TOTALNUMY X IOXY S $X=0 ; Move to the Total number location (saved above)
 W $$CJ^XLFSTR("Total Orders to Complete: "_IOINHI_"("_ORDCOUNT_")"_IOINORM,IOM+8) ; IOM+8 b/c $CJ doesn't filter out control characters
 ;
 ; If we don't have any orders left, then tell the user we can file the pick list
 I ORDCOUNT=0 D
 . S DX=0,DY=TOTALNUMY+1 X IOXY S $X=0
 . S FILEMSG="Pick List ready to file. Press [F2] to file."
 . N REDWHITE S REDWHITE=$C(27)_"[37;41m" ; Red foreground with white background
 . N RESET S RESET=$C(27)_"[0m"
 . W $$CJ^XLFSTR(REDWHITE_FILEMSG_RESET,IOM+12) ; IOM+12 b/c we have 12 control characters that shouldn't count
 E  D
 . S DX=0,DY=TOTALNUMY+1 X IOXY S $X=0
 . N MSG S MSG="Press [F1] for help"
 . W $$CJ^XLFSTR(MSG,IOM)
 ; Retore cursor
 W IORC
 ;
 ; Write second header
 S $X=0
 N % W !,IOUON X "F %=$X:1:IOM-1 W "" """ W IOUOFF,!!
 W $$CJ^XLFSTR(IOINHI_"Select a Patient"_IOINORM_" | Key: Patient [Room-Bed] (Orders Remaining)",IOM+6),!!
 QUIT
 ;
EPROC(EVENT) ; [Private] Popped up events processing
 ; Also NOOP, but we don't do anything with that one.
 N DX,DY
 N OLDWDN S OLDWDN=WDN ; Old Ward Name... we need that for later
 ;
 ; Start Event Handling
 ; Handle page up and page down
 I EVENT="NEXT"!(EVENT="PREV") D
 . N CNT S CNT=$P(^TMP("VFDPL-WARD",$J),U)                         ; Count
 . I CNT=1 S DX=3,DY=2 X IOXY W CUROFF,"NO MORE WARDS" H 1 W CURON ; If there is only one, can't move...
 . N CURRWARD S CURRWARD=$P(^TMP("VFDPL-WARD",$J),U,2)             ; What's the current ward?
 . S CURRWARD=$S(EVENT="NEXT":CURRWARD+1,1:CURRWARD-1)             ; Next or Previous?
 . I CURRWARD<1 S CURRWARD=CNT                                     ; Wrap around if we page down too far
 . I CURRWARD>CNT S CURRWARD=1                                     ; Wrap around if we page up too far
 . D:$T(^XTMLOG)]"" INFO^XTMLOG("EVENT, COUNT, CURRWARD: "_EVENT_" "_CNT_" "_CURRWARD)  ; Log
 . S $P(^TMP("VFDPL-WARD",$J),U,2)=CURRWARD                        ; Set the current one
 . D:$T(^XTMLOG)]"" SAVEARR^XTMLOG($NA(^TMP("VFDPL-WARD",$J)))     ; log this one too
 . D WARDDRAW                                                      ; draw
 ;
 ; Repaint event
 I EVENT="REPAINT" D WARDDRAW
 QUIT
 ;
WARD(POPEVENT) ; [Internal] Main Loop that draws the screen for each ward
 N CURRWARD S CURRWARD=$P(^TMP("VFDPL-WARD",$J),U,2) ; Current ward
 N WDN S WDN=^TMP("VFDPL-WARD",$J,CURRWARD,"WDN") ; Ward Name
 N TM S TM=^TMP("VFDPL-WARD",$J,CURRWARD,"TM") ; Team Name
 ;
 N RB S RB="" ; Room bed
 N PN S PN="" ; Patient Name
 ;
 ; Count how many patients are in this ward
 N CNT S CNT=0
 F  S RB=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB)) Q:RB=""  D
 . F  S PN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN)) Q:PN=""  S CNT=CNT+1
 ;
 ; IOSL contains our screen length
 ; and if we need 30 chars for each col, so our maxcol is...
 N MAXROWS,MAXCOL
 S MAXCOL=IOM\30
 N COLWIDTH S COLWIDTH=IOM/MAXCOL\1+1
 ;
 N WY,DX,DY
 S WY=$Y
 ;
 ; Drawing code
 N R S R=$NA(^TMP("VFDPL-PT",$J))
 K @R
 N ROW S ROW=0
 N COL S COL=0
 F  S RB=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB)) Q:RB=""  D  Q:(EVENT="QUIT")       ; Loops through RB
 . F  S PN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN)) Q:PN=""  D  Q:(EVENT="QUIT")  ; then through Patient
 .. S $X=0                                                                      ; Prevent wrapping.
 .. N PSGP S PSGP=$P(PN,U,2)                                                    ; Patient IEN
 .. N PATNAME S PATNAME=$E($P(^DPT(PSGP,0),U),1,19)                             ; 1-19 of patient name
 .. I $L($P(^DPT(PSGP,0),U))>19 S $E(PATNAME,$L(PATNAME))=">"                   ; If longer, put >
 .. N TXT S TXT=PATNAME_" ["_$S(RB="zz":"n/a",1:RB)_"]["_$$PATCNT^VFDPSJPP(PN)_"]" ; Patient name [rb][# of orders to fill]
 .. S DX=COLWIDTH*COL                                                        ; X
 .. S DY=WY+ROW                                                                 ; Y
 .. S @R@(PN,"TXT")=TXT                                                         ; store
 .. S @R@(PN,"X")=DX                                                            ; ditto
 .. S @R@(PN,"Y")=DY                                                            ; ditto
 .. X IOXY
 .. W $E(TXT,1,29)                                                              ; write
 .. S COL=COL+1                                                                 ; Next column
 .. I COL#MAXCOL=0 S COL=0,ROW=ROW+1                                            ; Wrap around
 ;
 ; Move To Start X and prompt the user
 S DX=0,DY=DY+2 X IOXY  ; Move over to the prompt
 S $X=DX,$Y=DY          ; And convince M to write there
 ;
 N % S %="Type an entry or scan a barcode: " W %
 N TEXTORIGIN S TEXTORIGIN=$X_U_$Y  ; Store this position for later
 ;
 N XGRT,X ; event emitter.
 ;
 ; Initialize keyboard listening
 D INITKB^XGF
 F  D  Q:(POPEVENT'="")  ; Loop and pop if we have event that we need to bubble up
 . S X=$$READ^XGF()      ; R *X and process
 . ;
 . ; See if the event in XGRT (if it exists) exists in VFDPSJPP. Handle it if so.
 . ; .POPEVENT tells us if we have something to process
 . I $L(XGRT),$L($T(@XGRT^VFDPSJPP)) N XEC S XEC=XGRT_"^VFDPSJPP(X,.POPEVENT)" D @XEC
 . ; W X,"  ",XGRT  ; for debugging
 . ;
 . ; Get out if...
 . I X="",XGRT="" D CLEAR^VFDPSJPP(X,.POPEVENT)  ; two escapes or one escape and a timeout.
 . Q:(POPEVENT'="")  ; Event! Quit and process
 . ;
 . ; Clear entered text and return to prompt
 . S ($X,DX)=$P(TEXTORIGIN,U),($Y,DY)=$P(TEXTORIGIN,U,2) ; Move over to the original question
 . X IOXY
 . W IOEDEOP  ; Erase from there to the end of the page to delete all the below it
 ;
 ; If we quit, remove escape processing
 D RESETKB^XGF
 ;
 ; Really pop for the event
 Q:(POPEVENT'="")
 ;
 ; No other event. Then out event is quit.
 K ^TMP("VFDPL-PT",$J)
 ;
 S POPEVENT="QUIT"
 QUIT
 ;
ST ; [Internal] Status Loop - this is the main loop that does patient orders
 ; PN,PSGP,PSGPLG,RB,TM,WDN
 ;
 ; Data structure for patient orders
 K ^TMP("VFDPL",$J)
 ;
 N CNT S CNT=0
 N ST S ST="A"
 ;
 D:($T(^XTMLOG)]"") INFO^XTMLOG("Array By Index")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN)))
 D:($T(^XTMLOG)]"") INFO^XTMLOG("Array By Patient Before Filing")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^PS(53.5,PSGPLG,1,PSGP)))
 ;
 S ^TMP("VFDPL",$J,"PSGPLG")=PSGPLG
 S ^TMP("VFDPL",$J,"PSGP")=PSGP
 ;
 ; Data collection loop for all patient drugs
 F  S ST=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST)) Q:"Z"[ST  D DRG
 ;
 ;
 D:($T(^XTMLOG)]"") INFO^XTMLOG("TMP Array before POPULATING")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^TMP("VFDPL",$J)))
 ;
 ; Patient has drugs. Capture quantity and NDC.
 D:$D(^TMP("VFDPL",$J)) UI^VFDPSJPM(TM,WDN,PN,PSGP)
 ;
 D:($T(^XTMLOG)]"") INFO^XTMLOG("TMP Array before FILING")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^TMP("VFDPL",$J)))
 ;
 D:($T(^XTMLOG)]"") INFO^XTMLOG("Array by Patient After Filing")
 D:($T(^XTMLOG)]"") SAVEARR^XTMLOG($NA(^PS(53.5,PSGPLG,1,PSGP)))
 QUIT
 ;
DRG ; [Internal] Order Loop
 ; PN,PSGORD,PSGP,PSGPLG,PSJJORD,RB,SCH,ST,TM,WDN
 N DRG S DRG=""
 F  S DRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG)) Q:DRG=""  D
 . S PSGORD=$P(DRG,"^",2)
 . S PSJJORD=+$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,0))
 . S SCH=$P($G(^PS(55,PSGP,5,PSJJORD,2)),"^")
 . D GD
 Q
 ;
GD ; [Internal] Grab Drug and put data into ^TMP("VFDPL",$J)
 ; CNT,DN,DRG,PDD,PDN,PN,PSGORD,PSGP,PSGPLG,PSJJORD,RB,ST,TM,WDN
 ; NB: DN(0)
 ; DN p1 = Drug Number in Order (1 multiple under order in 55)
 ; DN p2 = Doses needed
 ; DN p3 = Doses dispensed
 ; DN p4 = filed way flag
 ; DN 21600 = ndc^scan
 ;
 ; Store in ^TMP Array
 N DDRG S DDRG=""  ; Drug Shorthand entry
 F  S DDRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG,DDRG)) Q:DDRG=""  D
 .N DNNUM S DNNUM=+$P(DDRG,"^",2) ; DN Number; We separate that into a seprate variable so I won't get confused.
 .S DN=$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DNNUM,0)) ; DN = Dispense Node, way at the bottom of the tree.
 .I DN["DI" QUIT  ; Don't know what that does, but it's in the original code
 .;
 .S CNT=CNT+1
 .S PDD=$P(DN,"^",3),PDN=$P(DN,"^",2)   ; *** PDD = Doses actually dispensed; PDN = Doses needeed ***
 .N DR S DR=$P($G(^PS(55,PSGP,5,PSJJORD,1,+DN,0)),"^") ; Drug IEN
 .N NDC S NDC=$P($G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DNNUM,21600)),U) ; NDC
 .N DRGNM S DRGNM=$$ENDDN^PSGMI(DR)  ; DRGNM = Drug name
 .N DU S DU=$$GET1^DIQ(50,DR,14.5),DU=" ("_DU_") "  ; Drug Units
 .;
 .; PDD = Doses actually dispensed; PDN = Doses needeed
 .; Now store in ^TMP, plus create a poor man's index, for DIC.
 .S ^TMP("VFDPL",$J,CNT,0)=DR_U_DRGNM_U_DU
 .S ^TMP("VFDPL",$J,CNT,"DDRG")=DDRG
 .S ^TMP("VFDPL",$J,CNT,"DN")=DN
 .S ^TMP("VFDPL",$J,CNT,"DNNUM")=DNNUM
 .S ^TMP("VFDPL",$J,CNT,"PDD")=PDD,^("PDN")=PDN,^("LMT")=9999 ; LMT is limit
 .S ^TMP("VFDPL",$J,CNT,"ORD")=PSGORD
 .S ^TMP("VFDPL",$J,CNT,"NDC")=NDC
 .S ^TMP("VFDPL",$J,CNT,"ST")=ST
 .S ^TMP("VFDPL",$J,"B",DR,CNT)=""     ; Index by IEN
 .S ^TMP("VFDPL",$J,"C",DRGNM,CNT)=""  ; Index by Drug Name
 .;
 .; Onion peeling array. We kill that each time we process something until we have none left.
 .I (+PDN'=0) S ^TMP("VFDPL",$J,"D",CNT)=""  ; Our done array. If the needed isn't 0, we need to do capture it.
 .S ^TMP("VFDPL",$J,0)="PICK LIST ARRAY"_U_1.01_U_CNT_U_CNT  ; Make the zero node a pretend Fileman zero node so we can use ^DIC and ^DIR
 QUIT
