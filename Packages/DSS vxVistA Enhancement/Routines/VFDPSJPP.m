VFDPSJPP ;VEN/SMH - Picklist Patient Selection;12/23/2014
 ;;2013.2;DSS INC VXVISTA OPEN SOURCE;;;Build 11
 ;
UP(X,E) ; [Called only by VFDPSJPL] Up Event
 W CUROFF ; turn off cursor
 I $G(@R)'="" D  Q:PN=""  ; Previous cursor movement exists; Move up or wrap up (i.e we are at the bottom)
 . S PN=@R
 . S DX=@R@(PN,"X")
 . S DY=@R@(PN,"Y")
 . S TXT=@R@(PN,"TXT")
 . X IOXY
 . W $E(TXT,1,30)
 . S PN=$O(@R@(PN),-1)
 . I PN="" S PN=$O(@R@(""),-1)
 . I PN="" QUIT
 E  D  Q:PN=""
 . S PN=$O(@R@(""),-1)
 . Q:PN=""
 ;
 S @R=PN ; Now save the current patient
 ;
 ; Draw this patient
 S DX=@R@(PN,"X")
 S DY=@R@(PN,"Y")
 S TXT=@R@(PN,"TXT")
 X IOXY
 W IORVON_$E(TXT,1,29)_IORVOFF
 W CURON ; turn on cursor
 S DX=$P(TEXTORIGIN,U)
 S DY=$P(TEXTORIGIN,U,2)
 X IOXY
 QUIT
 ;
DOWN(X,E) ; [Called only by VFDPSJPL] Down Event
 W CUROFF ; turn off cursor
 I $G(@R)'="" D  Q:PN=""  ; Previous cursor movement exists
 . S PN=@R
 . S DX=@R@(PN,"X")
 . S DY=@R@(PN,"Y")
 . S TXT=@R@(PN,"TXT")
 . X IOXY
 . W $E(TXT,1,30)
 . S PN=$O(@R@(PN),1)
 . I PN="" S PN=$O(@R@(""),1)
 . I PN="" QUIT
 E  D  Q:PN=""
 . S PN=$O(@R@(""),1)
 . Q:PN=""
 S @R=PN
 ;
 S DX=@R@(PN,"X")
 S DY=@R@(PN,"Y")
 S TXT=@R@(PN,"TXT")
 X IOXY
 W IORVON_$E(TXT,1,29)_IORVOFF
 W CURON ; turn on cursor
 S DX=$P(TEXTORIGIN,U)
 S DY=$P(TEXTORIGIN,U,2)
 X IOXY
 QUIT
 ;
NEXT(X,E) ; Page Up Event
 S E="NEXT" ; Bubble up
 QUIT
 ;
PREV(X,E) ; Page down Event
 S E="PREV" ; Bubble up
 QUIT
 ;
CR(X,E) ; Enter pressed...
 ; TODO: Figure out how to pass WDN and TM
 I X="",$G(@R)]"" D  QUIT  ; Entry selected (previously) and nothing typed in X (plain enter)
 . S PN=$G(@R)
 . N PSGP S PSGP=$P(PN,U,2)  ; Patient IEN; See if we need that here--yes we do in CR below
 . S RB=$$RB()
 . D ST^VFDPSJPL
 . S E="REPAINT"
 I X'="" D  QUIT  ; Entry typed
 . I X="^" S E="QUIT" QUIT
 . I X?1.AP D  QUIT  ; PATIENT NAME TYPED, ? OR SPACE
 .. N Y
 .. N DIC
 .. S DIC="^PS(53.5,"_PSGPLG_",1,"
 .. S DIC(0)="EQVZ"
 .. S DIC("W")="D EN^DDIOL($P(^DPT(Y,0),U),,""?$X+2"") X ^DD(2,0,""ID"",.03) X ^DD(2,0,""ID"",.09)"
 .. S DIC("S")="I $P(^(0),U,3)="""_WDN_""""
 .. I $E(X)="?" D ^DIC N DIR S DIR(0)="E" D ^DIR QUIT
 .. D ^DIC I +Y<0 H .5 QUIT
 .. S PSGP=+Y,PN=$E($P(^DPT(PSGP,0),U),1,12)_U_PSGP
 .. S RB=$$RB()
 .. D ST^VFDPSJPL
 .. S E="REPAINT"
 . I X?.AN D  QUIT  ; Medical Record Number
 .. S PSGP=$$DFN^VFDDFN(X)
 .. I PSGP<0 W "  MRN NOT FOUND" H .5 QUIT
 .. I '$D(^PS(53.5,PSGPLG,1,"B",PSGP)) D  QUIT
 ... W !,$P(^DPT(PSGP,0),U)_" is not on this pick list (on this or any ward)!" N DIR S DIR(0)="E" D ^DIR QUIT
 .. I $P(^PS(53.5,PSGPLG,1,PSGP,0),U,3)'=WDN D  QUIT
 ... N NWDN S NWDN=$P(^PS(53.5,PSGPLG,1,PSGP,0),U,3)
 ... W !!,$P(^DPT(PSGP,0),U)_" is not on this ward but is on "_NWDN_".",!
 ... N DIR S DIR(0)="E" D ^DIR QUIT
 .. S PN=$E($P(^DPT(PSGP,0),U),1,12)_U_PSGP
 .. S RB=$$RB()
 .. D ST^VFDPSJPL
 .. S E="REPAINT"
 E  D
 . W ?50,IORVON_"--- WUFF WUFF ---"_IORVOFF HANG 0.1
 . ; S E="NOOP"
 QUIT
 ;
RB() ; [Internal] Get Room Bed for patients who are looked up
 Q $P(^PS(53.5,PSGPLG,1,PSGP,0),U,4)
 ;
CLEAR(X,E) ; Clear Selection if present or quit
 I '+$G(@R) S E="QUIT" QUIT
 W CUROFF
 S I=@R
 S DX=@R@(I,"X")
 S DY=@R@(I,"Y")
 S TXT=@R@(I,"TXT")
 X IOXY
 W $E(TXT,1,29)
 W CURON
 S DX=$P(TEXTORIGIN,U)
 S DY=$P(TEXTORIGIN,U,2)
 X IOXY
 S @R=""
 QUIT
 ;
PF1(X,E) ; F1 (Help) handling
 W @IOF
 K ^UTILITY($J,"W")
 N DN,DIWL,DIWR,DIWF ; (don't use DN... Fileman steps on that)
 S DIWL=3
 S DIWR=IOM-10
 N VFDI,X F VFDI=1:1 S X=$T(HELP+VFDI) Q:X'[";;"  S X=$P(X,";;",2,9) D ^DIWP
 D ^DIWW
 K ^UTILITY($J,"W")
 N DIR S DIR(0)="E" D ^DIR
 S E="REPAINT"
 QUIT
 ;
PF2(X,E) ; F2 handling to file the pick list
 ; Confirm first that we meet the count
 N TM,WDN S (TM,WDN)=""
 N CNT S CNT=0
 F  S TM=$O(^PS(53.5,"AC",PSGPLG,TM)) Q:TM=""  D
 . F  S WDN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN)) Q:WDN=""  D
 .. S CNT=CNT+$$WARDCNT(WDN)
 ;
 D ASSERT(CNT'<0)
 I CNT>0 D  N DIR S DIR(0)="E" D ^DIR QUIT
 . W !,"Can't file this Pick List away yet!",!
 . W "There are orders that have yet to be completed",!
 D FILE^PSGPLDP0
 S E="QUIT"
 QUIT
 ;
ASSERT(X) ; [Internal Assert]
 I 'X S $EC=",UASSERT,"
 QUIT
 ;
HELP ; F1 help text
 ;;This form helps you fill a pick list. It works by allowing you to
 ;;select a patient by typing or scanning and then allows you to fill
 ;;each medication the patient has.
 ;;
 ;;To operate this form, find the wards in the ward list that have orders.
 ;;To move between wards, use the Page Up and Page Down buttons.
 ;;
 ;;When you are at a ward that has orders, scan the patient's MRN, or
 ;;type the patient's last name, or you can even try a patient's DOB.
 ;;If you don't use the MRN, the software will ask you to confirm your
 ;;selection. Typing ? and ?? will display the list of patients.
 ;;
 ;;If you scan an invalid MRN, you are given one of 2 messages:
 ;; 1. The patient is not on this pick list.
 ;; 2. The patient is on this pick list, but not on this current ward.
 ;;
 ;;You can also scroll through patients using the Up and Down arrows.
 ;;Pressing enter when one of these is selected is the same as selecting
 ;;the patient.
 ;;
 ;;When you are ready to file the pick list, press F2.
 ;;This help is obtainable via F1.
 ;
WARDCNT(WDN) ; $$ Count of all unfilled orders for each ward
 N CNT S CNT=0
 N RB S RB="" ; Room bed
 N PN S PN="" ; Patient Name
 ;
 F  S RB=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB)) Q:RB=""  D
 . F  S PN=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN)) Q:PN=""  D
 .. S CNT=CNT+$$PATCNT(PN)
 QUIT CNT
 ;
PATCNT(PN) ; $$ Count of all unfilled orders by patient
 N CNT S CNT=0
 I $O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,""))="NO ORDERS" QUIT 0
 N ST S ST="A"
 F  S ST=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST)) Q:"Z"[ST  D
 . N DRG S DRG=""
 . F  S DRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG)) Q:DRG=""  D
 .. N PSGP S PSGP=$P(PN,U,2)
 .. N PSGORD S PSGORD=$P(DRG,"^",2)
 .. N PSJJORD S PSJJORD=+$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,0))
 .. N SCH S SCH=$P($G(^PS(55,PSGP,5,PSJJORD,2)),"^")
 .. N DDRG S DDRG=""  ; Drug Shorthand entry
 .. F  S DDRG=$O(^PS(53.5,"AC",PSGPLG,TM,WDN,RB,PN,ST,DRG,DDRG)) Q:DDRG=""  D
 ... N DNNUM S DNNUM=+$P(DDRG,"^",2) ; DN Number; We separate that into a seprate variable so I won't get confused.
 ... S DN=$G(^PS(53.5,PSGPLG,1,PSGP,1,PSGORD,1,DNNUM,0)) ; DN = Dispense Node, way at the bottom of the tree.
 ... ; I DN["DI" QUIT  ; Don't know what that does
 ... S PDD=$P(DN,"^",3),PDN=$P(DN,"^",2)   ; PDD = Doses actually dispensed; PDN = Doses needeed
 ... I PDN="WS"!(PDN="NV") QUIT   ; Ward Stock and Non-verified.
 ... I PDN<1 QUIT  ; < 1 quantity doesn't count towards the final number
 ... I PDD="" S CNT=CNT+1
 QUIT CNT
 ;
IOSETUP ; [Private] Set-up Screen Drawing stuff
 ; ZEXCEPT: CUROFF,CURON,IOM,IOSL ; Screen drawing vars
 S CUROFF=$C(13,27,91)_"?25l"_$C(13),CURON=$C(13,27,91)_"?25h"_$C(13)
 N X S X=$$IO() D ENDR^%ZISS
 S X=0 X ^%ZOSF("RM") ; Reset margins from Automargin to be limitless.
 N % S %=$$AUTOMARG()  ; Auto margin to find out our real margins.
 S IOM=$P(%,U) ; Put in IOM
 S IOSL=$P(%,U,2) ; Put in IOSL
 QUIT
 ;
IOKILL ; [Private] Kill everything
 ; ZEXCEPT: CUROFF,CURON ; ST vars for screen drawing
 K CUROFF,CURON
 N I,% F I=1:1:$L($$IO(),";") S %=$P($$IO(),";",I) K @% QUIT
 ;
IO() ; [Private] -- what device params
 Q "IORVON;IORVOFF;IOIL;IOSTBM;IOSC;IORC;IOEDEOP;IOINHI;IOINORM;IOUON;IOUOFF;IOBON;IOBOFF;IOSGR0"
 ;
AUTOMARG() ;RETURNS IOM^IOSL IF IT CAN and resets terminal to those dimensions; GT.M and Cache
 ; Stolen from George Timson's %ZIS3.
 ; ZEXCEPT: APC,TERM,WIDTH - these are not really variables
 N X S X=0 X ^%ZOSF("RM")
 N %I,%T,ESC,DIM S %I=$I,%T=$T D
 . ; resize terminal to match actual dimensions
 . S ESC=$C(27)
 . W ESC,"7",ESC,"[r",ESC,"[999;999H",ESC,"[6n"
 . I +$SY=0 U $P:(:"+S+I":"R") R DIM:1 E  Q
 . I +$SY=47 U $P:(TERM="R":NOECHO) R DIM:1 E  Q
 . W ESC,"8"
 . I +$SY=0 I DIM?.APC U $P:("") Q
 . I +$SY=47 I DIM?.APC U $P:(TERM="":ECHO) Q
 . S DIM=+$P(DIM,";",2)_"^"_+$P(DIM,"[",2)
 . I +$SY=0 U $P:(+DIM:"")
 . I +$SY=47 U $P:(TERM="":ECHO:WIDTH=+$P(DIM,";",2):LENGTH=+$P(DIM,"[",2))
 ; restore state
 U %I I %T
 Q:$Q $S(DIM:DIM,1:"") Q
 ; VEN/SMH - END MODS
