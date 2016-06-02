VFDHHLNW ;DSS/PDW ROUTINE TO HOUSE NEW CODE ;JUNE 22 2012
 ;;1.18;VENDOR - DOCUMENT STORAGE SYS;;20 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ;This routine houses new code until it is cooked in and
 ;can be placed into the normal routine function name spaces
 ;
 ; ONE2MANY called by TRANS^VFDHHLOT => TABVALTR^VFDHHL01
 ; pulls from a table multiple values from a table
 Q
ONE2MANY(VFDHSYS,DTYP,VAL,FMHL) ;$$ RETURN MANY OR -1^NOT FOUND
 ; Set FMHL = "M" to invoke One to Many.
 ; The return is xxx^yyy^zzz where
 ; VAL is the .01 and the Multiples to return are in field .03
 ;HL7 Value   Definition (by client)   EMR Value Mnemonic
 ;M120.0100   urine culture            L300.3120 URINE MACROSDOPIC
 ;M120.0100   urine culture            L300.3125 UA w/micro exam
 I FMHL'="M" Q "-1^NOT VALID"
 N IENS,HL7TAB,I,FIELD,NUMBER,IEN,IEN60,VFDSYSDA,VFDSYSNM,VFDTABDA,XX,IENS
 N VFDIENS
 S VFDSYSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDSYSDA,'$D(^VFDH(21625.01,+VFDHSYS)) Q "-1^SYSTEM NOT FOUND"
 I 'VFDSYSDA S VFDSYSDA=+VFDHSYS ;T15
 I 'VFDSYSDA Q "-1^SYSTEM NOT FOUND" ;T15
 S VFDHSYS=VFDSYSDA
 S VFDSYSNM=$$GET1^DIQ(21625.01,VFDHSYS,.01)
 ; find table
 N DA
 S DA(1)=VFDHSYS S IENS=$$IENS^DILF(.DA)
 N XX,INDX,FLD,XXX,DA,TABDA,YYY,X,SCR
 I DTYP=+DTYP S VFDTABDA=DTYP I 1
 E  S VFDTABDA=$$FIND1^DIC(21625.0105,IENS,"XM",DTYP)
 S XX="-1^Table: "_DTYP_" VALUE: "_VAL_"Not Found"
 I VFDTABDA'>0 Q XX
 ;check for executible code
 ;executable code will need to use X, VFDHSYS, TAB, FMHL in the local partition
 S DA(1)=VFDTABDA,DA(2)=VFDHSYS S VFDIENS=$$IENS^DILF(.DA)
 N VFDFILE,VFDIS,VFDFLDS,VFDFLAGS,VFDVAL,VFDNUM,VFDIDX,VFDSCR,VFDID,VFDTAR,VFDMSG
 S (VFDFILE,VFDIS,VFDFLDS,VFDFLAGS,VFDVAL,VFDNUM,VFDIDX,VFDSCR,VFDID,VFDTAR,VFDMSG)=""
 S VFDFLAGS="BOPX",VFDIS=VFDIENS
 S VFDVAL=VAL,VFDFLDS=".01;.03",VFDIDX="D"
 S VFDFILE=21625.0106
 K VFDTAR,VFDMSG
 D FIND^DIC(VFDFILE,VFDIS,VFDFLDS,VFDFLAGS,VFDVAL,VFDNUM,VFDIDX,VFDSCR,VFDID,"VFDTAR","VFDMSG")
 Q:'$D(VFDTAR("DILIST")) XX
 S XX="" F I=1:1 Q:'$D(VFDTAR("DILIST",I))  S XX=XX_$P(VFDTAR("DILIST",I,0),"^",2)_"^"
 Q XX
 ;
SEGDEFN(VFDHSYS,VFDHSGNM,DEF) ;build Segment ELEMENT definitions into DEF(IEN,FLD)=Value
 ;.01=Location,.02=Definition,.04=DataType,.08=variable
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 N DA,IENS,ELM,IEN,ADX,ORD,V,X,DEFDA
 S DA(1)=VFDHSYS,DA=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA
 S IENS=$$IENS^DILF(.DA)
 ; MOVE return ELM(FILE,IENS,FLD)  to ELM(+IENS,FLD)=value
 K ELM D GETS^DIQ(21625.0101,IENS,"*","","ELM","ER")
 S IENS=""  F  S IENS=$O(ELM(21625.0102,IENS)) Q:IENS=""  M DEFDA(+IENS)=ELM(21625.0102,IENS) ;restore by DA
 ;generate reorder array and reoder values by V(ord)=IEN
 M ADX=^VFDH(21625.01,DA(1),10,DA,10,"AD") ;grab ordered index
 S X="ADX" F I=1:1 S X=$Q(@X) Q:X=""  S ORD(I)=+$P(X,",",2) ;create order by location
 S ORD=0 F  S ORD=$O(ORD(ORD)) Q:ORD'>0  M DEF(ORD)=DEFDA(ORD(ORD)) ;reorder by V(ord)=ien
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  D
 .S:DEF(IEN,.02)="" DEF(IEN,.02)="missing ITEM name "_IEN
 .S DEF("B",DEF(IEN,.01),IEN)="",DEF("NM",DEF(IEN,.02),IEN)=""
 Q
SEGDEFP(VFDHSYS,VFDHSGNM,DEF) ;build Segment ELEMENT definitions into DEF(IEN,FLD)=Value
 ;.01=Location,.02=Definition,.04=DataType,.08=variable
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 N DA,IENS,ELM,IEN,ADX,ORD,V,X,DEFDA
 N VFDFILE,VFDIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,VFDMSG
 S (VFDFILE,VFDIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,VFDMSG)=""
 S DA(1)=VFDHSYS,DA=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA
 S VFDIENS=$$IENS^DILF(.DA)
 ; MOVE return ELM(FILE,IENS,FLD)  to ELM(+IENS,FLD)=value
 ;K ELM ;D GETS^DIQ(21625.0101,IENS,"**","","ELM","ER")
 ;Set needed variables
 S VFDFILE=21625.0102,VFDFLDS="@;*",VFDSCR="I $L($P(^(0),U,8))"
 K VFDTAR,VFDMSG
 D LIST^DIC(VFDFILE,VFDIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,"VFDTAR","VFDMSG")
 S IENS=""  F  S IENS=$O(ELM(21625.0102,IENS)) Q:IENS=""  M DEFDA(+IENS)=ELM(21625.0102,IENS) ;restore by DA
 ;generate reorder array and reoder values by V(ord)=IEN
 M ADX=^VFDH(21625.01,DA(1),10,DA,10,"AD") ;grab ordered index
 S X="ADX" F I=1:1 S X=$Q(@X) Q:X=""  S ORD(I)=+$P(X,",",2) ;create order by location
 S ORD=0 F  S ORD=$O(ORD(ORD)) Q:ORD'>0  M DEF(ORD)=DEFDA(ORD(ORD)) ;reorder by V(ord)=ien
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  D
 .S:DEF(IEN,.02)="" DEF(IEN,.02)="missing ITEM name "_IEN
 .S DEF("B",DEF(IEN,.01),IEN)="",DEF("NM",DEF(IEN,.02),IEN)=""
 Q
SELTRAN() ;EP SELECT TRANSPORT
 N DIC,Y
 S DIC=21625.01,DIC(0)="AQEML"
 D ^DIC
 Q +Y
 ;
SELTAB(X)          ;EP SELECT TABLE GIVEN SYSTEM
 N DA,DIC,Y,IENS
 S DA(1)=X,IENS=$$IENS^DILF(.DA)
 W !,"Transport: ",$$GET1^DIQ(21625.01,X,.01)
 S DIC=$$ROOT^DILFD(21625.0105,IENS),DIC(0)="AEQML",DIC("W")="W ?25,$P(^(0),U,2),?55,$P($G(^(11)),U,1)"
 D ^DIC
 Q +Y
SELROW(X,X1) ;EP SELECT TABLE ROW GIVEN SYSTEM & TABLE
 N DA,DIC,Y,IENS
 S DA=X1,DA(1)=X,IENS=$$IENS^DILF(.DA)
 W @IOF,!,"Transport: ",$$GET1^DIQ(21625.01,X,.01)
 W !,"Table: ",?12,$$GET1^DIQ(21625.0105,IENS,.01)
 S IENS=","_IENS
 S DIC=$$ROOT^DILFD(21625.0106,IENS),DIC(0)="AEQML",DIC("W")="W ?40,$P(^(0),U,2)"
 K DA S DA(2)=X,DA(1)=X1
 D ^DIC
 Q +Y
EDTABROW ;EP EDIT TABLE ROW
 N X1,X2,X3,DIC,DR,DIE,DIR,DA
 F  S X1=$$SELTRAN() Q:X1'>0  D
 . F  S X2=$$SELTAB(X1) Q:X2'>0  D
 .. D ROWTABED(X1,X2)
 .. Q
 ;D EXIT
 Q
ROWTABED(VFDHSYS,VFDHTAB) ;pass in system and table to edit a table row
 N SYSDA,TABDA,FLDS,DR,DA,DIC,DIE,TABDA,ROWDA,SYSDA,LINE,FLD,CC,CVALS
 I VFDHSYS'=+VFDHSYS S SYSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS) S VFDHSYS=SYSDA
 I 'VFDHSYS Q
 S SYSDA=VFDHSYS
 K DA
 S DA(1)=SYSDA S IENS=$$IENS^DILF(.DA)
 S TABDA=VFDHTAB
 I TABDA'=+VFDHTAB S TABDA=$$FIND1^DIC(21625.0105,IENS,"XM",VFDHTAB)
 I TABDA'>0 Q
 S ROWDA=$$SELROW(SYSDA,TABDA) Q:ROWDA'>0
 I '$D(LINE) S $P(LINE,"=",79)=""
 K DA
 S DA=TABDA,DA(1)=SYSDA S IENS=$$IENS^DILF(.DA)
 S CC=$$GET1^DIQ(21625.0105,IENS,.06)
 K DA
 S DA=ROWDA,DA(1)=TABDA,DA(2)=SYSDA S IENS=$$IENS^DILF(.DA)
 S (DR,FLDS)=".01;.02;.03;.04;.05"
 I $L(CC) W !,LINE,!,"Alternate Item Names:",!,CC
 D ROWDISP
LOOPED K DIC,DIE S DIE=$$ROOT^DILFD(21625.0106,IENS)
 S (DR,FLDS)=".01;.02;.03;.04;.05" D ^DIE
 I $L(CC) W !,LINE,!,"Alternate Item Names:",!,CC
 D ROWDISP
 K DIR S DIR(0)="YO",DIR("A")=" Is the above correct ? Y/N " D ^DIR
 I +Y=1 Q
 I X="^" Q
 G LOOPED
ROWDISP ;
 K FLDS
 I '$D(LINE) S $P(LINE,"=",79)=""
 W !,LINE
 K DA
 S DA=ROWDA,DA(1)=TABDA,DA(2)=SYSDA S IENS=$$IENS^DILF(.DA)
 S FLDS=DR
 K CVALS
 D GETS^DIQ(21625.0106,IENS,FLDS,"","CVALS","MSG")
 K VCUR
 ;display blanks if entry deleted
 I $D(CVALS) M VCUR=CVALS(21625.0106,IENS) I 1
 E  N I F I=1:1 S FLD=$P(FLDS,";",I) Q:FLD'>0  S VCUR(FLD)=""
 S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 . D FIELD^DID(21625.0106,FLD,"","LABEL","NM")
 . W !,NM("LABEL"),?30,$G(VCUR(FLD))
 W !,LINE
 Q
MSGDISP ;
 ; ask exchange (loop)
 ; read in message
 ; ask printer (loop) (to allow browser)
 W @IOF,!
READ2  W !,"1) Select Client System",!,"2) Load Message",!
 W !,"End message input with <cr>",!
SYS ;select system
 K DIC
 S DIC=21625.01,DIC(0)="AEQMZ" D ^DIC I Y'>0 Q
 S VFDHSYS=+Y
 S XXX=$$MSGLOAD^VFDHHL03(.XXX) I XXX'>0 G READ2
 D ^%ZIS Q:$G(POP)
 ;generate display
 N VFDHSGNM,VFDHTSEG
 F I=1:1 Q:'$D(XXX(I))  S VFDHSGNM=$E(XXX(I),1,3),VFDHTSEG=XXX(I) D
 . N INT,XXX,I
 . U IO D SEGPRINT(VFDHSYS,VFDHTSEG)
 D ^%ZISC
 D E
 G READ2 ;
ZIS D ^%ZIS
 N VFDHSGNM,VFDHTSEG
 F I=1:1 Q:'$D(XXX(I))  S VFDHSGNM=$E(XXX(I),1,3),VFDHTSEG=XXX(I) D
 . N INT,XXX,I
 . U IO D SEGPRINT(VFDHSYS,VFDHTSEG)
 D ^%ZISC
 D E
 Q
 ;
SEGPRINT(VFDHSYS,VFDHTSEG) ; ignore wether or not the variable exists, report location and value
 ;Displays atom items that have either have a value or a variables
 ; Creates an array XVAR("local variable name")=""
 ;S:'$D(SEG("FIELD SEPARATOR")) SEG("FIELD SEPARATOR")="|"
 S VFDHSEG("FIELD SEPARATOR")="|"
 N VOFF,E,DA
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS Q
 K DEF N DEF,DTYP,LOC
 S VFDHSGNM=$P(VFDHTSEG,"|")
 S VOFF=1 I VFDHSGNM="MSH" S VOFF=0 ; adjust subscript offset
 D PARSE^VFDHHLOT(.VFDHSEG,VFDHTSEG,VOFF)
 ;++++++++++++++++++++++++++ NEW SEGPRINT no variables
 ;move VFDHSEG(x) to VFDMSEG(x) for working data
 ;N VFDMSEG
 K VFDMSEG,VPRT
 W !!,VFDHTSEG,!
 S E=0 F  S E=$O(VFDHSEG(E)) Q:E'>0  M VFDMSEG(E)=VFDHSEG(E)
 ;locat SEG in HL7 Exchange
 N DA,IENS,ELM,IEN,ADX,ORD,V,X,DEFDA
 S DA(1)=VFDHSYS,(VFDSEGDA,DA)=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA
 S VFDMIENS=$$IENS^DILF(.DA)
 ;N VFDVCIX,VFDVDA,VFDVLOC,VFDV
 K VFDVCIX,VFDVDA,VFDVLOC,VFDV
 ;build VARV(E,S,C)=DA for segment defined partition variables
 ; SEGV BLOCKED so s not to print lines for Variables pdw/ may 6,2013
SEGV ;D SEGVBLD ;build variable locations VFDV(E,C,S)=DA
WALK ;
 ;walk VFDMSEG(E,R,C,S) print elements' values and definitions
 ; VFDLSTLC contains location last worked with
 ;D INTSET(.VFDMSEG) ; ADD IN HEADERS.
 K VPRT
 N VFDLSTLC S VFDLSTLC=0
 S E=0 F  S E=$O(VFDMSEG(E)) Q:E=""  D
 .S R=0 F  S R=$O(VFDMSEG(E,R)) Q:R'>0  D
 ..S C=0 F  S C=$O(VFDMSEG(E,R,C)) Q:C'>0  D
 ...S S=0 F  S S=$O(VFDMSEG(E,R,C,S)) Q:S'>0  D VPRINT(E,R,C,S,VFDMSEG(E,R,C,S))
 Q  ;; QUIT - previous code removed
VPRINT(E,R,C,S,VFDVAL) ; given . . print unprinted headers and the value at the right place
 N VFDA
 D ITMDASET(E,R,C,S,.VFDA) ;gather headers definition into VFDA
 D E1(E,R,C,S),C1(E,R,C,S),S1(E,R,C,S)
 Q
ITMDASET(E,R,C,S,VFDITDA) ;in HLO parse E.R.1.1 is equivalent to E.R.1.0 and E.R.0.0
 ;VFDMLOC="E.C.S.0" uses E,C,S
 ;ORC|||||||^^^^^R||||||||||||||^^^^^^^^^2144
 ;for each E,R,C,S in the parsed segment, pull the E.0 E.C.0 E.C.S.0
 ; definitions for printing the trail to the specific E.R.C.S segment value 
 ; R is not used in getting definitions
 ;VFDA("C")=27                  HL7 IEN
 ;VFDA("CL")="7.1.6.0"          HL7 location (no R)
 ;VFDA("E")=19                  HL7 IEN
 ;VFDA("EL")="7.1.0.0"          HL7 location (no R)
 ;VFDA("S")=""
 ;VFDA("SL")=""
 K VFDITDA
 S VFDMLOC=E_"."_C_"."_S_".0"
 S E1=E,C1=C,S1=S,VFDMLOC1=VFDMLOC
 S (VFDITDA("SL"),VFDITDA("CL"),VFDITDA("EL"))=0
 S VFDITDA("S")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)) D
 . I VFDITDA("S") S VFDITDA("SL")=E_"."_R_"."_C_"."_S
 S VFDMLOC=E_"."_C_"."_"0.0",VFDITDA("C")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)) D
 . I VFDITDA("C") S VFDITDA("CL")=E_"."_R_"."_C_".0"
 S VFDMLOC=E_".0.0.0",VFDITDA("E")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)),VFDITDA("EL")=E_"."_R_".0.0"
  ;capture/set last Location printed
 Q
E1(E,R,C,S) ;E,R,C,S,VFDVAL,VFDA
 ;ORC|||||||^^^^^R||||||||||||||^^^^^^^^^2144
 ; it is tricky handling the 2144. It does not have an HL7X definition (*1)
 N VFDE S VFDE=""
 ;Q:'VFDA("E")
 I 'VFDA("E"),E=1 Q
 I 'VFDA("C"),C=1 S VFDE=VFDVAL ;(*1) singular element as in MHS.3
 ;test if undefined address to print, make "CL"
 I E>1,'$G(VFDA("EL")) S VFDA("EL")="1.0.0.0",$P(VFDA("EL"),".",1)=E
 I '$D(VPRT(VFDA("EL"))) D ITMVPRT(VFDA("E"),VFDE,VFDA("EL"))
 S VPRT(VFDA("EL"))=VFDE ;store E as printed
 Q
C1(E,R,C,S) ;E,R,C,S,VFDVAL,VFDA
 N VFDC S VFDC=""
 ;Q:'VFDA("C")
 I 'VFDA("C"),C=1 Q
 I 'VFDA("S"),S=1 S VFDC=VFDVAL
 ;test if undefined definition to print, make VFDA("CL")
 I C>1,'$G(VFDA("CL")) S VFDA("CL")=VFDA("EL"),$P(VFDA("CL"),".",2)=C
 I '$D(VPRT(VFDA("CL"))) D ITMVPRT(VFDA("C"),VFDC,VFDA("CL"))
 S VPRT(VFDA("CL"))=VFDC ;store C as printed
 Q
S1(E,R,C,S) ;E,R,C,S,VFDVAL,VFDA
 N VFDS S VFDS=VFDVAL
 Q:'VFDA("S")
 ;test if undefined definition to print, make VFDA("SL")
 I S>1,$G(VFDA("SL")) S VFDA("SL")=VFDA("CL"),$P(VFDA("SL"),".",3)=S
 I '$D(VPRT(VFDA("SL"))) D ITMVPRT(VFDA("S"),VFDS,VFDA("SL"))
 S VPRT(VFDA("SL"))=VFDS ;store S as printed
 Q
 ;
ITMVPRT(VFDITMDA,VFDVAL,VFDVMLOC) ;Print line (DEF("DA"),value,VPRT location)
 N VFDV0,VFDVIENS,VFDEFA,VFDEF,VFDMSG
 ; test for item not in definitions
 I 'VFDITMDA D  Q
 .W !,VFDVMLOC,?10,">> Unknown <<",?37,VFDVAL
 S VFDVIENS=VFDITMDA_","_VFDSEGDA_","_VFDHSYS_","
 ;.01-LOC;.02-DEF;.04-TYPE;.07-REP;.08-VAR; .05-REQ
 D GETS^DIQ(21625.0102,VFDVIENS,".01;.02;.04;.05;.07;.08",,"VFDEFA","VFDMSG")
 M VFDEF=VFDEFA(21625.0102,VFDVIENS)
 W !,VFDEF(.01) D REPS W ?10,$E(VFDEF(.02),1,19),?30,VFDEF(.04),?37,VFDVAL D
 . W ?49,$E(VFDEF(.07)),?77,$E(VFDEF(.05))
 .; W ?49,$E(VFDEF(.07)),?53,$G(VFDEF(.08)),?77,$E(VFDEF(.05))
 ;. W ?49,$E(VFDEF(.07)),?55,VFDEF(.08),?77,$E(VFDEF(.05))
 Q
REPS ;indicate repeats
 Q:R'>1
 I VFDVMLOC[".0.0" W "-",R
 Q
 ;
SEND ; 
 W !,"Select how to send the message."
 K DIR S DIR(0)="SO^P:Protocol;L:Link;T:TCP/IP;H:HFS TCP"
 D ^DIR
 Q:'$L(Y)
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) Q
 I Y="P" D PROTSEND G SEND
 I Y="L" D LINKSEND G SEND
 I Y="T" D TCPSEND G SEND
 I Y="H" D MSG2TCP^VFDHHLUT G SEND
 G SEND
 ;
TCPSEND ; set HL7ADD,HL7PORT
 K DIR S DIR(0)="FO",DIR("A")="ENTER TCP Address"
 S HL7ADD="127.0.0.1",DIC("B")=HL7ADD
 S:$D(^TMP($J,"HL7ADD")) DIR("B")=^TMP($J,"HL7ADD")
 D ^DIR
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) K ^TMP($J,"HL7ADD")  Q
 I $L(Y)>2 S HL7ADD=Y,^TMP($J,"HL7ADD")=Y
 ;I '$D(^TMP($J,"HL7PORT")) D ^%SS
 K DIR S DIR(0)="N0",DIR("A")="Enter HL7 port "
 S:$D(^TMP($J,"HL7PORT")) DIR("B")=^TMP($J,"HL7PORT")
 D ^DIR
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT))!(+Y'>0) K ^TMP($J,"HL7ADD") Q
 I +Y S HL7PORT=+Y,^TMP($J,"HL7PORT")=+Y I 1
 S X=$$MSGLOAD^VFDHHLOT(.XXX)
 I $D(XXX)'>9 W !!,"No Message Loaded",!! Q
 D SENDTCP(HL7ADD,HL7PORT,.XXX)
 Q
PROTSEND ; Copy/Paste/Send message with HL7 vxvista protocol
 W !!,"Select a Protocol and paste in a message.",!
 W !,"The MSH will be reconstructed.",!
 ;
 N DIC,Y,XXX,LN,XXXB,HLA,SEG,VFDWP,END,YYY,VFD101DA,VFD101NM,VFDPRONM
 N END,VFDMSH,I,LN,SEG,VFDWP,HLA
 K DIC
 W !!,?5,"Select the Server Protocol to be used. "
 K DIC
 ; event driver
 I $D(^TMP($J,"VFD101NM")) S DIC("B")=^TMP($J,"VFD101NM")
 S DIC=101,DIC(0)="AEQMZ",DIC("S")="I $P(^(0),U,4)=""E"""
 D ^DIC
 I Y'>0 Q
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) Q
 S VFD101DA=+Y,VFD101NM=Y(0,0)
 S ^TMP($J,"VFD101NM")=VFD101NM
 S X=$$MSGLOAD^VFDHHL03(.XXX)
 I X'>0 Q
 ;remove MSH , reconstructed by HL7 protocol
 S VFDMSH=XXX(1)
 K XXX(1)
 S END=$O(XXX("A"),-1)
 F I=2:1:END S YYY(I-1)=XXX(I)
 K XXX M XXX=YYY K YYY
 ;split lines as needed for HL16
 S LN=0 F  S LN=$O(XXX(LN)) Q:LN'>0  S SEG=XXX(LN) D
 .K VFDWP
 .D SPLIT16^VFDHHLOT(.VFDWP,SEG,230)
 .M YYY(LN)=VFDWP(1)
 K XXX M XXX=YYY
 M HLA("HLS")=XXX
 N VFD101SC,VFD870DA,DA
 W !,VFD101DA,!
 S DIC="^ORD(101,",DA=VFD101DA D EN^DIQ
 S (VFD101M,VFD101SC)=0
 S VFD101M=$O(^ORD(101,VFD101DA,775,0))
 S:VFD101M VFD101SC=+^ORD(101,VFD101DA,775,VFD101M,0)
 W !,VFD101SC,!
 I VFD101SC S DA=VFD101SC,DIC="^ORD(101," D EN^DIQ
 S VFD870DA=$$GET1^DIQ(101,VFD101SC,770.7,"I")
 W !,VFD870DA,!
 I VFD870DA S DA=VFD870DA,DIC="^HLCS(870," D EN^DIQ
 W !!,$G(VFDMSH)
 W !,XXX(1),!
 K DIR S DIR(0)="YO",DIR("A")="Is the Above Correct?",DIR("B")="Y"
 D ^DIR
 I Y'>0 Q
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) Q
PROTHL7 ; generate HL message
PROTQU ;queue to Protocol send
 ;S ZTRTN="PROTDEQ^VFDHHLNW",ZTDESC="HL send HLA to protocol"
 ;S ZTSAVE("HLS*")="",ZTSAVE("VFD101DA")="",ZTIO="",ZTDTH=$H 
 ;D ^%ZTLOAD W !!,"Tasked With Job: ",$G(ZTSK),!
 ;G PROTSEND
 ;Q
PROTDEQ ; send HLA into the protocol
 ;S ZTQUE="@"
 D INIT^HLFNC2(VFD101DA,.HL)
 ;build HLA("HLS",I)
 K HLA("HLS",0)
 W ! ZW HLA
 D GENERATE^HLMA(VFD101DA,"LM",1,.VFDRSLT)
 W !,"RESULT",?10,VFDRSLT
 Q
LINKSEND ;Select link and send to IP and Port
 N VFDLNKNM,VFDLNKDA,VFD2IP,VFD2PORT,DIC,VFD2IP,VFD2PORT,XXX
 K DIC
 S DIC=870,DIC(0)="AEQMZ"
 I $D(^TMP("VFDHLINK",$J)) S DIC("B")=^TMP("VFDHLINK",$J)
 D ^DIC
 I Y'>0 Q
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) Q
 S VFDLNKNM=Y(0,0),^TMP("VFDHLINK",$J)=VFDLNKNM
 S VFDLNKDA=+Y
 S VFD2IP=$$GET1^DIQ(870,VFDLNKDA,400.01)
 S VFD2PORT=$$GET1^DIQ(870,VFDLNKDA,400.02)
 S Y=$$MSGLOAD^VFDHHL03(.XXX)
 I $D(XXX)'>9 W !,"No Message Loaded" Q
 D SENDTCP(VFD2IP,VFD2PORT,.XXX)
 Q
SENDTCP(HL7ADD,HL7PORT,HL7TEXT) ; Send into TCP
 W:$G(VFDLNKNM) !,"Link",?10,"VFDLNKNM"
 W !,"HL7 IP",?10,HL7ADD
 W !,"HL7 PORT",?10,HL7PORT
 S $P(XXX(1),"|",10)=$$NOW^XLFDT
 W !,"First Line",!,?3,$G(XXX(1)),!
 K DIR S DIR(0)="YO",DIR("A")="Is the Above Correct?",DIR("B")="N"
 D ^DIR
 I Y'=1 Q
 I ($D(DUOUT)!$D(DIRUT)!$D(DTOUT)) Q 
 S FORMAT=1,WAIT=2,MAXLINES=600
 D CALL^%ZISTCP(HL7ADD,HL7PORT,10)
 I POP W !,?20,">>>> TCP/IP connect failed <<<<" Q
 N TCPIO S TCPIO=IO
 ; Next is based on WRITE^HLCSTCP2
 N LINENO,X S LINENO=0
 ;F  U FILEIO Q:$$STATUS^%ZISH!(MAXLINES&(LINENO>MAXLINES))  R X:5 Q:'$T  D
 F  S LINENO=$O(HL7TEXT(LINENO)) Q:LINENO'>0  S X=HL7TEXT(LINENO)  D
 . Q:MAXLINES&(LINENO>MAXLINES)
 .S:LINENO=1 X=$C(11)_X
 .U TCPIO
 .I FORMAT=1 W:X]"" X_$C(13),!
 .;I FORMAT=2 W X,! W:X="" $C(13),!
 .Q
 U TCPIO W $C(28,13),! ;End of message
 ;F I=1:1:4 U TCPIO R X:2 I $L(X) W X S I=0
 H $G(WAIT,0) D CLOSE^%ZISTCP
 W !!,"SENT TO: ",HL7ADD,":",HL7PORT,!,$G(VFDMSH),!!
SENDTCPQ Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
MSGBREAK ;
 ZB SENDTCP
 ZB PROTSEND
 ZB LINKSEND
 ZB TCPSEND
 ZB SENDTCP
 Q
DSPBRKPT ;
 ZB ITMDASET
 ZB ITMVPRT
 ZB VPRINT
 Q
HISTORY ;
 ;; FEB 2 2014  MSH.10 set to $$NOW^XLFDT
