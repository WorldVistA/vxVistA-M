VFDHHL03 ;DSS/PDW - EXTEND VFDHHLOT ;09 Mar 2011 13:31
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
EXIT ;
 K CC,COM,COMN,EC,ELMN,ESEG,LC,LE,LR,LS,RC,REPN,SUB,SUBN
 K POP,DTOUT,DIRUT,%ZIS
 Q
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 U IO W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  U IO W !,X,"=",@X
 Q
READMSG ;
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
 S XXX=$$MSGLOAD(.XXX) I XXX'>0 G READ2
 D ^%ZIS Q:$G(POP)
 ;generate display
 N VFDHSGNM,VFDHTSEG
 F I=1:1 Q:'$D(XXX(I))  S VFDHSGNM=$E(XXX(I),1,3),VFDHTSEG=XXX(I) D
 . N INT,XXX,I
 . U IO D SEGPRINT(VFDHSYS,VFDHTSEG)
 D ^%ZISC
 D E
 G READ2
 ;
MSGLOAD(XXX) ;$$ read in message , return array # LINES or XXX=-1
LOADREDO K XXX
 N VFDIOM,Y,I
 K DIR S DIR(0)="YO",DIR("A")="READY TO CONTINUE Y/N ",DIR("B")="Y"
 D ^DIR I Y'>0 S XXX=0 Q 0
 W !!,"Paste message in now. Complete loading with <CR> ",!!
 F I=1:1 R Y:40 Q:Y=""  W !! S XXX(I)=Y
 K DIR S DIR(0)="YO",DIR("B")="Y",DIR("A")="Loaded Properly" D ^DIR
 I Y'=1 G LOADREDO
 Q I-1
 ;
SEGPRINT(VFDHSYS,VFDHTSEG) ; ignore wether or not the variable exists, report location and value
 ;Displays atom items that have either have a value or a variables
 ; Creates an array XVAR("local variable name")=""
 ;S:'$D(SEG("FIELD SEPARATOR")) SEG("FIELD SEPARATOR")="|"
 S VFDHSEG("FIELD SEPARATOR")="|"
 N VOFF
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS Q
 K DEF N DEF,DTYP,LOC
 S VFDHSGNM=$P(VFDHTSEG,"|")
 S VOFF=1 I VFDHSGNM="MSH" S VOFF=0 ; adjust subscript offset
 D PARSE^VFDHHLOT(.VFDHSEG,VFDHTSEG,VOFF)
 ;++++++++++++++++++++++++++ NEW SEGPRINT
 ;++++++++++++++++++++++++++ NEW SEGPRINT
 ;move VFDHSEG(x) to VFDMSEG(x-1) to align with exchange locations
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
SEGV D SEGVBLD ;build variable locations VFDV(E,C,S)=DA
WALK ;
 ;walk VFDMSEG(E,R,C,S) print elements' values and variable definitions
 ; VFDLSTLC contains location last worked with
 ;D INTSET(.VFDMSEG) ; ADD IN HEADERS.
 K VPRT
 N VFDLSTLC S VFDLSTLC=0
 S E=0 F  S E=$O(VFDMSEG(E)) Q:E=""  D
 .S R=0 F  S R=$O(VFDMSEG(E,R)) Q:R'>0  D
 ..S C=0 F  S C=$O(VFDMSEG(E,R,C)) Q:C'>0  D
 ...;S S=0 F  S S=$O(VFDMSEG(E,R,C,S)) Q:S'>0  D WALKPRT
 ...S S=0 F  S S=$O(VFDMSEG(E,R,C,S)) Q:S'>0  D VPRINT(E,R,C,S,VFDMSEG(E,R,C,S))
 Q  ;;TEMP QUIT
 S VFDVLC=VFDLSTLC
 F  S VFDVLC=$O(VFDV(VFDVLC)) Q:VFDVLC'>0  D
 .S VFDITMDA=VFDV(VFDVLC)
 .;D ITMPRT(VFDITMDA,"")
 Q
SEGVBLD ;entry to build index array
 ;VFDVCIX("PID(""PTID"")",3)=""
 ;VFDVCIX("PID(""PTLNAME"")",49)=""
 ;VFDVCIX("PID(""PTMARSTAT"")",170)=""
 M VFDVCIX=^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"C")
 S VFDVNM="" F  S VFDVNM=$O(VFDVCIX(VFDVNM)) Q:VFDVNM=""  D VARVECS
 ; finished with data, look for and print remaining VFDV(location) items
 Q
VARVECS ;use C index to get variables
 ;return E,C,S and set VARV((E*100,000,000)+(C*10,000)+S)=VFDVDA
 ;VFDVCIX is the C index of variables
 ; a computed comparable index
 ;VFDV(500010001)=49
 ;VFDV(500010002)=50
 N E,C,S,VFDVDA,VFDVLOC,X
 S VFDVDA=$O(VFDVCIX(VFDVNM,0)),VFDVLOC=$P(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,VFDVDA,0),U)
 S X=VFDVLOC,E=+$P(X,".",1),C=+$P(X,".",2),S=+$P(X,".",3)
 S:C=0 C=1 S:S=0 S=1 S VFDV(E,1,C,S)=""
 I '$D(VFDMSEG(E,1,C,S)) S VFDMSEG(E,1,C,S)=""
 S VFDV((E*100000000)+(C*10000)+S)=VFDVDA ; looks like 10300230011
 Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
INTSET(INT) ;E,R,C,S ;put in headers into INT array
 N E,R
 S E=0  F  S E=$O(INT(E)) Q:E'>0  S R=0 F  S R=$O(INT(E,R)) Q:R'>0  D
 . I '$D(INT(E,R,1,1)) S INT(E,R,1,1)=""
 .D CLOOP
 Q
CLOOP ;
 N C
 S C=0 F  S C=$O(INT(E,R,C)) Q:C'>0  D
 . I '$D(INT(E,R,C,1)) S INT(E,R,C,1)=""
 Q
ITMDASET(E,R,C,S,VFDITDA) ;in HLO parse E.R.1.1 is equivalent to E.R.1.0 and E.R.0.0
 ;VFDMLOC="E.C.S.0" uses E,C,S
 ;return array of DAs and Locations print
 ;SISI-20 PID-93
 S:'$G(VFDHSYS) VFDHSYS=20 S:'$G(VFDSEGDA) VFDSEGDA=93
 K VFDITDA
 S VFDMLOC=E_"."_C_"."_S_".0"
 S E1=E,C1=C,S1=S,VFDMLOC1=VFDMLOC
 S (VFDITDA("SL"),VFDITDA("CL"),VFDITDA("EL"))=""
 S VFDITDA("S")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)) D
 . I VFDITDA("S") S VFDITDA("SL")=E_"."_R_"."_C_"."_S
 S VFDMLOC=E_"."_C_"."_"0.0",VFDITDA("C")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)) D
 . I VFDITDA("C") S VFDITDA("CL")=E_"."_R_"."_C_".0"
 S VFDMLOC=E_".0.0.0",VFDITDA("E")=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0)),VFDITDA("EL")=E_"."_R_".0.0"
  ;capture/set last Location printed
 Q
VPRINT(E,R,C,S,VFDVAL) ; given . . print unprinted headers and the value at the right place
 N VFDA
 D ITMDASET(E,R,C,S,.VFDA) ;gather headers
 D E1,C1,S1
 Q
E1 ;E,R,C,S,VFDVAL,VFDA
 N VFDE S VFDE=""
 Q:'VFDA("E")
 I 'VFDA("C") S VFDE=VFDVAL
 I '$D(VPRT(VFDA("EL"))) D ITMVPRT(VFDA("E"),VFDE,VFDA("EL"))
 S VPRT(VFDA("EL"))=VFDE ;store E as printed
 Q
C1 ;E,R,C,S,VFDVAL,VFDA
 N VFDC S VFDC=""
 Q:'VFDA("C")
 I 'VFDA("S") S VFDC=VFDVAL
 I '$D(VPRT(VFDA("CL"))) D ITMVPRT(VFDA("C"),VFDC,VFDA("CL"))
 S VPRT(VFDA("CL"))=VFDC ;store C as printed
 Q
S1 ;E,R,C,S,VFDVAL,VFDA
 N VFDS S VFDS=VFDVAL
 Q:'VFDA("S")
 I '$D(VPRT(VFDA("SL"))) D ITMVPRT(VFDA("S"),VFDS,VFDA("SL"))
 S VPRT(VFDA("SL"))=VFDS ;store S as printed
 Q
 ;
ITMVPRT(VFDITMDA,VFDVAL,VFDVMLOC) ;Print line (DEF("DA"),value,VPRT location)
 N VFDV0,VFDVIENS,VFDEFA,VFDEF,VFDMSG,VFDMPIEN,VFDMPNM,VFDMPWRK,VFDMPDAT
 ; test for item not in definitions
 I 'VFDITMDA D  Q
 .W !,VFDMLOC,?40,VFDVAL
 S VFDVIENS=VFDITMDA_","_VFDSEGDA_","_VFDHSYS_","
 ;.01-LOC;.02-DEF;.04-TYPE;.07-REP;.08-VAR; .05-REQ
 D GETS^DIQ(21625.0102,VFDVIENS,".01;.02;.04;.05;.07;.08;1.01",,"VFDEFA","VFDMSG")
 M VFDEF=VFDEFA(21625.0102,VFDVIENS)
 W !,VFDEF(.01) D REPS W ?10,$E(VFDEF(.02),1,19),?30,VFDEF(.04),?37,VFDVAL D
 . ;W ?49,$E(VFDEF(.07)),?77,$E(VFDEF(.05))
 . I '$G(VFDMAP) W ?49,$E(VFDEF(.07)),?53,$G(VFDEF(.08)),?77,$E(VFDEF(.05)) Q
 . S VFDMPIEN=$$GET1^DIQ(21625.0102,VFDVIENS,1.01,"I")_"," Q:'+VFDMPIEN
 . D GETS^DIQ(21625.02,VFDMPIEN_",",".01:.07",,"VFDMPDEF","VFDMSG")
 . M VFDMPDAT=VFDMPDEF(21625.02,VFDMPIEN) 
 . ;W " ",?50,VFDMPDAT(.06),?65,VFDMPDAT(.01)
 . W " ",?50,VFDMPDAT(.01)
 .;W ! ZW VFDMPDAT
REPS ;indicate repeats
 Q:R'>1
 I VFDVMLOC[".0.0" W "-",R
 Q
HISTORY ;
HST1 ;READMSG allow more than 245 characters
HST2 ;QUEST MAPPING OF SPEC TO SITE
HST3 ;15OCT2010 PARSE VFDHFLAG move INT(1,1,1,1) to INT(0,1,1,1)
HST4 ;10Dec2010 TAVALDA changed to 'B' index only
HST5 ;diagram of HL7 message corrected 4 Jan 11
 ;14 Jan 11 moved calls BUILD,TABLIST, TABVALDA to VFDHHL01
 ;15 APR 11 added required to segment print analysis
 ; 16 June 11 added VFDTFLD to TRANS,TABVALTR, also in VFDHHL01 ;
 ;    return the table column field entered
 ;OCT 24 2013 added DD Map to print if VFDMAP=1
