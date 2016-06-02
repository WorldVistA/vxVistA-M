VFDXPDPRA ; DSS/SMP - ZZLRK AUTOINSTALL ; 12/09/2013 11:00
 ;;3.1;DSS,INC VXVISTA SUPPORTED;;09 December 2013;Build 127
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ; ******************************************************************************************************************
 ; This program is used by the Patch Team to reset the AutoInstall fields in 21692 VFDXPD BUILD DESCRIPTION file.
 ; When invoked, the user is prompted for the name of an existing batch - "Select VFD PATCH GROUP NAME:"
 ; The program will execute when a valid VFD PATCH GROUP NAME is entered. i.e. "VAVISTA 2013.10" 
 ; If AutoInstall is off (0 or NULL), AutoInstall will be reset to on (1) unless:
 ;			a.  The PKG name is "PSN",
 ;			b.  The PRE-installation field = "YES" (2),
 ;			c.  The POST-installation field = "YES" (2)
 ; If AutoInstall is on (1), AutoInstall will be reset to off (0).
 ; This program produces a report that displays pertinent field values both before and after the "TOGGLE"
 ; Field values displayed are BUILD NAME, PRE, POST, and AUTOINSTALL
 ; ******************************************************************************************************************
 ;
TOGGLE ; VFDXPD Toggle AutoInstall													; Program Entry Points
 N PARM
 S PARM="TOGGLE" D START ; FLIP THE AUTOINSTALL SWITCH
 Q
 ;
OFF    ; TURN AUTOINSTALL OFF FOR ALL PATCHES IN THE BATCH 
 S PARM="OFF" D START
 Q
 ; 
ON     ; TURN AUTOINSTALL ON  FOR ALL PATCHES IN THE BATCH 
 S PARM="ON" D START
 Q
 ;   
VIEW   ; VIEW AUTOINSTALL SWITCHES FOR ALL PATCHES IN THE BATCH 
 S PARM="VIEW" D START
 Q
 ;   
START
 N I,J,X,Y,Z,BATCH,PID,VFDIORPT
 D KILL,PID(0,1,0) I PID>0 D VFDSORT("VFDIORPT",PID,1,,1),KILL
 Q
 ;
PID(A,B,C) ;
 S PID=$$PID^VFDXPD0(A,B,C)
 I PID>0 S PID(0)=$P(PID,U,2),PID=+PID
 Q
 ;
KILL K ^TMP("VFDXPD",$J)
 Q
 ;
VFDSORT(VFDVR,VFDVNUM,VFDPRINT,VFDARRAY,VFDADD,FILTER) ;
 ; Entry point to be used as an API call
 ; VFDVR is the return array
 ; VFDVNUM is the IEN of the build
 ; VFDPRINT is a boolean value to print
 ; VFDARRAY is an array that can hold patches to add to a batch
 ; VFDADD is a flag for adding/removing patches from the batch - defualt is true
 ; FILTER is a flag for filtering the results
 ; FILTER="I" removes all the non-KIDS installable patches and sub-builds w/in a multibuild
 ; VFDADD is a flag for adding/removing builds from a batch
 ;
 N I,ANS,ARRAY,BATCH,COUNT,DIR,DIRUT,DTOUT,NA
 S VFDVNUM=$G(VFDVNUM),VFDARRAY=$G(VFDARRAY,"VFDBATCH")
 ;
 I $G(VFDVR)="" Q
 D BATCH(VFDVNUM,VFDARRAY)
 I $D(BATCH("R",0)) D ERR(6) Q
 Q:'$D(BATCH("B"))
 S VFDPRINT=+$G(VFDPRINT) N:VFDPRINT ERR,RPT 
 ;K @VFDVR
 S COUNT=1,@VFDVR=0,NA=$NA(^TMP("VFDXPD",$J))
 K @NA,ARRAY
 S RPT=$$SEQTEST(.RPT)
 D GETBLD,SETUP
 I $G(ERR) D ERR(3,ERR(1)) K @VFDVR K:$G(FLAG)="" ERR,RPT Q
 D LIST Q:VFDERR
 I $G(FILTER)'="" D FILTER
 ;
 ; Autoinstall toggle below ************************************************************************************************************** ZZLRKAUTO
 N ARRAYPTR,ARRAYMAX
 ;
 IF PARM="VIEW" W !!,"AutoInstall Values --------------------------------------------------------",! ;    Write the "VIEW " Report Line
 IF PARM'="VIEW" W !!,"BEFORE the Toggle ---------------------------------------------------------",!	; Write the "BEFORE the Toggle" Report Line
 W !,"BUILD NAME",$C(9),"PRE",$C(9),"POST",$C(9),"AUTO"									; Add Column Header
 FOR ARRAYPTR=1:1 Q:'$D(VFDIORPT(ARRAYPTR))  D BEFOREPROC								; Step through the 21692 subset array - Toggle - Print line items 
 ;Q
 ;
 N I,ANS,ARRAY,BATCH,COUNT,DIR,DIRUT,DTOUT,NA
 S VFDVNUM=$G(VFDVNUM),VFDARRAY=$G(VFDARRAY,"VFDBATCH")
 ;
 I $G(VFDVR)="" Q
 D BATCH(VFDVNUM,VFDARRAY)
 I $D(BATCH("R",0)) D ERR(6) Q
 Q:'$D(BATCH("B"))
 S VFDPRINT=+$G(VFDPRINT) N:VFDPRINT ERR,RPT K @VFDVR
 S COUNT=1,@VFDVR=0,NA=$NA(^TMP("VFDXPD",$J))
 K @NA,ARRAY
 S RPT=$$SEQTEST(.RPT)
 D GETBLD,SETUP
 I $G(ERR) D ERR(3,ERR(1)) K @VFDVR K:$G(FLAG)="" ERR,RPT Q
 D LIST Q:VFDERR
 I $G(FILTER)'="" D FILTER
 IF PARM="VIEW" Q
 ;
 N ARRAYPTR,ARRAYMAX
 ;                                                                                  	; "AFTER the Toggle" Report Line
 W !!,"AFTER the Toggle ----------------------------------------------------------",!	; Add Report Header
 W !,"BUILD NAME",$C(9),"PRE",$C(9),"POST",$C(9),"AUTO",$C(9),"EXCEPTIONS"				; Add Column Header
 FOR ARRAYPTR=1:1 Q:'$D(VFDIORPT(ARRAYPTR))  D AFTERPROC								; Print the line items with AutoInstall values after the Toggle 
 W !!
 ; Autoinstall toggle above ************************************************************************************************************** ZZLRKAUTO
 E  K:'$G(FLAG) RPT
 K VFDBATCH,@NA
 Q
 ;
BATCH(PID,ARRAY,FLAG) ;
 ; if $G(FLAG) set BATCH("E",IEN)= patch subject
 ; BATCH(21692_IEN)=
 ;  PIECE   FIELD              FIELD NUM
 ;    1      NAME                 .01
 ;    2      SEQ                  .07
 ;    3      PRE                  .11
 ;    4      POST                 .12
 ;    5      PKG                  .05
 ;    6      VER                  .06
 ;    7      PATCH #              .061
 ;    8      AUTO-INST            .18
 ;    9      STATUS               .1
 ;   10      MB INST ORDER        .16
 ;   11      IN MULTI             .15
 ;   12      DATE RELEASED        .04
 ;
 N X,ANS,CNT,FLDS,IEN,LIST,OUT,ZZ
 S PID=$G(PID),ARRAY=$G(ARRAY),CNT=1
 S FLDS=".01;.04;.05;.06;.061;.07;.1;.11;.12;.15;.16;.18;.9"
 S LIST=$$GETS^VFDXPDA("LIST",21692.1,PID,"1*","I") I LIST=1 D
 .S X=0 F  S X=$O(LIST(21692.11,X)) Q:'X  S BATCH(+X)=""
 I ARRAY'="",$D(@ARRAY) M BATCH=@ARRAY
 ;
 ; clean up the batch a bit and
 ; get info/setup cross references
 S X="" F  S X=$O(BATCH(X)) Q:'X  D
 .N OUT S IEN=X_",",OUT=$$GETS^VFDXPDA("OUT",21692,IEN,FLDS,"I")
 .I +OUT=-1 K BATCH(X) Q
 .D BATCH1(X,.OUT)
 .S BATCH=CNT,CNT=CNT+1 
 ;.W !," BATCH = ",BATCH	;DEBUG - FIND OUT WHAT THIS IS
 Q
 ;
 ;
BATCH1(X,OUT) ;
 N AUTO,DTREL,INMULT,MBORDER,NM,OUT2,PKG,PNUM,POST,PRE,SEQ,STAT,SUB,VER
 N ZZ S ZZ=$NA(OUT(21692,X_","))
 F  S ZZ=$Q(@ZZ) Q:ZZ=""  S OUT2($QS(ZZ,3))=@ZZ
 ;
 S AUTO=OUT2(.18),DTREL=OUT2(.04),INMULT=OUT2(.15)
 S MBORDER=OUT2(.16),NM=OUT2(.01),PKG=OUT2(.05)
 S PNUM=OUT2(.061),POST=OUT2(.12),PRE=OUT2(.11)
 S SEQ=OUT2(.07),STAT=OUT2(.1),VER=OUT2(.06),SUB=OUT2(.9)
 ;
 ;           1    2     3      4     5     6      7     8
 S BATCH(X)=NM_U_SEQ_U_PRE_U_POST_U_PKG_U_VER_U_PNUM_U_AUTO
 ;                       9       10       11      12
 S BATCH(X)=BATCH(X)_U_STAT_U_MBORDER_U_INMULT_U_DTREL
 S BATCH("B",NM)=X,BATCH("C",PKG,+VER,+SEQ)=X
 S BATCH("D",PKG,+VER,+PNUM)=X
 S DTREL=$S(DTREL:DTREL,1:0),BATCH("R",DTREL,NM)=X
 S:$G(FLAG) BATCH("E",X)=SUB
 Q
 ;
 ;
GETBLD ; get list of all Builds in a batch group
 N I,VFDERR,VFDI,TEMP
 S (VFDI,VFDERR)=0
 F  S VFDI=$O(BATCH(VFDI)) Q:'VFDI  D
 .Q:'$P(BATCH(VFDI),U,9)
 .I $P(BATCH(VFDI),U,9)=3 Q:$P(BATCH(VFDI),U,4)'=2
 .;S ARRAY(VFDI)=0,^TMP("VFDXPD",$J,0,$P(BATCH(VFDI),U))=VFDI
 .S ARRAY(VFDI)=0,^TMP("VFDXPD",$J,0,$P(BATCH(VFDI),U,12),VFDI)=VFDI
 Q
 ;
 ;
SETUP ;
 N VFDI
 S VFDI="" F  S VFDI=$O(ARRAY(VFDI)) Q:VFDI=""  D
 ;W "VFDI = ",VFDI	;DEBUG - WRITE THIS TO FIND OUT WHAT IT IS
 .D FINDREQ(VFDI) Q:+$G(ERR)  D MULTI(VFDI),CHECKNUM(VFDI)
 D FIXMULT
 Q
 ;
 ;
FINDREQ(X) ; finds the required builds for the specified patch
 ; X is the IEN of the patch
 ; Checks to see if the required build is in the batch
 ; If the build is in the batch add the required build to the list
 ;   of required builds for X
 ;
 N I,J,TEMP,Y,CNT,REQ,REQERR
 S Y=$$GETS^VFDXPDA("REQ",21692,X_",","6*","I") Q:'$D(REQ)!(Y'=1)
 S Y=0 F  S Y=$O(REQ(21692.06,Y)) Q:'Y  S TEMP(+Y)=""
 S Y=0,CNT=1 F  S Y=$O(TEMP(Y)) Q:'Y  D
 .I $D(ARRAY(Y)) S $P(@NA@(1,X),U,CNT)=Y,CNT=CNT+1
 Q
 ;
 ;
MULTI(X) ; checks to see if the specified build is part of a multibuild
 ; X is the IEN of the patch
 ; Piece 15 of ^VFDV(21692,IEN,0) contains the IEN of the primary of the
 ;   multibuild
 ; Piece 15 is null if the patch is not part of a multibuild
 ; Sets ^TMP("VFDXPD",$J,2,PRIMARY)="IEN of Primary^IEN of 2nd patch^
 ;   IEN of 3rd patch..."
 ;
 N HEAD,PRIMARY
 S PRIMARY=$$PRIMARY(X)
 S HEAD=$S(PRIMARY:X,1:$P(BATCH(X),U,11))
 I HEAD'=""!PRIMARY D
 .I '$D(ARRAY(HEAD)) D  Q
 ..D:'$D(@NA@("RPT",HEAD)) ERR(1,$$GETNAME(HEAD))
 ..S @NA@("RPT",HEAD)=""
 .S $P(@NA@(2,HEAD),U)=HEAD
 .I X'=HEAD D
 ..S @NA@(5,HEAD,$P(BATCH(X),U))=X
 ..S @NA@(4,X)=HEAD
 .I @NA@(2,HEAD)'[X S @NA@(2,HEAD)=@NA@(2,HEAD)_U_X
 Q
 ;
 ;
PRIMARY(X) ;
 ; checks to see if a build is the head of a multibuild
 ;  Piece    Field
 ;    9      STATUS
 ;   10      MB INST ORDER
 ;   11      IN MULTI
 N Y S Y=BATCH(X)
 Q:$P(Y,U,9)'=2 0
 I $P(Y,U,11)=X!($P(Y,U,10)=1) Q 1
 I $P(Y,U,10)=""&($P(Y,U,11)="") Q 1
 Q 0
 ;
 ;
CHECKNUM(X) ;
 ; checks to see if the specified build has builds with the same name,
 ;   version #, and a lesser seq #
 N Y,Z,IEN,PKG,SEQ,VER
 S PKG=$P(BATCH(X),U,5),VER=+$P(BATCH(X),U,6)
 D VERCHECK(X)
 S SEQ=+$P(BATCH(X),U,2)
 S Z=$NA(BATCH("C",PKG,VER))
 F  S SEQ=$O(@Z@(SEQ),-1) Q:'SEQ  D
 .S IEN=@Z@(SEQ) Q:'$D(ARRAY(IEN))
 .I $D(@NA@(3,X)) S @NA@(3,X)=@NA@(3,X)_U_IEN
 .E  S @NA@(3,X)=IEN
 Q
 ;
 ;
VERCHECK(X) ;
 ; checks to see if there are patches within the same namespace, 
 ; from a previous version
 ; only called from CHECKNUM^VFDXPDG2
 ; PKG and VER must be defined
 N Y,FLAG,IEN,PRE,ZZ
 S ZZ=$NA(BATCH("D",PKG))
 Q:$O(@ZZ@(VER),-1)=""
 S PRE="" F  S PRE=$O(@ZZ@(PRE)) Q:+PRE=+VER  D
 .S FLAG=1
 .S Y="" F  S Y=$O(@ZZ@(PRE,Y)) Q:Y=""  D
 ..S IEN=@ZZ@(PRE,Y) Q:'$D(ARRAY(IEN))
 ..I $$GET1^DIQ(21692,IEN,.15,"I")=X S FLAG=0
 ..I $D(@NA@(3,X)) S @NA@(3,X)=@NA@(3,X)_U_IEN
 ..E  S @NA@(3,X)=IEN
 .K:'FLAG @NA@(3,X)
 Q
 ;
 ;
FIXMULT ;
 ; adds requirements and previous SEQ#'ed builds of patches w/in a 
 ; multibuild to the primary of that multibuild
 ;
 ; these requirements are added to the list of requirements for 
 ; the primary build, ensuring that they are added first
 ;
 N I,J,IEN,TEMP
 S IEN="" F  S IEN=$O(@NA@(2,IEN)) Q:IEN=""  D
 .F I=2:1:$L(@NA@(2,IEN),U) S TEMP=$P(@NA@(2,IEN),U,I) D
 ..Q:TEMP=""
 ..I $G(@NA@(1,TEMP))'="" D
 ...F J=1:1:$L(@NA@(1,TEMP),U) D FIXMULT1(IEN,1)
 ..I $G(@NA@(3,TEMP))'="" D
 ...F J=1:1:$L(@NA@(3,TEMP),U) D FIXMULT1(IEN,3)
 Q
 ;
 ;
FIXMULT1(IEN,SUB) ;
 ; adds requirements and previous SEQ#'ed builds of patches w/in a 
 ; multibuild to the primary of that multibuild
 ; only called from FIXMULTI^VFDXPDG2
 ; SUB=1 deals with requirements
 ; SUB=3 deals with previous SEQ #
 I $G(@NA@(SUB,IEN))'="" D
 .I $P(@NA@(SUB,TEMP),U,J)'=IEN,@NA@(2,IEN)'[$P(@NA@(SUB,TEMP),U,J) D
 ..I @NA@(SUB,IEN)'[$P(@NA@(SUB,TEMP),U,J) D
 ...S @NA@(SUB,IEN)=@NA@(SUB,IEN)_U_$P(@NA@(SUB,TEMP),U,J)
 E  D
 .I $P(@NA@(SUB,TEMP),U,J)'=IEN,@NA@(2,IEN)'[$P(@NA@(SUB,TEMP),U,J) D
 ..S @NA@(SUB,IEN)=$P(@NA@(SUB,TEMP),U,J)
 Q
 ;
 ;
LIST ; creates the ordered list
 ; Orders through ^TMP("VFDXPD",$J,0) (which is sorted by name)
 ; Calls CHECK
 N IEN,CNT,CHECKED S VFDERR=0,CNT=1
 S IEN=$NA(^TMP("VFDXPD",$J,0))
 F  S IEN=$Q(@IEN) Q:$$LIST1(IEN)  D  Q:VFDERR
 .D:ARRAY(@IEN)'=1 CHECK(@IEN)
 Q
 ;
LIST1(X) ;
 Q $S(X="":1,$QS(X,3):1,$QS(X,2)'=$J:1,1:0)
 ;
CHKED(X,CNT) ; 
 N I,J,ERPT
 F I=2:1:10 D CHKED1(X,CNT,I) Q:VFDERR
 Q:'VFDERR
 F J=1:1:I S ERPT($$GETNAME(CHECKED(CNT-J+1)))=""
 S I="" F J=1:1 S I=$O(ERPT(I)) Q:I=""  S ERPT(J)=I K ERPT(I)
 S ERPT(.01)="" D ERR(5) K ^TMP("VFDXPD",$J)
 Q
 ;
 ;
CHKED1(X,CNT,GAP) ;
 N Y,Z,I,EQ
 F I=1:1:10 D  Q:VFDERR!('EQ)
 .S Y=$G(CHECKED(CNT-(GAP*I)))
 .S EQ=X=Y I I=10,EQ S VFDERR=1 D 
 ..S Y=$G(CHECKED(CNT-1))
 Q
 ;
 ;
CHECK(X) ; checks the pre-made arrays for the specified build
 ;         Checks to see if the specified patch has prev seq num patches,
 ;         requirements, or is part/the priamry of a multibuild
 N I,TEMP,PART Q:VFDERR
 S PART=0,CHECKED(CNT)=X,CNT=CNT+1
 D CHKED(X,CNT-1) Q:VFDERR
 ;
 ; PART=1 means that the patch is part of a multibuild (not primary)
 ;
 I $G(ARRAY(X)) Q
 ;W "CHECKING    ",$$GETNAME(X),!
 ; Checks for patchs with the same namespace and version number but 
 ; smaller sequence numbers
 ; Checks the value at ^TMP("VFDXPD",$J,3,X)
 ; Calls CHECK() on any IENs stored in ^TMP("VFDXPD",$J,3,X)
 I $D(@NA@(3,X)) D
 .S TEMP=@NA@(3,X)
 .F I=1:1:$L(TEMP,U) D
 ..I 'ARRAY(+$P(TEMP,U,I)) D CHECK(+$P(TEMP,U,I))
 Q:VFDERR
 ;
 ; Checks if the specified patch is part of a multibuild (not primary)
 ; Checks the value at ^TMP("VFDXPD",$J,4,X), the IEN of the primary
 ; Calls CHECK() on the primary of the multibuild
 I $G(@NA@(4,X))'="" D
 .S PART=1 D CHECK(@NA@(4,X))
 Q:VFDERR
 ;
 ; Checks for patches that are required for the specified patch
 ; Checks the value at ^TMP("VFDXPD",$J,1,X)
 ; Calls CHECK() on any IENs stored at ^TMP("VFDXPD",$J,1,X)
 I $D(@NA@(1,X)) D
 .S TEMP=@NA@(1,X)
 .F I=1:1:$L(TEMP,U) D
 ..I 'ARRAY($P(TEMP,U,I)) D CHECK($P(TEMP,U,I))
 Q:VFDERR
 ;
 ; Checks if the specified patch is the primary of a multibuild
 ; Checks the value at ^TMP("VFDXPD",$J,2,X), which is an ordered, 
 ; delimited string of the patches in this multibuild
 ; Adds the primary and all the sub-patches to the list in alpha order
 I $D(@NA@(2,X)) D
 .D ADD(X)
 .S TEMP=""
 .F  S TEMP=$O(@NA@(5,X,TEMP)) Q:TEMP=""  D ADD(@NA@(5,X,TEMP),1)
 Q:VFDERR
 ;
 ; Adds the specified patch to the list if after all checks have been 
 ; made and its not a sub-patch for a multibuild
 I 'PART D ADD(X)
 Q
 ;
 ;
ADD(X,PART) ; adds the patch to the IOR
 ; If the specified patch is not null and hasn't been added to the list 
 ; already (ARRAY(X)=0) add it to the list
 ; If the specified patch is designated as a PART, it is part of a 
 ; multibuild but not the primary add it to the list with >> in front 
 ; of the name (formatting)
 I $G(X),$G(ARRAY(X))=0 D
 .I $G(PART) S @VFDVR@(COUNT)=">>"_$$ADDPIECE(X)
 .E  S @VFDVR@(COUNT)=$$ADDPIECE(X)
 .S ARRAY(X)=1,COUNT=COUNT+1,@VFDVR=@VFDVR+1
 Q
 ;
 ;
ADDPIECE(X) ;
 N I,RET
 S RET=$P(BATCH(X),U,1,8)
 ; Pre- and Post-Install
 F I=3,4 S $P(RET,U,I)=$S($P(RET,U,I)=2:"YES",1:"")
 Q RET
 ;
 ;
GETNAME(X) ; converts from number to name
 ; returns the .01 field for a given IEN, the name
 Q $$GET1^VFDXPDA(21692,X,.01)
 ;
 ;
GETIEN(NM) ; converts from name to IEN
 Q:NM="" -1
 N I,OUT,IEN,FLAG
 S OUT=$$FIND^VFDXPDA("OUT",21692,,".01",,$E(NM,1,30),,,,)
 I OUT<1 Q -1
 F I=1:1:OUT S IEN=+OUT(I,0) I $P(OUT(I,0),U,2)=NM S FLAG=1
 Q:$G(FLAG) IEN
 Q -1
 ;
 ;
DIR(NUM) ;
 N I,X,Y,DIR,DIRUT,DTOUT,DUOUT,LINE,TEXT
 S LINE=$S(NUM=1:1,NUM=2:5,NUM=3:9,NUM=4:13,1:16)
 F I=0:1 S TEXT=$P($T(DIR+LINE+I),";;",2) Q:TEXT=""  D
 .S DIR($P(TEXT,";"))=$P(TEXT,";",2)
 Q:'$D(DIR(0)) -1
 S Y=$$DIR^VFDXPDA(.DIR)
 Q:Y="" -1
 Q Y
 ;
 ;
ERR(A,X) ; error messaging
 ;;Primary of a Multibuild is not in the batch:
 ;;There are missing SEQ # in the batch.  The following should be examined:
 ;;Patch requires itself.
 ;;Post Manual Patches:
 ;;There is an issue with the following patches:
 ;;The following patches do not have a release date:
 ;;
 N Y
 I '+$G(VFDPRINT)&('+$G(FLAG)) Q
 S A=$G(A),X=$G(X)
 I A S X=$TR($T(ERR+A),";"," ")_" "_X
 I A'=4 W !!!,"WARNING"
 W !!,X
 I A=2 W ! S Y="" F  S Y=$O(RPT(Y)) Q:'Y  W !?3,RPT(Y)
 I A=5 S Y="" F  S Y=$O(ERPT(Y)) Q:'Y  W !?3,ERPT(Y)
 I A=6 S Y="" F  S Y=$O(BATCH("R",0,Y)) Q:Y=""  W !?3,Y
 W !
 I A=2!(A=5)!(A=6) D CONT^VFDXPDA
 Q
 ;
 ;
SEQTEST(RTN) ; checks for missing SEQ #'s w/in a batch
 N I,X,T,FIRST,NA,LAST,PKG,TARR,VER,CNT S CNT=1
 K RTN S T=$C(9),NA=$NA(BATCH("C"))
 S PKG="" F  S PKG=$O(@NA@(PKG)) Q:PKG=""  D
 .S VER="" F  S VER=$O(@NA@(PKG,VER)) Q:VER=""  D
 ..S FIRST=$O(@NA@(PKG,VER,""))
 ..S LAST=$O(@NA@(PKG,VER,""),-1) Q:FIRST=LAST
 ..F I=FIRST:1:LAST D:'$D(@NA@(PKG,VER,I)) SEQTEST1(I)
 S:$D(RTN) RTN(.1)="Build Name"_T_"Seq#"
 Q $D(RTN(1))
 ;
SEQTEST1(SEQ) ;
 N INFO,IEN,NAME,STATUS,VAL,VFDERR
 S VAL(1)=PKG,VAL(2)=VER,VAL(3)=SEQ,VAL=1
 S IEN=$$FIND1^VFDXPDA(21692,,"X","AD",,.VAL)
 I 'IEN,'(VER#1) D
 .S VAL(2)=VER_".0",IEN=$$FIND1^VFDXPDA(21692,,"X","AD",,.VAL)
 Q:'IEN
 S IEN=IEN_",",INFO=$$GETS^VFDXPDA("INFO",21692,IEN,".01;.1","I")
 Q:'INFO
 S STATUS=INFO(21692,IEN,.1,"I") Q:'STATUS
 S NAME=INFO(21692,IEN,.01,"I")
 I STATUS'=3 S:$$QACHK^VFDXPDL(IEN) RTN(CNT)=NAME_T_SEQ,CNT=CNT+1
 Q
 ;
 W $$FIND1^DIC(21692,,X,.VAL,"AD",,"VFDERR")
 ;
TRANS(A,BEFORE,AFTER) ;
 N I S I=""
 F  S I=$O(A(I)) Q:I=""  S A(I)=$TR(A(I),BEFORE,AFTER)
 Q
 ;
FILTER ;
 N X,IEN,NAME,STAT
 I FILTER="I" D
 .S X="" F  S X=$O(@VFDVR@(X)) Q:X=""  D
 ..S NAME=$P(@VFDVR@(X),U) I NAME[">>" K @VFDVR@(X) Q
 ..S IEN=BATCH("B",NAME),STAT=$P(BATCH(IEN),U,9)
 ..I 'STAT!(STAT=3) K @VFDVR@(X) Q
 Q
 ;
 ; Autoinstall toggle below ************************************************************************************************************** ZZLRKAUTO
BEFOREPROC
 N IEN
 S IEN=BATCH("B",$P(VFDIORPT(ARRAYPTR),U,1)),STAT=$P(BATCH(IEN),U,9)				; Get the 21692 IEN for for the Build using "B" Index
 ;																					; Write the "BEFORE the Toggle" Report Line
 ;      |-------- BUILD --------|          |--------- PRE ---------|           |--------- POST --------|           |----- AUTOINSTALL -----|
 W !,$P(^VFDV(21692,IEN,0),"^",1),$C(9),$P(^VFDV(21692,IEN,0),"^",11),$C(9),$P(^VFDV(21692,IEN,0),"^",12),$C(9),$P(^VFDV(21692,IEN,0),"^",18)
 ;
 IF PARM="ON" D ALLON
 IF PARM="OFF" D ALLOFF
 IF PARM="TOGGLE" D FLIP
 Q
 ;
ALLON
 IF $P(VFDIORPT(ARRAYPTR),U,8)="" D AUTOON											; If it is off(NULL), turn it on (1)
 IF $P(VFDIORPT(ARRAYPTR),U,8)="0" D AUTOON											; If it is off(ZERO), turn it on (1)
 Q																					; Quit the sub-routine
 ;
ALLOFF
 IF $P(VFDIORPT(ARRAYPTR),U,8)="1" D AUTOOFF										; If it is on, turn it off (ZERO)
 Q																					; Quit the sub-routine
 ;
FLIP
 IF $P(VFDIORPT(ARRAYPTR),U,8)="1" D AUTOOFF										; If it is on, turn it off (ZERO)
 IF $P(VFDIORPT(ARRAYPTR),U,8)="" D AUTOON											; If it is off(NULL), turn it on (1)
 IF $P(VFDIORPT(ARRAYPTR),U,8)="0" D AUTOON											; If it is off(ZERO), turn it on (1)
 Q																					; Quit the sub-routine
 ;
AFTERPROC
 N IEN
 S IEN=BATCH("B",$P(VFDIORPT(ARRAYPTR),U,1)),STAT=$P(BATCH(IEN),U,9)				; Get the 21692 IEN for for the Build using "B" Index
 ;                                                                                  ; Write the "AFTER the Toggle" Report Line
 ;      |-------- BUILD --------|          |--------- PRE ---------|           |--------- POST --------|           |----- AUTOINSTALL -----|
 W !,$P(^VFDV(21692,IEN,0),"^",1),$C(9),$P(^VFDV(21692,IEN,0),"^",11),$C(9),$P(^VFDV(21692,IEN,0),"^",12),$C(9),$P(^VFDV(21692,IEN,0),"^",18)
 ;
 D EXCEPTIONS																		; Append Exception text to the report line for the current build
 K PARM
 Q																					; Quit the sub-routine
 ;
AUTOON 																			
 S $P(^VFDV(21692,IEN,0),"^",18)="1"												; Turn AutoInstall On for the current build except for the following 3 conditions
 IF $P(VFDIORPT(ARRAYPTR),U,5)="PSN" D AUTOOFF										; PSN Patches are never Autoinstallable (PKG="PSN")
 IF $P(VFDIORPT(ARRAYPTR),U,3)="YES" D AUTOOFF										; YES (2) IN PRE means not Autoinstallable
 IF $P(VFDIORPT(ARRAYPTR),U,4)="YES" D AUTOOFF										; YES (2) IN POST means not Autoinstallable
 Q																					; Quit the sub-routine
 ;
AUTOOFF 																			
 S $P(^VFDV(21692,IEN,0),"^",18)="0"												; Turn AutoInstall Off for the current build
 Q																					; Quit the sub-routine
 ;
EXCEPTIONS
 IF $P(VFDIORPT(ARRAYPTR),U,5)="PSN" W $C(9),"PKG=PSN",$C(9),": Off"				; PSN Patches are never Autoinstallable - append to report line
 IF $P(VFDIORPT(ARRAYPTR),U,3)="YES" W $C(9),"PRE=YES",$C(9),": Off"				; YES IN PRE means not Autoinstallable - append to report line
 IF $P(VFDIORPT(ARRAYPTR),U,4)="YES" W $C(9),"POST=YES",": Off"						; YES IN POST means not Autoinstallable - append to report line
 Q																					; Quit the sub-routine
 ; Autoinstall toggle above ************************************************************************************************************** ZZLRKAUTO
 ;
