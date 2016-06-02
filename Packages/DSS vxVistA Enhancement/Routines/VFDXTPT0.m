VFDXTPT0 ;DSS/SMP - Pointer Tool ;02/27/2014 09:45
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; Orginal writer was Llyod, RAC adding the following:
 ;    1.  Prompt to dispplay IEN
 ;    2.  Change from an ignore list to search ALL or by file range
 ;    3.  Added IEN count 
 ;    4.  Look for a specific Entry IEN.
 ;    5.  Changed report display.
 ;
EN ;[Public]
 ;
 N X,Y,Z,DELIM,RET,FILE,RPT,ROOT,FILELST,ALL,NA,START
 ;S START=$$NOW^XLFDT
 W !,"This option classifies actual pointers-in to a file."
 W !,"It produces a cross-listing of entries in the original file,"
 W !,"and entries or sub-entries that point to the given entries.",!
 S RET=$$ASKFILE Q:RET<1  S FILE=+RET,FILE("NM")=$P(RET,U,2)
 S NA=$NA(^TMP($J,"VFDXTPT0")),RPT=$NA(@NA@("RPT")) K @NA
 S ROOT=$$ROOT^DILFD(FILE,,1) Q:'$$FILTER(FILE,ROOT)
 I '$D(@NA@("LST")) D  Q
 .W !!,"You must select at least one entry!",!
 D FILELST("FILELST") Q:'$D(FILELST)
 D ADELIM
 W !,"This report should be queued."
 ;
 ;DEVICE PROMPT
DEV ;
 N %ZIS,POP
 W !
 S %ZIS="Q",%ZIS("A")="(queuing recommended) Select DEVICE: ",%ZIS("B")=""
 D ^%ZIS Q:POP
 I $G(IO("Q")) N ZTDESC,ZTRTN,ZTSAVE,ZTSK D  Q  ;If queued
 .S ZTDESC="VFD Pointers Analysis",ZTSAVE("*")="",ZTRTN="DQ^VFDXTPT0"
 .D ^%ZTLOAD K IO("Q") D HOME^%ZIS
 .Q
 ; Fall through to DQ, if not queued
 D WAIT^DICD W !
 ;
 ;
DQ ; 
 S START=$$NOW^XLFDT
 D PTRLIST(.FILELST,ALL)
 S X=0 F  S X=$O(@NA@(X)) Q:'X  D FIND(FILE,X)
 I DELIM D DELIM(FILE,START)
 E  D RPT(FILE,START)
 K @NA
 Q
 ;
 ;------------------------ Private Subroutines ------------------------
 ;
ASKFILE() ; Ask pointed to file
 N X,Y,DIC,DTOUT,DUOUT
 S DIC=1,DIC(0)="AEQMZ" D ^DIC Q:Y<0 -1
 I $D(DTOUT)!$D(DUOUT) Q -1
 Q Y
 ;
ADELIM ; Ask delimited format
 W !,"Print in delimited format?"
 N DIRUT,DTOUT,DUOUT
 S DIR(0)="Y",DIR("B")="No" D ^DIR
 Q:$D(DTOUT)!$D(DUOUT)!$D(DIRUT)
 S DELIM=Y
 Q
 ;
FILTER(FILE,ROOT) ; Filter entries in the pointed to file
 N X,Y,Z,DIR,DIC,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="S^A:All Entries;S:Specific Entries"
 S DIR("A")="Search for specific entries or all entries"
 D ^DIR K DIR W !! I $D(DIRUT) Q:0
 I X="A" D  Q 1
 .S X=0 F  S X=$O(@ROOT@(X)) Q:'X  D
 ..S @NA@("LST",X)=$$GET1^DIQ(FILE,X,.01)
 I X="S" D  Q '$D(DTOUT)
 .S DIC=FILE,DIC(0)="AEOQ"
 .F  D ^DIC Q:Y=-1  S @NA@("LST",+Y)=$P(Y,U,2) K X,Y W !!
 Q 1
 ;
FIND(OGFILE,FILE) ;
 ; OGFILE = File being pointed to
 ; FILE   = File doing the pointing
 N X,Y,Z,REF
 S REF=@NA@(FILE,0)_"0)"
 F Z=1:1 S REF=$Q(@REF) Q:REF'[@NA@(FILE,0)  D
 .;W:Z#100=0 "." Q:'$D(@NA@("L",FILE,$QL(REF)))
 .Q:'$D(@NA@("L",FILE,$QL(REF)))
 .D FIND1(FILE,REF,$P($$ROOT^DILFD(OGFILE),U,2))
 Q
 ;
FIND1(FILE,GREF,ROOT) ;
 N I,DD,FLD,FLAG
 S DD=0 F  S DD=$O(@NA@(FILE,DD)) Q:'DD  D  Q:$G(FLAG)
 .S FLD=0 F  S FLD=$O(@NA@(FILE,DD,FLD)) Q:'FLD  D  Q:$G(FLAG)
 ..Q:$QL(GREF)'=@NA@(FILE,DD,FLD,"L")
 ..S FLAG=$$MATCH(FILE,DD,FLD,GREF) Q:'FLAG  S X=$P(FLAG,U,2)
 ..Q:'$D(@NA@("LST",+X))
 ..I X=+X D ADD(FILE,DD,FLD,X,GREF,0) Q
 ..I $P(X,";",2)=ROOT D ADD(FILE,DD,FLD,+X,GREF,1) Q
 Q
 ;
ADD(FILE,DD,FLD,PTR,GREF,VAL) ;
 S @RPT@(PTR,FILE,DD,FLD,GREF)=""
 S @RPT@(PTR,FILE,DD,FLD,0)=1+$G(@RPT@(PTR,FILE,DD,FLD,0))
 I VAL S @RPT@(PTR,FILE,DD,FLD,0)=@RPT@(PTR,FILE,DD,FLD,0)_"[V]"
 Q
 ;
MATCH(FILE,DD,FLD,GREF) ;
 N I,X,REF,QL,QS
 S REF=$NA(@NA@(FILE,DD,FLD)),QL=@REF@("L"),FLAG=1
 S I=0 F  S I=$O(@REF@(I)) Q:'I  D  Q:'FLAG
 .I @REF@(I)'=$QS(GREF,I) S FLAG=0
 .E  S FLAG=1_U_$P(@GREF,U,@REF@("P"))
 Q FLAG
 ;
PTRLIST(LIST,ALL) ;
 N X,Y,Z
 I ALL K LIST
 S X=0 F  S X=$O(^DD(FILE,0,"PT",X)) Q:'X  D
 .I ALL!$D(LIST(X)) D
 ..K LIST(X) S Y=0 F  S Y=$O(^DD(FILE,0,"PT",X,Y)) Q:'Y  D
 ...S LIST(X,Y)=""
 S X=0 F  S X=$O(LIST(X)) Q:'X  D
 .S Y=0 F  S Y=$O(LIST(X,Y)) Q:'Y  D
 ..K RET D GROOT^VFDXTGBL(.RET,X,Y) Q:'$D(RET)
 ..M @NA@(RET("F"),X,Y)=RET
 ..S @NA@(RET("F"),0)=RET("F",0)
 ..S @NA@("L",RET("F"),RET("L"))=""
 Q
 ;
FILELST(LIST) ; Populate list of files or subfiles to ignore
 ; LIST=[Required] $NAME of return list
 S:'$L($G(LIST)) LIST=$NA(VFDLST) ;Default
 N DIRUT,DIROUT,DTOUT,DUOUT,DIR,Y,Y1,Y2 S DIR("A")="DD#",DIR(0)="LC^1:9999999999:9"
 S DIR("B")="999999999",ALL=0
 W !!,"Look in ALL or range (ex. 1-999) of files/sub-files. "
 W !,"If 999999999 is entered, ALL files will be selected."
 W !,"Enter file or subfile number, or press <ENTER> to quit.",!
 ;
DLP ;
 N J
 D ^DIR Q:$D(DIRUT)  W !!
 I Y[999999999 S @LIST@(999999999)="",ALL=1 Q
 I Y["," D
 . S J=1
 . F  S X=$P(Y,",",J) Q:X=""   D
 . . S:X["-" Y1=$P(X,"-",1)-.999,Y2=$P(X,"-",2)
 . . S:X'["-" Y1=X-.0009,Y2=X
 . . F  S Y1=$O(^DD(FILE,0,"PT",Y1)) Q:'Y1!(Y1>Y2)  S @LIST@(Y1)=""
 . . S J=J+1
 . . Q
 . Q
 W !
 Q
 ;
RPT(FILE,START) ; Print out report
 N I,X,DD,FLD,GR,PTR,RPT1,EQ
 S EQ=$$REPEAT^XLFSTR("-",79),START=$S($D(START):START,1:$$NOW^XLFDT)
 W !!!!,"Start: ",$$FMTE^XLFDT(START),?45,"SITE: ",$P($$SITE^VASITE,U,2),!!
 W "File# ",FILE,"  "_$$GET1^DIQ(1,FILE,.01),!,EQ,!
 S PTR=0 F  S PTR=$O(@RPT@(PTR)) Q:'PTR  D
 .W "Entry# "_PTR_"  "_@NA@("LST",PTR)
 .S FILE=0 F  S FILE=$O(@RPT@(PTR,FILE)) Q:'FILE  D
 ..S FILE(1)=$$GET1^DIQ(1,FILE,.01)_"(#"_FILE_")"
 ..S DD=0 F  S DD=$O(@RPT@(PTR,FILE,DD)) Q:'DD  D
 ...I FILE=DD W !,?2,FILE(1),!
 ...E  W !,?2,FILE(1)_";"_$P(^DD(DD,0),U)_"(#"_DD_")",!
 ...S FLD=0 F  S FLD=$O(@RPT@(PTR,FILE,DD,FLD)) Q:'FLD  D
 ....W ?15,$P($G(^DD(DD,FLD,0)),U)_"(#"_FLD_")"
 ....;W ?15,"Field# "_FLD_"  "_$P($G(^DD(DD,FLD,0)),U)
 ....W:@RPT@(PTR,FILE,DD,FLD,0)["[V]" "  [V]"
 ....W ?55,"Record Count:  "_@RPT@(PTR,FILE,DD,FLD,0),!
 .W EQ,!
 S X=$$NOW^XLFDT
 W !,"Completed:  ",$$FMTE^XLFDT(X),!
 S X=$$FMDIFF^XLFDT(X,START,3)
 I X="" W "Run Time:  0 seconds",!!
 E  W "Run Time:  ",X,!!
 Q
DELIM(FILE,START) ; Print deliminated report
 N I,X,DD,DDNR,DDREF,FLD,GR,PTR,RPT1,EQ
 S START=$S($D(START):START,1:$$NOW^XLFDT)
 W "Start:~",$$FMTE^XLFDT(START),"~","SITE:~",$P($$SITE^VASITE,U,2)
 W !,"File#~",FILE,"  "_$$GET1^DIQ(1,FILE,.01)
 W !,"IEN~Total Ptrs~DD Ref~DD Named Ref"
 S PTR=0 F  S PTR=$O(@RPT@(PTR)) Q:'PTR  D
 .S FILE=0 F  S FILE=$O(@RPT@(PTR,FILE)) Q:'FILE  D
 ..S FILE(1)=$$GET1^DIQ(1,FILE,.01)_"(#"_FILE_")"
 ..S DD=0 F  S DD=$O(@RPT@(PTR,FILE,DD)) Q:'DD  D
 ...N DDNR,DDREF
 ...S (DDNR,DDREF)=""
 ...I FILE=DD S DDREF=FILE,DDNR=FILE(1)
 ...E  S DDREF=FILE_":"_DD,DDNR=FILE(1)_":"_$P(^DD(DD,0),U)_"(#"_DD_")"
 ...S FLD=0 F  S FLD=$O(@RPT@(PTR,FILE,DD,FLD)) Q:'FLD  D
 ....S DDREF=DDREF_":"_FLD,DDNR=DDNR_":"_$P($G(^DD(DD,FLD,0)),U)_"(#"_FLD_")"
 ....S:@RPT@(PTR,FILE,DD,FLD,0)["[V]" DDREF=DDREF_" [V]",DDNR=DDNR_" [V]"
 ....W !,PTR_"~"_@RPT@(PTR,FILE,DD,FLD,0)_"~"_DDREF_"~"_DDNR
 S X=$$NOW^XLFDT
 W !,"Completed:~",$$FMTE^XLFDT(X),!
 S X=$$FMDIFF^XLFDT(X,START,3)
 I X="" W "Run Time:~0 seconds",!!
 E  W "Run Time:~",X,!!
 Q
