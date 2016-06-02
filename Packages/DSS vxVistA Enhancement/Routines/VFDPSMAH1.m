VFDPSMAH1 ;DSS/RJS - cont-VFD PSO MED ADMIN HISTORY RPC; 05/26/2015 16:10
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**54**;28 Jan 2013;Build 2
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This is a continuation of ^VFDPSMAH
 Q
 ;
AL ; ALLERGIES
 F DR=0:0 S DR=$O(GMRAL(DR)) Q:'DR  S ^TMP($J,"AL",$S($P(GMRAL(DR),U,4):1,1:2),$S('$P(GMRAL(DR),U,5):1,1:2),$P(GMRAL(DR),U,7),$P(GMRAL(DR),U,2))=""
 S IEN=0,PSOHDR("AL",0)="Allergies "
 S:$O(^TMP($J,"AL",1,1,""))]"" IEN=IEN+1,PSOHDR("AL",IEN,0)="    Verified: "
 S (DR,TY)="" F I=0:0 S TY=$O(^TMP($J,"AL",1,1,TY)) Q:TY=""  F D=0:0 S DR=$O(^TMP($J,"AL",1,1,TY,DR)) Q:DR=""  D
 .S:$L(PSOHDR("AL",IEN,0)_DR_", ")>80 IEN=IEN+1,$P(PSOHDR("AL",IEN,0)," ",14)=" " S PSOHDR("AL",IEN,0)=$G(PSOHDR("AL",IEN,0))_DR_", "
 S:$O(^TMP($J,"AL",2,1,""))]"" IEN=IEN+1,PSOHDR("AL",IEN,0)="Non-Verified: "
 S (DR,TY)="" F I=0:0 S TY=$O(^TMP($J,"AL",2,1,TY)) Q:TY=""  F D=0:0 S DR=$O(^TMP($J,"AL",2,1,TY,DR)) Q:DR=""  D
 .S:$L(PSOHDR("AL",IEN,0)_DR_", ")>80 IEN=IEN+1,$P(PSOHDR("AL",IEN,0)," ",14)=" " S PSOHDR("AL",IEN,0)=$G(PSOHDR("AL",IEN,0))_DR_", "
 S IEN=0,PSOHDR("AD",0)="Adverse Reactions "
 S:$O(^TMP($J,"AL",1,2,""))]"" IEN=IEN+1,PSOHDR("AD",IEN,0)="    Verified: "
 S (DR,TY)="" F I=0:0 S TY=$O(^TMP($J,"AL",1,2,TY)) Q:TY=""  F D=0:0 S DR=$O(^TMP($J,"AL",1,2,TY,DR)) Q:DR=""  D
 .S:$L(PSOHDR("AD",IEN,0)_DR_", ")>80 IEN=IEN+1,$P(PSOHDR("AD",IEN,0)," ",14)=" " S PSOHDR("AD",IEN,0)=$G(PSOHDR("AD",IEN,0))_DR_", "
 S:$O(^TMP($J,"AL",2,2,""))]"" IEN=IEN+1,PSOHDR("AD",IEN,0)="Non-Verified: "
 S (DR,TY)="" F I=0:0 S TY=$O(^TMP($J,"AL",2,2,TY)) Q:TY=""  F D=0:0 S DR=$O(^TMP($J,"AL",2,2,TY,DR)) Q:DR=""  D
 .S:$L(PSOHDR("AD",IEN,0)_DR_", ")>80 IEN=IEN+1,$P(PSOHDR("AD",IEN,0)," ",15)=" " S PSOHDR("AD",IEN,0)=$G(PSOHDR("AD",IEN,0))_DR_", "
 K TY,D,I,GMRA,GMRAL,DR,AD,ADL,^TMP($J,"AL")
 Q
 ;
PTOP(PG,TITLE,START,STOP) ;header for printouts
 ;PG=Page #
 ;TITLE=Report title
 ;START=Internal start date^External start date
 ;STOP=Internal stop date^External Stop date
 Q:'$G(PG)  Q:'$D(TITLE)
 N ORHLINE S $P(ORHLINE,"=",IOM+1)=""
 W !?(IOM-$L(ORTIT)\2),ORTIT,?(IOM-($L(PG)+5)),"Page "_PG
 W ! I $L($P(START,"^",2)) D  W ?(IOM-($L(X))/2),X
 . S X=$P(START,"^",2)_" thru "_$P(STOP,"^",2)
 I $G(VFDTODAY) S X="Today" W ?(IOM-($L(X))/2),X
 S X=$P($$HTE^XLFDT($H),":",1,2) W ?IOM-($L(X)+8),"Printed "_X
 Q
EN(PAT,GROUP,FLG,EXPAND,SDATE,EDATE,DETAIL,MULT,XREF,GETKID,EVENT) ;Get orders
 S SDATE=$P(^OR(100,VFDORD,0),U,8)
 S GROUP=$O(^ORD(100.98,"B","O RX",0))
 S FLG=2
 I $G(EDATE)<$G(SDATE) S X=EDATE,EDATE=SDATE,SDATE=X
 I $G(EDATE) S EDATE=$S($L(EDATE,".")=2:EDATE+.0001,1:EDATE+1)
 I $G(SDATE) S SDATE=$S($L(SDATE,".")=2:SDATE-.0001,1:SDATE)
 S SDATE=9999999-$S($G(SDATE):SDATE,FLG=6:DT-.7641,1:0),EDATE=9999999-$S($G(EDATE):EDATE,FLG=6:DT+.2359,1:9999998)
 S ORLST=0,X=EDATE,EDATE=SDATE,SDATE=X D GRP^ORQ1(GROUP),LIST^ORQ1,CUR
ENQ K ^TMP("ORGOTIT",$J)
 Q
CUR ; 2 -- Active/Current
 N X,X0,X1,X2,X3,X8,%H,YD,%,TM,ACTOR,NORX
 S TM=SDATE I $D(^OR(100,VFDORD,0)),$D(^(3)) S X0=^(0),X3=^(3) D
 . Q:'$D(ORGRP($P(X0,U,11)))  S ACTOR=0
 . F  S ACTOR=$O(^OR(100,VFDORD,8,ACTOR)) Q:ACTOR<1  S X8=^(0) D CUR1
 S ^TMP("ORR",$J,ORLIST,"TOT")=ORLST
 Q
CUR1 ; 2 -- secondary pass for Active/Current
 N STS,STOP S STOP=$P(X0,U,9)
 N EVNT S EVNT=$P(X0,U,17)
 Q:$P(X3,U,8)  Q:$P(X3,U,3)=99  S STS=$P(X3,U,3)
 I STS=13,EVNT,'$D(^ORE(100.2,EVNT,1)) Q  ;no cancelled orders linked to unreleased delay
 I $P(X8,U,4)=2,$P(X8,U,15)=11 G CURX ;incl all unsig/unrel actions
 I $G(NORX),NORX=$P(X0,U,11) Q  ;skip Rx for inpatients
CURX D GET^ORQ12(VFDORD,ORLIST,DETAIL,ACTOR)
 Q
HFSOPEN(HANDLE) ; 
 N VFDPSDIR,VFDPSFILE
 S VFDPSDIR=$$GET^XPAR("DIV","PSB HFS SCRATCH")
 S VFDPSFILE="VFDPS"_DUZ_".DAT"
 D OPEN^%ZISH(HANDLE,VFDPSDIR,VFDPSFILE,"W") Q:POP
 S IOM=80,IOSL=64,IOST="P-DUMMY",IOF=""""""
 Q
 ;
HFSCLOSE(HANDLE) ; 
 N VFDPSDIR,VFDPSFILE,VFDPSDEL
 D CLOSE^%ZISH(HANDLE)
 K ^TMP("VFDPS",$J)
 S VFDPSDIR=$$GET^XPAR("DIV","PSB HFS SCRATCH")
 S VFDPSFILE="VFDPS"_DUZ_".DAT",VFDPSDEL(VFDPSFILE)=""
 S X=$$FTG^%ZISH(VFDPSDIR,VFDPSFILE,$NAME(^TMP("VFDPSMAH",$J,2)),3)
 S X=$$DEL^%ZISH(VFDPSDIR,$NA(VFDPSDEL))
 Q
HEAD(ORDFN,PAGE,TITLE,STATION) ;Print a patient header
 Q:'$G(ORDFN)
 N %,%H,%I,DISYS,ORAGE,ORDOB,ORHLINE,ORL,ORNP,ORPNM,ORPV,ORSEX,ORSSN,ORTS,ORWARD,VA,X,ORI
 S:'$L($G(TITLE)) TITLE="PATIENT REPORT"
 D PAT^ORPR03(ORDFN)
 D SITE^ORWRPP($G(STATION)) S IOSL=64
 W !,TITLE,?(IOM-$L("Page "_PAGE)),"Page "_PAGE
 W !,$$VFD(ORPNM,ORSSN,ORDOB,ORAGE,ORSEX)
 S X=$G(ORL(0))_$S($L($G(ORL(1))):"/"_ORL(1),1:"") W:X'="" !,X
 S $P(ORHLINE,"=",IOM+1)=""
 W !,ORHLINE
 S X="Printed: "_$$DATE^ORU($$NOW^XLFDT,"MM/DD/CCYY HR:MIN")
 ;I '$$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !?27,"*** WORK COPY ONLY ***",?(IOM-($L(X))-1),X
 ;E  I $$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !,"** Accurate only through date/time printed in report ** ",X
 ;W !,ORHLINE
 Q
HURL(Y,ORDFN,TITLE,FORMAT,STATION,READ) ;Write out the file
 ;FORMAT tells me which node to go after
 N L,NOHURL,A,OUT
 S OUT=0,L="",NOHURL=0
 F  S L=$O(@Y@(L)) Q:L=""  Q:OUT  D
 . I $Y+4>IOSL D
 .. S PAGE=PAGE+1
 .. ;I '$$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !?27,"*** WORK COPY ONLY ***     (continued...)"
 .. ;E  I $$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !?5,"** Accurate only through date/time printed in report **  (continued...)"
 .. I $G(READ),$G(IOT)'["HFS" R !,"^ TO STOP: ",A:DTIME I A["^" S OUT=1 Q
 .. W @IOF
 .. I $G(IOT)["HFS" S $Y=0
 .. D HEAD(ORDFN,PAGE,$G(TITLE),$G(STATION))
 .. W !,"(...continued)"
 . I $G(FORMAT) D  Q
 .. Q:'$D(@Y@(L))
 .. I NOHURL,$P(@Y@(L),"^")'="[REPORT TEXT]" Q
 .. I NOHURL,$P(@Y@(L),"^")="[REPORT TEXT]" S NOHURL=0 Q
 .. I $P(@Y@(L),"^")="[HIDDEN TEXT]" S NOHURL=1 Q
 .. I @Y@(L)["**PAGE BREAK**" Q
 .. W !,@Y@(L)
 . Q:'$D(@Y@(L,0))
 . I NOHURL,$P(@Y@(L,0),"^")'="[REPORT TEXT]" Q
 . I NOHURL,$P(@Y@(L,0),"^")="[REPORT TEXT]" S NOHURL=0 Q
 . I $P(@Y@(L,0),"^")="[HIDDEN TEXT]" S NOHURL=1 Q
 . I @Y@(L,0)["**PAGE BREAK**" Q
 . W !,@Y@(L,0)
 ;I '$$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !?27,"*** WORK COPY ONLY ***"
 ;E  I $$GET^XPAR("ALL","VFD ORWRP REPORT WORDING") W !?5,"** Accurate only through date/time printed in report **"
 Q
 ;DSS/SGM - BEGIN MODS - extrinsic function for patient demographics
VFD(NAME,MRN,DOB,AGE,SEX) ;
 ; NAME - patient name
 ;  MRN - medical record number
 ;  DOB - human readable date of birth
 ;  AGE - patient's age
 ;  SEX - single character patient sex
 ;S X=ORDOB_" ("_ORAGE_")"
 ;W !,ORPNM_"   "_ORSSN,?39,$G(ORL(0))_$S($L($G(ORL(1))):"/"_ORL(1),1:""),?(79-$L(X)),X
 N X S NAME=$G(NAME),MRN=$G(MRN),DOB=$G(DOB),AGE=$G(AGE),SEX=$G(SEX)
 N X S X=$E($G(NAME),1,26),$E(X,29)="MRN: "_MRN,$E(X,45)="Sex: "_SEX
 S $E(X,53)="DOB" S:AGE X=X_" (Age)" S X=X_": "_DOB S:AGE X=X_" ("_AGE_")"
 Q X
