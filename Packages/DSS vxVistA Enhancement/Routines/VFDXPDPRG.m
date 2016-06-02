VFDXPDPRG ; DSS/SMP - GUI ROUTINES for a Batch ; 08/13/2014 08:47
 ;;3.1;DSS,INC VXVISTA SUPPORTED;;13 August 2013;Build 127
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;PRINTS A TEXT REPORT LOB FOR THE GUI REPORT
 Q
LIST 
 ; print a list of builds in a batch along with any missing SEQ#'s, regardless of status
 ; build name ^ seq ^ stat ^ patch subject ^ multibuild header
 ;      1        2     3          4                 5
 N X,Y,Z,BATCH,LIST,PID,RPT,T
 D PID^VFDXPD(0,1,0) W !! Q:PID<0  S T=$C(9)
 D BATCH^VFDXPDG2(PID,,1),LIST^VFDXPDL,RPT^VFDXPD0(.LIST)
 S RPT=$$SEQTEST(PID) D:RPT ERR(1),RPT^VFDXPD0(.RPT,,) W !
 Q
 ;
 ;;==================================================
 ;;           U S E D   W I T H   L I S T
 ;;================================================== 
SEQTEST(PID) 
 N I,X,FIRST,IEN,INFO,LAST,PKG,SEQ,STATUS,TEMP,VER
 M TEMP=BATCH("C") K BATCH M BATCH=TEMP K TEMP
 S PKG="" F  S PKG=$O(BATCH(PKG)) Q:PKG=""  D
 .S VER="" F  S VER=$O(BATCH(PKG,VER)) Q:VER=""  D
 ..S FIRST=$O(BATCH(PKG,VER,""))
 ..S LAST=$O(BATCH(PKG,VER,""),-1)
 ..Q:FIRST=LAST
 ..K TEMP F I=FIRST:1:LAST S TEMP(I)=""
 ..S SEQ="" F  S SEQ=$O(BATCH(PKG,VER,SEQ)) Q:SEQ=""  K TEMP(SEQ)
 ..S SEQ="" F  S SEQ=$O(TEMP(SEQ)) Q:SEQ=""  D
 ...S IEN=$O(^VFDV(21692,"AD",PKG,VER,SEQ,"")) Q:IEN=""
 ...S IEN=IEN_",",INFO=$$GETS^VFDXPDA("INFO",21692,IEN,".01;.1","I")
 ...Q:'INFO
 ...S RPT(INFO(21692,IEN,.01,"I"))=""
 ...K INFO
 S X="" F I=1:1 S X=$O(RPT(X)) Q:X=""  S RPT(I)=X K RPT(X)
 Q $D(RPT(1)) 
 ;
 ;
ERR(A,X) ; error messaging
 ;;There are missing SEQ # in the batch.  The following should be examined
 I $G(A) S X=$TR($T(ERR+A),";"," ")_" "_$G(X)
 W !!,X,!!
 Q 
 ;
 ; 
 ;;==================================================
 ;;                      E N D                       
 ;;==================================================
SET 
 N X,Y,Z,DIC,NAME,SEQ
 K ^TMP($J)
 S Z="",DIC=21692,DIC(0)="Z"
 F  S Z=$O(^XPD(9.7,"B",Z)) Q:Z=""  D
 .S X=Z D ^DIC
 .I '$D(Y(0)) Q
 .S SEQ=$P(Y(0),U,7)
 .S ^TMP($J,X)=SEQ
 .K Y
 Q
KILL 
 N X
 S X=""
 F  S X=$O(^XPD(9.7,"B",X)) Q:X=""  D
 . K ^TMP($J,X)
 Q
 ; 
DBTEST ;
 N X,Y,NAME,COUNT
 K ^TMP($J)
 S X=0,Y="",COUNT=0
 F  S X=$O(^VFDV(21692,X)) Q:'X  D
 .S NAME=$P(^VFDV(21692,X,0),U,1)
 .I $G(^TMP($J,NAME))'="" S ^TMP("DBTEST",$J,NAME)=^TMP("DBTEST",$J,NAME)+1
 .E  S ^TMP($J,NAME)=1
 F  S Y=$O(^TMP($J,Y)) Q:Y=""  D
 .I ^TMP($J,Y)>1 W Y,! S COUNT=COUNT+1
 W !,COUNT_" duplicate patches",!
 K ^TMP($J)
 Q
 ;
TEST 
 N X,NAME
 S X=0 F  S X=$O(^VFDV(21692.1,PID,1,X)) Q:'X  D
 .S NAME=$P(^VFDV(21692,X,0),U)
 .I $D(^TMP($J,"TESTPATCH",NAME)) K ^TMP($J,"TESTPATCH",NAME)
 .E  S ^TMP($J,"XPD",NAME)=""
 Q 
 ;
LATEST 
 ; List of the last patch installed for each namespace
 N X,Y,Z,NAME
 K ^TMP($J)
 S X=""
 F  S X=$O(^XPD(9.7,"B",X)) Q:X=""  D
 .S Y=""
 .I X'["*" S NAME=$P(X," ",1)
 .E  S NAME=$P(X,"*",1,2)
 .F  S Y=$O(^XPD(9.7,"B",X,Y)) Q:Y=""  D
 ..S Z=$P($G(^XPD(9.7,Y,0)),U,1)_"^"		;GET NAME
 ..S Z=Z_$P($G(^XPD(9.7,Y,6)),U,2)_"^"		;GET SEQ#
 ..S Z=Z_$P($G(^XPD(9.7,Y,1)),U,3)			;GET DATE
 ..I $P($G(^TMP($J,$P(NAME,"*",1))),U,3)<$P(Z,U,3) S ^TMP($J,$P(NAME,"*",1))=Z
 Q
 ;
LATEST2 ;
 N X,Y,Z,PKG,NA,DATE,VER
 S X=0 F  S X=$O(^DIC(9.4,X)) Q:'X  S PKG=$P(^(X,0),U,2) D
 .S Y=0 F  S Y=$O(^DIC(9.4,X,22,Y)) Q:'Y  S VER=$P(^(Y,0),U) D
 ..S Z=0  F  S Z=$O(^DIC(9.4,X,22,Y,"PAH",Z)) Q:'Z  S NUM=+^(Z,0) D
 ...S DATE=$P(^DIC(9.4,X,22,Y,"PAH",Z,0),U,2),NM=PKG_"*"_VER_"*"_NUM
 ...;Q:'$D(^VFDV(21692,"B",NM))
 ...Q:'$D(^VFDV(21692,"AC",PKG))
 ...;I DATE>+$G(TEMP(X,Y)) S TEMP(X,Y)=DATE_U_NM_U_PKG
 ...I DATE>+$G(TEMP(PKG)) S TEMP(PKG)=DATE_U_NM
 Q
 ;
NULL(PID) 
 ; Prints a list of all the patches with no KID or TXT file on record
 ; for a given batch
 Q:$G(PID)=""
 N X,Y,FILE,TEMP
 S X=0 F  S X=$O(^VFDV(21692.1,PID,1,X)) Q:'X  D
 .S FILE=$G(^VFDV(21692,X,"FILE"))
 .I FILE=""!(FILE=U) S TEMP("BOTH",$$GETNAME^ZZZSMP(X))="" Q
 .I $P(FILE,U,1)="" S TEMP("KID",$$GETNAME^ZZZSMP(X))=""
 .I $P(FILE,U,2)="" S TEMP("TXT",$$GETNAME^ZZZSMP(X))=""
 S X="" F  S X=$O(TEMP(X)) Q:X=""  D
 .W !!,X,!,"=====================",! S Y="" 
 .F  S Y=$O(TEMP(X,Y)) Q:Y=""  W Y,!
 K ^TMP($J)
 Q
 ;
 ;
STATUS 
 N X,STATUS
 S X=0
 F  S X=$O(^VFDV(21692,X)) Q:'X  D
 .S STATUS=$P(^VFDV(21692,X,0),U,10)
 .I $G(^TMP($J,STATUS))'="" S ^TMP($J,STATUS)=^TMP($J,STATUS)+1
 .E  S ^TMP($J,STATUS)=1
 .K STATUS
 Q
 ;
 ;
MOST 
 N X,Y,Z,TEMP,MAX
 S X=0 F  S X=$O(^VFDV(21692,X)) Q:'X  D
 .S Y=$P(^(X,0),U,15) Q:Y=""
 .S TEMP(Y)=+$G(TEMP(Y))+1,TEMP(Y,X)=TEMP(Y)
 S (MAX,X)=0 F  S X=$O(TEMP(X)) Q:X=""  D
 .I TEMP(X)>MAX S MAX=TEMP(X),MAX(1)=X
 W $$GETNAME^ZZZSMP(MAX(1))
 Q
 ;
REPORT
 N X,Y,Z,I,J,K,T,BATCH,FILE,FLAG,LIST,MAX,NAME,NM,PID,RPT,SUB,TEMP
 D KILL^VFDXPD,PID^VFDXPD(0,1,0) I PID>0 D BATCH^VFDXPD(PID)
 Q:'$D(BATCH)  D 7^VFDXPDE1 Q:'$D(RPT)  M LIST=RPT K RPT
 D 11^VFDXPDE1 Q:'$D(RPT)  M FILE=RPT S T=$C(9) K RPT
 ;;FILE STARTS ON LINE 3
 ;;LIST STARTS ON LINE 4
 S MAX=$O(FILE(""),-1)
 F I=3:1:MAX S X=$P(FILE(I),"| ",3) D
 .Q:X'?1.U.E  F J=$L(X):-1:1 Q:$E(X,J)'=" "
 .Q:$P(FILE(I),"| ",5)=""
 .S NAME(I)=$E(X,1,J),NAME(I,1)=$P(FILE(I),"| ",5),SUB=2
 .F J=I+1:1 Q:'$D(FILE(J))  Q:$P(FILE(J),"| ",3)'?1.P  D
 ..S NAME(I,SUB)=$P(FILE(J),"| ",5),SUB=SUB+1
 S MAX=$O(LIST(""),-1)
 F I=4:1:MAX S X="" F  S X=$O(NAME(X)) Q:'X  D
 .S NM=$P(LIST(I),T) S:NM[">>" NM=$P(NM,">>",2)
 .I NM=NAME(X) S $P(NAME(X),U,2)=I
 S X="" F  S X=$O(NAME(X)) Q:X=""  S TEMP=$P(NAME(X),U,2) D
 .F I=1:1 Q:'$D(NAME(X,I))  S $P(LIST(TEMP+((I-1)/100)),T,5)=NAME(X,I)
 S $P(LIST(3),T,5)="File Names" K LIST(1),LIST(2)
 W !! D RPT^VFDXPD0(.LIST)
 Q
