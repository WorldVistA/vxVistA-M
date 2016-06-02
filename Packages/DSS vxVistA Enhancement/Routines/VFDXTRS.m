VFDXTRS ;DSS/SGM/SMP - SAVE/LOAD ROUTINES TO HFS FILES; 06/19/2015 12:30
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; ICR#  Supported Description
 ;-----  ------------------------------------------------
 ;10005  DT^DICRW
 ;10086  HOME^%ZIS 
 ;10096  All nodes in %ZOSF may be referenced
 ;10103  ^XLFDT: $$FMTE,$$NOW, $$HTE
 ;10104  $$CJ^XLFSTR
 ;
 N I,X,Y,Z,CH,PATH
 D DT^DICRW,HOME^%ZIS,KILL W @IOF
 S X=$$DIR^VFDXTR09(13) Q:"SR"'[X  S CH=X
 S PATH=$$PATH I PATH'=-2 S VROOT=$NA(^UTILITY($J)) D KILL,@CH,KILL
 Q
 ;
 ;------------------------ Private Subroutines ------------------------
R ; restore routine
 ; ^TMP($J,2,<routine_name>)=hfs filename
 N I,J,K,L,X,Y,Z,CNT,ERR,EXT,LIST,ROU,RTNX,SAVE,SP
 N VERR,VEXT,VFDIN,VFDR,VFILES,VINPUT
 ; get list of rsv files from directory
 S VFDR=$NA(^TMP("VFDXTRS",$J)) K @VFDR
 S $E(SP,15)=""
 S X=$$R1 I +X<1 D WR($P(X,U,2),,1,1) Q
 D WR($$MSG(1,1)_X,,1,1),SEL
 Q:'$G(^TMP($J,1,0))
 D WR($$MSG(.1),,1,1) S X=$$DIR2("R") Q:X<0  Q:$$DIR4<1
 S SAVE="S VFDR=$$ZOSF1^VFDVZOSF(.VINPUT)"
 S VINPUT("NODE")="SAVE",VINPUT("ROOT")=$NA(^TMP($J,3))
 W ! S (L,X)=0 F  S X=$O(^TMP($J,1,X)) Q:X=""  S Z=^(X) D
 .; Z=filename  X=routine name  RTNX() contains routine
 .S ROU=X W "." K RTNX S Y=$$FTG^VFDXTRU(PATH,Z,"RTNX(1)",1)
 .S LIST(ROU)="" I 'Y S LIST(ROU)="@" Q
 .S Y=RTNX(1)
 .I $P(Y," ")'="Routine"!(ROU="VFDXTRS") S LIST(ROU)="!" Q
 .; kept for files saved with prior versions
 .S K=0,J=2,Y=RTNX(2) I Y=""!(Y["^INT^") S J=3
 .K ^TMP($J,3) F I=J:1 Q:'$D(RTNX(I))  S K=K+1,^TMP($J,3,K)=RTNX(I)
 .S VINPUT("X")=ROU
 .S X=ROU X SAVE W:$X>72 ! S L=L+1 W:'(L#10) X
 .Q
 D ROUDSP^VFDXTRU("LIST"),WR($$MSG(2),,1),WR($$MSG(3,1),,1,,2)
 Q
 ;
S ; save routines
 N I,J,X,Y,Z,CNT,DATE,RPT,RTN,SAVE,STRT,TMP,VFD,VFDIN,VINPUT
 S VFD=$$ASK^VFDXTR(,,,+$O(^XTMP("XPDI",0))) I VFD<1 D WR($$MSG(4,1),,1) Q
 S RPT=$P(VFD,U,2)'="L",RPT(1)=3
 S (I,X)=0 F  S X=$O(^UTILITY($J,X)) Q:X=""  S I=I+1,^TMP($J,1,X)=""
 S ^TMP($J,1,0)=I Q:$$DIR2("S")<0
 D WR($$MSG(.2),,1,,1) Q:$$DIR4<1
 S DATE=$TR($P($$HTE^XLFDT($H),":",1,2),"@"," "),CNT=0
 ; actually save the routines to hfs files
 S VINPUT("NODE")="LOAD",VINPUT("ROOT")="RTN"
 I $P(VFD,U,2)="L" S LOAD="S VFDR=$$LOAD^VFDXTRS(.VINPUT)"
 E  S LOAD="S VFDR=$$ZOSF1^VFDVZOSF(.VINPUT)"
 S STRT=$$NOW^XLFDT
 W ! S X=0 F  S X=$O(^TMP($J,1,X)) Q:X=""  D
 .S ROU=X K RTN S VINPUT("X")=ROU X LOAD K TMP
 .S I=0,J=2 F  S I=$O(RTN(I)) Q:'I  S J=J+1,TMP(J)=RTN(I)
 .S TMP(1)=$$MSG(.3),TMP(2)=$$ROW2(X,$P(VFD,U,2)="L")
 .S Y=$$GTF^VFDXTRU(PATH,X_".rsv","TMP(1)",1) I Y=1 S CNT=CNT+1
 .D WROU(X_$S(Y<1:"*",1:"")) I RPT D RPT(X) I 1
 .E  D RPT1(X)
 .Q
 I $D(RPT(3)) D CHKSUM^VFDXTR01(.RPT,PATH)
 S X="   Any routine preceded by an * was not saved" D WR(X,,1,1,2)
 D STATS(CNT,STRT)
 Q
 ;
 ;----------------------------- Components ----------------------------
DIR() Q $$DIR^VFDXTRU(.Z)
 ;
DIR2(OPT) ; 
 ; OPT - req - S for Save; R for Restore
 N I,X,Y,Z
 S Z("A")="Do you wish to see all the routines selected? "
 S Z("B")="YES",Z(0)="YOA"
 S X=$$DIR I X=1 D LIST(1,OPT) S X=1
 Q X
 ;
DIR3(REPEAT) ; rtns to restore
 ;;You can restore all routines or selected routines.
 ;; * = if selecting routines, use the * as a wildcard designation.
 ;;     E.g., VFD* will select only those routines starting with VFD.
 ;; - = Remove one or more routines from the list by preceding your
 ;;     input value with a minus (-) sign.  E.g., -VFDV or -VFDV*
 ;;?? = Display list of all the routines and the selection status.
 ;;<<Input is Case Sensitive as there may be both UC and LC routines>>
 ;;
 ;;Minus (-) behavior:
 ;; If routine(s) are found on the list of routines already selected,
 ;;  then move those routines back to the list of selectable routines
 ;; Else, if routine(s) are found on the routines that are selectable
 ;;  then remove those routine names complete from all lists (i.e,
 ;;  they will no longer be selectable.
 ;;No Minus (-): move entry(s) from the list of selectable routines to
 ;; the list of routines already selected.
 ;;
 N I,T,X,Y,Z
 F I=1:1:17 S T(I)=$TR($T(DIR3+I),";"," ")
 S Z(0)="FO^1:9",Z("A")="Select routines to be restored"
 I 'REPEAT S Z("B")="ALL" M Z("A")=T
 E  S Z("?")="   " M Z("?")=T
 S Z("??")="^D LIST^VFDXTRS(12,""R"")"
 Q $$DIR
 ;
DIR4() ; continue?
 N I,X,Y,Z
 S Z(0)="YOA",Z("B")="NO"
 S Z("A")="Are you sure you wish to continue? "
 Q $$DIR
 ;
KILL K ^TMP("VFDXTRS",$J),^TMP($J),^UTILITY($J) Q
 ;
LIST(T,OPT) ; list routines
 N I,L,X,Y,Z,CNT,SP
 I OPT="S" D
 .D ROUDSP^VFDXTRU($NA(^TMP($J,1)),$$MSG(13))
 I OPT="R" D
 .F Y=1,2 I T[Y D
 ..D ROUDSP^VFDXTRU($NA(^TMP($J,Y)),$$MSG(10+Y))
 ..Q
 Q
 ;
LOAD(VINPUT) ; Get routine from loaded build
 ;S VINPUT("NODE")="LOAD"
 ;S VINPUT("ROOT")="RTN"
 ;S VINPUT("X")=ROU
 N X,Y,Z,BLD,ROOT,ROU
 S ROU=$G(VINPUT("X")) Q:ROU="" 0
 S BLD=$G(^UTILITY($J,ROU)) Q:'BLD 0
 S ROOT=$G(VINPUT("ROOT")) Q:ROOT="" 0
 F X=1:1 Q:'$D(^XTMP("XPDI",BLD,"RTN",ROU,X))  S @ROOT@(X)=^(X,0)
 Q 1
 ;
MSG(A,B) ;
 ;;Total files found with file extension .rsv: 
 ;;Legend: @ = routines not restored, hfs file not retrieved
 ;;     ! = routines not restored, hfs file not created by VFDXTRS
 ;;No routines were selected to be saved as HFS files
 ;;Now enter the path or directory where the files will be placed
 ;; This path must already exist!
 ;;Enter the path (directory) where the files saved by this routine
 ;; reside.  Only files with the extension .rsv are valid
 ;;Multiple file names found for a single routine name
 ;;List of Routines To Be Restored From HFS Files
 ;; Routines Selected To Be Restored 
 ;; Routines Not Selected To Be Restored 
 ;; Routines Selected To Be Saved
 N X
 I A=.1 S X=^TMP($J,1,0)_" routines selected to be restored"
 I A=.2 S X="   Routines will be saved in "_PATH_" as filename.rsv"
 I A=.3 S X="Routine "_ROU_" saved using VFDXTRS routine on "_DATE
 I A=.4 S X="-1^No files found in "_PATH_" with file extension .rsv"
 I A=.5 S X="Total number of routines: "_$G(^TMP($J,Y,0))
 I '$D(X) S X=$TR($T(MSG+A),";"," ") S:'$G(B) X=$E(X,4,$L(X))
 Q X
 ;
PATH() ; ask for directory
 ;;Enter the path (directory) where the routine HFS files are to be placed
 ;;Enter the path (directory) where the HFS files saved by this routine
 ;;reside.  Only files with the extension .rsv are valid
 N I,X,Y,Z
 F I=1,2,3 S Z(I)=$TR($T(PATH+I),";"," ")
 W !?3,$S(CH="S":Z(1),1:Z(2)) W:CH="R" !?3,Z(3)
 Q $$ASKPATH^VFDXTR
 ;
R1() ; get list of filenames
 N I,J,K,X,Y,Z,CNT,ERR,EXT,FNM,ROOT,RTN,VFDIN
 ;
 N I,J,X,Y,Z,VEXT,VFDR,VFDX
 S ROOT=$NA(^TMP("VFDXTRS",$J)) K @ROOT
 S VEXT("*.rsv")="" D HFLIST^VFDXTRU(ROOT,PATH,.VEXT)
 I '$D(@ROOT) Q "-1^No files found in "_PATH_" with file extension .rsv"
 ;
 K ^TMP($J),^UTILITY($J)
 S (K,FNM)=0 F I=1:1 S FNM=$O(@ROOT@(FNM)) Q:FNM=""  D
 .S RTN=$P(FNM,"."),^TMP($J,2,RTN)=FNM,^(0)=I
 K @ROOT
 Q +$G(^TMP($J,2,0))
 ;
ROW2(X,LOAD) ;
 N Y,Z S Z=""
 I LOAD D  Q Z
 .S Y=$H
 .S Z=X_"^INT^"_Y_$P($$HTE^XLFDT(Y),":",1,2)
 I $$ZOSF^VFDVZOSF("OS")["OpenM",$G(X)'="" D
 .S Y=$G(^ROUTINE(X,0))
 .I Y>0 S Z=X_"^INT^"_Y_U_$P($$HTE^XLFDT(Y),":",1,2)
 .Q
 Q Z
 ;
RPT(X) ;
 S RPT(RPT(1))=$$RPT^VFDXTR01(X),RPT(1)=1+RPT(1)
 Q
 ;
RPT1(ROU) ;
 N X,Y,Z,BLD,ROOT
 S BLD=$G(^UTILITY($J,ROU)) Q:'BLD
 S CSUM=$E($P($G(^XTMP("XPDI",BLD,"RTN",ROU)),U,3),2,99)
 S RPT(RPT(1))=$$LJ^XLFSTR(ROU,25)_CSUM,RPT(1)=1+RPT(1)
 Q
 ;
SEL ; Select routines to be restored
 ; expects ^TMP($J,2,routine)=filename,^(0)=total
 ;   ^TMP($J,2) contains files not to be restored
 ;   ^TMP($J,1) contains files to be restored
 ;   ^TMP($J,1,routine)=filename,^(0)=total
 ;
 N A,B,I,R,X,Y,Z,GOTONE,MINUS,RTN,STAR
 F I=1:1 S Z=$$DIR3(I>1) Q:Z<0!(Z="")  D
 .I Z="ALL" M ^TMP($J,1)=^TMP($J,2) K ^TMP($J,2) Q
 .S X=Z,STAR=$E(Z,$L(Z))="*" I STAR S X=$P(X,"*")
 .S MINUS=($E(X)="-") I MINUS S X=$E(X,2,$L(X))
 .I X="" W "   ??" Q
 .; chose to remove routine(s) from list
 .S Y=X,GOTONE=0 I MINUS D  Q
 ..S Z=$G(^TMP($J,1,X)) I Z'="" D SEL2(X),SEL3(X,Z) S GOTONE=1
 ..I STAR S R=X F  S R=$O(^TMP($J,1,R)) Q:$E(R,1,$L(X))'=X  S Z=^(R) D
 ...D SEL2(R),SEL3(R,Z) S GOTONE=1
 ...Q
 ..Q:GOTONE
 ..S Z=$G(^TMP($J,2,X)) I Z'="" D SEL1(X)
 ..I STAR S R=X F  S R=$O(^TMP($J,2,R)) Q:$E(R,1,$L(X))'=X  D SEL1(X)
 ..Q
 .;----- no minus at this point, adding to selected list
 .S Z=$G(^TMP($J,2,X)) I Z'="" D SEL1(X),SEL4(X,Z)
 .Q:'STAR  S R=X
 .F  S R=$O(^TMP($J,2,R)) Q:$E(R,1,$L(X))'=X  S Z=^(R) D
 ..D SEL1(R),SEL4(R,Z)
 ..Q
 .Q
 Q
 ;
SEL1(R) ; remove from selectable list
 N A S A=$G(^TMP($J,2,0)),^(0)=A-1 K ^(R) Q
 ;
SEL2(R) ; remove from selected list
 N A S A=$G(^TMP($J,1,0)),^(0)=A-1 K ^(R) Q
 ;
SEL3(R,Z) ; add to selectable list
 N A,Y S A=$G(^TMP($J,2,0)),Y=$G(^(R)) S:Y="" ^(0)=A+1,^(R)=Z Q
 ;
SEL4(R,Z) ; add to selected list
 N A,Y S A=$G(^TMP($J,1,0)),Y=$G(^(R)) S:Y="" ^(0)=A+1,^(R)=Z Q
 ;
STATS(NUM,STRT) ;
 N X,Y,END,MSG,TIME S END=$$NOW^XLFDT
 D WR(NUM_" Routines saved successfully")
 D WR("Time Elasped: "_$$TIME(END,STRT),,2)
 Q
 ;
TIME(END,STRT) ;
 N X,MSG,TIME
 S TIME=$$FMDIFF^XLFDT(END,STRT,2),MSG=""
 S X=TIME\3600,TIME=TIME-(3600*X),TIME("Hour"_$S(X>1:"s",1:""))=X
 S X=TIME\60,TIME=TIME-(60*X),TIME("Minute"_$S(X>1:"s",1:""))=X
 S TIME("Second"_$S(TIME>1:"s",1:""))=TIME
 S X="" F  S X=$O(TIME(X)) Q:X=""  D
 .S MSG=MSG_$S(TIME(X):" "_TIME(X)_" "_X_$S(X["S":"",1:","),1:"")
 Q MSG
 ;
WR(X,CJ,SLF,ELF,LINE) ; do screen writes
 D WR^VFDXTRU($G(X),$G(CJ),$G(SLF),$G(ELF),$G(LINE))
 Q
 ;
WROU(R,C) ; write out routine and do line feed if needed
 ; C - opt - column for line feed check
 S C=$G(C) S:'C C=70
 W $E(R_"          ",1,10) W:$X>C !
 Q
