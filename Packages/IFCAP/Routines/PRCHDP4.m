PRCHDP4 ;WISC/RSD/RHD-DISPLAY P.O.(CONT.) ;12/1/93  09:49
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
EN ;ONLINE DISPLAY OF DELIVERY SCHEDULE
 S PRCHONL=1,PRCHQUIT=0
 ;
EN2 ;ENTRY POINT USED TO EITHER PRINT OR DISPLAY DELIVERY SCHEDULE
 Q:'$D(D0)  S PRCHPONO=$P(^PRC(442,D0,0),U,1),PRCHQUIT="" G:PRCHPONO="" EXDELV K ^TMP($J,"MDEL")
 F PRCHI=0:0 S PRCHI=$O(^PRC(442.8,"B",PRCHPONO,PRCHI)) Q:'PRCHI  I $D(^PRC(442.8,PRCHI,0)) S Y(0)=^(0) D BLD
 G:'$D(^TMP($J,"MDEL")) EXDELV S L=100
 F I=0:0 S I=$O(^TMP($J,"MDEL",I)) Q:'I  Q:PRCHQUIT  S PRCHX=^(I) D PRT1 S PRCHZ="" F J=0:0 S J=$O(^TMP($J,"MDEL",I,J)) Q:'J  Q:PRCHQUIT  S PRCHY="" F K=0:0 S PRCHY=$O(^TMP($J,"MDEL",I,J,PRCHY)) Q:PRCHY=""  S PRCHX=^(PRCHY) D PRT2
 ;
EXDELV W ! K ^TMP($J,"MDEL"),PRCHI,PRCHONL,PRCHPONO,PRCHQUIT,PRCHX,PRCHY,PRCHZ,I,J,K,X,Y,Z,L
 Q
 ;
BLD S I=+$P(Y(0),U,2) I '$D(^TMP($J,"MDEL",I)) S X=$G(^PRC(442,D0,2,I,0)),J=$O(^PRC(442,D0,2,I,1,0)),K=$S($D(^(+J,0)):^(0),1:""),^TMP($J,"MDEL",I)=$P(X,U,5)_U_+$P(X,U,2)_U_$E(K,1,40)_U_+X
 ; <<< FOR NOIS PTB-0695-20438
 ;
 ; S I=+$P(Y(0),U,2) I '$D(^TMP($J,"MDEL",I)) S J=+$O(^PRC(442,D0,2,"B",I,0)),X=$G(^PRC(442,D0,2,J,0)),J=$O(^PRC(442,D0,2,J,1,0)),K=$S($D(^(+J,0)):^(0),1:""),^TMP($J,"MDEL",I)=+$P(X,U,5)_U_+$P(X,U,2)_U_$E(K,1,40)
 ;
 S Y=$P(Y(0),U,3),Y=$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3),Z=$P($G(^PRCS(410.8,+$P(Y(0),U,4),0)),U,1) S:Z="" Z=" "
 S ^TMP($J,"MDEL",I,+$P(Y(0),U,3),Z)=Y_U_$P(Y(0),U,5)
 Q
 ;
PRT1 D:L>(IOSL-3) PAUSE Q:PRCHQUIT  W !,$J($P(PRCHX,U,4),4),?5,$J($P(PRCHX,U),4),?11,$P(PRCHX,U,3),?54,$J($P(PRCHX,U,2),6),! S L=L+2
 ; <<< FOR NOIS PTB-0695-20438
 Q
 ;
PRT2 D:L>(IOSL-2) PAUSE Q:PRCHQUIT
 I PRCHZ'=$P(PRCHX,U,1) S PRCHZ=$P(PRCHX,U,1) W ?31,PRCHZ
 W ?41,PRCHY,?73,$J($P(PRCHX,U,2),6),!
 S L=L+1
 Q
 ;
H W:$Y>0 @IOF W !!,?10,"****  P.O. "_$P(PRCHPONO,"-",2)_"  DELIVERY SCHEDULE ****"
 W:L'=100 ?55,"(CONTINUED)" W !!
 W "LI#",?5,"IMF#  DESCRIPTION",?53,"QTY.ORD",!,?31,"DEL.DATE  LOCATION",?72,"DELV.QTY",!
 S L=5
 Q
 ;
PAUSE I 'PRCHONL D H Q
 W !?8,"ENTER '^' TO HALT: " S PRCHQUIT=0 R X:DTIME S:X["^" PRCHQUIT=1 Q:PRCHQUIT  D H
 Q
