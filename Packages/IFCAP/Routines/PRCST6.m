PRCST6 ; GENERATED FROM 'PRCS REQUESTS FOR APPROVAL' PRINT TEMPLATE (#304) ; 10/27/00 ; (FILE 410, MARGIN=80)
 G BEGIN
N W !
T W:$X ! I '$D(DIOT(2)),DN,$D(IOSL),$S('$D(DIWF):1,$P(DIWF,"B",2):$P(DIWF,"B",2),1:1)+$Y'<IOSL,$D(^UTILITY($J,1))#2,^(1)?1U1P1E.E X ^(1)
 S DISTP=DISTP+1,DILCT=DILCT+1 D:'(DISTP#100) CSTP^DIO2
 Q
DT I $G(DUZ("LANG"))>1,Y W $$OUT^DIALOGU(Y,"DD") Q
 I Y W $P("JAN^FEB^MAR^APR^MAY^JUN^JUL^AUG^SEP^OCT^NOV^DEC",U,$E(Y,4,5))_" " W:Y#100 $J(Y#100\1,2)_"," W Y\10000+1700 W:Y#1 "  "_$E(Y_0,9,10)_":"_$E(Y_"000",11,12) Q
 W Y Q
M D @DIXX
 Q
BEGIN ;
 S:'$D(DN) DN=1 S DISTP=$G(DISTP),DILCT=$G(DILCT)
 I $D(DXS)<9 M DXS=^DIPT(304,"DXS")
 S I(0)="^PRCS(410,",J(0)=410
 S X=$G(^PRCS(410,D0,0)) W ?0,$E($P(X,U,1),1,18)
 W ?20 S Y=$P(X,U,2) W:Y]"" $S($D(DXS(2,Y)):DXS(2,Y),1:Y)
 W ?26 S Y=$P(X,U,4) S Y=$S(Y="":Y,$D(^PRCS(410.5,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,30)
 S X=$G(^PRCS(410,D0,7)) D N:$X>2 Q:'DN  W ?2 S Y=$P(X,U,1) S Y=$S(Y="":Y,$D(^VA(200,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,35)
 S X=$G(^PRCS(410,D0,1)) W ?39 S Y=$P(X,U,1) D DT
 W ?52 S Y=$P(X,U,4) D DT
 S X=$G(^PRCS(410,D0,4)) W ?65 S Y=$P(X,U,1) W:Y]"" $J(Y,11,2)
 S X=$G(^PRCS(410,D0,2)) D N:$X>5 Q:'DN  W ?5,$E($P(X,U,1),1,30)
 S DICMX="D ^DIWP" S DIWL=38,DIWR=78 X DXS(1,9.3) S DIP(102)=X S X=1,DIP(103)=X S X=30,X=$E(DIP(102),DIP(103),X) S D0=I(0,0) S D1=I(1,0) K DIP K:DN Y
 D A^DIWW
 D N:$X>0 Q:'DN  W ?0 W " "
 K Y K DIWF
 Q
HEAD ;
 W !,?0,"TRANSACTION NUMBER",?20,"TYPE",?26,"FORM TYPE"
 W !,?2,"REQUESTOR",?39,"REQUESTED",?52,"REQUIRED",?68,"EST COST"
 W !,?5,"VENDOR",?37,"FIRST ITEM DESCRIPTION"
 W !,"--------------------------------------------------------------------------------",!!
