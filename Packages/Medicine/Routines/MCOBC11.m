MCOBC11 ; GENERATED FROM 'MCARCATHBRPR' PRINT TEMPLATE (#1019) ; 10/04/96 ; (continued)
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
 D N:$X>39 Q:'DN  W ?39 S Y=$P(X,U,4) W:Y]"" $J(Y,7,2)
 Q
H1R ;
 S I(1)=37,J(1)=691.36 F D1=0:0 Q:$O(^MCAR(691.1,D0,37,D1))'>0  X:$D(DSC(691.36)) DSC(691.36) S D1=$O(^(D1)) Q:D1'>0  D:$X>48 T Q:'DN  D I1
 G I1R
I1 ;
 D T Q:'DN  D N D N:$X>23 Q:'DN  W ?23 W "STARTING"
 D N:$X>38 Q:'DN  W ?38 W "RESULTING"
 D N:$X>21 Q:'DN  W ?21 W "% OBSTRUCTION"
 D N:$X>36 Q:'DN  W ?36 W "% OBSTRUCTION"
 S X=$G(^MCAR(691.1,D0,37,D1,0)) D N:$X>7 Q:'DN  W ?7 S Y=$P(X,U,1) S Y=$S(Y="":Y,$D(^MCAR(696,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,30)
 D N:$X>23 Q:'DN  W ?23 S Y=$P(X,U,2) S Y=$S(Y="":Y,$D(^MCAR(696.1,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,17)
 S X=$G(^MCAR(691.1,D0,37,D1,3)) D N:$X>23 Q:'DN  W ?23 S Y=$P(X,U,1) W:Y]"" $J(Y,7,2)
 S X=$G(^MCAR(691.1,D0,37,D1,0)) D N:$X>40 Q:'DN  W ?40 S Y=$P(X,U,3) S Y=$S(Y="":Y,$D(^MCAR(696.1,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,17)
 S X=$G(^MCAR(691.1,D0,37,D1,3)) D N:$X>40 Q:'DN  W ?40 S Y=$P(X,U,2) W:Y]"" $J(Y,7,2)
 Q
I1R ;
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 S DIP(1)=$S($D(^MCAR(691.1,D0,40)):^(40),1:"") S X="EJECTION FRACTION: "_$P(DIP(1),U,3) K DIP K:DN Y W X
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "COMPLICATIONS: "
 S DICMX="D L^DIWP" D N:$X>20 Q:'DN  S DIWL=21,DIWR=78 X DXS(2,9) K DIP K:DN Y
 D A^DIWW
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "CONCLUSION: "
 S I(1)=43,J(1)=691.41 F D1=0:0 Q:$O(^MCAR(691.1,D0,43,D1))'>0  S D1=$O(^(D1)) D:$X>18 T Q:'DN  D J1
 G J1R
J1 ;
 S X=$G(^MCAR(691.1,D0,43,D1,0)) S DIWL=18,DIWR=77 D ^DIWP
 Q
J1R ;
 D A^DIWW
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "PLAN: "
 S I(1)=44,J(1)=691.42 F D1=0:0 Q:$O(^MCAR(691.1,D0,44,D1))'>0  S D1=$O(^(D1)) D:$X>12 T Q:'DN  D K1
 G K1R
K1 ;
 S X=$G(^MCAR(691.1,D0,44,D1,0)) S DIWL=13,DIWR=77 D ^DIWP
 Q
K1R ;
 D A^DIWW
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "SUMMARY: "
 S X=$G(^MCAR(691.1,D0,.2)) S Y=$P(X,U,1) W:Y]"" $S($D(DXS(4,Y)):DXS(4,Y),1:Y)
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "PROCEDURE SUMMARY: "
 D N:$X>17 Q:'DN  S DIWL=18,DIWR=77 S Y=$P(X,U,2) S X=Y D ^DIWP
 D A^DIWW
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "CARDIOLOGY STAFF: "
 S X=$G(^MCAR(691.1,D0,45)) S Y=$P(X,U,2) S Y=$S(Y="":Y,$D(^VA(200,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,35)
 W ?24 S MCFILE=691.1 D DISP^MCMAG K DIP K:DN Y
 W ?35 K MCFILE K DIP K:DN Y
 K Y K DIWF
 Q
HEAD ;
 W !,"--------------------------------------------------------------------------------",!!
