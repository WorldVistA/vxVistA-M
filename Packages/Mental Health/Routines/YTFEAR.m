YTFEAR ;SLC/DKG-TEST PKG: FEAR INV. ; 10/20/88  08:43 ;
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;
 S YSLFT=0,X=^YTD(601.2,YSDFN,1,YSET,1,YSED,1) D DTA^YTREPT W !!?25,$P(^YTT(601,YSTEST,"P"),U) I '$D(^YTT(601,YSTEST,"Q",1,"T",1,0)) W !!,"NOT OPERATIONAL" Q
 W !!,"--- VERY MUCH DISTURBING ---",!
 S K=5 D CK I YSLFT G END
 W !!,"--- DISTURBING ---",! S K=4 D CK I YSLFT G END
 I IOST?1"P".E D:$Y>40 DTA^YTREPT
 E  D WAIT G:YSLFT END
 W !!!?25,"--- ITEM RESPONSES ---",!!
 S K=10,YSIT=1 F I=1:1:10 D RLN
 S K=8 D RLN
END ;
 K I,YSIT,J,K,YSKK,X Q
RLN ;
 W ?1 F YSKK=1:1:K W $J(YSIT,3,0)," ",$E(X,YSIT),"  " S YSIT=YSIT+1
 W ! Q
CK ;
 I X'[K W !,"NONE INDICATED",! Q
 F J=1:1:108 W:$E(X,J)=K !,^YTT(601,YSTEST,"Q",J,"T",1,0) D:$Y>55&(IOST?1"P".E) DTA^YTREPT W:$Y>55&(IOST?1"P".E) !!! I IOST?1"C-".E D:$Y>(IOSL-4) WAIT Q:YSLFT
 Q
WAIT ;
 ;  Added 5/6/94 LJA
 N A,B,B1,C,D,E,E1,F,F1,G,G1,H,I,J,J1,J2,J3,J4,K,L,L1,L2,M,N
 N N1,N2,N3,N4,P,P0,P1,P3,R,R1,S,S1,T,T1,T2,TT,V,V1,V2,V3
 N V4,V5,V6,W,X,X0,X1,X2,X3,X4,X7,X8,X9,Y,Y1,Y2,Z,Z1,Z3
 ;
 F I0=1:1:(IOSL-$Y-2) W !
 N DTOUT,DUOUT,DIRUT
 S DIR(0)="E" D ^DIR K DIR S YSTOUT=$D(DTOUT),YSUOUT=$D(DUOUT),YSLFT=$D(DIRUT)
 W @IOF Q
 Q
