DENTDNQ ;WASH ISC/TJK,JA,NCA-SCREEN INPUT-QUESTIONMARKS ;8/28/92  09:08
 ;;1.2;DENTAL;**15,23**;NOV 1993
 S D=""
A S DJX="" X DJCP
 I DJ4["R" W "**REQUIRED**",*7
 S:'$D(X) X=DJXX G:X'="??" F G:'$D(^DD(DJDD,DJAT,21,0)) F G:'$P(^(0),U,4) F
 S DJZ1=0,DIWL=1,DIWR=79,DIWF="" K ^UTILITY($J,"W")
 S DJXX=X F DJK=1:1 S DJZ1=$O(^DD(DJDD,DJAT,21,DJZ1)) Q:DJZ1=""  S X=^(DJZ1,0) D ^DIWP
 D ^DIWW ;    purge WP buffer
 S DJZ1=0 F DJK=1:1 S DJZ1=$O(^UTILITY($J,"W",DIWL,DJZ1)) Q:DJZ1=""  D:$Y>21 CONT Q:DJX[U  W !,^(DJZ1,0)
 K DJZ1,DJK,^UTILITY($J,"W",DIWL),DIWL,DIWR,DIWF S X=DJXX G:DJX'[U F Q
CONT W !,"Press <RETURN> to Continue, '^' to Quit: " R DJX:DTIME
 I $Y>22 S DJSV=V D N^DENTDPL S V=DJSV
 X DJCP W !
 Q
F D:$Y>21 CONT I DJX[U D FUNC^DENTDNQ2 Q
 D:$D(^DD(DJDD,DJAT,3)) DEDJHELP I $D(^DD(DJDD,DJAT,4)) W ! X ^(4)
 I DJ4["S",DJ4'["M" D:$Y>21 CONT G:DJX[U END D  D FUNC^DENTDNQ2
 .W !,"CHOOSE FROM:" S DJS=$P(^DD(DJDD,DJAT,0),U,3)
 .F DJK=1:1 S Y=$P(DJS,";",DJK) Q:Y=""  D
 ..S Y="'"_$P(Y,":",1)_"'  FOR "_$P(Y,":",2)
 ..D:$Y>21 CONT Q:DJX[U
 ..W !,Y Q
 .D CONT Q
 D DCS
 I DJ4["P" K DIC("S") S DJDIC=DIC,DJD0=D0,DIC(0)=$S(DJ4["'":"MEQZ",1:"MEQZL"),DIC=+$P(DJ4,"P",2) D:$Y>21 CONT X:$P(^DD(DJDD,DJAT,0),U,2)["*" ^(12.1) D ^DENTDC K DIC S DIC=DJDIC,D0=DJD0 G:Y<0 R1 S V(V)=$P(Y,U,2) G P1
 I DJ4["D" S:'$D(%DT) %DT="E" D HELP
 I DJ4["N" D FUNC^DENTDNQ2
 K DJS,DJZ1 I $Y>23 X DJCL R "Press <RETURN> to Continue",X:DTIME S DJZ=V D N^DENTDPL S V=DJZ Q
 S @$P(DJJ(V),U,2) X XY W V(V) Q
P D DCS
 K DIC("S") S DJDIC=DIC,DJD0=D0,DIC(0)=$S(DJ4["'":"MEQZ",1:"MEQZL"),DIC=+$P(DJ4,"P",2) X DJCP X:$P(^DD(DJDD,DJAT,0),U,2)["*" ^(12.1) D ^DENTDC K DIC S:+Y>0 V(V)=$E($P(Y,U,2),1,+DJJ(V)),DIC=DJDIC,D0=DJD0 G:Y<0 R1
P1 S X=+Y,V(V)=$E($P(Y,U,2),1,+DJJ(V)),(DIE,DIC)=DJDIC,DA=DJDN,DR=DJ3_"////"_X X DJCP W ! D ^DIE S D0=DJD0 K DJD0 D PP S V(V)=$E(V(V),1,+DJJ(V))
 S YMLH=$O(^DENT(697.3,DJN,1,"A",V,0)) S:YMLH="" YMLH=-1
 X:$D(^DENT(697.3,DJN,1,YMLH,1)) ^(1)
 I $Y>23 S DJZ=V D N^DENTDPL S V=DJZ Q
 S DY=17,DX=0 X XY W DJEOP S @$P(DJJ(V),U,2) X XY W DJHIN X XY W V(V),DJLIN D FUNC^DENTDNQ2 Q
END ; Exit and Display Commands
 D FUNC^DENTDNQ2 Q
R1 K DJD0 S DIC=DJDIC
 S @$P(DJJ(V),U,2) X XY W:$D(V(V)) DJHIN W:$D(V(V)) V(V) X XY D FUNC^DENTDNQ2 Q
HELP D:$Y>21 R W !,"EXAMPLES OF VALID DATES:"
 D:$Y>21 R W !,"  JAN 22 1957 or 22 JAN 57 or 1/22/57 or 012257"
 D:$Y>21 R W !,"  T (FOR TODAY), T+1 (FOR TOMORROW), T+2, T+7, etc."," T-1 (FOR YESTERDAY)"
 D:$Y>21 R W !,"  T-3W (3 WEEKS AGO), etc."
 D:$Y>21 R W !,"IF THE YEAR IS OMITTED, THE COMPUTER USES THE CURRENT YEAR",!
 D:$Y>21 R I %DT'["X" W "YOU MAY OMIT THE PRECISE DAY, AS:  JAN, 1957",!
 D:$Y>21 R I %DT["T" W "FOLLOW DATE WITH TIME, AS:  JAN 22@10,    T@10PM,   ETC."
 Q
R X DJCL W "Press <RETURN> to Continue" R DJX:10 X DJCP
PP S DJZ=+$P($P(^DD(DJDD,DJAT,0),"^",2),"P",2) Q:$P(^DD(DJZ,.01,0),"^",2)'["P"
P11 I $D(@("^"_$P(^DD(DJZ,.01,0),U,3)_"V(V),0)")) S V(V)=$P(^(0),U,1)
 S DJZ=+$P($P(^DD(DJZ,.01,0),"^",2),"P",2) Q:$P(^DD(DJZ,.01,0),"^",2)'["P"  G P11
DCS S DJNODE=.01 Q:DJ4'["P"
 S DJ44=$S(DJ4'["'":DJ4,1:$P(DJ4,"P",2))
 Q:'$D(^DD(+DJ44,0,"UP"))  I $D(^DD(^DD(+DJ44,0,"UP"),0,"UP")) S DJ44=^DD(+DJ44,0,"UP"),DJNODE=$P(DJJ(V),U,3)
 I DJNODE=.01,$D(^DD(DJDD,DJ3,12.1)) X ^(12.1) G DCS1
 K DIC("S") Q:'$D(^DD(+DJ44,DJNODE))  S:'$D(DIC(0)) DIC(0)=""
 I $D(^DD(+DJ44,DJNODE,12.1)) X ^(12.1)
 E  K DJNODE,DJ44 Q
DCS1 S:'$D(DIC(0)) DIC(0)="" K:DIC(0)'="" DIC("S") K DJNODE,DJ44 Q
DEDJHELP ;
 S DECNT=0,DEDJHELP(1)=^DD(DJDD,DJAT,3)
 D DEDJHEL1
 R !,"Press <RETURN> to Continue: ",DEXRET:DTIME
 X DJCP
 K DEDJHELP,DECNT,DEXRET
 Q
DEDJHEL1 ;
 S DECNT=DECNT+1
 Q:'$D(DEDJHELP(DECNT))
 F I=79:-1:1 Q:$E(DEDJHELP(DECNT),I)=" "
 I $L(DEDJHELP(DECNT))>79 S DEDJHELP(DECNT+1)=$E(DEDJHELP(DECNT),I+1,$L(DEDJHELP(DECNT))),DEDJHELP(DECNT)=$E(DEDJHELP(DECNT),1,I-1)
 W !,DEDJHELP(DECNT)
 G DEDJHEL1
