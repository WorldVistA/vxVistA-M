ABSVLS1 ;VAMC ALTOONA/CTB - SCREEN SAVER  ;4/13/00  11:52 AM
V ;;4.0;VOLUNTARY TIMEKEEPING;**7,15,17,18**;JULY 6, 1994
 N DX,DY,ABSVXX,POP,I,N,X1,X2,X3,X4,X5,Z,X,Y,%
CL K %,X S IOP=0,(%("EOL"),XY)="" D ^%ZIS I POP D HOME^%ZIS QUIT
 I $D(^%ZOSF("TYPE-AHEAD")) X ^%ZOSF("TYPE-AHEAD")
 I $D(IOST(0)),IOST(0)'="" S:$D(^%ZIS(2,IOST(0),1)) XY=$P(^(1),"^",5)_" "_^%ZOSF("XY") S:$D(^%ZIS(2,IOST(0),5)) %("EOL")=$P(^(5),"^",6)
 D NOW^ABSVQ S X=+$P(%,".",2),X=$S(X<1200:33,X>1700:35,1:34)
 S X(1)=$$GET^ABSVU1(X,ABSVDL)_", "_$$GET^ABSVU1("WELCOME",ABSVDL),X(2)=$$GET^ABSVU1("VA MEDICAL CENTER",ABSVDL),X(3)="  ",X(4)=$$GET^ABSVU1("PRESS ANY KEY",ABSVDL) D MSG
RUN S DX=$R($S((79-X2)>1:79-X2,1:1)),DY=$R(23-X5) W @IOF X XY S N=0 F I=1:1 S N=$O(X(N)) Q:'N  W ?DX,X(N),!
 ;W ?(DX+($L(X(1))\2)) F Z=1:1:30 H 1 R X:0 I $T!(X]"") K Z Q
 W ?(DX+($L(X(1))\2)) R *X:30 I $T!(X>0) K Z
 I $$HALT^ABSVLS K ^ABS("ABSVKILL",ABSV("SITE"),IO) W @IOF,"VOLUNTARY SERVICE PROGRAM TERMINATED" G H^XUS
 Q:'$D(Z)  G RUN
 W @IOF Q
MSG S N=0,X2=30,$P(X3," ",80)="",X5=4 F I=1:1 G:'$D(ABSV("INST")) X K X(3),X(4) S N=$O(^ABS(503338,ABSV("INST"),1,N)) Q:'N  I $D(^(N,0)) S X1(I)=^(0)
 Q:'$D(X1)  S N=0 F I=1:1 S N=$O(X1(N)) Q:N=""  S:$L(X1(N))>X2 X2=$L(X1(N))
 F I=2:1 S X4=$O(X1(0)) Q:X4=""  S X(I)=X1(X4) K X1(X4)
 S X(I+1)="",X(I+2)=$$GET^ABSVU1("PRESS ANY KEY",ABSVDL),X5=I+2
X S N=0 F I=1:1 S N=$O(X(N)) Q:'N  S Z=$L(X(N)),Z=X2-Z\2,Z=$E(X3,1,Z),X(N)=Z_X(N)
 Q
