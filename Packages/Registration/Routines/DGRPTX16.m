DGRPTX16 ; ;03/19/13
 D DE G BEGIN
DE S DIE="^DPT(",DIC=DIE,DP=2,DL=1,DIEL=0,DU="" K DG,DE,DB Q:$O(^DPT(DA,""))=""
 I $D(^(.22)) S %Z=^(.22) S %=$P(%Z,U,1) S:%]"" DE(16)=%
 I $D(^(.33)) S %Z=^(.33) S %=$P(%Z,U,1) S:%]"" DE(6)=% S %=$P(%Z,U,2) S:%]"" DE(8)=% S %=$P(%Z,U,3) S:%]"" DE(9)=% S %=$P(%Z,U,4) S:%]"" DE(11)=% S %=$P(%Z,U,5) S:%]"" DE(13)=% S %=$P(%Z,U,6) S:%]"" DE(14)=% S %=$P(%Z,U,7) S:%]"" DE(15)=%
 I  S %=$P(%Z,U,10) S:%]"" DE(1)=%
 K %Z Q
 ;
W W !?DL+DL-2,DLB_": "
 Q
O D W W Y W:$X>45 !?9
 I $L(Y)>19,'DV,DV'["I",(DV["F"!(DV["K")) G RW^DIR2
 W:Y]"" "// " I 'DV,DV["I",$D(DE(DQ))#2 S X="" W "  (No Editing)" Q
TR R X:DTIME E  S (DTOUT,X)=U W $C(7)
 Q
A K DQ(DQ) S DQ=DQ+1
B G @DQ
RE G PR:$D(DE(DQ)) D W,TR
N I X="" G NKEY:$D(^DD("KEY","F",DP,DIFLD)),A:DV'["R",X:'DV,X:D'>0,A
RD G QS:X?."?" I X["^" D D G ^DIE17
 I X="@" D D G Z^DIE2
 I X=" ",DV["d",DV'["P",$D(^DISV(DUZ,"DIE",DLB)) S X=^(DLB) I DV'["D",DV'["S" W "  "_X
T G M^DIE17:DV,^DIE3:DV["V",P:DV'["S" X:$D(^DD(DP,DIFLD,12.1)) ^(12.1) I X?.ANP D SET I 'DDER X:$D(DIC("S")) DIC("S") I  W:'$D(DB(DQ)) "  "_% G V
 K DDER G X
P I DV["P" S DIC=U_DU,DIC(0)=$E("EN",$D(DB(DQ))+1)_"M"_$E("L",DV'["'") S:DIC(0)["L" DLAYGO=+$P(DV,"P",2) G:DV["*" AST^DIED D NOSCR^DIED S X=+Y,DIC=DIE G X:X<0
 G V:DV'["N" D D I $L($P(X,"."))>24 K X G Z
 I $P(DQ(DQ),U,5)'["$",X?.1"-".N.1".".N,$P(DQ(DQ),U,5,99)["+X'=X" S X=+X
V D @("X"_DQ) K YS
Z K DIC("S"),DLAYGO I $D(X),X'=U D:$G(DE(DW,"INDEX")) SAVEVALS G:'$$KEYCHK UNIQFERR^DIE17 S DG(DW)=X S:DV["d" ^DISV(DUZ,"DIE",DLB)=X G A
X W:'$D(ZTQUEUED) $C(7),"??" I $D(DB(DQ)) G Z^DIE17
 S X="?BAD"
QS S DZ=X D D,QQ^DIEQ G B
D S D=DIFLD,DQ(DQ)=DLB_U_DV_U_DU_U_DW_U_$P($T(@("X"_DQ))," ",2,99) Q
Y I '$D(DE(DQ)) D O G RD:"@"'[X,A:DV'["R"&(X="@"),X:X="@" S X=Y G N
PR S DG=DV,Y=DE(DQ),X=DU I $D(DQ(DQ,2)) X DQ(DQ,2) G RP
R I DG["P",@("$D(^"_X_"0))") S X=+$P(^(0),U,2) G RP:'$D(^(Y,0)) S Y=$P(^(0),U),X=$P(^DD(X,.01,0),U,3),DG=$P(^(0),U,2) G R
 I DG["V",+Y,$P(Y,";",2)["(",$D(@(U_$P(Y,";",2)_"0)")) S X=+$P(^(0),U,2) G RP:'$D(^(+Y,0)) S Y=$P(^(0),U) I $D(^DD(+X,.01,0)) S DG=$P(^(0),U,2),X=$P(^(0),U,3) G R
 X:DG["D" ^DD("DD") I DG["S" S %=$P($P(";"_X,";"_Y_":",2),";") S:%]"" Y=%
RP D O I X="" S X=DE(DQ) G A:'DV,A:DC<2,N^DIE17
I I DV'["I",DV'["#" G RD
 D E^DIE0 G RD:$D(X),PR
 Q
SET N DIR S DIR(0)="SV"_$E("o",$D(DB(DQ)))_U_DU,DIR("V")=1
 I $D(DB(DQ)),'$D(DIQUIET) N DIQUIET S DIQUIET=1
 D ^DIR I 'DDER S %=Y(0),X=Y
 Q
SAVEVALS S @DIEZTMP@("V",DP,DIIENS,DIFLD,"O")=$G(DE(DQ)) S:$D(^("F"))[0 ^("F")=$G(DE(DQ))
 I $D(DE(DW,"4/")) S @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")=""
 E  K @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")
 Q
NKEY W:'$D(ZTQUEUED) "??  Required key field" S X="?BAD" G QS
KEYCHK() Q:$G(DE(DW,"KEY"))="" 1 Q @DE(DW,"KEY")
BEGIN S DNM="DGRPTX16",DQ=1
1 S DW=".33;10",DV="SX",DU="",DLB="E-EMER. CONTACT SAME AS NOK?",DIFLD=.3305
 S DE(DW)="C1^DGRPTX16",DE(DW,"INDEX")=1
 S DU="Y:YES;N:NO;"
 S Y="NO"
 G Y
C1 G C1S:$D(DE(1))[0 K DB
C1S S X="" G:DG(DQ)=X C1F1 K DB
C1F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X1 I $D(X),X="Y" D K1^DGLOCK2
 Q
 ;
2 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=2 D X2 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X2 I X'="Y" S Y=.331
 Q
3 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=3 D X3 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X3 S X=$S($D(^DPT(DA,.21)):^(.21),1:"") S:X'="" ^(.33)=$P(X_"^^^^^^^^^^^",U,1,9)_U_$P(^(.33),U,10)_U_$P(X,U,11)
 Q
4 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=4 D X4 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X4 S:$D(^DPT(DFN,.22)) $P(^(.22),U,1)=$P(^(.22),U,7)
 Q
5 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=5 D X5 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X5 S Y=.33011
 Q
6 D:$D(DG)>9 F^DIE17,DE S DQ=6,DW=".33;1",DV="F",DU="",DLB="E-NAME",DIFLD=.331
 S DE(DW)="C6^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C6 G C6S:$D(DE(6))[0 K DB
 S X=DE(6),DIC=DIE
 X "S DGXRF=.331 D ^DGDDC Q"
C6S S X="" G:DG(DQ)=X C6F1 K DB
 S X=DG(DQ),DIC=DIE
 ;
C6F1 N X,X1,X2 S DIXR=595 D C6X1(U) K X2 M X2=X D C6X1("O") K X1 M X1=X
 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1 D DELCOMP^XLFNAME2(2,.DA,.331,1.07) Q
 K X M X=X2 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1,DG20NAME=X D NARY^XLFNAME7(.DG20NAME),UPDCOMP^XLFNAME2(2,.DA,.331,.DG20NAME,1.07,+$P($G(^DPT(DA,"NAME")),U,7),"CL35") K DG20NAME Q
 G C6F2
C6X1(DION) K X
 S X(1)=$G(@DIEZTMP@("V",2,DIIENS,.331,DION),$P($G(^DPT(DA,.33)),U,1))
 S X=$G(X(1))
 Q
C6F2 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X6 K:$L(X)>35!($L(X)<3) X I $D(X) S DG20NAME=X,(X,DG20NAME)=$$FORMAT^XLFNAME7(.DG20NAME,3,35) K:'$L(X) X,DG20NAME
 I $D(X),X'?.ANP K X
 Q
 ;
7 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=7 D X7 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X7 S:X="" Y="@40"
 Q
8 D:$D(DG)>9 F^DIE17,DE S DQ=8,DW=".33;2",DV="FX",DU="",DLB="E-RELATIONSHIP TO PATIENT",DIFLD=.332
 S DE(DW)="C8^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C8 G C8S:$D(DE(8))[0 K DB
C8S S X="" G:DG(DQ)=X C8F1 K DB
C8F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X8 K:$L(X)>30!($L(X)<2) X I $D(X) S DFN=DA D E1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
9 D:$D(DG)>9 F^DIE17,DE S DQ=9,DW=".33;3",DV="FX",DU="",DLB="E-STREET ADDRESS [LINE 1]",DIFLD=.333
 S DE(DW)="C9^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C9 G C9S:$D(DE(9))[0 K DB
 S X=DE(9),DIC=DIE
 X "S DGXRF=.333 D ^DGDDC Q"
C9S S X="" G:DG(DQ)=X C9F1 K DB
 S X=DG(DQ),DIC=DIE
 ;
C9F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X9 K:X[""""!($A(X)=45) X I $D(X) K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D E1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
10 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=10 D X10 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X10 S:X="" Y=.336
 Q
11 D:$D(DG)>9 F^DIE17,DE S DQ=11,DW=".33;4",DV="FX",DU="",DLB="E-STREET ADDRESS [LINE 2]",DIFLD=.334
 S DE(DW)="C11^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C11 G C11S:$D(DE(11))[0 K DB
 S X=DE(11),DIC=DIE
 X "S DGXRF=.334 D ^DGDDC Q"
C11S S X="" G:DG(DQ)=X C11F1 K DB
 S X=DG(DQ),DIC=DIE
 ;
C11F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X11 K:X[""""!($A(X)=45) X I $D(X) K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D E1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
12 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=12 D X12 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X12 S:X="" Y=.336
 Q
13 D:$D(DG)>9 F^DIE17,DE S DQ=13,DW=".33;5",DV="FX",DU="",DLB="E-STREET ADDRESS [LINE 3]",DIFLD=.335
 S DE(DW)="C13^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C13 G C13S:$D(DE(13))[0 K DB
C13S S X="" G:DG(DQ)=X C13F1 K DB
C13F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X13 K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D E1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
14 D:$D(DG)>9 F^DIE17,DE S DQ=14,DW=".33;6",DV="FX",DU="",DLB="E-CITY",DIFLD=.336
 S DE(DW)="C14^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C14 G C14S:$D(DE(14))[0 K DB
C14S S X="" G:DG(DQ)=X C14F1 K DB
C14F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X14 K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D E1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
15 D:$D(DG)>9 F^DIE17,DE S DQ=15,DW=".33;7",DV="P5'X",DU="",DLB="E-STATE",DIFLD=.337
 S DE(DW)="C15^DGRPTX16",DE(DW,"INDEX")=1
 S DU="DIC(5,"
 G RE
C15 G C15S:$D(DE(15))[0 K DB
C15S S X="" G:DG(DQ)=X C15F1 K DB
C15F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X15 I $D(X) S DFN=DA D E1^DGLOCK2
 Q
 ;
16 D:$D(DG)>9 F^DIE17,DE S DQ=16,DW=".22;1",DV="FOX",DU="",DLB="E-ZIP+4",DIFLD=.2201
 S DQ(16,2)="S Y(0)=Y D ZIPOUT^VAFADDR"
 S DE(DW)="C16^DGRPTX16",DE(DW,"INDEX")=1
 G RE
C16 G C16S:$D(DE(16))[0 K DB
 D ^DGRPTX17
C16S S X="" G:DG(DQ)=X C16F1 K DB
 D ^DGRPTX18
C16F1 S DIEZRXR(2,DIIENS)=$$OREF^DILF($NA(@$$CREF^DILF(DIE)))
 F DIXR=607 S DIEZRXR(2,DIXR)=""
 Q
X16 K:X[""""!($A(X)=45) X I $D(X) S DFN=DA D E1^DGLOCK2 I $D(X) K:$L(X)>15!($L(X)<5) X I $D(X) D ZIPIN^VAFADDR
 I $D(X),X'?.ANP K X
 Q
 ;
17 D:$D(DG)>9 F^DIE17 G ^DGRPTX19
