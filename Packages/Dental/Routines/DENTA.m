DENTA ;ISC2/SAW,HAG-DENTAL SERVICE REPORTS OPTIONS ;10/23/90  14:37 ;
 ;;1.2;DENTAL;**8,24**;JAN 26 1989
 S %O="OPT"
 D:'$D(DT) DT^DICRW S U="^",S=";",O=$T(@(%O)),DENTV=$$VERSION^XPDUTL("DENT") I $D(^DOPT($P(O,S,5),"VERSION")),(DENTV=^DOPT($P(O,S,5),"VERSION")) G IN
 K ^DOPT($P(O,S,5))
 F I=1:1 Q:$T(@(%O)+I)=""  S ^DOPT($P(O,S,5),I,0)=$P($T(@(%O)+I),S,3),^DOPT($P(O,S,5),"B",$P($P($T(@(%O)+I),S,3),"^",1),I)=""
 S K=I-1,^DOPT($P(O,S,5),0)=$P(O,S,4)_U_1_U_K_U_K K I,K,X S ^DOPT($P(O,S,5),"VERSION")=DENTV
IN I $P(O,S,6)'="" D @($P(O,S,6))
PR S O=$T(@(%O)),S=";" S IOP=$I D ^%ZIS W:IOST'["PK-" @IOF K IOP
 I $P(O,S,7)'="" D @($P(O,S,7))
 E  W !!,$P(O,S,3),":",!,$$VERSION^XPDUTL("DENT")," ",$P($T(+1),S,1),!!,$P(O,S,4),"S:",!
 F J=1:1 Q:'$D(^DOPT($P(O,S,5),J,0))  S K=$S(J<10:15,1:14) W !,?K,J,". ",$P(^DOPT($P(O,S,5),J,0),U,1)
RE W ! S DIC("A")="Select "_$P($T(OPT),S,4)_": EXIT// ",DIC="^DOPT("_""""_$P($T(OPT),S,5)_""""_",",DIC(0)="AEQMN" D ^DIC G:X=""!(X=U) EXIT G:Y<0 RE K DIC,J,O D @($P($T(OPT+Y),S,4)) G PR
ADM S X1=3,X3=223,X4="I $P(^(0),""^"",29)=DENTSTA" D REL G EXIT:Y<0,ADM
PERS S X1=4,X3=224,X4="I $P(^(0),""^"",10)=DENTSTA" D REL G EXIT:Y<0,PERS
FEE S X1=5,X3=222,X4="I $P(^(0),""^"",28)=DENTSTA" D REL G EXIT:Y<0,FEE
REL W !! S U="^",Z1=0 G:'$D(^DENT(225,0)) W F Z3=0:1:2 S Z1=$O(^(Z1)) Q:Z1'>0  S Z2=Z1
 G:Z3=0 W I Z3>1 S DIC="^DENT(225,",DIC(0)="AEMNQ",DIC("A")="Select STATION.DIVISION: " S:$D(DENTSTA) DIC("B")=$S(DENTSTA[" ":+DENTSTA,1:DENTSTA) D ^DIC Q:Y<0  K DIC("A"),DIC("B")
 S Z=$S(Z3=1:Z2,1:+Y) S DENTSTA=$P(^DENT(225,Z,0),U,1) I DENTSTA="" S Y=-1 G W
 S DIC="^DENT("_X3_",",DIC(0)="AEMQZ",DIC("S")=X4 D ^DIC Q:Y<0  K DIC("S")
 S Z=$P(Y(0),U,1),Z1=1700+$E(Z,1,3),Z=+$E(Z,4,5)+2,Z=$P($T(DATE),";",Z),Z1=Z_" "_Z1,DENTY0=Y(0)
 S X2="^DENTA"_X1,%ZIS="MQ" K IO("Q") D ^%ZIS G EXIT:IO=""
 I $D(IO("Q")) S ZTRTN="QUE^DENTA",ZTSAVE("DENTSTA")="",ZTSAVE("X2")="",ZTSAVE("DENTY0")="",ZTSAVE("Z1")="",ZTSAVE("U")="" D ^%ZTLOAD K ZTSK,ZTRTN,ZTSAVE G CLOSE
QUE U IO D @X2
 G CLOSE
W W !!,"Stations have not been entered in the Dental Site Parameter file.",!,"You must enter a station before you can use this option" S Y=-1 Q
DATE ;;JANUARY;FEBRUARY;MARCH;APRIL;MAY;JUNE;JULY;AUGUST;SEPTEMBER;OCTOBER;NOVEMBER;DECEMBER
CLOSE X ^%ZIS("C") S Y=1
EXIT K %,DENT,DENT1,DENT2,DENTSD1,DENTSTA,DENTV,DENTY0,DIC,I,S,J,O,K,X,X1,X2,X3,X4,Z,Z1,Z2,Z3 K:$D(ZTSK) ^%ZTSK(ZTSK),ZTSK Q
OPT ;;SERVICE REPORTS;OPTION;DENTA
 ;;TREATMENT DATA SERVICE REPORT MENU;^DENTA1
 ;;CLASS I-VI ADMIN INFO (TYPE 3);ADM
 ;;PERSONNEL INFO (TYPE 4);PERS
 ;;NON CLINICAL TIME BY PROVIDER;^DENTA4A
 ;;APPLICATIONS AND DENTAL FEE (TYPE 5);FEE
