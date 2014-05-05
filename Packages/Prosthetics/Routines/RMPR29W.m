RMPR29W ;PHX/JLT/HNB-DISPLAY JOB RECORD HDR AND ITEM [ 11/04/94  10:07 AM ]
 ;;3.0;PROSTHETICS;;Feb 09, 1996
GET(PRDA) ;INFOR FOR JOB SECTION
 K RMPRWO
 S DFN=$P(^RMPR(664.1,PRDA,0),U,2),SRC=$P(^(0),U,11)
 D KVAR^VADPT,DEM^VADPT,ELIG^VADPT
 K DIQ,TMP,TECH,^UTILITY($J,"TEXT"),^UTILITY("DIQ1",$J)
 S DIC="^RMPR(664.1,"
 S DA=PRDA,DR=".02;4;12;12.1;12.2;12.3;12.4;13;15;19"
 D EN^DIQ1
 S RI=0
 K RCK
 F  S RI=$O(^RMPR(664.1,PRDA,2,RI)) Q:RI'>0  Q:'$D(^RMPR(664.1,PRDA,2,RI,0))  D
 .S DA=PRDA,DIC="^RMPR(664.1,"
 .S DR="6",DR(664.16)=".01;2;3;8;10",DA(664.16)=RI
 .S DA660=$P(^RMPR(664.1,PRDA,2,RI,0),U,5),TYPE=$P(^(0),U,7)
 .I +DA660 S RMPRWO=$O(^RMPR(664.2,"C",DA660,0))
 .Q:'+$G(RMPRWO)
 .S RMPRJOB=$P(^RMPR(664.2,RMPRWO,0),U,4)
 .S RCK(RMPRJOB)=$$ITM1^RMPR31U(+$P(^RMPR(664.1,PRDA,2,RI,0),U))_U_DA660_U_RMPRWO_U_RI_U_TYPE
 .D EN^DIQ1
 I '$D(RCK) Q
 K DR S RI=0
 F  S RI=$O(RCK(RI)) Q:RI'>0  D
 .S DIC="^RMPR(664.2,",DR="4;5;8;9;10;11;12"
 .S (RMPRWO,DA)=$P(RCK(RI),U,3)
 .S DIQ(0)="IE" D EN^DIQ1 K DIQ
 .K DR S DA=RMPRWO,RJ=0
 .F  S RJ=$O(^RMPR(664.2,DA,1,RJ)) Q:RJ'>0  D
 ..S DIQ="TMP("_DA_",",DA(664.22)=RJ
 ..S DR="2",DR(664.22)=".01;1;2;3;4;6;10;11"
 ..D EN^DIQ1 K DIQ
 ..I $P($G(^RMPR(664.2,DA,1,RJ,0)),U,11) S TMP(DA,664.22,RJ,3)="P"
 .D WP
 S RI=0
 F  S RI=$O(RCK(RI)) Q:RI'>0  D
 .S RDA=$P(RCK(RI),U,2),RMPRWO=$P(RCK(RI),U,3)
 .Q:'+RDA
 .S DA=0
 .F  S DA=$O(^RMPR(664.3,"C",RDA,DA)) Q:DA'>0  I $D(^RMPR(664.3,DA,0)) S RMPRDT=$P(^(0),U) D
 ..K DR S RT=0
 ..F  S RT=$O(^RMPR(664.3,DA,1,RT)) Q:RT'>0  D
 ...S DIC="^RMPR(664.3,"
 ...;DIQ array should start with DI
 ...S DIQ(0)="IE",DIQ="TECH("_RMPRWO_","_RMPRDT_","
 ...S DA(664.33)=RT,DR="3",DR(664.33)=".01;1;2"
 ...D EN^DIQ1 K DIQ
 Q
 ;see internal notes
EXIT ;common exit
 K DA,DA660,DFN,DIRUT,DIWF,DIWL,DTOUT,PAGE,PDA,PRDA,RMPRBACK
 K RMPRDA,RMPRDFN,RMPRDIR3,RMPRDIR7,RMPRDT,RMPRJOB
 K RMPRWO,XRC,VADM,VAEL,TYPE,RDA,RJ,RT,RWP,RR Q
 Q
HDR(PRDA) ;DISPLAY JOB RECORD HEADER
 ;
 S PAGE=PAGE+1
 W @IOF,!,?31,"JOB RECORD SECTION",?65,"PAGE:"
 W ?72,PAGE,!,?19,"(To be completed by VA Shop or Clinic only)"
 W !,"VETERAN",?25,"CLAIM #",?37,"WARD",?52,"SSN"
 W ?64,"WORK ORDER #",!,RMPR("L")
 W !,"|"_^UTILITY("DIQ1",$J,664.1,PRDA,.02)
 W ?24,"|"_$P($G(VAEL(7)),U),?36,"|"_^UTILITY("DIQ1",$J,664.1,PRDA,12)
 W ?51,"|"_$P(VADM(2),U)
 W ?64,"|"_^UTILITY("DIQ1",$J,664.1,PRDA,4),!,RMPR("L")
 W !,"|DATE ASSIGNED:",?16,^UTILITY("DIQ1",$J,664.1,PRDA,19)
 W ?36,"|ASSIGNED TO: ",^UTILITY("DIQ1",$J,664.1,PRDA,15),!,RMPR("L")
 I ^UTILITY("DIQ1",$J,664.1,PRDA,12)'="" W !,"|PHYSICIAN:",?16,^(12.1),?42,"|DIAGNOSIS:",?57,^(12.2),!,?0,"|TREATING SPEC:",?16,^(12.3),?42,"|EXT:",?57,^(12.4),!,RMPR("L")
 Q
 ;
WP ;use DIWP to print REMARKS word processing field
 ;
 K ^UTILITY($J,"W") S RWP=0,RW=0
 F  S RW=$O(^UTILITY("DIQ1",$J,664.2,RMPRWO,12,RW)) Q:RW'>0  D
 .S X=^(RW)
 .S DIWF="R",DIWL=1,DIWR=79
 .D ^DIWP
 .S RR=0
 .F  S RR=$O(^UTILITY($J,"W",DIWL,RR)) Q:RR'>0  D
 ..S RWP=RWP+1,^UTILITY($J,"TEXT",RMPRWO,RWP)=^(RR,0)
 ..K ^UTILITY($J,"W")
 Q
HD ;print header
 W @IOF
 W ?10,"REQUEST AND RECEIPT FOR PROSTHETIC APPLIANCES OR SERVICES"
 W ?70,"PAGE:",?77,PAGE,!,?34,"(Section I)"
 W !,"VETERAN",?25,"WORK ORDER #",?44,"VENDOR",?60,"REQUESTOR"
 W !,"|"_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,.02),1,20)
 W ?24,"|"_^UTILITY("DIQ1",$J,664.1,RMPRDA,4)
 W ?43,"|"_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,2),1,15),?59,"|"_$E(^(13),1,20)
 W !,"|ORDERING STATION: ",?25,^UTILITY("DIQ1",$J,664.1,RMPRDA,.04)
 W !,RMPR("L"),!,"|AUTHORITY: CFR 17.115",?42,"|DATE REQUIRED:"
 W ?57,^UTILITY("DIQ1",$J,664.1,RMPRDA,.09)
 W !,RMPR("L"),!,"|ASSIGNED TO:"
 W ?16,^UTILITY("DIQ1",$J,664.1,RMPRDA,15),?42,"|DATE ASSIGNED:"
 W ?57,^UTILITY("DIQ1",$J,664.1,RMPRDA,19),!,RMPR("L")
 Q
 ;
HDC ;print header of mult. page
 W @IOF
 W ?10,"REQUEST AND RECEIPT FOR PROSTHETIC APPLIANCES OR SERVICES"
 W ?70,"PAGE:",?77,PAGE,!,?34,"(Section I)"
 W !,"VETERAN",?44,"VENDOR",?60,"REQUESTOR"
 W !,"|"_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,.02),1,20)
 W ?43,"|"_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,2),1,15)
 W ?59,"|"_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,13),1,20),!,RMPR("L")
 W !,"|TO: "_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,.11),1,30)
 W ?42,"ORDERING STATION: "_$E(^UTILITY("DIQ1",$J,664.1,RMPRDA,.04),1,20),RMPR("L")
 W !,"|AUTHORITY: CFR 17.115",?42,"|DATE REQUIRED:",?57,^(.09)
 W !,RMPR("L")
 Q
 ;
HELP ;DISPLAY HELP FOR SCREENS
 ;
 N RMPR90DP,RMPR90I
 W !
 S RMPR90DP=$P(DIR(0),U,2,999)
 F RMPR90I=1:1:6 I $P($P(RMPR90DP,";",RMPR90I),":",1)'="" W:RMPR90I=4 ! W "("_$P($P(RMPR90DP,";",RMPR90I),":",1)_") "_$P($P(RMPR90DP,";",RMPR90I),":",2)_"  "
 W !
 Q
 ;
DIS(RMPRDFN,PDA) ;GET DISABILITY CODES PASS RMPRDFN AND PDA
 ;
LK ;do a lookup on 2529-3 record patient/disability code
 K DIR S DIR(0)=$S($O(^RMPR(664.1,RMPRDA,1,0)):"FO",1:"F")
 S DIR("A")="Select 2529-3 DISABILITY CODE"
 S DIR("?")="^D DSP^RMPR29W"
 D ^DIR Q:$D(DTOUT)!($D(DIRUT))
 K DIC
 S DIC("W")="S RA=^(0) D LP^RMPRDIS"
 S DIC("S")="I '$P(^(0),U,10)"
 S DIC="^RMPR(665,"_RMPRDFN_",1,"
 S DIC("P")="665.01IP",DIC(0)="EQMZ"
 D ^DIC G:+Y'>0 LK
 Q
 ;
DSP ;DISPLAY DISABILITY CODES
 ;see internal notes 6/23/95
 ;Q:'$D(^RMPR(664.1,PDA,1,0))
 D LP^RMPRDIS
 ;W !!,?5,"Select 2529-3 DISABILITY CODE",!
 S RI=0
 F  S RI=$O(^RMPR(664.1,PDA,1,RI)) Q:RI'>0  I $D(^(RI,0)) S RT=^(0) W !,?5,$P($G(^RMPR(662,+RT,0)),U),?15,$S($P(RT,U,2)=1:"SC ",1:"NSC ")
 K RI
 W !
 S (RMPRDIR7,RMPRDIR3,RMPRBACK)=1 W !! D EN^RMPRDIS
 Q
