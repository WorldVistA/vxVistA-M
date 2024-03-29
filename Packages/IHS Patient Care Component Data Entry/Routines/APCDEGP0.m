APCDEGP0 ; IHS/CMI/TUCSON - CONT. APCDEGP ; [ 07/24/01  5:03 PM ]
 ;;2.0;IHS RPMS/PCC Data Entry;**2,5**;MAR 09, 1999
POV1 ;EP;
 W !
 K APCDLOOK,APCDTNQP,APCDTERR,APCDTSKI,X
 D ^APCDAPOV
 I $D(APCDTSKI) S APCDPOV="" Q
 I $D(APCDTERR) G POV1
 S APCDPOV=APCDLOOK
 K APCDLOOK
 K DA,DIR,DIRUT,DUOUT,DTOUT,X,Y S DIR("A")="PROVIDER NARRATIVE",DIR(0)="9000010.07,.04" D ^DIR K DIR
 G:$D(DIRUT) POV1
 S APCDEGC=APCDEGC+1
 S ^TMP("APCDEGP",$J,"POV",APCDEGC,"APCDTPOV")=APCDPOV_U_"`"_+Y_U_2
 Q
PROV1 ;EP
 X:$D(^DD(9000010.06,.01,12.1)) ^DD(9000010.06,.01,12.1)
 S DIC=$S($P(^DD(9000010.06,.01,0),U,2)[200:"^VA(200,",1:"^DIC(6,"),DIC(0)="AEMQ",DIC("A")=$S('APCDEGPS:"Enter PRIMARY Provider.: ",1:"Enter SECONDARY Provider: ") D ^DIC K DIC ;IHS/CMI/LAB - added file 200 check
 I Y=-1 S APCDEGPR="" Q
 S APCDEGPR=+Y
 I APCDEGPS S APCDEGPC="S" G PROV11
 S APCDEGPS=1,APCDEGPC="P"
PROV11 S APCDEGC=APCDEGC+1
 S ^TMP("APCDEGP",$J,"PROV",APCDEGC,"APCDTPRV")="`"_APCDEGPR_U_APCDEGPC
 Q
CHECK ;EP;SEE IF PV AND PRO ENTERED CORRECTLY
 Q:$D(APCDMOD)
 S APCDMPQ=1
 I $P(^AUPNVSIT(APCDVSIT,0),U,7)="E" Q
 K APCDNOCL D ^APCDVCHK
 I APCDMODE'="M",'$D(^AUPNVPOV("AD",APCDVSIT)) W !,"PV mnemonic required!",!,APCDBEEP S:'$D(DTOUT) APCDMPQ=0 Q
 I APCDMODE'="M",'$D(^AUPNVPRV("AD",APCDVSIT)) W !,"PRV mnemonic required!",!,APCDBEEP S:'$D(DTOUT) APCDMPQ=0 Q
 I APCDMODE'="M",$D(APCDNOCL) W !,"CL mnemonic required!",!,$C(7) S:'$D(DTOUT) APCDMPQ=0 K APCDNOCL Q
 I APCDMODE'="M",$P(^AUPNVSIT(APCDVSIT,0),U,3)="C",'$D(^AUPNVCHS("AD",APCDVSIT)) W !,"CHA, CHH or CHI mnemonic required with Contract Visits!",$C(7) S:'$D(DTOUT) APCDMPQ=0 Q
 D DEDT^APCDEA2(APCDVSIT) I $P(APCDPARM,U,5)="Y",'$D(^APCDFORM("AB",APCDVSIT)) S APCDFV=APCDVSIT D ^APCDFORM K APCDFV
 Q
