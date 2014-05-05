APCDEHI2 ; IHS/CMI/TUCSON - HISTORICAL HOSPITALIZATION CONT ; [ 07/24/01  5:03 PM ]
 ;;2.0;IHS RPMS/PCC Data Entry;**5**;MAR 09, 1999
 ;   Generate VISIT, then process MNEMONICS/TEMPLATE
 ;
START ;
 S APCDX=""
 I $D(APCDLPAT),APCDLPAT=APCDPAT,$D(APCDLDAT),APCDLDAT=APCDDATE,$D(APCDLVST),'APCDTPLT D SAMEPAT
 S APCDLPAT=APCDPAT
 S APCDLDAT=APCDDATE
 I APCDX=1 D MNEPROC Q
 Q:APCDX=2
 K APCDLVST,APCDCLN,ZTSK
 I $D(APCDTVST) S APCDTYPE=APCDTTYP,APCDCAT=APCDTCAT,APCDLOC=APCDTLOC K APCDTVST,APCDTTYP,APCDTCAT,APCDTLOC
 S APCDNOXV="" D ^APCDALV K APCDNOXV
 I $D(APCDAFLG)#2,APCDAFLG=2 W $C(7),!,"VISIT date not valid for current patient!",! S APCDFLG=1 Q
 Q:APCDVSIT=""
 S APCDLVST=APCDVSIT
 S DIE="^AUPNPAT(",DR=".16///TODAY",DA=APCDPAT D ^DIE
 S DA=APCDVSIT,DIE="^AUPNVSIT(",DR=".14////"_DT D ^DIE K DIE,DR,DA,DIU,DIV,^AUPNVSIT("APCIS",DT,APCDVSIT)
 ;S DIE="^AUPNVSIT(",DA=APCDVSIT,DR=".13////"_DT D ^DIE K DR,DA,DIE
 S AUPNVSIT=APCDVSIT D MOD^AUPNVSIT
 I AUPNDOB]"" S X2=AUPNDOB,X1=APCDDATE D ^%DTC S AUPNDAYS=X
 K DR S APCDVDSP=APCDVSIT D ^APCDVDSP
 F X="IP","PRV","PV" W !! D GET1
 D MNEPROC
 Q
 ;
MNEPROC ; PROCESS MNEMONICS UNTIL DONE
 S APCDMPQ=0
 F  D GETMNE Q:APCDMPQ
 D GETMNEK
 K APCDMPQ
 Q
 ;
GETMNE ; GET MNEMONIC
 W !
 S DIC="^APCDTKW(",DIC(0)="AEMQ",DIC("A")="MNEMONIC: ",DIC("S")="I $L($P(^(0),U))<5" D ^DIC K DIC("A"),DIC("S")
 I Y<0 D CHECK Q
 S APCDMNE=+Y,APCDMNE("NAME")=$P(Y,U,2)
 K APCDMOD
 D ^APCDEA3
 I $D(APCDEQX) D ^APCDEQX I $D(APCDEQX) S APCDMPQ=1 Q
 I $D(APCDMOD) W !!,"Switching to Modify Mode for ONE Mnemonic ONLY!" S APCDMODE="M",APCDVSIT=APCDLVST,APCDVLK=APCDVSIT D GETMNE K APCDVLK,APCDMOD S APCDMODE="A" W !!,"Switching back to ENTER Mode!" Q
 Q
 ;
GETMNEK ; KILL GETMNE SPECIFIC VARIABLES
 K APCDVSIT,APCDX,APCDEQX
 Q
 ;
CHECK ; SEE IF PV AND PRO ENTERED CORRECTLY
 Q:$D(APCDMOD)
 S APCDMPQ=1
 I $P(^AUPNVSIT(APCDVSIT,0),U,7)="E" Q
 ;K APCDNOCL D ^APCDVCHK
 I APCDMODE'="M",'$D(^AUPNVPOV("AD",APCDVSIT)) W !,"PV mnemonic required!",!,APCDBEEP S:'$D(DTOUT) APCDMPQ=0 Q
 I APCDMODE'="M",'$D(^AUPNVPRV("AD",APCDVSIT)) W !,"PRV mnemonic required!",!,APCDBEEP S:'$D(DTOUT) APCDMPQ=0 Q
 D DEDT^APCDEA2(APCDVSIT) I $P(APCDPARM,U,5)="Y",'$D(^APCDFORM("AB",APCDVSIT)) S APCDFV=APCDVSIT D ^APCDFORM K APCDFV
 Q
 ;
GET1 ;
 W !!
 S DIC="^APCDTKW(",DIC(0)="EMQX" D ^DIC K DIC
 I Y<0 W !!,$C(7),$C(7),X," Mnemonic is Missing - Notify your Supervisor!" K DIC,X Q
 S APCDMNE=+Y,APCDMNE("NAME")=$P(Y,U,2)
 D ^APCDEA3
 Q
 ;
SAMEPAT ; SAME PATIENT
 K DIR,DIRUT,DIROUT
 S APCDX=+^AUPNVSIT(APCDLVST,0),APCDX=$E(APCDX,4,5)_"-"_$E(APCDX,6,7)_"-"_(1700+$E(APCDX,1,3))_$S($P(APCDX,".",2)]"":"@"_$P(APCDX,".",2),1:"")
 W !!,"You have reselected the same patient.",!
 W !,"Last VISIT is ",APCDX,!
 S DIR("A")="Choose",DIR(0)="S^1:Modify last VISIT;2:Append to last VISIT;3:Create new VISIT;4:Quit"
 D ^DIR
 S APCDX=+Y
 I $D(DIRUT) S APCDX=4
 K DIR,DIRUT,DIROUT,DUOUT,DTOUT
 D @("SAMEPAT"_APCDX)
 Q
SAMEPAT1 ;
 W "  Switching to Modify Mode."
 S APCDMODE="M",APCDVSIT=APCDLVST,APCDVLK=APCDVSIT D MNEPROC S APCDMODE="A",APCDX=2 K APCDVLK
 W !!,"Returning to Enter Mode.",!
 Q
SAMEPAT2 ;
 W "  Switching to Append Mode."
 S APCDVSIT=APCDLVST,APCDX=1,APCDAPP=1
 Q
SAMEPAT3 ;
 W "  Creating new VISIT, still in Add Mode."
 S APCDX=3,APCDADD=1
 Q
SAMEPAT4 ;
 W "  Quit",!
 S APCDX=2
 Q
