PSNNDC ;BIR/WRT-auto set of translation file if ndc in national file ; 10/27/98 13:47
 ;;4.0; NATIONAL DRUG FILE;; 30 Oct 98
 D:$D(XRTL) T0^%ZOSV ; START
FIRST D CHECK,SECOND
 Q
CHECK I $P(^PS(59.7,1,10),"^",3)=0 W !!,"You must use the Conversion Rematch option first before using this option.",! Q
SECOND I $P(^PS(59.7,1,10),"^",3)=1 F PSNB=0:0 S PSNB=$O(^PSDRUG(PSNB)) Q:'PSNB  U IO W:'(PSNB#100) "." D STRT0
 Q
STRT0 Q:'$D(^PSDRUG(PSNB,0))
 S PSNP=$G(^PSDRUG(PSNB,"I")) I PSNP,PSNP<DT Q
 I $D(^PSDRUG(PSNB,"ND")),$P(^PSDRUG(PSNB,"ND"),"^",2)]"" Q
 Q:'$D(^PSDRUG(PSNB,2))  Q:$P(^PSDRUG(PSNB,2),"^",4)'?1.6N1"-"1.4N1"-"1.2N  S ANS=$P(^PSDRUG(PSNB,2),"^",4) I $D(^PSNTRAN(PSNB,0)) Q:$P(^PSNTRAN(PSNB,0),"^",2)]""
NDC F VV=1:1:3 S VV1=$S(VV=1:6,VV=2:4,VV=3:2) D NDCSET
 S ANS=$P(ANS,"-",1)_$P(ANS,"-",2)_$P(ANS,"-",3) K VV,VV1
 I '$D(^PSNDF(50.67,"NDC",ANS)) K ANS S NOM31=1 Q
 S PSNIEN=$O(^PSNDF(50.67,"NDC",ANS,0)),PSNFNM=$P(^PSNDF(50.67,PSNIEN,0),"^",6),PSNSIZE=$P(^PSNDF(50.67,PSNIEN,0),"^",8),PSNTYPE=$P(^PSNDF(50.67,PSNIEN,0),"^",9)
 I $D(^PSNDF(50.68,PSNFNM,7)) S PSNPD=$P(^PSNDF(50.68,PSNFNM,7),"^",3) I PSNPD]"",PSNPD<DT Q
 S PSNNDF=$P(^PSNDF(50.68,PSNFNM,0),"^",2),PSNFORM=$P(^PSNDF(50.68,PSNFNM,0),"^"),PSNCLASS=$P(^PSNDF(50.68,PSNFNM,3),"^") D ASKIT^PSNHIT
 Q
NDCSET I $L($P(ANS,"-",VV))<VV1 S $P(ANS,"-",VV)=$E("0000000",1,VV1-$L($P(ANS,"-",VV)))_$P(ANS,"-",VV)
 Q
SET S:'$D(^PSNTRAN(PSNB,0)) $P(^PSNTRAN(0),"^",4)=($P(^PSNTRAN(0),"^",4))+1,$P(^PSNTRAN(0),"^",3)=PSNB
 S ^PSNTRAN(PSNB,0)=PSNNDF_"^"_PSNFNM_"^"_PSNCLASS_"^^"_PSNSIZE_"^^"_PSNTYPE_"^"_DUZ
 Q
STRT1 Q:'$D(^PSDRUG(PSNB,2))  Q:$P(^PSDRUG(PSNB,2),"^",4)'?1.6N1"-"1.4N1"-"1.2N  S ANS=$P(^PSDRUG(PSNB,2),"^",4) I $D(^PSNTRAN(PSNB,0)) Q:$P(^PSNTRAN(PSNB,0),"^",2)]""
NDC1 F VV=1:1:3 S VV1=$S(VV=1:6,VV=2:4,VV=3:2) D NDCSET
 S ANS=$P(ANS,"-",1)_$P(ANS,"-",2)_$P(ANS,"-",3) K VV,VV1
 I '$D(^PSNDF(50.67,"NDC",ANS)) K ANS S NOMSYN=1 Q
 S PSNIEN=$O(^PSNDF(50.67,"NDC",ANS,0)),PSNFNM=$P(^PSNDF(50.67,PSNIEN,0),"^",6),PSNSIZE=$P(^PSNDF(50.67,PSNIEN,0),"^",8),PSNTYPE=$P(^PSNDF(50.67,PSNIEN,0),"^",9)
 I $D(^PSNDF(50.68,PSNFNM,7)) S PSNPD=$P(^PSNDF(50.68,PSNFNM,7),"^",3) I PSNPD]"",PSNPD<DT Q
 S PSNNDF=$P(^PSNDF(50.68,PSNFNM,0),"^",2),PSNFORM=$P(^PSNDF(50.68,PSNFNM,0),"^"),PSNCLASS=$P(^PSNDF(50.68,PSNFNM,3),"^") D ASKIT1^PSNHIT
 Q
SYN S PPQ=0 I $D(^PSNTRAN(PSNB,0)) Q:$P(^PSNTRAN(PSNB,0),"^",2)]""
 W !,?5,"I will attempt to match the NDCs from your SYNONYMS.",!
 I '$O(^PSDRUG(PSNB,1,0)) S NOMSYN=1
 I $O(^PSDRUG(PSNB,1,0)) F XXX=0:0 S XXX=$O(^PSDRUG(PSNB,1,XXX)) Q:'XXX  S ANS=$P(^PSDRUG(PSNB,1,XXX,0),"^",2) I ANS?1.6N1"-"1.4N1"-"1.2N Q:$P($G(^PSNTRAN(PSNB,0)),"^",2)]""  S TT=0,TTT=1 D NDC1 Q:DUNCE
