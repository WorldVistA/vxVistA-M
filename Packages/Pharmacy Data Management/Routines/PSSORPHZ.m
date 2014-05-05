PSSORPHZ ;BIR/RTR-Dosage by Dispense Units for report ;03/24/00
 ;;1.0;PHARMACY DATA MANAGEMENT;**40**;9/30/97
 ;Reference to ^PS(50.607 supported by DBIA 2221
 ;
 S DLOOP=$G(PD)
 Q:'$G(DLOOP)
 ;SET PSSX(1)=-1^DDRUG IS INACTIVE OR NOT APP USE ANYMORE?
 I $P($G(^PSDRUG(DLOOP,"I")),"^")&($P($G(^("I")),"^")'>DT) S PSSX(1)="-1^Drug is inactive" Q
 ;I $P($G(^PSDRUG(DLOOP,2)),"^",3)'[TYPE S PSSX(1)="-1^Drug not marked for application" Q
 S PSSTRN=$P($G(^PSDRUG(DLOOP,"DOS")),"^"),PSSUNITZ=$P($G(^("DOS")),"^",2)
 S PSSUNITX=$S($P($G(^PS(50.607,+$G(PSSUNITZ),0)),"^")'=""&($P($G(^(0)),"^")'["/"):$P($G(^(0)),"^"),1:"")
 S PSSDSE=+$P($G(^PS(50.7,POPD,0)),"^",2),PSSVERB=$P($G(^PS(50.606,PSSDSE,"MISC")),"^"),PSSPREP=$P($G(^("MISC")),"^",3)
 K PSNNN F PSNN=0:0 S PSNN=$O(^PS(50.606,PSSDSE,"NOUN",PSNN)) Q:'PSNN!($D(PSNNN))  S:$P($G(^(PSNN,0)),"^")'="" PSNNN=$P($G(^(0)),"^")
 S (PSSDOSE,PSSUNTS,PSSUDOS)=""
 S PSSUNTS=$P($G(^PS(50.607,+$P($G(^PSDRUG(DLOOP,"DOS")),"^",2),0)),"^")
 S PSSUDOS=$G(PSSUPD)
 S PSSDOSE=PSSUDOS*+PSSTRN
 I $G(PSSTRN)=""!('$G(PSSUNITZ)) D SET D LEAD^PSSORPH Q
 I '$G(PSSDOSE)!('$G(PSSUDOS)) D SET D LEAD^PSSORPH Q
 S DCNT1=1
 D PARN^PSSORPH
 S PSSX(DCNT1)=PSSDOSE_"^"_$S($G(TYPE)="O":$G(PSSUNITZ),1:$G(PSSUNTS))_"^"_PSSUDOS_"^"_DLOOP_"^"_$G(PSSTRN)_"^"_$S($G(PSSNP)'="":$G(PSSNP),1:$G(PSNNN))_"^"_$P($G(^PS(50.606,+$G(PSSDSE),0)),"^")_"^"_$G(PSSVERB)_"^"_$G(PSSPREP) K PSSNP
 S PSSA=1 D SLS^PSSORPH
 S (PSIEN,DLOOP)=+$P(PSSX(PSSA),"^",4) K PSSMAX D:$G(TYPE)["O" MAX^PSSORPH
 S PSSX("DD",PSIEN)=$P($G(^PSDRUG(PSIEN,0)),"^")_"^"_$P($G(^(660)),"^",6)_"^"_$P($G(^(0)),"^",9)_"^"_$P($G(^(660)),"^",8)_"^"_$P($G(^("DOS")),"^")_"^"_$G(PSSUNITX)_"^"_$G(PSSMAX)
 D REQS^PSSORPH S PSSX("DD",PSIEN)=PSSX("DD",PSIEN)_"^"_$G(PSSREQS)_"^"_$G(PSNNN)_"^"_$G(PSSVERB)
 D LEAD^PSSORPH
 Q
SET ;
 D PARN^PSSORPH
 S PSSX(1)="^"_$S($G(TYPE)="O":$G(PSSUNITZ),1:$G(PSSUNTS))_"^^"_DLOOP_"^"_$G(PSSTRN)_"^"_$S($G(PSSNP)'="":$G(PSSNP),1:$G(PSNNN))_"^"_$P($G(^PS(50.606,+$G(PSSDSE),0)),"^")_"^"_$G(PSSVERB)_"^"_$G(PSSPREP) K PSSNP
 S (PSIEN,DLOOP)=+$P(PSSX(1),"^",4) K PSSMAX D:$G(TYPE)["O" MAX^PSSORPH
 S PSSX("DD",PSIEN)=$P($G(^PSDRUG(PSIEN,0)),"^")_"^"_$P($G(^(660)),"^",6)_"^"_$P($G(^(0)),"^",9)_"^"_$P($G(^(660)),"^",8)_"^"_$P($G(^("DOS")),"^")_"^"_$G(PSSUNITX)_"^"_$G(PSSMAX)
 D REQS^PSSORPH S PSSX("DD",PSIEN)=PSSX("DD",PSIEN)_"^"_$G(PSSREQS)_"^"_$G(PSNNN)_"^"_$G(PSSVERB)
 Q
AMP ;Replace & with AND when returning local doses to CPRS
 N PSSAB,PSSABT,PSSABA,PSSABL,PSSABZ,PSSABX,PSSABF1,PSSABF2
 I PSLOCV="&" S PSLOCV=" AND " Q
 I $E(PSLOCV,1)="&" D
 .I $E(PSLOCV,2)=" " S PSLOCV=" AND"_$E(PSLOCV,2,999) Q
 .S PSLOCV=" AND "_$E(PSLOCV,2,999)
 S PSSABL=$L(PSLOCV)
 I $E(PSLOCV,PSSABL)="&" D
 .I $E(PSLOCV,(PSSABL-1))=" " S PSLOCV=$E(PSLOCV,1,(PSSABL-1))_"AND " Q
 .S PSLOCV=$E(PSLOCV,1,(PSSABL-1))_" AND "
 Q:$G(PSLOCV)'["&"
 S PSSABT=0
 F PSSAB=1:1:$L(PSLOCV) I $E(PSLOCV,PSSAB)="&" S PSSABT=PSSABT+1
 F PSSAB=1:1:(PSSABT+1) S PSSABA(PSSAB)=$P(PSLOCV,"&") S PSLOCV=$P(PSLOCV,"&",2,999)
 F PSSABZ=1:1:PSSABT D
 .K PSSABF1,PSSABF2
 .I $L($G(PSSABA(PSSABZ)))>0 S PSSABF1=$E(PSSABA(PSSABZ),$L(PSSABA(PSSABZ)))
 .I $D(PSSABA(PSSABZ+1)) S PSSABF2=$E(PSSABA(PSSABZ+1),1)
 .S PSSABA(PSSABZ)=PSSABA(PSSABZ)_$S($G(PSSABF1)=" ":"AND",1:" AND")_$S($G(PSSABF2)=" ":"",1:" ")
 K PSLOCV F PSSABX=1:1:(PSSABT+1) S PSLOCV=$G(PSLOCV)_$G(PSSABA(PSSABX))
 Q
