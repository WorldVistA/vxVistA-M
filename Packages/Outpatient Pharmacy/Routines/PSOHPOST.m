PSOHPOST ;BHAM ISC/RTR - Patch PSO*6*138 post init ; 06/12/95
 ;;6.0;OUTPATIENT PHARMACY;**138**;APRIL 1993
 S DIK="^PS(52.11,",DA=0 F  S DA=$O(^PS(52.11,DA)) Q:'DA  D ^DIK
 K ^PS(52.11,"AD"),^PS(52.11,"ANAM"),^PS(52.11,"ATIC"),^PS(52.11,"BI"),^PS(52.11,"B"),^PS(52.11,"BA"),^PS(52.11,"C")
 F ZZ=0:0 S ZZ=$O(^PS(52.5,"AC",ZZ)) Q:'ZZ  F XX=0:0 S XX=$O(^PS(52.5,"AC",ZZ,XX)) Q:'XX  F TT=0:0 S TT=$O(^PS(52.5,"AC",ZZ,XX,TT)) Q:'TT  D
 .I '$D(^PS(52.5,TT,0)) K ^PS(52.5,"AC",ZZ,XX,TT),^PS(52.5,"C",XX,TT) Q
 .I $D(^PS(52.5,TT,0)),XX'=$P(^(0),"^",2) D
 ..S PSOSDATE=$P(^PS(52.5,TT,0),"^",2) K ^PS(52.5,"AC",ZZ,XX,TT),^PS(52.5,"C",XX,TT) S ^PS(52.5,"C",PSOSDATE,TT)="" I $G(^PS(52.5,TT,"P"))=0!($G(^PS(52.5,TT,"P"))=2) S ^PS(52.5,"AC",ZZ,PSOSDATE,TT)=""
 .I $P($G(^PS(52.5,TT,0)),"^",7)'="" S PSCMDATE=+$P($G(^(0)),"^",2) K ^PS(52.5,"AC",ZZ,PSCMDATE,TT)
END K DA,DIE,ZZ,XX,TT,PSOQTIME,PSOSDATE,PSCMDATE Q
