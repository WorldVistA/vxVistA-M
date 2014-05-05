PSGLPI ;BIR/CML3-PATIENT INFO FOR LABELS ;15 DEC 95 / 10:26 AM
 ;;5.0; INPATIENT MEDICATIONS ;;16 DEC 97;Build 153
 ;
ENPI ;
 N VADM,VAIN,VAIP,PSGLWG S DFN=PSGOP D DEM^VADPT,INP^VADPT S PSGLPID=VA("PID"),PSGLBID=VA("BID")
 I 'VAIN(4) S VAIP("D")="L" D IN5^VADPT
 I VAIN(4) S PSGLWD=+VAIN(4),PSGLWDN=$P(VAIN(4),"^",2) S PSGLPR=VAIN(2),PSGLTS=VAIN(3),PSGLRB=VAIN(5),PSGLAD=+VAIN(7),PSGLDX=VAIN(9)
 E  S:$S('$D(PSGLWD):1,1:'PSGLWD) PSGLWD=+VAIP(5),PSGLWDN=$P(VAIP(5),"^",2) S PSGLRB=$P(VAIP(6),"^",2),PSGLPR=VAIP(7),PSGLTS=VAIP(8),PSGLDX=VAIP(9)
 S PSGLTM="" I PSGLWD,PSGLRB]"",$D(^PS(57.7,PSGLWD,1,+$O(^PS(57.7,"AWRT",PSGLWD,PSGLRB,0)),0)) S PSGLTM=$P(^(0),"^")
 D NOW^%DTC S PSGLDT=+$E(%,1,12)
 S PSGLPN=VADM(1),PSGLSSN=$P(VADM(2),"^",2),PSGLDOB=$E($$ENDTC^PSGMI(+VADM(3)),1,8),PSGLAGE=VADM(4),PSGLSEX=$S(VADM(5)]"":$P(VADM(5),"^",2),1:"____")
 S PSGLBS5=$E(PSGLPN)_$P(PSGLSSN,"-",3) I $S('$D(PSGLWG):1,1:'PSGLWG) S (PSGLWG,PSGLWGN)="" S PSGLWG=$O(^PS(57.5,"AB",PSGLWD,0)) I PSGLWG,$D(^PS(57.5,PSGLWG,0)) S PSGLWGN=$P(^(0),"^")
 ;
DONE ;
 K PSGLPIWF,PSGID,PSGOD,VADM,VAIN,VAIP Q
 ;
ENPVSET ;
 S PSGLAD=+PSJPAD,PSGLAGE=PSJPAGE,PSGLBS5=$E(PSGP(0))_$E($P(PSJPSSN,"^"),6,10),PSGLDOB=$E($P(PSJPDOB,"^",2),1,8),PSGLDX=PSJPDX,PSGLPN=$P(PSGP(0),"^"),PSGLRB=PSJPRB,PSGLSEX=$P(PSJPSEX,"^",2),PSGLSSN=VA("PID"),PSGLWD=PSJPWD,PSGLWDN=PSJPWDN
 I $S('$D(PSGLWG):1,1:'PSGLWG) S (PSGLWG,PSGLWGN)="" I PSGLWD S PSGLWG=$O(^PS(57.5,"AB",PSGLWD,0)) I PSGLWG,$D(^PS(57.5,PSGLWG,0)) S PSGLWGN=$P(^(0),"^")
 S PSGLTM="" I PSGLWD,PSGLRB]"",$D(^PS(57.7,PSGLWD,1,+$O(^PS(57.7,"AWRT",PSGLWD,PSGLRB,0)),0)) S PSGLTM=$P(^(0),"^")
 Q
 ;
ENHEDER ; Print MAR header labels.
 ;DSS/SGM - BEGIN MODS - MAR LABEL PRINT FOR ZEBRA,INTERMEC
 I $L($T(^VFDPSPRT)),$$EN^VFDPSPRT("PSGLPI") Q
 ;DSS/SGM - END MODS
 N NF S NF="NOT FOUND"
 W *13,?52,PSGLPN,?88,$J($S(PSGLRB]"":PSGLRB,1:"*NF*"),12)
 W !?(41-$L(PSGLPN)/2),"*** ",PSGLPN," ***",?52,PSGLSSN,?70,PSGLDOB," (",PSGLAGE,")",?85,$J($S(PSGLTM]"":PSGLTM,1:NF),15)
 W !?18,PSGLSSN,?52,$S(PSGLSEX]"":PSGLSEX,1:"____"),?65,"DX: ",PSGLDX
 D NOW^%DTC W !?52,$$ENDTC^PSGMI(%),!?52,$S(PSGLWGN]"":$E(PSGLWGN,1,21),1:NF),?79,$J($S(PSGLWDN]"":PSGLWDN,1:NF),21),!!
 Q
