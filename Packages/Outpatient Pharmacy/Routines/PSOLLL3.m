PSOLLL3 ;BHAM/JLC - LASER LABELS ;11/20/02
 ;;7.0;OUTPATIENT PHARMACY;**120,161,148,200**;DEC 1997;Build 153
 ;
 S PRCOPAY=$S('$D(PSOCPN):0,1:1)
PF ;PATIENT FILL DOCUMENT
 I $G(PSOIO("PFI"))]"" X PSOIO("PFI")
 D  S PSOFONT=OFONT
 . S OFONT=PSOFONT,PSOFONT=PSOHFONT
 . I $P(RXY,"^",9)=0 S T="NO REFILL for this prescription" D PRINT(T,1,1) S PSOY=PSOY+PSOYI Q
 . I RXF+1=(1+$P(RXY,"^",9)) S T="NO REFILLS LEFT for this prescription" D PRINT(T,1,1) S PSOY=PSOY+PSOYI Q
 . S T="PHONE IN OR MAIL THIS REFILL REQUEST" D PRINT(T,1,1)
 . S PSOFONT=OFONT,T="Follow the refill instructions provided with your prescription." D PRINT(T,0,1)
 . S PSOFONT=OFONT,OPSOX=PSOX,PSOX=PSOX+300,T="For Refill Call "_$P(PS,"^",3)_"-"_$P(PS,"^",4) D PRINT(T,0) S PSOX=OPSOX
 S T=PNM_"  "_$G(SSNPN) D PRINT(T,1)
 S T="Rx# "_RXN_"   " D PRINT(T,1)
 D STRT^PSOLLU1("SEC2",T,.L) S OPSOX=PSOX,PSOX=L($E(PSOFONT,2,99))*300+PSOX
 S T=DATE_"  Fill "_(RXF+1)_" of "_(1+$P(RXY,"^",9)),PSOY=PSOY-PSOYI D PRINT(T) S PSOX=OPSOX
 S T=$S($$STATUS^PSOBPSUT(RX,+RXF)'="":"3rd Party Rx",1:"") D PRINT(T,1)
 S T="Qty: "_$G(QTY)_"  "_$G(PSDU)_"    Days supply: "_$G(DAYS) D PRINT(T,0)
 S T=DRUG D PRINT(T,0)
 S T=$$GETNDC^PSONDCUT(RX,RXF) D PRINT(T,1)
 D  D PRINT(T,1)
 . S NOR=$P(RXY,"^",9)-RXF
 . I $P(RXY,"^",9)=0 S T="NO REFILL" Q
 . I NOR=0 S T="NO REFILLS LEFT" Q
 . S T="May refill "_NOR_"X by "_EXPDT
 S PS=$S($D(^PS(59,PSOSITE,0)):^(0),1:"")
 ;DSS/SGM - BEGIN MODS - do dot display copay message or station number
 I $G(VFDPSOLB) D
 . D COPAY^VFDPSOLB("LLL3")
 . S T=$P(PS,"^") D STRT^PSOLLU1("SEC2",T,.L)
 . Q
 E  D
 . D PRINT(COPAYVAR)
 . S T=$P(PS,"^")_"-"_$P(PS,"^",6) D STRT^PSOLLU1("SEC2",T,.L)
 . Q
 ;DSS/SGM - END MODS
 S OPSOX=PSOX,PSOX=2340-(L($E(PSOFONT,2,99))*300),PSOY=PSOY-PSOYI
 D PRINT(T)
 S PSOX=OPSOX,PSOYI=PSOBYI
 I $G(PSOIO("SBT"))]"" X PSOIO("SBT")
 S X2=PSOINST_"-"_RX
 W X2
 I $G(PSOIO("EBT"))]"" X PSOIO("EBT")
 I $G(PSOIO(PSOFONT))]"" X PSOIO(PSOFONT)
 Q
PRINT(T,BOLD,HDR) ;
 S BOLD=+$G(BOLD),HDR=+$G(HDR)
 I 'BOLD,$G(PSOIO(PSOFONT))]"" X PSOIO(PSOFONT)
 I BOLD,$G(PSOIO(PSOFONT_"B"))]"" X PSOIO(PSOFONT_"B")
 I HDR D
 . S OPSOX=PSOX D STRT^PSOLLU1("SEC2",T,.L)
 . S PSOX=4.2-L($E(PSOFONT,2,99))*300/2+OPSOX
 I $G(PSOIO("ST"))]"" X PSOIO("ST")
 W T,!
 I HDR S PSOX=OPSOX
 I $G(PSOIO("ET"))]"" X PSOIO("ET")
 I BOLD,$G(PSOIO(PSOFONT))]"" X PSOIO(PSOFONT) ;TURN OFF BOLDING
 Q
