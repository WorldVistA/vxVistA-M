RMPRUTL4 ;HINES CIOFO/HNC - FIX RECORDS IN FILE 660 WITH NO AMIS GROUPER ;3/25/04  12:47
 ;;3.0;PROSTHETICS;**34,35,87,90**;Feb 09, 1996
 ;post init
 I '$D(DUZ) W !,"DUZ NOT DEFINED!" Q
 D DIV4^RMPRSIT,HOME^%ZIS G:$D(X) END
 S RMPRA=0
 F  S RMPRA=$O(^RMPR(660,RMPRA)) Q:RMPRA'>0  D
 .Q:$G(^RMPR(660,RMPRA,"AMS"))'=""
 .Q:$G(^RMPR(660,RMPRA,0))=""
 .Q:$P(^RMPR(660,RMPRA,0),U,15)["*"
 .Q:$P(^RMPR(660,RMPRA,0),U,10)'=RMPR("STA")
 .W "."
 .L +^RMPR(669.9,RMPRSITE,0):9999 I $T=0 S RMPRG=DT_98
 .I '$G(RMPRG) S RMPRG=$P(^RMPR(669.9,RMPRSITE,0),U,7),RMPRG=RMPRG-1,$P(^RMPR(669.9,RMPRSITE,0),U,7)=RMPRG L -^RMPR(669.9,RMPRSITE,0)
 .W RMPRG
 .S $P(^RMPR(660,RMPRA,"AMS"),U,1)=RMPRG
 .K RMPRG
 W !,"ALL DONE"
 K RMPRA,RMPR,RMPRSITE Q
CHG ;change nppd line ; HCPCS
 S $P(^RMPR(661.1,2757,0),U,6)="R90 A"
 S $P(^RMPR(661.1,2757,0),U,7)=""
 Q
SERIAL ;enter product info for recall
 D GETS^DIQ(660,DA,".02;4;4.5;7;11;23;24;27;89","E","RMPRX")
 Q
END ;END
