PSOGNTEG ;ISC/XTSUMBLD KERNEL - Package checksum checker ;MAY 18, 1995@08:50:29
 ;;6.0;OUTPATIENT PHARMACY;**137**;APRIL 1993
 ;;7.1;MAY 18, 1995@08:50:29
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
PSOBBC ;;10161951
PSOCAN ;;15759004
PSOCAN2 ;;14527153
PSOCSRL ;;14755111
PSODIR1 ;;10869807
PSODISP ;;16740367
PSODISP1 ;;17466263
PSODISPS ;;7810713
PSODRDUP ;;16732998
PSOGI001 ;;3133176
PSOGINI1 ;;5656899
PSOGINI2 ;;5232598
PSOGINI3 ;;16094107
PSOGINI4 ;;3357770
PSOGINI5 ;;403257
PSOGINIS ;;2214327
PSOGINIT ;;10925572
PSOHELP ;;16209321
PSOHLD ;;18461984
PSOLBL ;;18827131
PSOLBLN2 ;;19264719
PSOLBLS ;;16417877
PSOLSET ;;19530509
PSORENW ;;2490160
PSORENW0 ;;7517324
PSORESK ;;16464482
PSORESK1 ;;5525164
PSORXDL ;;5883931
PSORXED ;;15856563
PSORXED1 ;;10637586
PSORXL ;;17854376
PSORXL1 ;;10370854
PSORXPAR ;;7295709
PSORXPR ;;19812244
PSORXRPT ;;14389557
PSOSUCH1 ;;12474740
PSOSUCHG ;;20642681
PSOSUDEL ;;3675039
PSOSULBL ;;17451890
PSOSUPAT ;;21205139
PSOSUPRX ;;20256226
PSOSURST ;;4729274
PSOSUTL ;;10861401
PSOTRLBL ;;12689978
