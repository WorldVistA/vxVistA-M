SISP10 ;SIS/LM - Clone and optionally modify routine DGRP10
 ;;1.0;NON-VA REGISTRATION;;;Build 15
 ;Copyright 2008 - 2009, Sea Island Systems, Inc.  Rights are granted as follows:
 ;Sea Island Systems, Inc. conveys this modified VistA routine to the PUBLIC DOMAIN.
 ;This software comes with NO warranty whatsoever.  Sea Island Systems, Inc. will NOT be
 ;liable for any damages that may result from either the use or misuse of this software.
 ;
DGRP10 ;ALB/MRL - REGISTRATION SCREEN 10/MISSING-INELIGIBLE INFORMATION ;06 JUN 88@2300
 ;;5.3;Registration;;Aug 13, 1993
 S DGRPS=10 D H^DGRPU F I=.15,.3,"INE" S DGRP(I)=$S($D(^DPT(DFN,I)):^(I),1:"")
 S (DGRPW,Z)=1 D WW^DGRPV S DGRPIN=$P(DGRP(.15),"^",2) W "  Ineligible Date: " S Y=DGRPIN X:Y ^DD("DD") S Z=$S(Y]"":Y,1:DGRPNA),Z1=25 D WW1^DGRPV
 S DGRPX=DGRP("INE"),X=$P(DGRPX,"^",1),X1=DGRPIN,X2=3,X3=4 D TWX
 W !?14,"Reason: ",$S('DGRPIN:DGRPNA,$P(DGRP(.3),"^",7)]"":$P(DGRP(.3),"^",7),1:DGRPU),!?7,"VARO Decision: ",$S('DGRPIN:DGRPNA,$P(DGRPX,"^",6)]"":$E($P(DGRPX,"^",6),1,60),1:DGRPU)
 S Z=2 D WW^DGRPV S DGRPMS=$P(DGRP(.15),"^",3) W "     Missing Date: " S (DGRPIN,Y)=DGRPMS X:Y ^DD("DD") S Z=$S(Y]"":Y,1:DGRPNA),Z1=25 D WW1^DGRPV
 S DGRPX=DGRP("INE"),X=$P(DGRPX,"^",7),X1=DGRPMS,X2=8,X3=9 D TWX
 W !?14,"Reason: " I '$O(^DPT(DFN,.16,0))!'DGRPIN W $S('DGRPIN:DGRPNA,1:"UNSPECIFIED") G Q
 K ^UTILITY($J,"W") S DGFL=0,DIWL=23,DIWF="WC50" F DGRP1=0:0 S DGRP1=$O(^DPT(DFN,.16,DGRP1)) Q:'DGRP1  S DGFL=1,X=^(DGRP1,0) K ^UTILITY($J,1) D ^DIWP
 D ^DIWW
Q K DGRP1,DGRPIN,DGRPMS,DIWF,DIWL
 G ^SISPP
TWX W "TWX Source: ",$S('X1:DGRPNA,X=1:"VAMC",X=2:"REGIONAL OFFICE",X=3:"RPC",X=4:"OTHER",1:DGRPU)
 W !?12,"TWX City: " S Z=$S('DGRPIN:DGRPNA,$P(DGRPX,"^",X2)]"":$E($P(DGRPX,"^",X2),1,20),1:DGRPU),Z1=26 D WW1^DGRPV W "TWX State: ",$S('DGRPIN:DGRPNA,$D(^DIC(5,+$P(DGRPX,"^",X3),0)):$P(^(0),"^",1),1:DGRPU) Q
