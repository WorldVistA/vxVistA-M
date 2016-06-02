VFDXTGBL ;DSS/PDW/SMP - GLOBAL UTILITIES; 05/15/15 10:00 
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 Q
 ;
GBL2GBL ;compare same global across two name spaces.
 S GBL="LEX(757.02)",GBLT=$E(GBL,1,$L(GBL)-1)
 S NS1="USER",NS2="VOS"
 S G1="^"_GBL
 F I=0:1 W:'(I#1000) "." S G1=$Q(@G1) Q:G1'[GBLT  S G2="^|NS2|"_$E(G1,2,99) D
 .I $D(@G2),@G1=@G2 Q
 .W !!,NS1,G1,?30,@G1,!,NS2,?30,$S($D(@G2):@G2,1:"none")
 Q
 ;
GROOT(VEFS,DD,FLD) ; Global root of given field.
 ; Input:
 ;   DD  - req - DD Number
 ;   FLD - req - Field Number
 ;
 ; Output:
 ;   VEFS(0)     = Complete Global Root
 ;   VEFS(I)     = Constants (non-variable nodes)
 ;   VFFS("F")   = File Number (top-level)
 ;   VEFS("F",0) = Explicit Global Root
 ;   VEFS("L")   =  $QL(VEFS(0))
 ;   VEFS("P")   = Piece # where field is found
 ;
 ; Example:
 ;   D GROOT^VEFSDD09(.RET,2.0216,.01)
 ;
 ;   RET(0)="^DPT(DA(1),21600,DA,0)"
 ;   RET(2)=21600
 ;   RET(4)=0
 ;   RET("F")=2
 ;   RET("F",0)="^DPT("
 ;   RET("L")=4
 ;   RET("P")=1
 ;
 ; 
 N I,J,L,X,Y,Z,ZFLD,ND,NM,QL,ROOT,UDD,UP,TEMP,GBL,NODE,PIECE
 I 'DD!'$D(^DD(+DD,0)) Q -1
 I FLD,'$D(^DD(+DD,FLD,0)) Q -1
 S UDD=DD,(Z,QL)=0
 S X=$P(^DD(DD,FLD,0),U,4),NODE=$P(X,";"),PIECE=$P(X,";",2)
 D SET(.ROOT,NODE)
 F  S UP=$G(^DD(UDD,0,"UP")) Q:'UP  D  Q:Z
 .S ZFLD=$O(^DD(UP,"SB",UDD,0)) I 'ZFLD S Z=-2 Q
 .S X=$G(^DD(UP,ZFLD,0)),Y=$P(X,U,4) I Y="" S Z=-2 Q
 .I $P(X,U,2)'[UDD S Z=-2 Q
 .S NODE=$P(Y,";") D SET(.ROOT,NODE)
 .S UDD=UP
 I Z Q Z
 S GBL=$G(^DIC(UDD,0,"GL")) I GBL="" S VEFS(0)=-3 Q -3
 D CLEAN(.VEFS,.ROOT,GBL) S VEFS("F")=UDD,VEFS("F",0)=GBL
 S VEFS("P")=PIECE
 Q VEFS(0)
 ;
SET(RET,NODE) ;
 S QL=QL+1,RET(QL)=NODE
 S QL=QL+1,RET(QL)="**DA**"
 Q
 ;
CLEAN(RET,ROOT,GBL) ;
 N X,Y,Z,I,DA,SUB
 S DA=0,SUB=1 I GBL["," S Z=$P($P(GBL,"(",2),","),RET(SUB)=Z,SUB=SUB+1
 S X=10000 F I=SUB:1 S X=$O(ROOT(X),-1) Q:'X  D
 .S RET(I)=ROOT(X),SUB=I I RET(I)="**DA**" S DA=DA+1
 .I SUB>1,I=1 Q
 .S GBL=GBL_RET(I)_","
 S $E(GBL,$L(GBL))=")",GBL(1)=$P($P(GBL,")"),"(",2)
 F I=1:1 Q:'$D(RET(I))  D
 .I RET(I)?.N Q
 .I RET(I)="**DA**" D
 ..K RET(I) S X="DA" I DA>1 S X=X_"("_(DA-1)_")",DA=DA-1
 ..S $P(GBL(1),",",I)=X
 .E  S $P(GBL(1),",",I)=""""_RET(I)_""""
 S RET(0)=$P(GBL,"(")_"("_GBL(1)_")",RET("L")=SUB
 Q
 ;
