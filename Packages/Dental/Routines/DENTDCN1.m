DENTDCN1 ;WASH ISC/TJK-MODIFIED DICN1 ROUTINE  ;6/29/92  14:36
 ;;1.2;DENTAL;***15**;Oct 08, 1992
 K DICRS,Y,DENTDRCR
 F Y="I","J","X","DO","DC","DA","DE","DG","DIE","DR","DIC","D","D0","D1","D2","D3","D4","D5","D6","DI","DH","DIA","DICR","DK","DIK","DL","DLAYGO","DM","DP","DQ","DU","DW","DIEL","DOV","DIOV","DIEC","DB","DV","DIFLD" S DENTDRCR(Y)=""
 S DZ="W !?3,$S("""_$P(DO,U,1)_"""'=$P(DQ(DQ),U,1):"""_$P(DO,U,1)_""",1:"""")_"" ""_$P(DQ(DQ),U,1)_"": """
 I $D(DIC("DR")) S DD=DIC("DR")
 E  S DD="",%=0 F Y=0:0 S Y=$O(^DD(+DO(2),0,"ID",Y)) S:Y="" Y=-1 Q:Y'>0  D CKID I '$D(%) D  G BAD
 .  ; Naked redernce in DENTCN1+8 refs to DENTDCN1+6
 .  W !,"SORRY!  A VALUE FOR '"_$P(^(0),U,1)_"' MUST BE ENTERED,"
 .  W !?6,"BUT YOU DON'T HAVE 'WRITE ACCESS' FOR THIS FIELD"
 .  S DENTDRCR="D^DENTDCN1"
 .  D STORLIST
 .  Q
 ;END IF
 ;
 S DENTDRCR="RCR^DENTDCN1" D STORLIST G D^DENTDCN:$D(Y)<9
BAD S:$D(D)#2 DA=D K Y I '$D(DO(1)) S Y=-1 G Q^DENTDC
 K DO G A^DENTDC
 ;
CKID I $D(DUZ(0)),DUZ(0)'="@",$D(^DD(+DO(2),Y,9)),^(9)]"" F %=1:1 I DUZ(0)[$E(^(9),%) Q:$L(^(9))'<%  K:$P(^(0),U,2)["R" % G Q
 S DD=DD_Y_";"
Q Q
 ;
RCR ;
 K DR,DQ,DG,DE,DO S DIE=DIC,DR=DD,DIE("W")=DZ K DIC I $D(DIE("NO^")) S DENTDRCR("DIE(""NO^"")")=DIE("NO^")
 S DIE("NO^")="OUTOK" D ^DIE K DIE("W"),DIE("NO^") I '$D(DA) S Y(0)=0 Q
 Q:$D(Y)<9
ZAP S DIK=DIE W !?6,"<'",*7,$P(@(DIK_"DA,0)"),U,1),"' DELETED>" D ^DIK S Y(0)=0 K DIK Q
D S DIE=DIC G ZAP
 ;
RIX ;
 K DENTDRCR F %="D0","Y","DIC","DIU","DIV","DO","D","DD","DICR","X" S DENTDRCR(%)=""
 S DENTDRCR="RR^DENTDCN1",DZ=^DD(+DO(2),.01,1,1) D STORLIST G IX^DENTDCN
 ;
RR X DZ Q
 ;
NUM ;
 I '$D(DD),DIC="^DIC(",$D(^DD("SITE",1)),X\1000'=^(1) S X=^(1)*1000 G F2^DENTDCN
 S %=$P(^DD(+Y,.001,0),U,2),X=$S(%'["N"!(%["O"):0,1:X),%Y=X I X F %=1:1 D N Q:$D(X)  S X=0 Q:%>50  S X=%Y+DIY,%Y=X
 W !?3,$P(DO,U,1)_" "_$P(^DD(+Y,.001,0),U,1),": " W:X X,"// " R Y:DTIME E  S DTOUT=1,Y=U W *7
 I Y="?" W:$D(^DD(+$P(D0,U,2),.001,3)) !,^(3) X:$D(^(4)) ^(4) G F1^DENTDCN
 G BAD^DENTDC1:Y[U S:Y]"" X=Y D N I '$D(X) W *7,"??" W:$D(^DD(+DO(2),.001,3)) !,^(3) X:$D(^(4)) ^(4) G F1^DENTDCN
 G LOCK^DENTDCN
 ;
N X:$D(^DD(+$P(DO,U,2),.001,0)) $P(^(0),U,5,99) I $D(X),$L(X)<15,+X=X,X>0,X>1!(DIC'="^DIC(") Q
 K X
STORLIST D INIT
O S DENTDJD=$O(DENTDRCR(DENTDJD)) S:DENTDJD="" DENTDJD=-1 G CALL:DENTDJD<0
 I $D(@DENTDJD)#2 S @(DENTDJE_")="_DENTDJD) G O:$D(@DENTDJD)=1
 S DENTDJX=DENTDJD_"(" D DENTDJXY G O
 ;
CALL S DENTDJE=DENTDRCR K DENTDRCR,DENTDJX,DENTDJY D @DENTDJE
 S DENTDJE="^TMP(""DENTDRCR"",$J,"_^TMP("DENTDRCR",$J)_",DENTDJD",^($J)=^($J)-1,DENTDJD=0,DENTDJX=DENTDJE_","
G S DENTDJD=$O(@(DENTDJE_")")) S:DENTDJD="" DENTDJD=-1 K:DENTDJD<0 DENTDJD,DENTDJE,DENTDJX,DENTDJY,^($J,^TMP("DENTDRCR",$J)+1) Q:'$D(DENTDJD)  I $D(^(DENTDJD))#2 S @DENTDJD=^(DENTDJD) G G:$D(^(DENTDJD))=1
 S DENTDJY=DENTDJD_"(" D DENTDJXY G G
 ;
DENTDJXY ;
 S DENTDJZ=1,DENTDJA="",DENTDJC(0)=0
S S DENTDJB=-1
N1 S DENTDJB=$O(@(DENTDJX_DENTDJA_"DENTDJB)")) S:DENTDJB="" DENTDJB=-1 S DENTDJC(DENTDJZ)=DENTDJC(DENTDJZ-1)
 I DENTDJB["," F DENTDJC=0:0 S DENTDJC=$F(DENTDJB,",",DENTDJC) Q:'DENTDJC  S DENTDJC(DENTDJZ)=DENTDJC(DENTDJZ)+1
 I DENTDJB=-1 G Q1:DENTDJZ=1 S DENTDJZ=DENTDJZ-1,@("DENTDJB="_$P(DENTDJA,",",DENTDJZ+DENTDJC(DENTDJZ-1),DENTDJZ+DENTDJC(DENTDJZ))),DENTDJA=$P(DENTDJA,",",1,DENTDJZ-1+DENTDJC(DENTDJZ-1))_$E(",",DENTDJZ>1) G N1
 I $D(@(DENTDJX_DENTDJA_"DENTDJB)"))#10=1 S @(DENTDJY_DENTDJA_"DENTDJB)="_DENTDJX_DENTDJA_"DENTDJB)")
 I $D(@(DENTDJX_DENTDJA_"DENTDJB)"))<9 G N1
 G DOWN:+DENTDJB=DENTDJB F DENTDJC=0:0 S DENTDJC=$F(DENTDJB,"""",DENTDJC) Q:'DENTDJC  S DENTDJB=$E(DENTDJB,1,DENTDJC-1)_""""_$E(DENTDJB,DENTDJC,999),DENTDJC=DENTDJC+1
 S DENTDJB=""""_DENTDJB_""""
DOWN S DENTDJA=DENTDJA_DENTDJB_",",DENTDJZ=DENTDJZ+1 G S
 ;
Q1 K DENTDJA,DENTDJB,DENTDJC,DENTDJZ Q
 ;
INIT I $D(^TMP("DENTDRCR",$J))[0 S ^TMP("DENTDRCR",$J)=0
 S ^TMP("DENTDRCR",$J)=^($J)+1,DENTDJD="%Z",DENTDJE="^TMP(""DENTDRCR"",$J,"_^($J)_",DENTDJD",DENTDJY=DENTDJE_"," K ^($J,^($J))
