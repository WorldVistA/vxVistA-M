GMVGETD2 ;HOIFO/YH-EXTRACT VITALS/MEASUREMENT RECORDS FOR A GIVEN DATE (CONT.) ;12/9/02  14:03
 ;;5.0;GEN. MED. REC. - VITALS;**1,23**;Oct 31, 2002;Build 8
 ;
 ; This routine uses the following IAs:
 ; <None>
 ;
SETLN ;CALLED BY GMVGETD1
 N GMRDAT,GMVLOOP,GMVQNAME,GMVUSER,X,Y
 S GJ=GJ+1,GMRDAT=$P(^TMP($J,"GMRV",GMRDATE,GMRVTY,GMRVDA),"|",1),^TMP($J,"GRPC",GJ)=GMRVDA_"^"_$E(GMRDATE,4,5)_"/"_$E(GMRDATE,6,7)_"/"_$E(GMRDATE,2,3)
 S GMVUSER=$$PERSON^GMVUTL1(+$P(GMRDAT,U,6)) ;user name
 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"@"_$E($P(GMRDATE,".",2)_"000000",1,2)_":"_$E($P(GMRDATE,".",2)_"000000",3,4)_"   "
 S GMRVTY(1)=$S(GMRVTY="T":"T: ",GMRVTY="P":"P: ",GMRVTY="R":"R: ",GMRVTY="BP":"B/P: ",GMRVTY="WT":"Wt: ",GMRVTY="HT":"Ht: ",GMRVTY="CG":"Circumference/Girth: ",GMRVTY="CVP":"Central Venous Pressure: ",GMRVTY="PO2":"Pulse Oximetry: ",1:"")
 I GMRVTY(1)="" S GMRVTY(1)=$S(GMRVTY="PN":"Pain: ",1:"")
 ;DSS/LM - Insert next to accommodate LMP and EDD - 2/16/2010 Add LEN
 ;       - 8/5/2013 - Add OSEHRA check
 I GMRVTY(1)="",$$VFD S GMRVTY(1)=$S(GMRVTY="LMP":"LMP: ",GMRVTY="EDD":"EDD: ",GMRVTY="LEN":"LEN: ",1:"")
 ;DSS/LM - End modification
 S GMRVTY(1)=GMRVTY(1)_"  "
 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_GMRVTY(1)
 I GMRVTY="PN" D  Q
 . S ^TMP($J,"GRPC",GJ)=^(GJ)_$P(GMRDAT,"^",8)_"  "_$S(+$P(GMRDAT,"^",8)=99:"Unable to respond",+$P(GMRDAT,"^",8)=10:"Worst imaginable pain",$P(GMRDAT,"^",8)="0":"No pain",1:"")_"  _"_GMVUSER
 ;DSS/LM - Insert next to accommodate LMP and EDD
 ;       - 8/5/2013 - Add OSEHRA check
 I $$VFD,"LMP^EDD"[GMRVTY S GMRVX=GMRVTY,GMRVX(0)=$P(GMRDAT,"^",8) D
 .S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_$$FMTE^XLFDT(GMRVX(0),"5D")_" "
 .Q
 ; 2/16/2010 - Add LENGTH (LEN)
 I $$VFD,"LEN"[GMRVTY S GMRVX=GMRVTY,GMRVX(0)=$P(GMRDAT,"^",8) D
 .S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_GMRVX(0)_" "
 .Q
 ;DSS/LM - End Insert
 I "PRBPCVPCGPO2"[GMRVTY S GMRVX=GMRVTY,GMRVX(0)=$P(GMRDAT,"^",8) D
 . I '(GMRVX(0)<0!(GMRVX(0)>0)!($E(GMRVX(0))="0")) S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_GMRVX(0)_" " Q
 . D EN1^GMVSAS0
 . S Z=$S(GMRVTY="CG":$J($P(GMRDAT,"^",8),0,2),GMRVTY="CVP":$J($P(GMRDAT,"^",8),0,1),GMRVTY'="BP":$J($P(GMRDAT,"^",8),3,0),1:$P(GMRDAT,"^",8)) D:GMRVTY'="BP" BLNK
 . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_Z_$S('$D(GMRVX(1)):" ",'GMRVX(1):"",1:"*")
 . ;DSS/SMP - BEGIN MODS
 . I GMRVTY="CG" D
 . . I $$VFD D  Q
 . . . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" in ("_$$IN2CM^VFDXLF(Z,2)_" cm)"
 . . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" in ("_$J(Z/.3937,0,2)_" cm)"
 . ;DSS/SMP - END MODS
 . I GMRVTY="CVP" S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" cmH2O ("_$J(Z/1.36,0,1)_" mmHg)"
 . I GMRVTY="PO2" D
 . . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"%"
 . . I $P(GMRDAT,"^",10)["%"!($P(GMRDAT,"^",10)[" l/min") D
 . . . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" with supplemental O2"
 . . . N GMV S GMV=$P(GMRDAT,"^",10) I GMV[" l/min" S GMV=$P(GMV," l/min")_"L/min"_$P(GMV," l/min",2)
 . . . S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" "_GMV
 I GMRVTY="T" S X=$P(GMRDAT,"^",8) D
 . I X'>0 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_X Q
 . S GMRVX=GMRVTY,GMRVX(0)=X D EN1^GMVSAS0
 . D EN1^GMVUTL S:'Y Y="" S Z=$J(X,5,1) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_Z_" F " S Z=$J(Y,4,1) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"("_Z_" C)"_$S('$D(GMRVX(1)):" ",'GMRVX(1):"",1:"*")
 I GMRVTY="HT" S X=$P(GMRDAT,"^",8) D
 . I X'>0 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_X Q
 . D EN2^GMVUTL S:'Y Y="" S Z=$J(X,5,2) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_Z_" in " S Z=$J(Y,5,2) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"("_Z_" cm)"
 I GMRVTY="WT" S X=$P(GMRDAT,"^",8) D
 . I X'>0 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_X Q
 . D EN3^GMVUTL S:'Y Y="" S Z=$J(X,7,2) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_Z_" lb " S Z=$J(Y,6,2) D BLNK S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"("_Z_" kg)"
 I (+$P(GMRDAT,"^",8)'>0)&($P(GMRDAT,"^",8)'="0") D  Q
 .S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_"  _"_GMVUSER
 .Q
 S GMRVITY=+$P(GMRDAT,"^",3)
 K GMRVARY
 S GMRZZ=""
 S GMRVARY=$P(^TMP($J,"GMRV",GMRDATE,GMRVTY,GMRVDA),"|",2)
 F GMVLOOP=1:1 Q:$P(GMRVARY,U,GMVLOOP)=""  D
 .S GMVQNAME=$$FIELD^GMVGETQL($P(GMRVARY,U,GMVLOOP),1,"E")
 .I GMVQNAME=""!(GMVQNAME=-1) Q
 .S GMRZZ=GMRZZ_$S(GMRZZ'="":", ",1:"")_GMVQNAME
 .Q
 S:GMRZZ'=""&(GMRVTY'="PO2") GMRZZ=" ("_GMRZZ_")"
 S:GMRVTY="PO2" ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" "_GMRZZ
 I GMRVTY="P",GMRZZ["DORSALIS PEDIS",$P(GMRDAT,"^",8)=1 S ^TMP($J,"GRPC",GJ)=$P(^(GJ),"*",1)_$P(^(GJ),"*",2)
 S:GMRVTY'="PO2" ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" "_GMRZZ
 S ^TMP($J,"GRPC",GJ)=^TMP($J,"GRPC",GJ)_" _"_GMVUSER
 I GMRVTY="WT" S GMRBMI="",GMRBMI(1)=$P(GMRDAT,"^"),GMRBMI(2)=+$P(GMRDAT,"^",8) D CALBMI^GMVBMI(.GMRBMI) D:GMRBMI'=""
 . S GJ=GJ+1,^TMP($J,"GRPC",GJ)=$E(GMRDATE,4,5)_"/"_$E(GMRDATE,6,7)_"/"_$E(GMRDATE,2,3)
 . S ^TMP($J,"GRPC",GJ)="       @"_$E($P(GMRDATE,".",2)_"000000",1,2)_":"_$E($P(GMRDATE,".",2)_"000000",3,4)_"   Body Mass Index:   "_GMRBMI
 K Z,GMRBMI
 Q
BLNK ;
 N I
 F I=1:1:$L(Z) Q:$E(Z,I)'=" "
 S Z=$E(Z,I,$L(Z))
 Q
 ;
 ;DSS/SMP - BEGIN MODS
VFD() ; OSEHRA Check
 Q $G(^%ZOSF("ZVX"))["VX"
