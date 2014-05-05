GMVKPO0 ;HIOFO/YH,FT-KYOCERA PULSE OXIMETRY/RESP. GRAPH - DATA ARRAY ;11/6/01  14:55
 ;;5.0;GEN. MED. REC. - VITALS;;Oct 31, 2002
 ;
 ; This routine uses the following IAs:
 ; #10061 - ^VADPT calls           (supported)
 ;
EN1 ;ENTRY POINT FROM GMVSR0 TO PRINT PO2/RESP. GRAPH
 N GAPICAL,GRADIAL,GBRACHI S GAPICAL=$O(^GMRD(120.52,"B","APICAL",0)),GRADIAL=$O(^GMRD(120.52,"B","RADIAL",0)),GBRACHI=$O(^GMRD(120.52,"B","BRACHIAL",0))
 S GMROUT=0 K ^TMP($J,"GMR"),^TMP($J,"GMRK"),^TMP($J,"GDT"),^TMP($J,"GMRVG"),^TMP($J,"GTNM") F GI=1:1:200 S ^TMP($J,"GMRK","G"_GI)=""
 S GMROUT=0,^TMP($J,"GMRK","G50M")=0.2,^TMP($J,"GMRK","G82M")=0.6
 S GSTART1=(9999999-GMRFIN)-.0001,GEND1=9999999-GMRSTRT
 F GTYPE="PO2","R" D SETT^GMVHPO0
 S GTYPE="P" D SETT^GMVBP0
 U IO D GRAPH
Q1 K GSOL,GIVDT,GMRHLOC,GMRVJ,GDATA,GDT,GEN,GEND1,GI,GJ,GK,GMRVX,GSTART1,GTNM,GTYP,GTYPE,GX,I D KVAR^VADPT K VA,VAROOT
 K GMRRMBD,GAGE,GCNT,GDOB,GCNTB,GCNTD,GCNTP,GCNTR,GCNTT,GCNTT1,GCNTI,GCNTO,GDT1,GCNTPD,GCNTTD,GCNTW,GPG,GPGS,GTYPE1,GCNTB3,GDTA,XDT,XIO,XX,^TMP($J,"GMRK"),^TMP($J,"GMR"),^TMP($J,"GDT"),^TMP($J,"GMRVG")
 K GMRQUAL,GLINE,^TMP($J,"GTNM") Q
GRAPH D DEM^VADPT,INP^VADPT,SETV^GMVGR1
 F GK="PO2","R","P" S ^TMP($J,"GTNM",GK)=0 F GI=0:0 S GI=$O(^TMP($J,"GMRVG",GK,GI)) Q:GI'>0  S GJ="" F X=0:0 S GJ=$O(^TMP($J,"GMRVG",GK,GI,GJ)) Q:GJ=""  S ^TMP($J,"GTNM",GK)=^TMP($J,"GTNM",GK)+1,^TMP($J,"GDT",GI)=""
 S GTNM=0 F X=0:0 S X=$O(^TMP($J,"GDT",X)) Q:X'>0  S GTNM=GTNM+1
 S GPG=$S(GTNM=0:1,1:GTNM\10+''(GTNM#10)),GDT1=0
 F GPGS=1:1:GPG K GMRQUAL,GLINE S (GLINE(1),GLINE(2),GLINE(3))="",^TMP($J,"GMRK","G199")="Page "_GPGS D SETP,DATE S ^TMP($J,"GMRK","G200")=GMRRMBD D PAGE D ^GMVKPO2,^GMVKPO3,^GMVKPO4 D
 .S ^TMP($J,"GMRK","G50M")=0.2,^TMP($J,"GMRK","G82M")=0.6
 D KVAR^VADPT K VA,GRAPHR,GRAPHP,GRAPHS,GRAPHD,GPA Q
PAGE ;SET GRAPH DATA
 ;DATA FOR RESPIRATION GRAPH
 I ^TMP($J,"GMRK","G50")="" F GI=51:1:59 S ^TMP($J,"GMRK","G50")=^TMP($J,"GMRK","G"_GI),^TMP($J,"GMRK","G50M")=0.2+(1.6*(GI-50)) Q:^TMP($J,"GMRK","G50")'=""
 I ^TMP($J,"GMRK","G82")="" F GI=83:1:91 S ^TMP($J,"GMRK","G82")=^TMP($J,"GMRK","G"_GI),^TMP($J,"GMRK","G82M")=0.6+(1.6*(GI-82)) Q:^TMP($J,"GMRK","G82")'=""
 I $D(GMRQUAL) D LEGEND^GMVLGQU
 Q
DATE F GCNTD=1:1:10 S:$L(GDT1) GDT1=$O(^TMP($J,"GDT",GDT1)) S ^TMP($J,"GMRK","G"_GCNTD)=$S($L(GDT1):$E(GDT1,4,5)_"-"_$E(GDT1,6,7)_"-"_$E(GDT1,2,3),1:"") D DATE1
 Q
DATE1 S Y=$E($P(GDT1,".",2)_"0000",1,4),^TMP($J,"GMRK","G"_(GCNTD+16))=$S($L(GDT1):$E(Y,1,2)_":"_$E(Y,3,4),1:"") D SETD
 Q
SETD F GI="R","PO2","P" S GJ=$S(GI="R":48,GI="PO2":80,GI="P":310,1:0) Q:GJ=0  S GK=$S($L(GDT1):$O(^TMP($J,"GMRVG",GI,GDT1,"")),1:"") D SETA^GMVKPO1
 Q
SETP ;INITIALIZE ^TMP FOR QUALIFIERS
 ; 410 - STORE R*   430 - STORE P*   310 - STORE PULSE  330 - STORE PULSE QUALIFIER
 ; 1430+I - PULSE OX. L/MIN
 ; 1450+I - PULSE OX. %  1470+I - PULSE OX. METHOD
 F I=1:1:10 S (^TMP($J,"GMRK","G"_(410+I)),^("G"_(430+I)),^("G"_(1430+I)),^("G"_(1450+I)),^("G"_(1470+I)),^("G"_(310+I)),^("G"_(330+I)))=""
 Q
SETT S GTYP=$O(^GMRD(120.51,"B",$S(GTYPE="R":"RESPIRATION",GTYPE="PO2":"PULSE OXIMETRY",GTYPE="P":"PULSE",1:""),""))
 I GTYP>0 F GX=GSTART1:0 S GX=$O(^GMR(120.5,"AA",DFN,GTYP,GX)) Q:GX>GEND1!(GX'>0)  F GEN=0:0 S GEN=$O(^GMR(120.5,"AA",DFN,GTYP,GX,GEN)) Q:GEN'>0  I '$D(^GMR(120.5,GEN,2)) D BLDARR^GMVGR0
 Q
