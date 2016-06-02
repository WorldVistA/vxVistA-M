VFDDIDST ;DSS/SGM - DISPLAY TEMPLATES FOR CAC ; 12/10/2013 14:00
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**22**;12 Feb 2014;Build 1
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ; Code to select templates taken from ^DIBT
 ;
 N I,J,X,Y,Z,D0,FILE,FTMP,TMPL
 D INTRO
 S FTMP=$$ASK1 Q:FTMP<.4
 S FILE=$$FILE Q:FILE<2
 S TMPL=$$TMPL Q:TMPL<0
 S D0=+TMPL,X=$P(FTMP,U,3,4) W !! D @X
 Q
 ;
ASK1() ; prompt for template file to display
 N I,J,X,Y,Z,DIR,DIROUT,DIRUT,DTOUT,DUOUT,VFDTEMP
 S J=0
 F I=.4,.401,.402 S J=J+1,VFDTEMP(I)=$P("Print^Sort^Input",U,J)_" Template"
 S Z="",I=0 F  S I=$O(VFDTEMP(I)) Q:'I  S Z=Z_I_":"_VFDTEMP(I)_";"
 S DIR(0)="SOM^"_Z,DIR("A")="Select TEMPLATE File"
 D ^DIR I Y=""!$D(DTOUT)!$D(DUOUT) Q -1
 S X=$S(+Y=.4:"^^DIPT",+Y=.401:"DIBT^DIPT",1:"^^DIET")
 S Y=Y_U_VFDTEMP(Y)_X
 Q Y
 ;
FILE() ; select file to display FM templates
 ;;N % W:$X>53 !?9 I Y-1.1,Y-.6,$D(^DIC(Y,0,"GL")),^("GL")'["[",$D(@(^("GL")_"0)")) S %=+$P(^(0),U,4) W ?40,"  ("_%_" entr"_$P("ies^y",U,%=1+1)_")"
 ; code taken from DIC^DICRW
 N I,J,X,Y,Z,DIC
 S DIC=1,DIC("A")="Select FILE: "
 S DIC(0)="QAEM"
 S Z=$G(^DISV(DUZ,DIC)) I Z>0,$D(^DIC(+Z,0)) S DIC("B")=$P(^(0),U)
 S DIC("W")=$P($T(FILE+1),";",3)
 S DIC("S")="I Y'<2"
 I DUZ(0)'="@" S DIC("S")="I Y'<2 N %,A1,DIAC,DIFILE S DIAC=""RD"",DIFILE=Y D ^DIAC I %"
 Q $$DIC
 ;
TMPL() ; select specific template
 N I,J,X,Y,Z,DIC,DTOUT,DUOUT
 S DIC=+FTMP,DIC(0)="QAEM",DIC("S")="I $P(^(0),U,4)=+FILE"
 Q $$DIC
 ;
DIC() D ^DIC S:Y=""!$D(DTOUT)!$D(DUOUT) Y=-1 Q Y
 ;
INTRO ;
 ;;This will display the fields in a FileMan template.  First choose
 ;;the type of template you wish to display.  Then you will be asked
 ;;for the FileMan which contains that template.  Then you will be
 ;;to select a template of the type chosen from the file selected.
 ;;
 ;
 N I,J,X,Y,Z
 W @IOF,$$CJ^XLFSTR("DISPLAY FILEMAN TEMPLATES",80),!
 F I=1:1 S X=$T(INTRO+I) W !,"  "_$TR(X,";"," ") Q:$P(X,";;",2)=""
 Q
