RORNTEG0 ;ISC/XTSUMBLD KERNEL - Package checksum checker ; 7/19/06 12:36pm
 ;;0.0;;**1**;;Build 24
 ;;7.3;3060622.15354
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
RORX005C ;;6689502
RORX006 ;;1957749
RORX006A ;;4045076
RORX006C ;;4479620
RORX007 ;;6378955
RORX007A ;;2467042
RORX008 ;;2076427
RORX008A ;;7092822
RORX009 ;;3203627
RORX009A ;;10011538
RORX009C ;;9417785
RORX010 ;;6851839
RORX011 ;;8741230
RORX012 ;;3000028
RORX012A ;;10697075
RORX013 ;;1815134
RORX013A ;;7019975
RORX013C ;;3235328
RORX014 ;;2137320
RORX014A ;;6097645
RORX015 ;;2476351
RORX015A ;;8934895
RORX015C ;;5096395
RORX016 ;;1551044
RORX016A ;;2574228
RORX016B ;;2976507
RORX016C ;;4657732
RORXU001 ;;2567828
RORXU002 ;;7826593
RORXU003 ;;5929587
RORXU004 ;;405249
RORXU005 ;;3236020
RORXU006 ;;5450262
RORXU007 ;;6706394
RORXU008 ;;2215188
