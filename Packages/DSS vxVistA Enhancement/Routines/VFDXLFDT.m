VFDXLFDT ;DSS/TFF - DT FUNCTIONS CALLED FROM VFDXLF ;12/16/2014 11:50
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**19**;12 Dec 2014;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;
 Q
 ;
DT(X) ; VALIDATE AND ATTEMPT TO RETURN A VALID DATE
 N %DT S %DT="ST" D ^%DT
 I +Y<1 S X=$$DT1(X) I +X<1 S X=$P(X,U,2) S %DT="STX" D ^%DT
 Q $S(+Y'<1:Y,+$G(X)'<1:X,1:"")
 ;
DT1(VAL) ; TRANSLATE EXTERNAL DATE TIME TO FM DATE TIME
 ;
 ; Converts these 9/23/2004 10:53:25 AM
 ;                09/03/2014 21:00:00
 ;                2009-10-22 05:00:00.0
 ;                10-22-2009 05:00:00.0
 ;                OCT 12,(, )2012 12:10 AM
 ;                OCT 12,(, )2012 13:00
 ;                July 26, 2014 @ 10
 ;                02-OCT-2014 13.29.28
 ;
 ; THE FM CALL BELOW DOES NOT WORK WITH THE TIME PORTIONS
 ; OF THE ABOVE FORMAT
 ; ------------------------------------------------------
 ; D DT^DILF("E","DEC 12,2014",.RESULT)
 ; RESULT=3141212
 ; RESULT(0)="DEC 12, 2014"
 ;
 Q:VAL="" ""
 I $$FMTH^XLFDT(VAL)'=-1 Q VAL
 N DATE,FLG,TIME S DATE=-1,FLG=0
 I $P(VAL," ",2)?.2N.1":".2N.1":".2N S FLG=1
 I $P(VAL," ",2)?.2N.1":".2N.1":".2N.1" ".2A S FLG=1
 I $P(VAL," ",2)?.2N.1":".2N.1":".2N.1".".2N S FLG=1
 I VAL?3A1" "1.2N1","4N.1" ".2N.1":".2N.1":".2N.1" ".2A S FLG=2
 I VAL?3A1" "1.2N1","1" "4N.1" ".2N.1":".2N.1":".2N.1" ".2A S FLG=3
 I VAL?1.A1" "1.2N1","1" "4N.1" ".1"@".1" ".2N.1":".2N S FLG=4
 I VAL?2N1"-"3A1"-"4N.1" ".2N.1".".2N.1".".2N S FLG=5
 I FLG=0 D DT^DILF("E",VAL,.DATE)
 I FLG=1 D DT^DILF("E",$P(VAL," "),.DATE) S TIME=$$DT2($P(VAL," ",2,99))
 I FLG=2 D DT^DILF("E",$P(VAL," ",1,2),.DATE) S TIME=$$DT2($P(VAL," ",3,99))
 I FLG=3 D DT^DILF("E",$P(VAL," ",1,3),.DATE) S TIME=$$DT2($P(VAL," ",4,99))
 I FLG=4 D DT^DILF("E",$$TRIM^XLFSTR($P(VAL,"@")),.DATE) D
 . S TIME=$$DT2($$TRIM^XLFSTR($P(VAL,"@",2)))
 I FLG=5 D DT^DILF("E",$P(VAL," "),.DATE) S TIME=$$DT2($TR($P(VAL," ",2,99),".",":"))
 I DATE=-1 Q -1_U_VAL
 Q DATE_$S($G(TIME)'="":"."_TIME,1:"")
 ;
DT2(TM) ; CONVERT TIME
 N SEC,MIN,HR S TM=$$TRIM^XLFSTR($$UP^XLFSTR(TM))
 I TM'[":",TM?.N D  Q $G(TM)
 . F I=$L(TM):-1:1 Q:$E(TM,I)'=0  I $E(TM,I)=0 S TM=$E(TM,1,I-1)
 Q:TM'[":" ""
 S SEC=$S(TM[" ":$P($P(TM,":",3)," "),TM[".":$P($P(TM,":",3),"."),1:$P(TM,":",3))
 S SEC=$$DT3(SEC) S:$L(SEC)=1 SEC=0_SEC
 S MIN=$P($P(TM,":",2)," "),MIN=$$DT3(MIN) S:$L(MIN)=1 MIN=0_MIN
 S HR=$P($P(TM,":")," "),HR=$$DT3(HR) S:$L(HR)=1 HR=0_HR
 I HR=12&((TM["AM")!($E(TM,$L(TM)-1,$L(TM))="AM")) S HR="00"
 I HR<12&((TM["PM")!($E(TM,$L(TM)-1,$L(TM))="PM")) S HR=HR+12
 S TM=HR_MIN_SEC
 F I=$L(TM):-1:1 Q:$E(TM,I)'=0  I $E(TM,I)=0 S TM=$E(TM,1,I-1)
 Q $S($G(TM)'="":TM,1:"")
 ;
DT3(SEG) ; ONLY NUMBERS
 Q $$TRIM^XLFSTR($TR($$UP^XLFSTR(SEG),"ABCDEFGHIJKLMNOPQRSTUVWXYZ",""))
