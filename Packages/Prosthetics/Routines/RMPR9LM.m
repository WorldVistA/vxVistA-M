RMPR9LM ;HOIFO/SPS - GUI 2319 Tab 2 Last Patient Movement  ;9/19/02  11:29
 ;;3.0;PROSTHETICS;**59**;Feb 09, 1996
A1(IEN) G A2
EN(RESULTS,IEN) ;broker entry point
A2 ;
 S DFN=$P($G(^RMPR(668,IEN,0)),U,2)
 I DFN="" S RESULTS(0)="NOTHING TO REPORT" G EXIT
 S VAIP("D")="L" D DEM^VADPT,IN5^VADPT,ELIG^VADPT
 ;Header Info
 ;Last movement
 S RESULTS(0)=VADM(1) ;NAME
 S RESULTS(1)=VADM(2) ;SSN
 S RESULTS(2)=VADM(3) ;DOB
 S RESULTS(3)=VAEL(7) ;CLAIM #
 ;Last Movement
 S RESULTS(4)=$S(VAIP(1)>0:1,1:0) ;1=MOVEMENT,0=NONE
 S RESULTS(5)=$P(VAIP(2),U,2) ;Trans Type
 S RESULTS(6)=$P(VAIP(3),U,2) ;Date
 S RESULTS(7)=$P(VAIP(4),U,2) ;Type of Movement
 S RESULTS(8)=$P(VAIP(5),U,2) ;Ward
 S RESULTS(9)=$P(VAIP(7),U,2) ;Physician
 S RESULTS(10)=VAIP(9) ;Diagnosis
 ;2nd from Last Movement
 S RESULTS(11)=$S(VAIP(13)>0:1,1:0) ;1=MOVEMENT,0=NONE
 S RESULTS(12)=$P(VAIP(13,2),U,2) ;Trans. type
 S RESULTS(13)=$P(VAIP(13,1),U,2) ;Date
 S RESULTS(14)=$P(VAIP(13,3),U,2) ;Type of Movement
 S RESULTS(15)=$P(VAIP(13,4),U,2) ;Ward
 S RESULTS(16)=$P(VAIP(13,5),U,2) ;Physician
 S RESULTS(17)=VAIP(13,7) ;Diagnosis
EXIT ;common exit point
 I '$D(RESULTS) S RESULTS(0)="NOTHING TO REPORT"
 K DFN,VADM,VAIL,VAIP
 ;END