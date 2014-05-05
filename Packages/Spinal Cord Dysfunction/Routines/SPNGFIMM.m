SPNGFIMM ;WDE/SD OUTCOME GRID FOR FIM'S 9/19/2002
 ;;2.0;Spinal Cord Dysfunction;**19**;01/02/1997
CALC ;  Create the values
 ;called from spngfimh  gets the scores to plug into the grids
 ;  da = the current fim with a score of 4,5,9 or 10
 ;  now get the START fim 
 S SPNGFIS=""
 S SPNXX=0 F  S SPNXX=$O(^TMP($J,SPNXX)) Q:SPNXX=""  S SPNYY=0 F  S SPNYY=$O(^TMP($J,SPNXX,SPNYY)) Q:SPNYY=""  S SPNZZ=0 F  S SPNZZ=$O(^TMP($J,SPNXX,SPNYY,SPNZZ)) Q:SPNZZ=""  D  Q:+SPNGFIS
 .I $P(^SPNL(154.1,SPNZZ,0),U,2)=2 I 16[$P(^SPNL(154.1,SPNZZ,2),U,17) S SPNGFIS=SPNZZ
 ;SPNGFIS IS THE FIM START RECORD
 Q:$D(SPNGFIS)=0
 Q:SPNGFIS=""
 ;total gain
 S SPNR1C1A=$$GET1^DIQ(154.1,DA_",",999.05)
 S SPNR1C1B=$$GET1^DIQ(154.1,SPNGFIS_",",999.05)
 S SPNR1C1=SPNR1C1A-SPNR1C1B
 ;Total efficiency
 S SPNR1C2A=$$GET1^DIQ(154.1,DA_",",999.08)
 I $G(SPNR1C2A)>0 S SPNR1C2=SPNR1C1/SPNR1C2A S SPNR1C2=$E(SPNR1C2,1,4)
 I $G(SPNR1C2A)=0 S SPNR1C2="N/A"
 ;Motor Gain
 S SPNR1C3A=$$GET1^DIQ(154.1,DA_",",999.03)
 S SPNR1C3B=$$GET1^DIQ(154.1,SPNGFIS_",",999.03)
 S SPNR1C3=SPNR1C3A-SPNR1C3B
 ;Motor efficiency
 S SPNR1C4A=$$GET1^DIQ(154.1,DA_",",999.08)
 I $G(SPNR1C4A)>0 S SPNR1C4=SPNR1C3/SPNR1C4A S SPNR1C4=$E(SPNR1C4,1,4)
 I $G(SPNR1C4A)=0 S SPNR1C4="N/A"
 ;Cognitive Gain
 S SPNR1C5A=$$GET1^DIQ(154.1,DA_",",999.04)
 S SPNR1C5B=$$GET1^DIQ(154.1,SPNGFIS_",",999.04)
 S SPNR1C5=SPNR1C5A-SPNR1C5B
 ;Cognitive Efficiency
 S SPNR1C6A=$$GET1^DIQ(154.1,DA_",",999.08)
 I $G(SPNR1C6A)>0 S SPNR1C6=SPNR1C5/SPNR1C6A S SPNR1C6=$E(SPNR1C6,1,4)
 I $G(SPNR1C6A)=0 S SPNR1C6="N/A"
