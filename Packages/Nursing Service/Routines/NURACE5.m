NURACE5 ;HIRMFO/RM-PATIENT CLASSIFICATION CRITICAL CARE ;08 Apr 86
 ;;4.0;NURSING SERVICE;;Apr 25, 1997
EN1 ;ENTER IN CLASSIFICATION FACTORS
 S ABORTSW=0,CHANGESW=0,NURS1SW=0,NURS3SW=0,NURS6SW=0,NURS8SW=0
 I FACT["?" D EN3^NURACE3 S REENTSW=1 Q
 F I=1:1:$L(FACT) S:(($A(FACT,I)<65)!($E(FACT,I)'?1A)!($A(FACT,I)>74)) ABORTSW=1 Q:ABORTSW=1  S FACT($E(FACT,I))=0
 I ABORTSW=1 W *7,"  *** BAD ENTRY - TRY AGAIN ***" S REENTSW=1 Q
 D ONECK
 S FACT="",NXT="" F I=0:0 S NXT=$O(FACT(NXT)) Q:NXT=""  S FACT=FACT_NXT
 I FACT="J" S:FACT=FACTORS NURSCKSW=1 I FACT'=FACTORS S FACTORS=FACT,CHANGESW=1 F I=1:1:$L(FACT) S:I=1 FACTX=$E(FACT,1) S:I'=1 FACTX=FACTX_","_$E(FACT,I)
 G:FACT="J" EN2
 I ((FACT="")&(FACTORS="")) W !,*7,"**** NO FACTORS ENTERED - CLASSIFICATION NOT UPDATED ****" H 3 S OUTSW=1 Q
 I NURS1SW=1 W !,*7,"*** FACTORS A or B CANNOT BE USED TOGETHER ***" S REENTSW=1 Q
 I NURS3SW=1 W !,*7,"*** FACTORS C,D or E CANNOT BE USED TOGETHER ***" S REENTSW=1 Q
 I NURS6SW=1 W !,*7,"*** FACTORS F or G CANNOT BE USED TOGETHER ***" S REENTSW=1 Q
 I NURS8SW=1 W !,*7,"*** FACTORS H or I CANNOT BE USED TOGETHER ***" S REENTSW=1 Q
 I ((FACT="")!(FACT=FACTORS)) S NURSCKSW=1 G EN2
 I ((FACT'["A")&(FACT'["B")) W !,*7,"*** YOU MUST PICK ONE OF THE FIRST TWO FACTORS A or B***" S REENTSW=1 Q
 I ((FACT'["C")&(FACT'["D")&(FACT'["E")) W !,*7,"*** YOU MUST PICK ONE OF THE SECOND THREE FACTORS C,D, or E***" S REENTSW=1 Q
 S FACTORS=FACT F I=1:1:$L(FACT) S:I=1 FACTX=$E(FACT,1) S:I'=1 FACTX=FACTX_","_$E(FACT,I)
 S CHANGESW=1
EN2 ;DETERMINE NEW CLASSIFICATION
 I FACTORS["J" S CLASSX=3 G CHKCLASS
 S CAT(1)=0,CAT(2)=0,CAT(3)=0
 F I="A","C","F","H" S:FACTORS[I CAT(1)=CAT(1)+1
 F I="B","D","F","G","H","I" S:FACTORS[I CAT(2)=CAT(2)+1
 F I="B","E","G","I" S:FACTORS[I CAT(3)=CAT(3)+1
 S CAT(2)=CAT(2)+1
 I ((CAT(1)>CAT(2))&(CAT(1)>CAT(3))) S CLASSX=1 G CHKCLASS
 I (((CAT(2)>CAT(1))!(CAT(2)=CAT(1)))&(CAT(2)>CAT(3))) S CLASSX=2 G CHKCLASS
 S CLASSX=3
CHKCLASS ;ENTER NEW CLASSIFICATION IF DESIRED
 Q:NURSNSW=1
 I $D(XCLAS) I ((NURSCKSW=1)&(CLASSX=XCLAS)) S CHANGESW=1
CHKCLAS1 ;
 W !,"Enter Classification: " W:(CLASSX'="") CLASSX,"//" R X:DTIME S X=$E(X,1,2)
 I X="^"!('$T) S OUTSW=1 Q
 I X["?" W !,"ANSWER WITH A NUMBER BETWEEN 1 AND 3" G CHKCLAS1
 I $L(X)=0 S:CHANGESW=1 CONFIGX="COMPUTER" Q
 I (($L(X)>1)!(X?1A)!(X<1)!(X>3)) W *7,"   *** BAD ENTRY - TRY AGAIN ***" G CHKCLASS
 I X=CLASSX S:CHANGESW=1 CONFIGX="COMPUTER" Q
 S CHANGESW=1,CLASSX=X,CONFIGX="USER"
 Q
ONECK ;VALIDATE FACTOR ENTRIES
 I FACT["A" I FACT["B" S NURS1SW=1
 I FACT["C" I ((FACT["D")!(FACT["E")) S NURS3SW=1
 I FACT["D" I FACT["E" S NURS3SW=1
 I FACT["F" I FACT["G" S NURS6SW=1
 I FACT["H" I FACT["I" S NURS8SW=1
 Q
