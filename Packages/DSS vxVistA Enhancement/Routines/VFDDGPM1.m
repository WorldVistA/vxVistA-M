VFDDGPM1 ;DSS/LM/SMP - PAMS Movement Events Support ; 10/15/2015 16:40
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**21**;11 Jun 2013;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; Only to be called from routine VFDDGPM
 ;
 ; ICR#  Supported Description
 ; ----  ---------------------
 ; 1609  Lexicon setup search parameters
 ; 3990  ICD code API
 ; 
 Q
 ;
ADMITDX() ; expects X=lookup value
 N I,J,X,Y,D0,DA,DIC,DTOUT,DUOUT,ICDT,ICD10
 S ICDT=+$G(DGPMAN) I 'ICDT S ICDT=DT
 S ICD10=ICDT>$$IMP^ICDEX(30)
 D CONFIG^LEXSET($S(ICD10:"10D",1:"ICD"),,ICDT)
 S DIC="^LEX(757.01,",DIC(0)="EQM",DIC("A")="DIAGNOSIS [ICD]: " D ^DIC
 I '$D(Y($S(ICD10:30,1:1))) Q ""
 Q +$$ICDDX^ICDEX($S(ICD10:$G(Y(30)),1:$G(Y(1))),ICDT)
 ;
