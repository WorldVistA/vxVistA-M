VFDGMTSSSN ; RAC - SSN component; 12/07/2015  13:45
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**34**;07 Dec 2015;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;                 
 ;                     
VX ; get vxvista specific fields from file 2
 N STR,STR1,GMTSQIT
 Q:'$G(ORDFN)
 D CKP^GMTSUP Q:$D(GMTSQIT)
 S STR1=$$CURSSN^VFDSSN(ORDFN)
 I $L(STR1)=9 S STR1=$E(STR1,1,3)_"-"_$E(STR1,4,5)_"-"_$E(STR1,6,9)
 I $L(STR1)=10 S STR1=$E(STR1,1,3)_"-"_$E(STR1,4,5)_"-"_$E(STR1,6,10)
 I STR1="" S STR1="No SSN"
 S STR="Social Security Number:  "_STR1
 S STR=$J(" ",45-$L(STR))_STR
 W STR,!
 D CKP^GMTSUP Q:$D(GMTSQIT)
 Q
