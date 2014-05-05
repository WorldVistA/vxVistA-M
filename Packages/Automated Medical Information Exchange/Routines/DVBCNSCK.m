DVBCNSCK ;ALB/GTS-557/THM-NOSE AND SINUS ; 7/1/91  10:14 AM
 ;;2.7;AMIE;;Apr 10, 1995
 ;
 S PG=1,HD91="Department of Veterans Affairs",HD9=$S($D(CMBN):"Abbreviated",1:"Full")_" Exam Worksheet"
EN D:'$D(IOF) SETIOF W:(IOST?1"C-".E) @IOF
 W !?25,HD91,!?22,"Compensation and Pension Examination",! W ?33,"# 1315 Worksheet" S HD7="NOSE AND SINUS",HD8="For "_HD7 W !?(40-($L(HD9)\2)),HD9,!?(40-($L(HD8)\2)),HD8,!!
 W !,"Name: ",NAME,?45,"SSN: ",SSN,!?45,"C-number: ",CNUM,!,"Date of exam: ____________________",!!,"Place of exam: ___________________",!!,"Type of Exam: ",HD7
 W !!!!,"Narrative:",?13,"Report both functional and cosmetic impairment.",!!
 I '$D(CMBN) W "A. Medical history:",!!!!!!!!!!,"B. Subjective complaints:",!!!!!!!!!!,"C. Objective findings:",! D HD2
 W $S($D(CMBN):"A. ",1:"D. "),"Specific evaluation information required by the rating board",!?4,"(if the information requested is included elsewhere, do not",!?4,"repeat here):",!!!
 W ?8,"1. External nose -",!!!!!?8,"2. Nasal vestibule -",!!!!!?8,"3. Right and left nasal cavities -",!!!!!
 W ?11,"a. Septum -",!!!?10," b. Floor of the nose -",!!!!?9,"  c. Inferior meatus -",!!!!
 W ?10," d. Inferior turbinates -",!!!!?10," e. The middle meati -",!!!!?10," f. The middle turbinate -",!!!! D HD2 W ?10," g. The spheno-ethmoidal recess -",!!!!?10," h. The olfactory area -",!!!!?10," i. The superior turbinates -",!!!!!!
 W ?8,"4. The paranasal sinuses- ",!!!!!!
 W $S($D(CMBN):"B. ",1:"E. "),"Diagnostic/clinical test results:",!!!!!!!!!! D:$Y>50 HD2 W $S($D(CMBN):"C. ",1:"F. "),"Diagnosis:",!!!!!!!!!?25,"Signature: ______________________________",!!?30,"Date: _________________________",!
 K LN,LN1,LN2
 Q
 ;
HD2 S PG=PG+1 W @IOF,!,"Page: ",PG,!!,"Compensation and Pension Exam for ",HD7,!,"for "_NAME,!!!
 Q
 ;
SETIOF ;  ** Set device control var's
 D HOME^%ZIS
 Q
