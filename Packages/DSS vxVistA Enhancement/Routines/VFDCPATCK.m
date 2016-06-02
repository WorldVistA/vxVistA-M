VFDCPATCK ;DSS/RAC/SMP - PATIENT VALIDATEION FOR SSN AND MRN ; 12/01/2015 15:45
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**2,32**;10 Sept 2015;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine identifies all patients that do not have/invalid SSN 
 ; and those patients that do not have/invalid an Alternate ID of the 
 ; type MRN and prints a report.
 ;
 Q
 ;
EN ;
 N X,Y,CNT,DFN,RPT
 W !!,"Invalid Patients",!
 S DFN=0 F  S DFN=$O(^DPT(DFN)) Q:'DFN  D
 .N RET
 .S Z=^DPT(DFN,0) Q:$D(^DPT(DFN,-9))
 .D IDCHECK^VFDCDPT(.RET,DFN)
 .I '$P(RET,U,2) S RPT(DFN,"SSN")="Invalid or No SSN"
 .I '$P(RET,U) S RPT(DFN,"MRN")="Invalid or No MRN"
PRT ;
 D ^%ZIS
 W #
 W $$CJ("Invalid Patient Report",62),!!
 I '$D(RPT) W "No data to report." Q
 W " |      IEN      |               NAME              | SSN | MRN |",!
 W " |---------------|---------------------------------|-----|-----|",!
 S DFN=0 F  S DFN=$O(RPT(DFN)) Q:'DFN  D
 .N SSN,MRN S CNT=$G(CNT)+1
 .S SSN=$S($D(RPT(DFN,"SSN")):"X",1:" ")
 .S MRN=$S($D(RPT(DFN,"MRN")):"X",1:" ")
 .W " |  "_$$LJ(DFN,13)_"|  "_$$LJ($$GET1^DIQ(2,DFN,.01),31)_"|  "_SSN_"  |  "_MRN_"  |",!
 W !,CNT_" Total Patients Identified",!!
 W "NOTE: An 'X' in either the SSN or MRN column means that the patient is ",!
 W "      missing a value for that field.",!!
 Q
 ;
LJ(X,Y) ;
 Q $$LJ^XLFSTR(X,Y)
 ;
CJ(X,Y) ;
 Q $$CJ^XLFSTR(X,Y)
