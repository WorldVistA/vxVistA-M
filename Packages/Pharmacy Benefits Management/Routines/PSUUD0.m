PSUUD0 ;BIR/TJH,PDW;PBM UNIT DOSE CONTROL POINT;07/08/1998
 ;;4.0;PHARMACY BENEFITS MANAGEMENT;;MARCH, 2005
EN ; Entry point for processing
 ;
 ;
 D ^PSUUD1 ; Collect all data
 I '$D(^XTMP("PSU_"_PSUJOB,"PSUFLAG")) D
 .D ^PSUUD3        ; Mail reports
 .D EN^PSUUD7      ; Mail AMIS summary reports
 ;
 ;pt. demographics summary reports
 D PULL^PSUCP
 F I=1:1:$L(PSUOPTS,",") S PSUMOD($P(PSUOPTS,",",I))=""
 ;
 I $D(PSUMOD(10)) D UDSSN^PSUDEM4   ;Provider extract
 ;
 I '$D(^XTMP("PSU_"_PSUJOB,"PSUFLAG")) D
 .I '$D(^XTMP("PSU_"_PSUJOB,"PSUMFLAG")) D
 ..D EN^PSUSUM3     ;UD PD summary
 ..;
 ..;IV/UD summary report
 ..I $D(PSUMOD(2))&$D(PSUMOD(1))&'$D(PSUMOD(4)) D
 ...D EN^PSUSUM5
 ;
 K ^XTMP("PSU_"_PSUJOB,"PSUFLAG1")
 D CLEAN
 K UDAM,SPEC,AMIS,DOSE,DOSTOT,DIVTOT,GTOT
 Q
PRINT ; Entry point for printing function
 D ^PSUUD5 ; Print summary reports
 D CLEAN
 Q
 ;
CLEAN ; clean up local symbol table
 S XPSUOPTN=PSUOPTN,XPSUJOB=PSUJOB M XPSUMOD=PSUMOD
 D VARKILL^PSUTL ; kill all PSU namespace variables
 S PSUOPTN=XPSUOPTN,PSUJOB=XPSUJOB M PSUMOD=XPSUMOD K XPSUOPTN,XPSUJOB,XPSUMOD
 K DADATE,DADRUG,DAHOW,DAMT,DASH,DFN,DIC,DLM,ENDIT,EXTD
 K PSDATE,PSDOSE,PSECT,PSPAT,REC1,REC2,SPACES,X1,X2,Y,Z
 D PULL^PSUCP,OPTS^PSUCP
CLEANQ Q