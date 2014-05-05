MDOUTOR ; HOIFO/NCA - Post Conversion Routine ; [04-14-2003 10:51]
 ;;1.0;CLINICAL PROCEDURES;**5**;Apr 01, 2004;Build 1
EN1 ; [Procedure]
 ; This post conversion routine will place the Medicine Enter/Edit 
 ; options out of order
 ; Reference IA # 1157 [Supported] Kernel XPDMENU calls.
 ;           IA # 2263 [Supported] XPAR parameter calls
 ;           IA #10075 [Supported] Option File Read.
 ;
 N LP,MDAR,MDFDA,MDLAB,MDPI,MDOPI,MDOPT,MDTXT,MDSEL,MDX
 I $$GET^XPAR("SYS","MD MEDICINE CONVERTED",1) W !!,"Options Already Disabled.",! Q
 K DIR S DIR(0)="YA",DIR("A")="Place the Medicine options OUT OF ORDER? ",DIR("B")="NO" D ^DIR K DIR Q:$D(DIRUT)!$D(DIROUT)!(Y<1)
 K DIR S DIR(0)="SO^1:CATH;2:ECG;3:ECHO;4:EP;5:HOLTER;6:ETT;7:SURGICAL RISK;8:CONSULT;9:GI;10:PULMONARY-GI;11:PFT;12:HEMATOLOGY;13:PACEMAKER;14:RHEUMATOLOGY;15:ALL"
 S DIR("?")="Enter a code from 1-15."
 D ^DIR K DIR Q:$D(DIRUT)!$D(DIROUT)!(Y<1)  S MDSEL=Y
 S MDTXT="Medicine Reports Converted to Clinical Procedures - OPTION OUT OF SERVICE"
 F MDX=1:1 S MDOPT=$P($T(LABEL+MDX),";;",2) Q:MDOPT="**END**"  D
 .Q:MDOPT=""
 .S MDAR(MDX)=MDOPT
 .Q
 S MDLAB=$S(MDSEL'=15:$G(MDAR(MDSEL)),1:"")
 W !!,"Placing the following Medicine options OUT OF ORDER:",!
 I MDSEL=15 F LP=1:1:15 S MDLAB=$G(MDAR(LP)) Q:MDLAB=""  D DISABLE
 I MDSEL=15 D EN^XPAR("SYS","MD MEDICINE CONVERTED",1,1) Q
 D:MDSEL'=15 DISABLE
 W !! K DIR S DIR(0)="YA",DIR("A")="Are All Medicine Reports Converted? ",DIR("B")="NO" D ^DIR K DIR Q:$D(DIRUT)!$D(DIROUT)!(Y<1)
 D EN^XPAR("SYS","MD MEDICINE CONVERTED",1,1)
 Q
DISABLE ; Disabling the options
 F MDX=1:1 S MDOPT=$P($T(@MDLAB+MDX),";;",2) Q:MDOPT="**END**"  D
 .Q:MDOPT=""
 .S MDPI=$$FIND1^DIC(19,"","MX",MDOPT) Q:'MDPI
 .D OUT^XPDMENU(MDOPT,MDTXT)
 .W !,$$GET1^DIQ(19,MDPI_",",1,"E"),"   [",MDOPT,"]"
 Q
 ;
LABEL ; [Labels]
 ;;CATH
 ;;ECG
 ;;ECHO
 ;;EP
 ;;HOLTER
 ;;ETT
 ;;SURG
 ;;CONSULT
 ;;GI
 ;;PULM
 ;;PFT
 ;;HEM
 ;;PACE
 ;;RHEUM
 ;;SUMM
 ;;**END**
OPTIONS ; [Data Module] Enter/Edit Medicine options
CATH ;;[Cardiac Catheterization options]
 ;;MCFSCATH
 ;;MCFPCATH
 ;;MCFLCATH
 ;;MCBLCATH
 ;;MCBSCATH
 ;;MCBPCATH
 ;;MCARCATHIMAGE
 ;;**END**
ECG ;;[Electrocardiogram options]
 ;;MCFSECG
 ;;MCFPECG
 ;;MCFLECG
 ;;MCARECGAUTOSUM
 ;;MCBLECG
 ;;MCBSECG
 ;;MCBPECG
 ;;**END**
ECHO ;;[Echocardiogram options]
 ;;MCFSECHO
 ;;MCFPECHO
 ;;MCFLECHO
 ;;MCARECHOIMAGE
 ;;MCBLECHO
 ;;MCBSECHO
 ;;MCBPECHO
 ;;**END**
EP ;;[Electrophysiology options]
 ;;MCFSEP
 ;;MCFPEP
 ;;MCFLEP
 ;;MCBLEP
 ;;MCBSEP
 ;;MCBPEP
 ;;**END**
HOLTER ;;[Holter options]
 ;;MCFSHOLTER
 ;;MCFPHOLTER
 ;;MCFLHOLTER
 ;;MCBLHOLTER
 ;;MCBSHOLTER
 ;;MCBPHOLTER
 ;;**END**
ETT ;;[Exercise Tolerance Test options]
 ;;MCFSETT
 ;;MCFPETT
 ;;MCFLETT
 ;;MCBLETT
 ;;MCBSETT
 ;;MCBPETT
 ;;**END**
SURG ;;[Surgical Risk Assessment options]
 ;;MCARCATHSRAPRE
 ;;MCARCATHSRAPOST
 ;;MCARSRAPRE
 ;;MCARSRAPOST
 ;;MCARCATHSRAPRINT
 ;;**END**
CONSULT ;;[Consult options]
 ;;MCARGICONSULTEDIT
 ;;MCCONSULTSCREEN
 ;;MCARGICONSULTPRINT
 ;;MCARGICONSULTBRIEF
 ;;MCCONSULTBRSCR
 ;;MCCONSULTBRREPORT
 ;;**END**
GI ;;[GI options]
 ;;MCFLGI
 ;;MCFSGI
 ;;MCFPGI
 ;;MCARGIDIAG
 ;;MCARGIRECALLIST
 ;;MCARGIMAGE
 ;;MCBLGI
 ;;MCBSGI
 ;;MCBPGI
 ;;MCFLNONENDO
 ;;MCFSNONENDO
 ;;MCFPNONENDO
 ;;MCBLNONENDO
 ;;MCBSNONENDO
 ;;MCBPNONENDO
 ;;**END**
PULM ;;[Pulmonary options]
 ;;MCFLPULM
 ;;MCFSPULM
 ;;MCFPPULM
 ;;MCARPULMDIAG
 ;;MCARPULMRECALLIST
 ;;MCARPULMIMAGE
 ;;MCBLPULM
 ;;MCBSPULM
 ;;MCBPPULM
 ;;**END**
PFT ;;[Pulmonary Function Test Options]
 ;;MCFLPFT
 ;;MCFLPFTI
 ;;MCFPPFT
 ;;MCFSPFT
 ;;MCBLPFT
 ;;MCBSPFT
 ;;MCBPPFT
 ;;**END**
HEM ;;[Hematology options]
 ;;MCFLHEM
 ;;MCFSHEM
 ;;MCFPHEM
 ;;MCARHEMIMAGE
 ;;MCBLHEM
 ;;MCBSHEM
 ;;MCBPHEM
 ;;**END**
PACE ;;[Pacemaker options]
 ;;MCARPACEMULTEDIT
 ;;MCARPACEGENIMP
 ;;MCFLALEAD
 ;;MCFLVLEAD
 ;;MCFLSURV
 ;;MCARPACEDIT
 ;;MCBLGENE
 ;;MCBLALEAD
 ;;MCBLVLEAD
 ;;MCBLSURV
 ;;MCFSMULTI
 ;;MCFSGENIMPL.
 ;;MCFSALEAD
 ;;MCFSVLEAD
 ;;MCARPACESCREENSURV
 ;;MCARPACESCREENDEMO
 ;;MCBSGENI
 ;;MCBSALEAD
 ;;MCBSVLEAD
 ;;MCPACSURVBRSCR
 ;;MCARPACEGENPRINT
 ;;MCFPALEAD
 ;;MCFPVLEAD
 ;;MCARPACESURVPRINT
 ;;MCARPACEPATIENT
 ;;MCBPGEN.IMPLANT
 ;;MCBPALEAD
 ;;MCBPVLEAD
 ;;MCPACSURVBRREPORT
 ;;**END**
RHEUM ;;[Rheumatology options]
 ;;MCRHDIAGF
 ;;MCRHBACKF
 ;;MCRHNARRF
 ;;MCRHLABF
 ;;MCRHHAQF
 ;;MCRHPATHISTF
 ;;MCRHPHYSF
 ;;MCRHDEATHF
 ;;MCRHDIAGP
 ;;MCRHBACKP
 ;;MCRHNARRP
 ;;MCRHLABP
 ;;MCRHHAQP
 ;;MCRHPATHISTP
 ;;MCRHPHYSP
 ;;MCRHDEATHP
 ;;MCRHALLP
 ;;MCBPRHEUM
 ;;MCRHIMAGE
 ;;MCRHDIAGL
 ;;MCRHNARRL
 ;;MCRHHAQL
 ;;MCRHPATHISTL
 ;;MCRHPHYSL
 ;;MCRHDEATHL
 ;;MCRHBRIEF
 ;;MCFLGEN
 ;;MCFSGEN
 ;;MCGENERICIMAGE
 ;;MCBLGEN
 ;;MCBSGEN
 ;;**END**
SUMM ;;[Procedure Summary option]
 ;;MCARSUMMARY
 ;;**END**
