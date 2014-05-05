PXCADX ;ISL/dee & LEA/Chylton - Translates data from the PCE Device Interface into PCE's PXK format for POV ;8/1/96
 ;;1.0;PCE PATIENT CARE ENCOUNTER;;Aug 12, 1996
 Q
 ;
POV(PXCADIAG,PXCANUMB,PXCAPRV,PXCAERRS) ;DIAGNOSIS node
 ; Variables
 ;   PXCADIAG  Copy of a Diagnosis node of the PXCA array
 ;   PXCAPRV   Pointer to the provider (200)
 ;   PXCANUMB  Count of the number if POVs
 ;   PXCAFTER  Temp used to build ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"AFTER")
 ;
 N PXCAFTER,PXCAITEM
 S PXCAFTER=$P(PXCADIAG,"^",1)_"^"
 S PXCAFTER=PXCAFTER_PXCAPAT_"^"_PXCAVSIT_"^"
 S PXCAFTER=PXCAFTER_$P(PXCADIAG,"^",8)_"^^^^^^^^"
 S PXCAITEM=$P(PXCADIAG,"^",2)
 S PXCAITEM=$S(PXCAITEM["P":"P",PXCAITEM["S":"S",1:"")
 S PXCAFTER=PXCAFTER_PXCAITEM_"^^^"
 S PXCAFTER=PXCAFTER_$P(PXCADIAG,"^",10)_"^"
 S PXCAFTER=PXCAFTER_$P(PXCADIAG,"^",7)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,"IEN")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"AFTER")=PXCAFTER
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,12,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,12,"AFTER")="^^^"_$S(PXCAPRV>0:PXCAPRV,1:"")
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,800,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,800,"AFTER")=$P(PXCADIAG,"^",3,6)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,802,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,802,"AFTER")=$P(PXCADIAG,"^",9)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,812,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,812,"AFTER")="^"_PXCAPKG_"^"_PXCASOR
 Q
 ;
 ;
DX(PXCADXPL,PXCANUMB,PXCAPRV,PXCAERRS) ;DIAGNOSIS/PROBLEM node
 ; Variables
 ;   PXCADXPL   Copy of a DIAGNOSIS/PROBLEM node of the PXCA array
 ;   PXCANUMB   Count of the number if POVs
 ;   PXCAPRV    Pointer to the provider
 ;   PXCAFTER   Temp used to build ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"AFTER")
 ;   PXCAITEM   Temp used to hold an item while processing it
 ;
 N PXCAFTER,PXCAITEM
 S PXCAFTER=$P(PXCADXPL,"^",1)_"^"
 S PXCAFTER=PXCAFTER_PXCAPAT_"^"_PXCAVSIT_"^"
 S PXCAFTER=PXCAFTER_$P(PXCADXPL,"^",13)_"^^^^^^^^"
 S PXCAITEM=$P(PXCADXPL,"^",2)
 S PXCAITEM=$S(PXCAITEM["P":"P",PXCAITEM["S":"S",1:"")
 S PXCAFTER=PXCAFTER_PXCAITEM_"^^^"
 S PXCAFTER=PXCAFTER_$P(PXCADXPL,"^",3)_"^"
 S PXCAFTER=PXCAFTER_$P(PXCADXPL,"^",4)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,"IEN")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,0,"AFTER")=PXCAFTER
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,12,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,12,"AFTER")="^^^"_$S(PXCAPRV>0:PXCAPRV,1:"")
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,800,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,800,"AFTER")=$P(PXCADXPL,"^",9,12)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,802,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,802,"AFTER")=$P(PXCADXPL,"^",14)
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,812,"BEFORE")=""
 S ^TMP(PXCAGLB,$J,"POV",PXCANUMB,812,"AFTER")="^"_PXCAPKG_"^"_PXCASOR
 Q
 ;
