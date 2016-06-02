VFDXUVX ;DSS/SGM - SUPPORT MODS TO VA KERNEL ROUTINES ;09/30/2013 15:28
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;25 Sep 2013;Build 6
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This routine will support the VFD() type modules added to the bottom
 ;of VA KERNEL routines in lieu of actually having that VFD() module in
 ;the VA routines.
 ;
 Q
 ; ICR#  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------------------------
 ; 2053  FILE^DIE
 ; 2056  $$GET1^DIQ
 ; 2263  $$GET^XPAR
 ;10103  $$NOW^XLFDT
 ;10096  All nodes exported by Kernel are useable
 ;       ----------------------------------------
 ;       No ICR supporting the external reference
 ;       ----------------------------------------
 ;       1. Fileman read, write, delete to file 14.5
 ;       2. ^%ZOSF("ZVX") not exported by Kernel but by vxVistA
 ;
 ;---------------------------------------------------------------------
 ;                        Modifications To ^ZTMB
 ;---------------------------------------------------------------------
ZTMB ; called from ZTMB
 ; Honor VFD ACTION ON TASKMAN STARTUP parameter
 Q:'$$OSEHRA
 N VFD,VFDI
 S VFD=$$GETXPAR("VFD ACTION ON TASKMAN STARTUP")
 I VFD S VFDI=0 F  S VFDI=$O(^%ZIS(14.5,VFDI)) Q:'VFDI  D
 . N DIERR,VFDERR,VFDFDA
 . I VFD=2,$$GET1^DIQ(14.5,VFDI,1,"I",,"VFDERR")'="S" Q
 . K DIERR,VFDERR S VFDFDA(14.5,VFDI_",",1)="@"
 . D FILE^DIE(,"VFDFDA","VFDERR")
 . Q
 Q
 ;
 ;-----------------------  PRIVATE SUBROUTINES  -----------------------
 ;
GETXPAR(PARM,ENT) ;
 S ENT=$G(ENT) S:ENT="" ENT="SYS"
 Q $$GET^XPAR(ENT,PARM)
 ;
OSEHRA() N VX S VX=$G(^%ZOSF("ZVX")) Q $S(VX="VXS":2,VX["VX":1,1:0)
