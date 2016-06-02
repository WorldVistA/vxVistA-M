VFDXWBC ;DSS/SGM - SUPPORT MODS TO VA KERNEL ROUTINES ;09/30/2013 14:30
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;25 Sep 2013;Build 6
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This routine will support the VFD() type modules added to the bottom
 ;of VA KERNEL routines in lieu of actually having that VFD() module in
 ;the VA routines.
 ;
 ;!!! This routine is only invoked via ^VFDXWB !!!
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
 ;                       Modifications To ^XWBTCPM
 ;---------------------------------------------------------------------
 ;
PARM ; called from INIT
 ; get parameter values
 S VFDAUDIT=$$GETXPAR("VFD RPC AUDIT ENABLED")
 S VFDPMODE=$$GETXPAR("VFD RPC AUDIT PATIENTS")
 S VFDUMODE=$$GETXPAR("VFD RPC AUDIT USERS")
 ;
 S (VFDCNT,VFDDFN,VFDIENS,VFDQUIT,VFDRPC)=""
 S VFDHNDL=$$NOW^XLFDT_"~"_$J
 I VFDAUDIT,XWBDEBUG S VFDAUDIT=2 ;Indicates AUDIT + DEBUG
 I VFDAUDIT S XWBDEBUG=3 ;         Need 'very verbose' for VFDAUDIT
 Q
 ;
VLIST() ; list of variable for argument of NEW or KILL command
 ; do not include a final comma at end of list
 ;;VFDAUDIT,VFDCNT,VFDDFN,VFDHNDL,VFDIENS,VFDPMODE,VFDQUIT,VFDRPC
 ;;VFDUMODE
 ;;
 N I,X,Z S Z=""
 F I=2:1 S X=$P($T(VLIST+I),";",3) Q:X=""  S:I>2 X=","_X S Z=Z_X
 Q Z
 ;
 ;-----------------------  PRIVATE SUBROUTINES  -----------------------
 ;
GETXPAR(PARM,ENT) ;
 S ENT=$G(ENT) S:ENT="" ENT="SYS"
 Q $$GET^XPAR(ENT,PARM)
 ;
OSEHRA() N VX S VX=$G(^%ZOSF("ZVX")) Q $S(VX="VXS":2,VX["VX":1,1:0)
