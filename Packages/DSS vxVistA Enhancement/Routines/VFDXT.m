VFDXT ;DSS/SGM - MAIN DRIVER FOR IT UTILITIES ; 06/20/2011 17:40
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;=============================  OPTIONS  =============================
 ;
DEL ; option: VFD IT ROUTINE DELETE
 ; delete selected routines
 G DEL^VFDXTR
 ;
BROKSTRT ; option: VFD XWB START LISTENER
 ; manually start the new style Broker not using Taskman
 G STRT^VFDXWB
 ;
BROKSTOP ; option: VFD XWB STOP LISTENER
 ; manually stop the new style Broker not using Taskman
 G STOP^VFDXWB
 ;
CHKSUM ; option: VFD IT CHECKSUM REPORT
 G CHKSUM^VFDXTR
 ;
FINDD ; option: VFD IT DD-NAME-NUMBERSPACE
 ; gets fields and namespaces from the data dictionary.
 G EN^VFDXTDD1
 ;
FINDINST ; option: VFD IT FIND INSTALL HISTORY
 G EN^VFDXTFND
 ;
FOIAZ ; option: VFD IT INIT CACHE.DAT
 ; initialize a Cache.dat file for baseline VistA functions
 G ^VFDVFOIZ
 ;
FOIAZC ; option: VFD IT CLONE A CACHE.DAT FILE
 ; Clone a Cache.dat File
 W !!?3,"The underlying code has been lost."
 I $P($$NOW^XLFDT,".",2)#2 D
 .W !!?3,"Hopefully it will be found someday."
 E  W !!?3,"Hopefully it existed in the past..."
 ;G CLONE^VFDVFOIZ
 Q
 ;
MULTI ; option VFD IT MULTI-ACCOUNT SIGN-ON
 ; vxVistA multi-Account Sign-on Utility
 W !!?3,"The underlying code has been lost."
 I $P($$NOW^XLFDT,".",2)#2 D
 .W !!?3,"Hopefully it will be found someday."
 E  W !!?3,"Hopefully it existed in the past..."
 ;G EN^VFDZUMU
 Q
 ;
NUM ; option: VFD IT ROUTINE UPDATE
 ; updated version of VA XTVNUM utility
 ; Date stamp 1st line, update version#, update patch list
 ; insert copyright statement
 G NUM^VFDXTR
 ;
POINTER ; option: VFD IT POINTER REPORT
 G EN^VFDXTPT0
 ;
SIZE ; option: VFD IT ROUTINE SIZE
 ; display routine size per March 2007 VA Programming SAC
 ; 20K total size, 15K executable code size, 5K comments
 ; Any line with a label or starts with " ;;" is counted as executable
 G SIZE^VFDXTR
 ;
SVRES ; option: VFD IT ROUTINE SAVE/RESTORE
 ; save routines to indiviudual HFS files
 ; restored routines from individual HFS files previously saved by this
 G SVRES^VFDXTR
 ;
 ;========================  REMOTE PROCEDURES  ========================
 ;
 ;=============  APPLICATION PROGRAMMER INTERFACES (APIs)  ============
 ;
MEDIAN(VFDAR) ; Return median value for a list of numbers
 Q:$G(VFDAR)="" ""  Q:$O(@VFDAR@(""))="" ""  Q $$MEDIAN^VFDXTMTH(VFDAR)
