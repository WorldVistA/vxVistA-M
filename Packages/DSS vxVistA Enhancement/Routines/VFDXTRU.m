VFDXTRU ;DSS/SGM - ROUTINE UTILITIES ; 07/28/2011 14:37
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; ICR#   Description
 ;------  -------------------------------------------------------
 ;        $$CJ^XLFSTR
 ;        ^%ZOSF - all nodes can be referenced
 ;
 ;
 ;===============  GET LIST OF ROUTINES FROM BUILD FILE  ==============
BLD(VFDR,VAL,KU) ;
 Q $$BLD^VFDXTRU1($G(VFDR),$G(VAL),$G(KU))
 ;
 ;=====================  GET BUILD NAME AND DATA  =====================
BLDNM(VFDVAL) ;
 Q $$BLDNM^VFDXTRU1(.VFDVAL)
 ;
 ;=============================  READER  ==============================
DIR(DIR) ;
 Q $$DIR^VFDXTRU1(.DIR)
 ;
 ;========================== FILE TO GLOBAL ===========================
FTG(PATH,FILE,ROOT,INC) ;
 Q $$FTG^VFDXTRU2
 ;
 ;========================== GLOBAL TO FILE ===========================
GTF(PATH,FILE,ROOT,INC) ;
 Q $$GTF^VFDXTRU2
 ;
 ;====================== DISPLAY LIST OF ROUTINES =====================
ROUDSP(VFDR,TITLE) ;
 S VFDR=$G(VFDR),TITLE=$G(TITLE) D ROUDSP^VFDXTRU1
 Q
 ;
 ;==================  ASK TO VIEW SELECTED ROUTINES  ==================
ROUSEE(VFDR,TITLE) ;
 S VFDR=$G(VFDR),TITLE=$G(TITLE) D ROUSEE^VFDXTRU1
 Q
 ;
 ;==================== COMMON SCREEN WRITE UTILITY ====================
WR(X,CJ,SLF,ELF,LINE) ;
 D WR^VFDXTRU1
 Q
 ;================  RETURN THE CONTENTS OF ^%ZOSF NODE  ===============
ZOSF(NODE) ;
 Q $$ZOSF^VFDXTRU1
 ;
 ;===================== CALCULATE SIZE OF ROUTINE =====================
SIZE(X,VFDS) ; calculate size of routine
 ; either X or VFDS is required.
 ; X = routine name
 ; VFDS() contains routine at @vfds@(n) or @vfds@(n,0) where
 ;  @vfds@(n) is the nth line of the routine for n=1,2,3,4,...
 ; If $G(VFDS)'=""&($O(@VFDS@(0))=1) then ignore X even if passed
 ; Else get the routine X
 ; Return execute_size^comment_size^total_size^#_lines
 N I,L,Y,Z,VRTN,ZTOT
 S X=$G(X),VFDS=$G(VFDS) I VFDS'="" M VRTN=@VFDS
 I $O(VRTN(0))'=1,X'="" D  I '$D(VRTN) Q ""
 .K VRTN S Z=$$ZOSF^VFDVZOSF("LOAD",,X,,"VRTN")
 .I Z'=1 K VRTN
 .Q
 I '$D(VRTN(1)) Q ""
 F I=1:1:4 S ZTOT(I)=0
 F I=1:1 Q:'$D(VRTN(I))  D
 .S L=$G(VRTN(I)) S:L="" L=$G(VRTN(I,0)) Q:L=""
 .S Y=$L(L)+2,ZTOT(1)=ZTOT(1)+Y,ZTOT(4)=ZTOT(4)+1
 .I L?1" ;".E,$E(L,3)'=";" S ZTOT(3)=ZTOT(3)+Y
 .E  I L?1" ."." "1";".E S ZTOT(3)=ZTOT(3)+Y
 .E  S ZTOT(2)=ZTOT(2)+Y
 .Q
 Q ZTOT(1)_U_ZTOT(2)_U_ZTOT(3)_U_ZTOT(4)
 ;
 ;
 ;===================== GET LIST OF HFS FILES =====================
HFLIST(VFRET,PATH,VFLIST) ;
 ;  VFRET  - req - $name of array in which to return files found
 ;  PATH   - req - folder or directory where hfs files reside
 ; .VFLIST - opt - list of HFS files to get.  VFLIST(name)="" where
 ;           name is any acceptable value for list^%zish
 ;           name=full_hfs_filename
 ;           name=<char(s))_"*" eg. vflist("C*")
 ;           if vflist=filename, then look for that filename only
 ;              in this case, vfret need not be passed in
 ;           default to vflist("*") if no value passed in
 N X,Y,I
 S X=$$LIST^VFDXTRU2(PATH,.VFLIST,VFRET)
 Q X
