VFDXTDD1 ;DSS/CFS - MODIFICATIONS REPORT/DD COMPARISON ; 12/28/2012 10:00
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 Q
 ;
EN ;
 ; This routine checks for fields and namespaces in the Data Dictionary
 ; (^DD) based on user input from below prompts:
 ;
 ; Prompt user for specific field number or a range of field numbers.
 ;    ex.  .02 for a specific field number (^DD(.02)).
 ;
 ;    ex.  21600-21700 for a range of field numbers (^DD(21600:21700))
 ;
 ;
 ; Prompt user for a Vista Namespace in the ^DD:
 ;    ex.  VFD or PSC
 ;
 N VFDFLD,VFDNS
 I '$D(DTIME) S DTIME=600
 S U="^"
 K ^TMP("VFDXTRA",$J)
 S VFDNS=""
 D FLDPROMP I $D(DTOUT)!$D(DUOUT) Q
 D NSPROMP I $D(DTOUT)!$D(DUOUT) Q
 I VFDFLD="",VFDNS="" W !!,"Must have either a Field Number/Range or a Namespace" Q
 S Y=""
 S VFDEVICE=$$DEVPROMP Q:VFDEVICE<0
 D DDCHK(VFDFLD,VFDNS)
 W #
 W !,"^DD FILE NUMBER",?20,"^DD FIELD NUMBER",?40,"VISTA NAMESPACE",?60,"NEW CROSS REFERENCE"
 D VFDWRITE
 ;I '$D(^TMP("VFDXTRA",$J)) W !,?22,"NO DATA FOUND FOR SELECTED CRITERIA"
 K ^TMP("VFDXTRA",$J)
 Q
 ;-------------- ASK USER FOR A FIELD NUMBER OR RANGE OF FIELD NUMBERS -----------------
FLDPROMP ;
 N DIR,VFDCOMP1,VFDCOMP2,VFDQUIT,VFDY
 S VFDQUIT=0
 S DIR(0)="FO^.000000001:999999999",DIR("A")="Enter a Field Number or a valid range of Field Numbers"
 S DIR("?",1)="Enter the Field Number you wish to review from the ^DD File"
 S DIR("?")="or enter '??' for more help or enter '^' to exit."
 S DIR("??")="^D HELP^VFDXTDD1"
 D ^DIR
 I $D(DTOUT)!($D(DUOUT)) Q
 S (VFDY,VFDFLD)=Y
 I '$F(VFDY,"-") D  G:VFDQUIT FLDPROMP
 .I VFDY'?1".".9N,(VFDY'?1.9N1"."1.9N),(VFDY'?1.9N),(VFDY'="") D
 ..W !!,?2,"Invalid selection. Enter selection or '??' for more help or '^' to exit.",!
 ..S VFDQUIT=1
 I $F(VFDY,"-") D  G:VFDQUIT FLDPROMP
 .I VFDY'?1".".9N1"-"1.9N,(VFDY'?1".".9N1"-"1".".9N),(VFDY'?1.9N1"."1.9N1"-"1.9N1"."1.9N),(VFDY'?1.9N1"-"1.9N),(VFDY'?1.9N1"-"1.9N1"."1.9N),(VFDY'="") D
 ..W !!,?2,"Invalid selection. Enter selection or '??' for more help or '^' to exit.",!
 ..S VFDQUIT=1
 .Q:VFDQUIT
 .S VFDCOMP1=$P(VFDY,"-"),VFDCOMP2=$P(VFDY,"-",2)
 .I VFDCOMP2']VFDCOMP1 W !!,?2,"Invalid range. "_VFDCOMP2_" cannot be larger than "_VFDCOMP1,! S VFDQUIT=1 Q
 Q
 ;-------------- ASK USER FOR A VALID VISTA PACKAGE NAMESPACE -----------------
NSPROMP ;
 N DIR,VFDQUIT,VFDY
 S VFDQUIT=0
 W !
 S DIR(0)="FO^",DIR("A")="Enter the Vista Namespace or '^' to exit"
 S DIR("?")="Enter the Vista Namespace you wish to review such as 'VFD'."
 D ^DIR
 I $D(DTOUT)!($D(DUOUT))!(Y="") Q
 S (VFDY,VFDNS)=Y
 I $L(VFDY)>6 W !!,?2,"Length cannot be greater than 6." G NSPROMP
 I VFDY'?1U1.5NU,(VFDY'="") D  G NSPROMP
 .W !!,?2,"Must begin with 1 Uppercase followed by 1 to 5 Numerics"
 .W !!,?2,"and/or 1 to 5 Uppercase."
 Q
 ;-------------- ASK USER FOR DEVICE -----------------
DEVPROMP() ;
 N %ZIS,DIR,IOP,POP,Y
 S DIR(0)="SO^0:80 Columns;1:132 Columns"
 S DIR("B")=0,DIR("A")="Select number of columns for display"
 D ^DIR
 S Y=$S($D(DUOUT):-1,$D(DTOUT):-1,Y="":-1,1:Y)
 I Y<1 Q Y
 S %ZIS="",IOP=";C-VT132;132;9999" D ^%ZIS I POP Q -1
 Q 1
 ;-------------- CHECK ^DD FOR USER SELECTED FIELDS AND NAMESPACES -----------------
DDCHK(VFDFLD,VFDNS) ;
 N VFDFOUND,VFDHIGH,VFDI,VFDLOW,VFDNSA,VFDPREFX,VFDX,VFDY
 S VFDFLD=$G(VFDFLD),VFDNS=$G(VFDNS)
 S (VFDLOW,VFDHIGH)=""
 I $F(VFDFLD,"-") S VFDLOW=$P(VFDFLD,"-"),VFDHIGH=$P(VFDFLD,"-",2)
 S VFDX=0 F  S VFDX=$O(^DD(VFDX)) Q:'VFDX  D
 .S VFDY=0 F  S VFDY=$O(^DD(VFDX,VFDY)) Q:'VFDY  D
 ..S VFDFOUND=0
 ..I $F(VFDFLD,"-") D  Q
 ...I VFDY'<VFDLOW,VFDY'>VFDHIGH D
 ....D IXCHK(VFDX,VFDY,VFDFLD,VFDNS,.VFDPREFX,.VFDFOUND)
 ....I VFDFOUND Q
 ....D VFDSET(VFDX,VFDY,"","")  ;Falls within the valid range.
 ..I '$F(VFDFLD,"-"),VFDFLD'="" D  Q
 ...I VFDFLD=VFDY D
 ....D IXCHK(VFDX,VFDY,VFDFLD,VFDNS,.VFDPREFX,.VFDFOUND)
 ....I VFDFOUND Q
 ....D VFDSET(VFDX,VFDY,"","")
 ..I VFDFLD="" D IXCHK(VFDX,VFDY,VFDFLD,VFDNS,.VFDPREFX,.VFDFOUND) Q
 ..I VFDFLD'="" D VFDSET(VFDX,VFDY,"","") Q
 .I VFDNS'="" D NEWIX(VFDX,VFDNS)
 Q
DDCHK1(VFDFLD,VFDNS) ;
 ;S VFDNA=$NA(^DD)
 ;N UP,VFDFLD,ROOT S UP=$G(@VFDNA@(X,0,"UP")),UP=$S(UP="":X,1:UP)
 ;S X=0 F  S X=$O(@VFDNA@(X)) Q:'X  D
 ;.S VFDFLD=0 F  S VFDFLD=$O(@VFDNA@(X,VFDFLD)) Q:'VFDFLD  D
 ;..I VFDFLD<21600!(VFDFLD>21700) Q
 ;..D IXCHK(X,VFDFLD) S ROOT=$NA(@VFDNA@(X,VFDFLD))
 ;..N ATR S ATR=$P(@ROOT@(0),U,2)
 ;..D SET(.DD,UP_U_X_U_VFDFLD_U_ATR)
 ;.D NEWIX(X,VFDNS,.VFDPREFX)
 ;I $D(DD(2)) W !!?3,"Data Dictionary Modifications" D
 ;.D RPT^VFDXPD0(.DD)
 Q
 ;-------------- CHECK OLD CROSS REFERENCE NAMESPACES -----------------
IXCHK(VFDDFILE,VFDDFLD,VFDFLD,VFDNS,VFDPREFX,VFDFOUND) ; Look for valid namespaces.
 N VFDFLAG,VFDNSA,VFDX,VFDY
 S VFDPREFX=$G(VFDPREFX)
 I VFDNS="VFD" S VFDNSA="AVFD"
 Q:'$D(^DD(VFDDFILE,VFDDFLD,1))  ;Looking for valid Vista namespaces.
 S VFDX=0 F  S VFDX=$O(^DD(VFDDFILE,VFDDFLD,1,VFDX)) Q:'VFDX  D
 .S VFDPREFX=$P(^DD(VFDDFILE,VFDDFLD,1,VFDX,0),U,2)
 .I VFDFLD=""!(VFDFLD'=""&VFDNS'="") D
 ..I $E(VFDPREFX,1,$L(VFDPREFX))=VFDNS!(VFDPREFX=$G(VFDNSA)) D
 ...S VFDFOUND=1
 ...D VFDSET(VFDDFILE,VFDDFLD,VFDPREFX,"")
 ..Q
 Q
 ;-------------- CHECK NEW CROSS REFERENCE NAMESPACES -----------------
NEWIX(VFDDFILE,VFDNS) ; Check for new style x-ref
 N VFDX,VFDPREFX,VFDNSA
 I VFDNS="VFD" S VFDNSA="AVFD"
 S VFDPREFX="" F  S VFDPREFX=$O(^DD("IX","BB",VFDDFILE,VFDPREFX)) Q:VFDPREFX=""  D
 .I VFDNS'="",$E(VFDPREFX,1,$L(VFDPREFX))=VFDNS!(VFDPREFX=$G(VFDNSA)) D VFDSET(VFDDFILE,"",VFDPREFX,"Found in ^DD(""IX"")")
 Q
 ;-------------- SET ^TMP GLOBAL FOR FOUND FIELDS AND NAMESPACES IN ^DD FILE -----------------
VFDSET(VFDDFILE,VFDDFLD,VFDPREFX,VFDNEWIX) ;Set up temp global
 N VFDX
 S VFDDFILE=$G(VFDDFILE),VFDDFLD=$G(VFDDFLD),VFDNS=$G(VFDNS),VFDNEWIX=$G(VFDNEWIX)
 S VFDX=+$O(^TMP("VFDXTRA",$J,""),-1)+1
 S ^TMP("VFDXTRA",$J,VFDX)=VFDDFILE_"|"_VFDDFLD_"|"_VFDPREFX_"|"_VFDNEWIX
 ;W !,VFDDFILE,?20,VFDDFLD,?40,VFDPREFX,?60,VFDNEWIX
 Q
 ;-------------- WRITE ^DD INFO -----------------
VFDWRITE ;
 N DIR,VFDCNT,VFDATA,VFDX,VFDQUIT
 I '$D(^TMP("VFDXTRA",$J)) W !,?22,"NO DATA FOUND FOR SELECTED CRITERIA" Q
 S (VFDCNT,VFDQUIT)=0
 S VFDX="" F  S VFDX=$O(^TMP("VFDXTRA",$J,VFDX)) Q:VFDX=""!(VFDQUIT)  D
 .S VFDATA=^TMP("VFDXTRA",$J,VFDX)
 .W !,$P(VFDATA,"|"),?20,$P(VFDATA,"|",2),?40,$P(VFDATA,"|",3),?60,$P(VFDATA,"|",4)
 .S VFDCNT=VFDCNT+1
 .I VFDCNT=20 D
 ..W !
 ..S DIR(0)="E"
 ..D ^DIR
 ..S VFDCNT=0
 ..I $D(DTOUT)!($D(DUOUT)) S VFDQUIT=1 Q
 ..W #
 Q
 ;
SET(RTN,STR) ;
 ;N X S X=+$O(RTN(""),-1)+1,RTN(X)=STR
 Q
 ;-------------- HELP TEXT MESSAGE -----------------
HELP ;
 W #
 W !,?2,"Enter a Field Number or a valid range of Field Numbers you wish to review"
 W !,?2,"from the Data Dictionary File (^DD(FileNumber,FieldNumber))."
 W !
 W !,?2,"You can enter a whole number up to 9 characters"
 W !,?2,"or a decimal point followed by a whole number up to 9 characters (fraction)"
 W !,?2,"or a whole number up to 9 characters followed by a decimal point"
 W !,?2,"followed by a whole number up to 9 characters (mixed fraction)."
 W !,?2,"For a range of numbers, enter the selected number followed by 1 dash"
 W !,?2,"followed by selected number. The range must be from lowest to highest."
 W !
 W !,?2,"For example:"
 W !
 W !,?4,"1. A whole number up to 9 characters: 100000000"
 W !,?4,"2. A decimal point followed by a whole number up to 9 characters"
 W !,?4,"   (fraction): .000000001"
 W !,?4,"3. A whole number up to 9 characters followed by a decimal point"
 W !,?4,"   followed by a whole number up to 9 characters"
 W !,?4,"   (mixed fraction): 100000000.000000001"
 W !,?4,"4. Range of numbers - Enter the selected number followed by 1 dash"
 W !,?4,"   followed by selected number from lowest to highest: .000000001-100000000"
 W !,?4,"   NOTE: You can only have 1 set for a range of numbers. For example,"
 W !,?4,"   you cannot have .001-100-200."
 Q
