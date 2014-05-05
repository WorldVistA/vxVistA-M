WVBRNED1 ;HCIOFO/FT,JR IHS/ANMC/MWR - BROWSE TX NEEDS PAST DUE; ;2/28/00  15:12
 ;;1.0;WOMEN'S HEALTH;**10**;Sep 30, 1998
 ;;* MICHAEL REMILLARD, DDS * ALASKA NATIVE MEDICAL CENTER *
 ;;  DISPLAY CODE FOR BROWSING TX NEEDS.  CALLED BY WVBRNED.
 ;
DISPLAY ;EP
 ;---> WVCONF=DISPLAY "CONFIDENTIAL PT INFO" BANNER.
 ;---> WVTITLE=TITLE AT TOP OF DISPLAY HEADER.
 ;---> WVSUBH=CODE TO EXECUTE FOR SUBHEADER (COLUMN TITLES).
 ;---> WVCODE=CODE TO EXECUTE AS 3RD PIECE OF DIR(0) (AFTER DIR READ).
 ;---> WVCRT=1 IF OUTPUT IS TO SCREEN (ALLOWS SELECTIONS TO EDIT).
 ;---> WVTAB=6 IF OUTPUT IS TO SCREEN, =3 IF OUTPUT IS TO PRINTER.
 ;---> WVPRMT(1,Q)=PROMPTS FOR DIR.
 ;
 U IO
 S WVCONF=1
 S WVTITLE1=$S(WVB=1:"BY NEED DATE",WVB=2:"ALPHABETICALLY",WVB=3:"BY PRIMARY CARE PROVIDER",1:"?")
 S WVTITLE="*  PATIENTS LISTED "_WVTITLE1_"  *"
 D CENTERT^WVUTL5(.WVTITLE)
 S WVSUBH="SUBHEAD^WVBRNED1"
 S WVCODE="D EDIT^WVBRNED1 N N D SORT^WVBRNED,COPYGBL^WVBRNED"
 N N S (WVPOP,N,Z)=0
 D TOPHEAD^WVUTL7
 ;---> *SET WVFAC FOR NOW; MAKE WVFAC SELECTABLE IN FUTURE VERSIONS.
 S WVFAC=DUZ(2)
 S WVTAB=$S(WVCRT:6,1:3)
 ;
NOMATCH ;EP
 ;---> QUIT IF NO RECORDS MATCH.
 I '$D(^TMP("WV",$J,1)) D  Q
 .D HEADER5^WVUTL7
 .K WVPRMT,WVPRMT1,WVPRMTQ,DIR
 .W !!?5,"No records match the selected criteria.",!
 .I WVCRT&('$D(IO("S"))) D DIRZ^WVUTL3 W @IOF
 .D ^%ZISC S WVPOP=1
 ;
DISPLAY1 ;EP
 ;---> IF A PROCEDURE IS EDITED ON THE LAST PAGE, GOTO HERE
 ;---> FROM LINELABEL "END" BELOW.
 N M,Y
 D HEADER5^WVUTL7
 F  S N=$O(^TMP("WV",$J,2,N)) Q:'N!(WVPOP)  D
 .I $Y+6>IOSL D:WVCRT DIRZ^WVUTL3 Q:WVPOP  D
 ..S WVPAGE=WVPAGE+1
 ..D HEADER5^WVUTL7
 .S Y=^TMP("WV",$J,2,N),M=N
 .;---> DON'T WRITE BROWSE SELECTION#'S IF IO IS NOT A CRT (BRCRT).
 .W !! W:WVCRT $J(N,3),")"                  ;BROWSE SELECTION#
 .W ?WVTAB-2,$P(Y,U)                        ;SSN#
 .W ?WVTAB+10,$E($P(Y,U,2),1,16)," "        ;NAME
 .W $$REPEAT^XLFSTR(".",16-$L($P(Y,U,2)))   ;CONNECTING DOTS
 .W:'WVCRT "..."                            ;ADD DOTS IF NOT A CRT
 .W ?35,$E($P($P(Y,U,3),","),1,9)           ;CASE MANAGER
 .W ?46,$P(Y,U,4),!                         ;CERVICAL TX NEED&DATE
 .;I WVB=3
 .W ?5,"Pr. Provider => ",$E($$PROVI^WVUTL1A($P(Y,U,6)),1,25) ;PRIMARY CARE PROVIDER
 .W ?46,$P(Y,U,5)                           ;BREAST TX NEED&DATE
 .N WVDFN
 .S WVDFN=$P(Y,U,6)
 .W !?5,"Age: "_$$AGE^WVUTL9(WVDFN)_" / Veteran: "_$$VET^WVUTL1A(WVDFN)_" / Eligibility: "_$P($$ELIG^WVUTL9(WVDFN),U,2)
 ;
 D:'N
 .N WVTITLE S WVTITLE="-----  End of Report  -----"
 .D CENTERT^WVUTL5(.WVTITLE) W !!,WVTITLE
 I $D(^TMP("WV",$J,1)) I WVCRT&('$D(IO("S"))) D:'WVPOP DIRZ^WVUTL3 W @IOF
 ;
END ;EP
 D ^%ZISC
 Q
 ;
SUBHEAD ;EP
 ;---> SUB HEADER FOR PATIENT BROWSE OUTPUT.
 W !?WVTAB,$$PNLB^WVUTL5(),?WVTAB+10,"PATIENT",?35,"CASE MGR"
 W ?46,"TREATMENT NEED DUE BY DATE",!
 W $$REPEAT^XLFSTR("-",80)
 Q
 ;
EDIT ;EP
 ;---> FROM BROWSE, WVPOP IN TO EDIT AN INDIVIDUAL PATIENT.
 N WVPRMT,WVPRMT1,WVPRMT2,WVPRMTQ
 D SETVARS^WVUTL5
 S X=+X,WVDFN=$P(^TMP("WV",$J,2,X),U,6)
 S WVN=X N X
 D SCREEN^WVPATE(WVDFN)
 ;---> BACK UP 5 RECORDS AFTER EDIT.
 S N=$S(WVN<6:1,1:WVN-5) K WVN
 Q
