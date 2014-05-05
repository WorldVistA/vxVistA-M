RMPRPIYR ;HINCIO/ODJ - PIP EDIT - PROMPTS ;3/8/01
 ;;3.0;PROSTHETICS;**61**;Feb 09, 1996
 Q
 ; The following subroutines are for selecting HCPCS
 ; and Inventory Item
 ;
 ;***** OK - Prompt for an OK
OK(RMPRYN,RMPREXC) ;
 N DIR,X,Y,DA,DUOUT,DTOUT,DIROUT,DIRUT
 S RMPREXC=""
 S RMPRYN="N"
 S DIR("A")="         ...OK"
 S DIR("B")="Yes"
 S DIR(0)="Y"
 D ^DIR
 I $D(DTOUT) S RMPREXC="T" G OKX
 I $D(DIROUT) S RMPREXC="P" G OKX
 I X=""!(X["^") S RMPREXC="^" G OKX
 S RMPRYN="N" S:Y RMPRYN="Y"
OKX Q
 ;
 ;***** PVEN - Prompt for current Stock Record
PVEN(RMPRSTN,RMPRLCN,RMPRHCPC,RMPRITM,RMPR6,RMPR7,RMPREXC) ;
 N RMPRERR,DIR,X,Y,DUOUT,DTOUT,DIROUT,DA,DIRUT,RMPRA,RMPRGBLR
 N RMPRMAX,RMPRLIN,RMPRGBL,RMPR7I,RMPRS
 S RMPRERR=0
 S RMPREXC=""
 S RMPRMAX=15
 S RMPRLIN=0
 K RMPR7,RMPR6
 S RMPRLCN=$G(RMPRLCN)
 ;
 ; See if just 1 record - no need to list if there is
 S RMPRGBLR="^RMPR(661.7,""XSHIDS"","_RMPRSTN_","""_RMPRHCPC_""","""_RMPRITM_""")"
 S RMPRGBL=$Q(@RMPRGBLR)
 I $$PVENE() G PVENX
 S RMPR7("IEN")=$QS(RMPRGBL,8)
 S RMPRGBL=$Q(@RMPRGBL)
 I $$PVENE() G PVENG
 ;
 ; Selection list of current stock records
 S RMPRGBL=RMPRGBLR
PVENL1 S RMPRGBL=$Q(@RMPRGBL)
 I $$PVENE G:'RMPRLIN PVENX G PVENP
 K RMPR7,RMPR7I
 S RMPR7("IEN")=$QS(RMPRGBL,8)
 S RMPRERR=$$GET^RMPRPIX7(.RMPR7)
 S RMPRERR=$$ETOI^RMPRPIX7(.RMPR7,.RMPR7I)
 I RMPRLCN'="",RMPRLCN'=RMPR7I("LOCATION") G PVENL1
 I RMPRLIN,'(RMPRLIN#RMPRMAX) D  G PVENP
 . S DIR("A",1)="Press <RETURN> to see more, '^' to exit this list, or"
 . Q
PVENL2 S RMPRLIN=RMPRLIN+1
 I RMPRLIN=1 D PVENH
 S RMPRS=$P(RMPR7I("DATE&TIME"),".",1)
 W !,$J(RMPRLIN,2)," ",$E(RMPRS,4,5)_"/"_$E(RMPRS,6,7)_"/"_$E(RMPRS,2,3)
 W ?11,$J(RMPR7("QUANTITY"),5,0)
 I +RMPR7("QUANTITY") D
 . W ?18,$J(RMPR7("VALUE")/RMPR7("QUANTITY"),8,2)
 . Q
 W ?26,$J(RMPR7("VALUE"),10,2)
 S RMPR6("DATE&TIME")=RMPR7I("DATE&TIME")
 S RMPR6("HCPCS")=RMPRHCPC
 S RMPRERR=$$GET^RMPRPIX6(.RMPR6)
 W ?38,$E(RMPR6("VENDOR"),1,30)
 W ?69,$E(RMPR7("LOCATION"),1,10)
 S RMPRA(RMPRLIN)=RMPR7("IEN")
 K RMPR7,RMPR7I,RMPR6
 G PVENL1
 ;
 ; Prompt for selection
PVENP S DIR(0)="FAO"
 S DIR("A")="Choose 1 - "_RMPRLIN_" : "
 D ^DIR
 I $D(DTOUT) S RMPREXC="T" G PVENX
 I $D(DIROUT) S RMPREXC="P" G PVENX
 I X="",$D(DIR("A",1)) K DIR("A",1) D PVENH G PVENL2
 I X="" S RMPREXC="^" G PVENX
 I X["^"!($D(DUOUT)) S RMPREXC="^" G PVENX
 I '$D(RMPRA(X)) D  G PVENP
 . W !,"Please select a current stock record"
 . W !,"by entering a line number in range 1 - "
 . W RMPRLIN
 . Q
 S RMPR7("IEN")=RMPRA(X)
PVENG S RMPRERR=$$GET^RMPRPIX7(.RMPR7)
 K RMPR7I
 S RMPRERR=$$ETOI^RMPRPIX7(.RMPR7,.RMPR7I)
 S RMPRLCN=RMPR7I("LOCATION")
 S RMPR6("DATE&TIME")=RMPR7I("DATE&TIME")
 S RMPR6("HCPCS")=RMPRHCPC
 S RMPRERR=$$GET^RMPRPIX6(.RMPR6)
PVENX Q
PVENE() ;
 Q:$QS(RMPRGBL,1)'=661.7 1
 Q:$QS(RMPRGBL,2)'="XSHIDS" 1
 Q:$QS(RMPRGBL,3)'=RMPRSTN 1
 Q:$QS(RMPRGBL,4)'=RMPRHCPC 1
 Q:$QS(RMPRGBL,5)'=RMPRITM 1
 Q 0
PVENH W !
 W !,"Select a current stock record...",!
 W ?3,"Date",?13,"Qty",?18,"Unit Cost",?31,"Value",?38,"Vendor"
 I RMPRLCN="" W ?69,"Location"
 Q
