AFJXMBOX ;FO-OAKLAND/GMB-SEARCH for PREVIOUSLY COMPLETED NETWORK HEALTH EX's ;03/17/2003  07:46
 ;;5.1;Network Health Exchange;**2,11,34**;Jan 23, 1996
 ; Totally rewritten 3/2003.  (Previously FJ/CWS.)
 ; Called from ^AFJXWCP1 & ^AFJXWCPM
ENTER ;
 N AXNHEDUZ,AXABORT
 S AXABORT=0
 S AXNHEDUZ=$$FIND1^DIC(200,"","X","NETWORK,HEALTH EXCHANGE","B")
 F  D  Q:AXABORT
 . N DIR,X,Y,AXLIST,AXCNT
 . W @IOF
 . S DIR(0)="SO^Y:Your Own;A:All"
 . S DIR("A")="Select the requests to list"
 . S DIR("B")="Your Own"
 . D ^DIR I $D(DIRUT) S AXABORT=1 Q
 . D LIST^AFJXMABX(AXNHEDUZ,Y,.AXLIST,.AXCNT) Q:'AXCNT
 . D CHOOSE(.AXLIST,AXCNT)
 Q
CHOOSE(AXLIST,AXCNT) ;
 N DIR,X,Y,AXWHICH
 W !
 S DIR(0)="LCO^1:"_AXCNT
 S DIR("A")="Select the reports you'd like to print"
 D ^DIR Q:$D(DIRUT)
 S AXWHICH=Y
 N AXSAVE,I,ZTSK
 W !
 F I="AXLIST(","AXWHICH" S AXSAVE(I)=""
 D EN^XUTMDEVQ("PRINT^AFJXMBOX","AFJX Print Completed NHE Results",.AXSAVE,,1)
 Q:'$D(ZTSK)
 W !,"Print queued.  Task number: ",ZTSK
 D WAIT^XMXUTIL
 Q
PRINT ;
 N AXI,AXRANGE,AXJ,AXMZ,AXPAGE,AXABORT
 S AXABORT=0
 F AXI=1:1:$L(AXWHICH,",")-1 D  Q:AXABORT
 . S AXRANGE=$P(AXWHICH,",",AXI)
 . F AXJ=$P(AXRANGE,"-",1):1:$S(AXRANGE["-":$P(AXRANGE,"-",2),1:AXRANGE) D  Q:AXABORT
 . . D REPORT(AXLIST(AXJ),.AXPAGE,.AXABORT)
 Q
REPORT(AXMZ,AXPAGE,AXABORT) ;
 N AXI,AXTXT
 S (AXI,AXPAGE)=0,AXI=3
 D PHDR^AFJXMABX(AXMZ,.AXPAGE)
 F  S AXI=$O(^XMB(3.9,AXMZ,2,AXI)) Q:'AXI  S AXTXT=$G(^(AXI,0)) D  Q:AXABORT
 . I $Y+3+($E(IOST,1,2)="C-")>IOSL D  Q:AXABORT
 . . I $E(IOST,1,2)="C-" W ! D PAGE^XMXUTIL(.AXABORT) Q:AXABORT
 . . D PHDR^AFJXMABX(AXMZ,.AXPAGE)
 . W !,AXTXT
 I 'AXABORT,$E(IOST,1,2)="C-" D PAGE^XMXUTIL(.AXABORT)
 Q
