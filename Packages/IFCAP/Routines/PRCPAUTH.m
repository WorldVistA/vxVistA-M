PRCPAUTH ;WISC/AKS,DWA-receive purchase order (list manager)            ;6/8/96  13:09
 ;;5.1;IFCAP;**22**;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
INV D ^PRCPUSEL Q:'$G(PRCP("I"))
 I "PW"'[PRCP("DPTYPE") W !,"YOU MUST BE A WAREHOUSE OR PRIMARY INVENTORY POINT TO RECEIVE THIS PO IN THE INVENTORY." S NOINV=1 Q
 I $$CHECK^PRCPCUT1(PRCP("I")) W !,"CAN NOT RECEIVE THIS PO IN THE INVENTORY" S NOINV=1 Q
 N %,PRCPFCOS,PRCPINPT,PRCPORDN,PRCPORDR,PRCPPARD,PRCPPART,PRCPTYPE,PRCPVEND,PRCPVENN,X,Y
 S PRCPINPT=PRCP("I"),PRCPTYPE=PRCP("DPTYPE")
 I '$D(^PRC(442,"G",PRCPINPT,PRCHPO)) S NOINV=1 Q
 S PRCPVEND=+$G(^PRC(442,PRCHPO,1)),PRCPVENN=$P($G(^PRC(440,PRCPVEND,0)),"^")
 I PRCPVEND="" W !,"ERROR - INVALID OR MISSING VENDOR ON THIS PURCHASE ORDER !",!,"CAN NOT RECEIVE THIS PO IN THE INVENTORY" S NOINV=1 Q
 S PRCPORDR=PRCHPO
REBUILD ;  called here to rebuild array
 K PRCPDATA
 S (PRCPFLAG,PRCHFLAG,PRCPFCOS)=0
 N RCHK,%,AVGCOST,CONV,INVDATA,ITEMDA,LINE,LINEDA,PODATA,POQTY,POUI,QTYRECVE,TOTCOST,TRANDA,TRANDATA,TRUI,UNITCOST,X
 S TRANDA=+$P(^PRC(442,PRCPORDR,0),"^",12)
 S LINE=0
 S LINEDA=0 F  S LINEDA=$O(^PRC(442,PRCPORDR,2,LINEDA)) Q:'LINEDA  S PODATA=$G(^(LINEDA,0)) I PODATA'="" D
 .   S ITEMDA=+$P(PODATA,"^",5) I 'ITEMDA,$P(PODATA,"^",13)'="" S ITEMDA=+$O(^PRC(441,"BB",$P(PODATA,"^",13),0))
 .   S (PRCPFLAG,RCHK)=""
 .   ;
 .   ;  get outstanding transaction data
 .   I 'TRANDA S TRANDA=+$P(PODATA,"^",10)
 .   Q:'$D(^PRCP(445,PRCPINPT,1,ITEMDA,0))
 .   I +$P($G(^PRC(442,PRCPORDR,2,LINEDA,2)),U,8)<$P($G(^PRC(442,PRCPORDR,2,LINEDA,0)),U,2) S RCHK=1
 .   S TRANDATA=$G(^PRCP(445,PRCPINPT,1,ITEMDA,7,TRANDA,0)),TRUI=$$UNITVAL^PRCPUX1($P(TRANDATA,"^",4),$P(TRANDATA,"^",3),"/"),CONV=$P(TRANDATA,"^",5)
 .   ;  if there is not a due-in established, look up conversion factor
 .   ;  from procurement source multiple
 .   I 'CONV S CONV=$P($$GETVEN^PRCPUVEN(PRCPINPT,ITEMDA,+$P($G(^PRC(442,PRCPORDR,1)),"^")_";PRC(440,",0),"^",4)
 .   I 'CONV S CONV="?"
 .   ;
 .   I $P($G(^PRCS(410,TRANDA,0)),"^",6)'=PRCPINPT S PRCPFLAG=1 W !,"ERROR: INVENTORY POINT NOT TIED TO 2237 "_$P($G(^PRCS(410,TRANDA,0)),"^")
 .   I TRANDATA="",RCHK=1 S PRCPFLAG=1 W !,"ERROR: 2237 "_$P($G(^PRCS(410,TRANDA,0)),"^")_" NOT ESTABLISHED AS A DUE-IN"
 .   S POUI=$$UNITVAL^PRCPUX1($P(PODATA,"^",12),$P(PODATA,"^",3),"/")
 .   I TRANDATA'="",POUI'=TRUI S PRCPFLAG=1 W !,"ERROR: PO U/I "_POUI_" DOES NOT EQUAL DUE-IN U/R "_TRUI
 .   I $D(^PRCP(445,PRCPINPT,1,ITEMDA,0)),'CONV S PRCPFLAG=1 W !,"ERROR: NO CONVERSION FACTOR.  EDIT THE DUE-IN OR VENDOR TO SET THE CF"
 .   I PRCPFLAG W !,"FIX ERRORS BEFORE RECEIVING" S PRCHFLAG=1 K PRCPFLAG
 QUIT
