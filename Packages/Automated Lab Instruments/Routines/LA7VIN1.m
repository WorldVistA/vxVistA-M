LA7VIN1 ;DALOI/JMC - Process Incoming UI Msgs, continued ;12/07/11  12:54
 ;;5.2;AUTOMATED LAB INSTRUMENTS;**46,64,74**;Sep 27, 1994;Build 2
 ;
 ; This routine is a continuation of LA7VIN and is only called from there.
 ; It is called with each message found in the incoming queue.
 Q
 ;
NXTMSG ;
 N FDA,LA7ABORT,LA7AERR,LA7CNT,LA7END,LA7ERR
 N LA7INDX,LA7KILAH,LA7QUIT,LA7SEG,LA7STYP
 ;DSS/JDB - BEGIN MOD (2009/12/15) 
 ; Expects LA76248
 N DIERR,VFDLAAP,VFDLAAPTXT,VFDLAMICRO,VFDLAMITXT,VFDLAMRN,VFDLAUID,VFDMSG,VFDX,VFDX1  ;DSS/RAF 2/11/2014
 S VFDLAUID=$$VFDUID(LA76249)  ;DSS/RAF 2/11/2014
 S VFDLAMRN=$$VFDMRN(LA76249)  ;DSS/RAF 4/17/2014
 I $$VFDGET1^VFDLRPC1(62.49,LA76249,108)="ORU" D  ;DSS/RAF 9/13/2014 added check to stop processing of MSA messages
 . I $$VFDGET1^VFDLRPC1(62.49,LA76249,102)["POC" Q  ;DSS/RAF 10/24/2014 added for POC compatibility
 . I VFDLAMRN'=$P($$UIDLRDFN^VFDLRX($G(VFDLAUID)),U,3) D
 . . N LA7ERR,LA7MSATM
 . . S LA7ERR=48,LA7QUIT=2,LA7MSATM="DFN mismatch between OBR-2 and PID-2 or missing UID from OBR-2"
 . . ;DSS/RAF - 8/1/2014 replaced 102 with 48 and changed text variable to LA7MSATM
 . . D CREATE^LA7LOG(LA7ERR)
 I +$G(LA7QUIT)>0 Q  ;DSS/RAF 4/18/2014
 I $G(VFDLAUID)]"" D  ;DSS/RAF 2/11/2014
 . S VFDX=$$FIND1^DIC(68,"","MOX",$G(VFDLAUID),"C","","VFDMSG")  ;DSS/RAF 2/11/2014
 . S VFDX1=$$GET1^DIQ(68,VFDX_",",.02,"I","VFDMSG")  ;DSS/RAF 2/11/2014
 I $G(VFDX1)="MI" S VFDLAMICRO=1  ;DSS/RAF 2/11/2014
 I "SPCYEM"[$G(VFDX1) S VFDLAAP=1  ;DSS/RAF 3/17/2014
 S VFDLAMITXT=$$GET^XPAR("SYS","VFD LAB NON-DISCRETE MICRO ORU")  ;DSS/RAF 3/6/2014
 S VFDLAAPTXT=$$GET^XPAR("SYS","VFD LAB NON-DISCRETE AP ORU")  ;DSS/RAF 3/17/2014
 ; parameter above will allow existing "blob" text micro messages to process as expected  ;DSS/RAF 3/6/2014
 N RULE,SEGID
 N VFDACTION,VFDLALAH,VFDLAHL7SEGS,VFDLAISAPOBR
 N VFDLANTE4PID,VFDLAPROCESS,VFDQUIT
 ; Pre load HL7 message
 K VFDLAHL7SEGS
 D SEGS2TMP^VFDLA7R0(LA76249,.VFDLAHL7SEGS)
 S VFDLAHL7SEGS(0)=0
 S VFDLAPROCESS=0
 ; Get Message Type from MSH
 S SEGID=$O(VFDLAHL7SEGS(2,"MSH",0))
 S LA7MTYP=$$GETTMP^VFDLA7R0("MSH",SEGID,9,1)
 I LA7MTYP="ORU" D  ;
 . S VFDLAPROCESS=$$HLPROC^VFDLA7U3(LA76248,"LAB PROCESS ORU")
 ;
 I +$G(VFDLAMICRO)=1,+$G(VFDLAMITXT)=1 D RULS2TMP^VFDLA7U3(VFDLAPROCESS)  ;DSS/RAF 3/26/2014
 I +$G(VFDLAAP)=1,+$G(VFDLAAPTXT)=1 D RULS2TMP^VFDLA7U3(VFDLAPROCESS)  ;DSS/RAF 3/26/2014
 I $G(VFDX1)="CH" D RULS2TMP^VFDLA7U3(VFDLAPROCESS)  ;DSS/RAF 3/26/2014
 K ^TMP("VFDLA7IFQSTRI5-REPORT",$J)
 ;
 ;DSS/JDB - END MOD (2009/12/15) 
 ;
 S (LA7AERR,LA7ERR)=""
 S (LA7ABORT,LA7CNT,LA7END,LA7INDX,LA7QUIT,LA7SEQ)=0
 S DT=$$DT^XLFDT
 S LA7ID="UNKNOWN-I-"
 ;
 ; Message built but no text.
 I '$O(^LAHM(62.49,LA76249,150,0)) D  Q
 . S (LA7ABORT,LA7ERR)=6
 . D CREATE^LA7LOG(LA7ERR)
 . D SETID^LA7VHLU1(LA76249,LA7ID,"UNKNOWN",1)
 ;
 ; Process message segments
 ; Lab currently does not accept segments beginning with the letter "Z" which are reserved for locally-defined messages.
 ; "Z" segments will be ignored by this software.
 F  S LA7END=$$GETSEG^LA7VHLU2(LA76249,.LA7INDX,.LA7SEG) Q:LA7END!(LA7ABORT)  D
 . S LA7STYP=$E(LA7SEG(0),1,3) ; Segment type
 . ;DSS/JDB - BEGIN MOD (2010/01/15)
 . I LA7ABORT!LA7ERR Q
 . S VFDLAHL7SEGS(0)=$G(VFDLAHL7SEGS(0))+1
 . I LA7STYP="PID" D GET624^VFDLA7R6
 . I LA7STYP="ORC" D GET624^VFDLA7R6
 . S RULE="LAB "_LA7MTYP_" "_LA7STYP_" VALIDATE"
 . S VFDACTION=$$ACTION^VFDLA7U3(RULE)
 . S VFDQUIT=0
 . I VFDACTION'="" D  ;
 . . X VFDACTION
 . ;
 . I VFDQUIT!LA7ERR!LA7ABORT Q
 . S RULE="LAB "_LA7MTYP_" "_LA7STYP
 . S VFDACTION=$$ACTION^VFDLA7U3(RULE)
 . ; build NTEseg rule  (eg  NTEOBR)
 . I LA7STYP="NTE" D  ;
 . . N I,SEG
 . . ; find previous non-NTE segment 
 . . S SEG=""
 . . F I=VFDLAHL7SEGS(0)-1:-1:1 D  Q:SEG'=""  ;
 . . . S X=VFDLAHL7SEGS(1,I)
 . . . I X'="NTE" S SEG=X
 . . ;
 . . I SEG="" Q
 . . S RULE="LAB "_LA7MTYP_" NTE"_SEG
 . . S X=$$ACTION^VFDLA7U3(RULE)
 . . I X'="" S VFDACTION=X
 . ;
 . S VFDQUIT=0
 . I VFDACTION'="" D  Q  ;
 . . S X=LA7STYP_"BEG^VFDLA7R6"
 . . S VFDQUIT=0
 . . I $T(@X)'="" D @X
 . . I VFDQUIT S VFDQUIT=0 Q
 . . I LA7QUIT=2 Q
 . . I LA7ABORT!LA7ERR Q
 . . ; protect DO level
 . . D  ;
 . . . X VFDACTION
 . . ;
 . . S X=LA7STYP_"END^VFDLA7R6"
 . . I $T(@X)'="" D @X
 . ;
 . ;DSS/JDB - END MOD (2010/01/15)
 . I $E(LA7STYP,1)="Z" Q
 . ; Not a valid segment type
 . I LA7STYP'?2U1UN D  Q
 . . S LA7ERR=34
 . . D CREATE^LA7LOG(LA7ERR)
 . ; Segment encoded wrong - field separator does not match
 . I "MSH^FSH^BHS^"'[(LA7STYP_"^"),$E(LA7SEG(0),4)'=LA7FS D  Q
 . . S LA7ERR=35
 . . D CREATE^LA7LOG(LA7ERR)
 . I $T(@LA7STYP)="" Q  ; No processing logic for this segment type
 . D @LA7STYP
 ;
 ; Send HL7 Application Acknowledgment message for selected interfaces/message types
 I LA7MTYP="ORM",LA7INTYP=10 D SENDACK
 ;
 ;DSS/JDB - BEGIN MOD (2009/12/15)
 S VFDACTION=$$ACTION^VFDLA7U3("LAB "_LA7MTYP_" DONE")
 I VFDACTION'="" D  ;
 . X VFDACTION
 ;
 ;DSS/JDB - END MOD (2009/12/15)
 ; Set id if only MSH segment received.
 I LA7SEQ<5 D
 . D SETID^LA7VHLU1(LA76249,LA7ID,"UNKNOWN",1)
 ;
 ; Set status to purgeable if no errors.
 I $P($G(^LAHM(62.49,LA76249,0)),"^",3)'="E" D
 . S FDA(1,62.49,LA76249_",",2)="X"
 . D FILE^DIE("","FDA(1)","LA7ERR(1)")
 ;
 ; Store identifier's found in message.
 D UPID^LA7VHLU1(LA76249)
 ;
 ; Send new result alert for ORU messages if turned on.
 I $G(LA7MTYP)="ORU",$D(^LAHM(62.48,+$G(LA76248),20,"B",1)) D
 . N LA7MSG,LA7ROOT
 . S LA7ROOT="^TMP(""LA7-ORU"",$J)"
 . F  S LA7ROOT=$Q(@LA7ROOT) Q:LA7ROOT=""  Q:$QS(LA7ROOT,1)'="LA7-ORU"!($QS(LA7ROOT,2)'=$J)  D
 . . S LA7MSG(1)=$S($QS(LA7ROOT,5)="CH":"Chemistry/Hematology",$QS(LA7ROOT,5)="MI":"Microbiology",$QS(LA7ROOT,5)="SP":"Surgical Pathology",$QS(LA7ROOT,5)="CY":"Cytology",$QS(LA7ROOT,5)="EM":"Electron Microscopy",1:"")
 . . I LA7MSG(1)'="" S LA7MSG(1)=" "_LA7MSG(1)
 . . S LA7MSG="Lab Msg - New"_LA7MSG(1)_" results received for "_$P($G(^LAHM(62.48,$QS(LA7ROOT,3),0),"UNKNOWN"),"^")_"^"_$QS(LA7ROOT,5)
 . . D XQA^LA7UXQA(1,$QS(LA7ROOT,3),"","",LA7MSG,"",0)
 ;
 ; Send new order alert for ORM messages if turned on.
 I $G(LA7MTYP)="ORM",$D(^LAHM(62.48,+$G(LA76248),20,"B",3)) D
 . N LA7ROOT
 . S LA7ROOT="^TMP(""LA7-ORM"",$J)"
 . F  S LA7ROOT=$Q(@LA7ROOT) Q:LA7ROOT=""  Q:$QS(LA7ROOT,1)'="LA7-ORM"!($QS(LA7ROOT,2)'=$J)  D
 . . D XQA^LA7UXQA(3,$QS(LA7ROOT,3),"",$QS(LA7ROOT,4),"",$QS(LA7ROOT,5))
 ;
 ; Create performing lab comment for entries in LAH.
 ;I $D(^TMP("LA7-PL-NTE",$J)) D PL^LA7VIN1B
 ;
 ; Cleanup shipping config test info used to process orders
 I $G(LA7MTYP)="ORM" K ^TMP("LA7TC",$J)
 ;
 ; If amended results received then send bulletins
 I $D(^TMP("LA7 AMENDED RESULTS",$J)) D SENDARB^LA7VIN1A
 ;
 ; If cancelled orders received then send bulletins
 I $D(^TMP("LA7 ORDER STATUS",$J)) D SENDOSB^LA7VIN1B
 ;
 ; If units/normals changed then send bulletins
 I $D(^TMP("LA7 UNITS/NORMALS CHANGED",$J)) D SENDUNCB^LA7VIN1A
 ;
 ; If abnormal/critical results then send bulletins
 I $D(^TMP("LA7 ABNORMAL RESULTS",$J)) D SENDACB^LA7VIN1A
 ;DSS/RAF - BEGIN MOD - make call to autoaccept routine to file results
 ; this might be the place to call ONE^VFDLRAV(LA7LWL,LA7ISQN,1)  ;DSS/RAF 3/12/2014
 ; the 1 flag tells Frank not to kill off ^LAH(LA7LWL when done moving to file 63
 I $G(VFDLAMICRO)=1,+$G(VFDLAMITXT)=0 D  ;DSS/RAF - check added to stop ack errors
 . D ONE^VFDLRAV(+$G(LA7LWL),+$G(LA7ISQN),0)  ;DSS/RAF 3/14/2014
 I $G(VFDLAAP)=1,$G(VFDLAAPTXT)=0 D  ;DSS/RAF - autoaccept for AP discrete
 . ;third parameter in following call: 1=delete ^LAH when done 0=save ^LAH
 . D ONE^VFDLRAV(+$G(LA7LWL),+$G(LA7ISQN))  ;DSS/RAF 3/18/2014
 . ; add code here to handle LREND error message somehow.
 ;DSS/RAF - END MOD
 ;
 D KILLMSH
 ;
 Q
 ;
 ;
MSA ;; Process MSA segment
 ;
 D KILLMSA
 ;
 D MSA^LA7VIN3
 ;
 ; Set sequence flag
 S LA7SEQ=5
 Q
 ;
 ;
BSH ;; Process various HL7 header segments
FSH ;;
MSH ;;
 D KILLMSH
 ;
 D MSH^LA7VIN2
 ;
 ; Set sequence flag
 S LA7SEQ=1
 Q
 ;
 ;
NTE ;; Process NTE segment
 ;
 I LA7SEQ<30 D  Q
 . ; Put code to log error - no OBR/OBX segment
 ;
 ; Flag set that there was problem with OBR segment,
 ; skip associated NTE segments that follow OBR/OBX segments
 I LA7QUIT=2 Q
 ;
 I LA7MTYP="ORU" D NTE^LA7VIN2
 I LA7MTYP="ORM" D NTE^LA7VIN2
 I LA7MTYP="ORR" D NTE^LA7VIN2
 ;
 Q
 ;
 ;
OBR ;; Process OBR segment
 ;
 D KILLOBR
 ;
 ; Clear flag to process this segment
 I LA7QUIT>0 S LA7QUIT=0
 ;
 ; If not UI interface and no PID segment
 I LA7INTYP'=1,LA7SEQ<10 D  Q
 . S (LA7ABORT,LA7ERR)=46
 . D CREATE^LA7LOG(LA7ERR)
 ;
 I LA7MTYP="ORR" D OBR^LA7VIN4
 I LA7MTYP="ORU" D OBR^LA7VIN4
 I LA7MTYP="ORM" D OBR^LA7VORM
 ;
 ; Set sequence flag
 S LA7SEQ=30
 Q
 ;
 ;
OBX ;; Process OBX segment
 ;
 D KILLOBX
 ;
 ; No OBR segment, can't process OBX
 I LA7SEQ<30 D  Q
 . S (LA7ABORT,LA7ERR)=9
 . D CREATE^LA7LOG(LA7ERR)
 ;
 ; Flag set that there was problem with OBR segment,
 ; skip associated OBX segments that follow OBR segment
 I LA7QUIT=2 Q
 ;
 ; Process result messages (ORU).
 I LA7MTYP="ORU" D
 . I '$G(LA7ISQN) Q  ; No place to store results
 . ; Process "CH" subscript results.
 . I $G(LA7SS)="CH" D OBX^LA7VIN5
 . ;
 . ; Process AP subscripts results. "AU" not currently supported
 . I $G(LA7SS)?1(1"SP",1"CY",1"EM") D OBX^LA7VIN7
 . ;
 . ; Process "MI" subscript results.
 . I $G(LA7SS)="MI" D OBX^LA7VIN7
 . ;
 . ; Process "BB" subscript results - not supported
 . ;
 . ; Update test status on manifest
 . I $G(LA7628),LA7UID'="",$G(LA7OTST) D UTS^LA7VHLU1(LA7628,LA7UID,LA7OTST)
 ;
 ; Process results that accompany orders
 I LA7MTYP="ORM" D OBX^LA7VIN5
 ;
 ; Set sequence flag
 S LA7SEQ=40
 Q
 ;
 ;
ORC ;; Process ORC segment
 ;
 D KILLORC
 ;
 ; If not UI interface and no PID segment
 I LA7INTYP'=1,LA7SEQ<10 D  Q
 . S (LA7ABORT,LA7ERR)=46
 . D CREATE^LA7LOG(LA7ERR)
 ;
 D ORC^LA7VIN2
 ;
 ; Set sequence flag
 S LA7SEQ=20
 Q
 ;
 ;
PID ;; Process PID segment
 ;
 D KILLPID
 ;
 ; no MSH segment
 I LA7SEQ<1 D  Q
 . S (LA7ABORT,LA7ERR)=7
 . D CREATE^LA7LOG(LA7ERR)
 ;
 ; Clear flag to process this segment
 I LA7QUIT=1 S LA7QUIT=0
 ;
 D PID^LA7VIN2
 ;
 ; Set sequence flag
 S LA7SEQ=10
 Q
 ;
 ;
PV1 ;; Process PV1 segment
 ;
 D KILLPV1
 ;
 ; no PID segment
 I LA7SEQ<10 D  Q
 . S (LA7ABORT,LA7ERR)=46
 . D CREATE^LA7LOG(LA7ERR)
 ;
 D PV1^LA7VIN2
 ;
 ; Set sequence flag
 S LA7SEQ=11
 Q
 ;
 ;
SENDACK ; Send HL7 Application Acknowledgment message for selected interfaces/message types
 ;
 N LA
 S LA(62.48)=LA76248,LA(62.49)=LA76249
 S LA("ACK")=$S(+LA7AERR:"AE",1:"AA")
 S LA("MSG")=$P(LA7AERR,"^",2)
 D ACK^LA7VHLU8(.LA)
 Q
 ;
 ;
 ; The section below is designed to clean up variables that are created during the processing of a segment type
 ; and any created by processing of segments that are within the message definition.
 ;
KILLMSH ; Clean up variables used by MSH and following segments
 K LA7CSITE,LA7CS,LA7ECH,LA7FS,LA7HLV,LA7MEDT,LA7MID,LA7MTYP
 K LA7RAP,LA7RFAC,LA7SAP,LA7SEQ,LA7SFAC
 K ^TMP("LA7-ID",$J),^TMP("LA7-ORM",$J),^TMP("LA7-ORU",$J),^TMP("LA7-PL-NTE",$J)
 ;
KILLMSA ; Clean up variables used by MSA and following segments
 K LA7MSATM
 ;
KILLPID ; Clean up variables used by PID and following segments
 K DFN
 K LA7DOB,LA7ICN,LA7PNM,LA7PRACE,LA7PTID2,LA7PTID3,LA7PTID4
 K LA7SEX,LA7SPID,LA7SSN
 K LRDFN,LRTDFN
 ;
KILLPV1 ; Clean up variables used by PV1 and following segments
 K LA7LOC,LA7SPV1,LAPSUBID
 ;
KILLORC ; Clean up variables used by ORC and following segments
 K LA7628,LA7629
 K LA7CSITE,LA7DUR,LA7DURU,LA7ODUR,LA7ODURU,LA7EOL,LA7OCR,LA7ORDT
 K LA7OTYPE,LA7OUR,LA7PEB,LA7PON,LA7POP,LA7PVB,LA7SM
 ;
KILLOBR ; Clean up variables used by OBR and following segments
 K LA70070,LA760,LA761,LA762,LA7624,LA7696
 K LA7AA,LA7AD,LA7ACC,LA7AN,LA7ARI,LA7CDT,LA7FID,LA7ISQN,LA7LWL,LA7ONLT,LA7OTST
 K LA7POC,LA7PRI,LA7RSDT,LA7SAC,LA7SID,LA7SOBR,LA7SPEC,LA7SPTY,LA7SS,LA7TECH,LA7UID,LA7UR
 K LA7OBR25,LA7OBR26,LA7OBR29,LA7OBR32,LA7OBR33,LA7OBR34,LA7VPSTG
 ;
KILLOBX ; Clean up variables used by OBX and following segments
 K LA7ORS,LA7PRODID,LA7RLNC,LA7RMK,LA7RNLT,LA7RO,LA7SOBX,LA7SUBID
 ;
KILLBLG ; Clean up variables used by BLG and following segments
 ;
 Q
 ;DSS/RAF - BEGIN MOD - 2/11/2014 extract UID out of 62.49
VFDUID(IEN) ;pull UID out of HL7 message
 N I,VAL
 I '$O(^LAHM(62.49,IEN,0)) Q ""
 S I=$NA(^LAHM(62.49,IEN))
 F  S I=$Q(@I) Q:I=""  Q:$QS(I,1)'="62.49"  Q:$QS(I,2)'=IEN  D
 .I $P(@I,"|")="OBR" S VAL=$P($P(@I,"|",3),U)  ;DSS/RAF 7/2/2014 added for ARRA II and HL7 v2.5.1
 .I '$D(VAL) S VAL=""
 Q VAL
VFDMRN(IEN) ;pull MRN ouf of HL7 message
 N I,VAL
 I '$O(^LAHM(62.49,IEN,0)) Q ""
 S I=$NA(^LAHM(62.49,IEN))
 F  S I=$Q(@I) Q:I=""  Q:$QS(I,1)'="62.49"  Q:$QS(I,2)'=IEN  D  Q:+VAL  ;DSS/RAF 7/2/14
 .I $P(@I,"|")="PID" S VAL=$P(@I,"|",3)
 .I $G(VAL)="" S VAL=+$P(@I,"|",4)  ;DSS/RAF 6/30/2014 added for ARRA II using PID-3
 .I '$D(VAL) S VAL=""
 Q VAL
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**16**;11 DEC 2014
