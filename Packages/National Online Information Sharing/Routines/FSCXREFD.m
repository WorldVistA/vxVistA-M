FSCXREFD ;SLC/STAFF-NOIS Xrefs Call Dev Status ;1/11/98  15:26
 ;;1.1;NOIS;;Sep 06, 1998
 ;
ADM(OP,FIELD,VALUE,CALL) ; from dd 7100
 N MOD,STATUS
 I OP="SET" D  Q
 .I FIELD="STATUS" D  Q
 ..I VALUE=1 S MOD=$P(^FSCD("CALL",CALL,0),U,8) I MOD S ^FSCD("CALL","ADM",MOD,CALL)=""
 .I FIELD="MOD" D  Q
 ..I VALUE S STATUS=$P(^FSCD("CALL",CALL,0),U,17) I STATUS=1 S ^FSCD("CALL","ADM",VALUE,CALL)=""
 I OP="KILL" D  Q
 .I FIELD="STATUS" D  Q
 ..S MOD=$P(^FSCD("CALL",CALL,0),U,8) I MOD K ^FSCD("CALL","ADM",MOD,CALL)
 .I FIELD="MOD" D  Q
 ..K ^FSCD("CALL","ADM",VALUE,CALL)
 Q
 ;
ADP(OP,FIELD,VALUE,CALL) ; from dd 7100
 N PACK,STATUS
 I OP="SET" D  Q
 .I FIELD="STATUS" D  Q
 ..I VALUE=1 S PACK=$P(^FSCD("CALL",CALL,120),U,9) I PACK S ^FSCD("CALL","ADP",PACK,CALL)=""
 .I FIELD="PACK" D  Q
 ..I VALUE S STATUS=$P(^FSCD("CALL",CALL,0),U,17) I STATUS=1 S ^FSCD("CALL","ADP",VALUE,CALL)=""
 I OP="KILL" D  Q
 .I FIELD="STATUS" D  Q
 ..S PACK=$P(^FSCD("CALL",CALL,120),U,9) I PACK K ^FSCD("CALL","ADP",PACK,CALL)
 .I FIELD="PACK" D  Q
 ..K ^FSCD("CALL","ADP",VALUE,CALL)
 Q
 ;
ADS(OP,FIELD,VALUE,CALL) ;
 N SITE,STATUS
 I OP="SET" D  Q
 .I FIELD="STATUS" D  Q
 ..I VALUE=1 S SITE=$P(^FSCD("CALL",CALL,0),U,5) I SITE S ^FSCD("CALL","ADS",SITE,CALL)=""
 .I FIELD="SITE" D  Q
 ..I VALUE S STATUS=$P(^FSCD("CALL",CALL,0),U,17) I STATUS=1 S ^FSCD("CALL","ADS",VALUE,CALL)=""
 I OP="KILL" D  Q
 .I FIELD="STATUS" D  Q
 ..S SITE=$P(^FSCD("CALL",CALL,0),U,5) I SITE K ^FSCD("CALL","ADS",SITE,CALL)
 .I FIELD="SITE" D  Q
 ..K ^FSCD("CALL","ADS",VALUE,CALL)
 Q
 ;
ADID(OP,FIELD,VALUE,CALL) ; from dd 7100
 N ISC,STATUS
 I OP="SET" D  Q
 .I FIELD="STATUS" D  Q
 ..I VALUE=1 S ISC=$P(^FSCD("CALL",CALL,0),U,20) I ISC S ^FSCD("CALL","ADID",ISC,CALL)=""
 .I FIELD="ISC" D  Q
 ..I VALUE S STATUS=$P(^FSCD("CALL",CALL,0),U,17) I STATUS=1 S ^FSCD("CALL","ADID",VALUE,CALL)=""
 I OP="KILL" D  Q
 .I FIELD="STATUS" D  Q
 ..S ISC=$P(^FSCD("CALL",CALL,0),U,20) I ISC K ^FSCD("CALL","ADID",ISC,CALL)
 .I FIELD="ISC" D  Q
 ..K ^FSCD("CALL","ADID",VALUE,CALL)
 Q
