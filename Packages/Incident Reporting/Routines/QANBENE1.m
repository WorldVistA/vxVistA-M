QANBENE1 ;HISC/GJC-Special incidents invol. a beneficiary ;3/3/92
 ;;2.0;Incident Reporting;**1,8,11,18,26,28**;08/07/1992
 ;
EN1 ;Format of the print for our Beneficiary Report.
 S QANWHICH=$S(QANFLG("WARD")="D":"Domiciliary",QANFLG("WARD")="NH":"NHCU",QANFLG("WARD")="I":"Inpatient",QANFLG("WARD")="O":"Outpatient",1:"total")
 Q:QANQUIT
 I '$D(^TMP("QANBEN",$J,"BEN")) D PRINT^QANBENE3 Q
 Q:QANQUIT
 S QANAA=""
 F  S QANAA=$O(^TMP("QANBEN",$J,"BEN",QANAA)) Q:QANAA']""  D
 . S QANBB=""
 . F  S QANBB=$O(^TMP("QANBEN",$J,"BEN",QANAA,QANBB)) Q:QANBB']""  D
 . . S QANLP(1)=0
 . . F  S QANLP(1)=$O(^TMP("QANBEN",$J,"BEN",QANAA,QANBB,QANLP(1))) Q:QANLP(1)'>0  D INC
 I '$D(^TMP("QANBEN",$J,"BEN")) W !!,"There exist zero (0) "_QANWHICH_" incidents for this date range."
 D EN1^QANBENE2 ;Prints out the data.
 Q
INC ;Checks for appropriate incident data.
 K QANLBL S QAN7424=$G(^QA(742.4,QANLP(1),0)) Q:QAN7424']""
 S QANINPT=$P(QAN7424,U,2),QANINVST=+$P(QAN7424,U,11)
 I $D(^QA(742.1,"BUPPER","PATIENT ABUSE",QANINPT)) D PROVE Q
 I $D(^QA(742.1,"BUPPER","DEATH",QANINPT)) D DEATH Q
 I $D(^QA(742.1,"BUPPER","FALL",QANINPT)) S QANLBL="FALLS"
 I $D(^QA(742.1,"BUPPER","INFORMED CONSENT-FAIL. TO OBTAIN",QANINPT)) S QANLBL="INFORMED"
 I $D(^QA(742.1,"BUPPER","INFORMED CONSENT, FAIL. TO OBTAIN",QANINPT)) S QANLBL="INFORMED"
 I $D(^QA(742.1,"BUPPER","HOMICIDE",QANINPT)) S QANLBL="HOMICIDE"
 I $D(^QA(742.1,"BUPPER","MEDICATION ERROR",QANINPT)) S QANLBL="MED ERR"
 I $D(^QA(742.1,"BUPPER","MISSING PATIENT",QANINPT)) S QANLBL="MISSING PAT"
 I $D(^QA(742.1,"BUPPER","ASSAULT-PATIENT TO PATIENT",QANINPT)) S QANLBL="ASSAULT PAT/PAT"
 I $D(^QA(742.1,"BUPPER","ASSAULT, PATIENT TO PATIENT",QANINPT)) S QANLBL="ASSAULT PAT/PAT"
 I $D(^QA(742.1,"BUPPER","ASSAULT-PATIENT/STAFF",QANINPT)) S QANLBL="ASSAULT PAT/STAFF"
 I $D(^QA(742.1,"BUPPER","ASSAULT, PATIENT/STAFF",QANINPT)) S QANLBL="ASSAULT PAT/STAFF"
 I $D(^QA(742.1,"BUPPER","INJURY NOT OTHERWISE LISTED",QANINPT)) S QANLBL="OTHER"
 I $D(^QA(742.1,"BUPPER","FIRE-PATIENT INVOLVED IN",QANINPT)) S QANLBL="FIRE"
 I $D(^QA(742.1,"BUPPER","FIRE, PATIENT INVOLVED IN",QANINPT)) S QANLBL="FIRE"
 I $D(^QA(742.1,"BUPPER","SEXUAL ASSAULT",QANINPT)) S QANLBL="SEX"
 I $D(^QA(742.1,"BUPPER","SUICIDE ATTEMPT",QANINPT)) S QANLBL="SUI ATT"
 I $D(^QA(742.1,"BUPPER","SUICIDE",QANINPT)) S QANLBL="SUICIDE"
 I $D(^QA(742.1,"BUPPER","TRANSFUSION ERROR",QANINPT)) S QANLBL="TRANS ERR"
 Q:$G(QANLBL)']""  ;Not a valid label
 F QANLP(2)=0:0 S QANLP(2)=$O(^TMP("QANBEN",$J,"BEN",QANAA,QANBB,QANLP(1),QANLP(2))) Q:QANLP(2)'>0  D
 . S QANSLEV=$P(^QA(742,QANLP(2),0),U,10) Q:$G(QANSLEV)']""
 . I $G(QANLBL)]"" D TALLY^QANBENE0
 I '$D(^TMP("QANBEN",$J,"BEN")) W !!,"There exist zero (0) incidents within this data range." Q
 Q
DEATH ;Tracking Deaths.
 F QANLP(2)=0:0 S QANLP(2)=$O(^TMP("QANBEN",$J,"BEN",QANAA,QANBB,QANLP(1),QANLP(2))) Q:QANLP(2)'>0  D
 . S QANSLEV=$P(^QA(742,QANLP(2),0),U,10) Q:'$G(QANSLEV)
 . D DEATH1
 . Q:$G(QANDTH)']""
 . D:$G(QANLBL)]"" TALLY^QANBENE0
 Q
DEATH1 ;Tracking Deaths.
 S QANDTH=$P(QAN7424,U,14) Q:QANDTH']""
 Q:$D(^QA(742.14,"BUPPER","OTHER",QANDTH))
 Q:$D(^QA(742.14,"BUPPER","WITHIN 24 HOURS OF ADMISSION (EX. DOA'S AND TERMINALS)",QANDTH))
 I $D(^QA(742.14,"BUPPER","CONJUNCTION WITH A PROCEDURE",QANDTH)) S QANLBL="DEATH-CON"
 I $D(^QA(742.14,"BUPPER","DURING INDUCTION OF ANES.",QANDTH)) S QANLBL="DEATH-ANESTH"
 I $D(^QA(742.14,"BUPPER","FAILURE TO DIAGNOSE OR TREAT",QANDTH)) S QANLBL="DEATH-FAIL"
 I $D(^QA(742.14,"BUPPER","ON MEDICAL CENTER GROUNDS",QANDTH)) S QANLBL="DEATH-MED CEN"
 I $D(^QA(742.14,"BUPPER","OPERATING ROOM",QANDTH)) S QANLBL="DEATH-OR"
 I $D(^QA(742.14,"BUPPER","RECOVERY ROOM",QANDTH)) S QANLBL="DEATH-RR"
 I $D(^QA(742.14,"BUPPER","CASES ACCEPTED BY M.E.",QANDTH)) S QANLBL="DEATH-M.E."
 I $D(^QA(742.14,"BUPPER","EQUIPMENT MALFUNCTION",QANDTH)) S QANLBL="DEATH-EQ"
 I $D(^QA(742.14,"BUPPER","WITHIN 48 HOURS OF SURGERY",QANDTH)) S QANLBL="DEATH-48"
 Q
PROVE ;Sets Patient Abuse Array.
 S QANAB=+$P(QAN7424,U,16),QANLBL=$S(QANAB=1:"PATIENT ABUSE/PROVEN",1:"PATIENT ABUSE/ALLEGED")
 F QANLP(2)=0:0 S QANLP(2)=$O(^TMP("QANBEN",$J,"BEN",QANAA,QANBB,QANLP(1),QANLP(2))) Q:QANLP(2)'>0  D
 . S QANSLEV=$P(^QA(742,QANLP(2),0),U,10)
 . Q:$G(QANLBL)']""!('$G(QANSLEV))
 . D TALLY^QANBENE0
 Q
