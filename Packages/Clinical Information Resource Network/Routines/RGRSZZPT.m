RGRSZZPT ;ALB/RJS-UTILITY FOR CIRN ;4/11/96
 ;;1.0; CLINICAL INFO RESOURCE NETWORK ;;30 Apr 99
 ;
SKIP(SILENT,ARRAY) ;
 ;Pass Silent=1 if you want this call to be silent
 S SILENT=$G(SILENT)
 I $G(@ARRAY@(.01))=""!($G(@ARRAY@(.01))["@") D  Q 1
 . I SILENT="" U IO(0) W !,"MISSING PATIENT NAME, CANNOT FILE"
 I $G(@ARRAY@(.09))=""!($G(@ARRAY@(.09))["@") D  Q 1
 . I SILENT="" U IO(0) W !,"MISSING PATIENT SSN, CANNOT FILE"
 I $G(@ARRAY@(.03))=""!($G(@ARRAY@(.03))["@") D  Q 1
 . I SILENT="" U IO(0) W !,"MISSING PATIENT DOB, CANNOT FILE"
 I $G(@ARRAY@(991.01))=""!($G(@ARRAY@(991.01))["@") D  Q 1
 . I SILENT="" U IO(0) W !,"MISSING ICN, CANNOT FILE"
 Q 0
