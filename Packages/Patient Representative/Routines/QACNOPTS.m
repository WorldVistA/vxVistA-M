QACNOPTS ;HISC/VAD -Contact records without patients ;12/29/98  09:41
 ;;2.0;Patient Representative;**9,17**;11/17/1998
MAIN ;
 S QAQPOP=0
 D DATDIV^QACUTL0 G:QAQPOP EXIT
 K DIC,FLDS,L,BY,FR,TO,DHD
 S L=0,DIC="^QA(745.1,"
 S FLDS=".01,1",DHD="[QAC NOPAT HEADER]"
 ;
 K DIC,FLDS,L,BY,FR,TO,DHD
 S L=0,DIC="^QA(745.1,"
 S FLDS=".01,1",DHD="[QAC NOPAT HEADER]"
 S BY="37;S3;C25,'2,1"
 S FR(1)="@",TO(1)=""
 I +$G(QAC1DIV) D
 . S QACDVNAM="" D INST^QACUTL0(QAC1DIV,.QACDVNAM)
 . S (FR(1),TO(1))=QACDVNAM
 S FR(2)="@",TO(2)="@"
 S FR(3)=QAQNBEG,TO(3)=QAQNEND
 D EN1^DIP
EXIT ;
 K DIC,FLDS,L,BY,FR,TO,DHD,DIP,QACPOP,QACDV,QAC1DIV,QACDVNAM
 K QAQNBEG,QAQNEND,QAQPOP
 D K^QAQDATE
 Q
