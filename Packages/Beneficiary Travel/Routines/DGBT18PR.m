DGBT18PR ;ALB/RAM - PRE-INSTALL DGBT*1.0*18 ; 9/10/10 9:22Am
 ;;1.0;Beneficiary Travel;**18**;September 25, 2001;Build 15
 Q
EN ;Pre install entry point
 N DGBTX,DGBTY,DGBTKEY,DGBTFLD,DGBTXREF
 F DGBTKEY="21,1","22,1,","25,1","26,1" D
 . S DGBTFLD=$P(DGBTKEY,","),DGBTXREF=$P(DGBTKEY,",",2)
 . D DEL
 Q
DEL ;Delete trigger on field=FLD, XREF#=XREF of file 392.
 N DGBTFILE,DGBTFIELD,DGBTREF
 S DGBTFILE=392,DGBTFIELD=DGBTFLD,DGBTREF=DGBTXREF
 D DELIX^DDMOD(DGBTFILE,DGBTFIELD,DGBTREF)
 Q