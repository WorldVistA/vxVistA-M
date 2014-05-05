FBXIP33 ;WCIOFO/MJE-IMPORT GPCI/ZIP CODE DATA ;9/18/2001
 ;;3.5;FEE BASIS;**33**;JAN 30, 1995
 Q
 ;
PS ; post-install entry point
 ; create KIDS checkpoints with call backs
 N FBX,Y
 F FBX="LOAD" D
 .S Y=$$NEWCP^XPDUTL(FBX,FBX_"^FBXIP33")
 .I 'Y D BMES^XPDUTL("ERROR Creating "_FBX_" Checkpoint.")
 N FBX,Y
 F FBX="LOADA" D
 .S Y=$$NEWCP^XPDUTL(FBX,FBX_"^FBXIP33A")
 .I 'Y D BMES^XPDUTL("ERROR Creating "_FBX_" Checkpoint.")
 N FBX,Y
 F FBX="MRGDOL" D
 .S Y=$$NEWCP^XPDUTL(FBX,FBX_"^FBXIP33B")
 .I 'Y D BMES^XPDUTL("ERROR Creating "_FBX_" Checkpoint.")
 Q
 ;
 ;
LOAD ; Load data into 162.96 using included DATA as source
 N FBI,FBY,X,ZIP
 D BMES^XPDUTL("--Updating file 162.96")
 F FBI=1:1 S FBY=$P($T(DATA+FBI),";;",2) Q:FBY="END"  D
 . S ZIP=$P(FBY,U)
 . S Y=$O(^FB(162.96,"B",ZIP,0))
 . I 'Y D  Q:Y'>0
 . . K DA,DIC
 . . S DIC="^FB(162.96,",DLAYGO=162.96,DIC(0)="L",X=ZIP
 . . K DD,DO D FILE^DICN I Y'>0 D BMES^XPDUTL("ERROR ADDING NEW ZIP "_ZIP)
 . . K DIC,DLAYGO
 . ;
 . K DA,DIC
 . S DA(1)=+Y
 . S Y=$O(^FB(162.96,DA(1),"CY","B",2001,0))
 . I 'Y D  Q:Y'>0
 . . S DIC="^FB(162.96,"_DA(1)_",""CY"",",DIC(0)="L",DIC("P")="162.961A"
 . . S X=2001
 . . K DD,DO D FILE^DICN I Y'>0 D BMES^XPDUTL("ERROR ADDING 2001 for "_ZIP)
 . . K DIC,DLAYGO
 . S DA=+Y
 . ;
 . S DIE="^FB(162.96,"_DA(1)_",""CY"","
 . S DR=".02///"_$P(FBY,U,2)_";.03///"_$P(FBY,U,3)_";.04///"_$P(FBY,U,4)
 . D ^DIE K DIE,DR,DA
 ;
 D BMES^XPDUTL("---Update of file 162.96 complete")
 Q
 ;
DATA ;This is the ZIP and GPCI data ZIP^GPCI1^GCPI2^GPCI3
 ;;12062^1.01^1.079^1.3
 ;;15295^0.989^0.93^0.705
 ;;18202^0.989^0.93^0.705
 ;;19415^1.023^1.09^1.31
 ;;19505^1.023^1.09^1.31
 ;;22107^1.05^1.164^0.97
 ;;22108^1.05^1.164^0.97
 ;;23114^0.985^0.939^0.529
 ;;27205^0.97^0.927^0.546
 ;;27332^0.97^0.927^0.546
 ;;27517^0.97^0.927^0.546
 ;;27537^0.97^0.927^0.546
 ;;28271^0.97^0.927^0.546
 ;;30156^1.006^1.046^0.943
 ;;30160^1.006^1.046^0.943
 ;;31059^0.97^0.896^0.943
 ;;32128^0.975^0.947^1.296
 ;;32162^0.975^0.947^1.296
 ;;33508^0.975^0.947^1.296
 ;;33558^0.975^0.947^1.296
 ;;33559^0.975^0.947^1.296
 ;;33896^0.975^0.947^1.296
 ;;33897^0.975^0.947^1.296
 ;;33898^0.975^0.947^1.296
 ;;34211^0.975^0.947^1.296
 ;;34212^0.975^0.947^1.296
 ;;34269^0.975^0.947^1.296
 ;;34288^0.975^0.947^1.296
 ;;34289^0.975^0.947^1.296
 ;;38016^0.975^0.9^0.572
 ;;38571^0.975^0.9^0.572
 ;;38572^0.975^0.9^0.572
 ;;39540^0.957^0.841^0.75
 ;;45280^0.989^0.941^1.016
 ;;48085^1.042^1.03^2.903
 ;;48088^1.042^1.03^2.903
 ;;60103^1.006^1.069^1.505
 ;;60133^1.027^1.09^1.745
 ;;60527^1.006^1.069^1.505
 ;;60696^1.027^1.09^1.745
 ;;61616^0.964^0.888^1.074
 ;;63041^0.946^0.826^0.979
 ;;63190^0.994^0.94^1.022
 ;;75071^0.966^0.884^0.914
 ;;75098^0.966^0.884^0.914
 ;;75109^0.966^0.884^0.914
 ;;75252^0.966^0.884^0.914
 ;;75254^1.01^1.04^0.93
 ;;75287^0.966^0.884^0.914
 ;;75645^0.966^0.884^0.914
 ;;75752^0.966^0.884^0.914
 ;;75803^0.966^0.884^0.914
 ;;75965^0.966^0.884^0.914
 ;;77053^0.966^0.884^0.914
 ;;77615^0.966^0.884^0.914
 ;;78641^0.986^0.998^0.854
 ;;78737^0.966^0.884^0.914
 ;;80285^0.986^0.981^0.817
 ;;84194^0.977^0.925^0.619
 ;;84195^0.977^0.925^0.619
 ;;85042^0.994^0.975^1.15
 ;;85218^0.994^0.975^1.15
 ;;85297^0.994^0.975^1.15
 ;;85383^0.994^0.975^1.15
 ;;87507^0.973^0.905^0.809
 ;;87508^0.973^0.905^0.809
 ;;89074^1.005^1.035^1.103
 ;;89084^1.005^1.035^1.103
 ;;89086^1.005^1.035^1.103
 ;;91387^1.055^1.169^0.901
 ;;91390^1.055^1.169^0.901
 ;;92637^1.036^1.187^0.901
 ;;92809^1.036^1.187^0.901
 ;;94303^1.062^1.321^0.653
 ;;95033^1.007^1.039^0.723
 ;;98082^0.982^0.974^0.765
 ;;00727^0.882^0.72^0.317
 ;;03258^0.987^1.032^0.919
 ;;07086^1.057^1.192^0.827
 ;;08205^1.028^1.102^0.827
 ;;END
 ;
 ;FBXIP33
