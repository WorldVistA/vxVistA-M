RGMTMONX ;BIR/CML,PTD-MPI/PD Monitor HL7 Messaging/Filers and Setups (CONT) ;07/30/02
 ;;1.0;CLINICAL INFO RESOURCE NETWORK;**20,30,31**;30 Apr 99
 ;
 ;Reference to ^DIC(4,"D", supported by IA #3627
 ;Reference to ^HLCS(870 supported by IA #3335
 ;Reference to ^DGCN(391.91, supported by IA #2911
 ;
START ;
 I DEV,'$D(RGHLMQ) W @IOF,"Logical Link Monitor:",!,"=====================",!
 ;
 K ^XTMP("RGMT","MONT")
 I '$D(TXTCNT) S TXTCNT=0
 I '$D(LOCSITE) S LOCSITE=$P($$SITE^VASITE(),"^")
 ;
 ;MULTID=# of multiple "D" xrefs in ^DIC(4 for Station #
 ;NOINST=# of VAxxx Links with no INSTITUTION defined
 ;BADINST=# of VAxxx Links that have an incorrect INSTITUTION defined
 ;BADTF=# of patients with invalid Treating Facilities
 ;NONVA=# of non-VAxxx Logical Links with INSTITUTION defined
 ;CNT=# of VAxxx Logical Links processed
 ;
 S (NOINST,BADINST,BADTF,MULTID,NONVA,CNT,DODHOST)=0
 ;
1 ;
 I DEV,'$D(RGHLMQ) W !,"=> Checks 1-3: Checking Link Setups..."
 K LINKARR
 S LINK=""
 F  S LINK=$O(^HLCS(870,"B",LINK)) Q:LINK=""  D
 .I LINK'="MPIVA"&($E(LINK,1,2)'="VA") Q
 .I $E(LINK,1,2)="VA"&($L(LINK," ")>1) Q
 .Q:LINK["-"
 .I $T(@LINK)']"" Q
 .S CNT=CNT+1
 .S VALSTA=$P($P($T(@LINK),";;",2),"^",2)
 .;build array of links with valid station numbers to do check 5
 .S LINKARR(VALSTA)=LINK
2 .;check for multiple "D" xrefs in ^DIC(4 for this station #)
 .S (XCNT,QQ)=0
 .F  S QQ=$O(^DIC(4,"D",VALSTA,QQ)) Q:'QQ  D
 ..S XCNT=XCNT+1
 ..I QQ["." D
 ...S ^XTMP("RGMT","MONT","DECIMEL INST IEN",VALSTA,QQ)=""
 .I XCNT>1 D
 ..S LINKIEN=$O(^HLCS(870,"B",LINK,0))
 ..S MULTID=MULTID+1
 ..S (XCNT,QQ)=0
 ..F  S QQ=$O(^DIC(4,"D",VALSTA,QQ)) Q:'QQ  D
 ...S ^XTMP("RGMT","MONT","MULTI DXREF",VALSTA,QQ)=$$GET1^DIQ(4,QQ_",",.01)_"^"_$$GET1^DIQ(870,LINKIEN_",",.01)_"^"_$$GET1^DIQ(870,LINKIEN_",",.02,"I")
3 .;check for defined INSTITUTION and set (if necessary) AUTOSTART and RESTART for links in file 870
 .S LINKIEN=$O(^HLCS(870,"B",LINK,0))
 .S LINKINST=$$GET1^DIQ(870,LINKIEN_",",.02,"I")
 .I 'LINKINST D  Q
 ..S NOINST=NOINST+1
 ..S ^XTMP("RGMT","MONT","NO INST",LINK)=LINKIEN_"^"_$P($P($T(@LINK),";;",2),"^",1,2)
 ..S TXT="   ** No INSTITUTION defined for "_LINK D TXT
4 .;check for incorrect INSTITUTION
 .S LOCSTA=$P($$NS^XUAF4(LINKINST),"^",2)
 .I LOCSTA'=VALSTA D
 ..S BADINST=BADINST+1
 ..S ^XTMP("RGMT","MONT","BAD INST",LINK)=LOCSTA
 ..S TXT="   ** Bad INSTITUTION of Station #"_LOCSTA_" defined for "_LINK_" - should be Station #"_VALSTA D TXT
 ;
5 ;
 I DEV,'$D(RGHLMQ) W !,"=> Check 4   : Checking for patients with invalid Treating Facilities..."
 S TFIEN=0
 F  S TFIEN=$O(^DGCN(391.91,"C",TFIEN)) Q:'TFIEN  D
 .S TF=$P($$NS^XUAF4(TFIEN),"^",2)
 .I TF="" D  Q
 ..S TFIFN=0 F  S TFIFN=$O(^DGCN(391.91,"C",TFIEN,TFIFN)) Q:'TFIFN  D
 ...S BADTF=BADTF+1
 ...S DFN=$$GET1^DIQ(391.91,TFIFN_",",.01,"I")
 ...I DEV,'$D(RGHLMQ) D
 ....W !?3,"** TF with no station # for ",$P(^DIC(4,TFIEN,0),"^")," for DFN #",DFN," - (TF IEN #",TFIFN,")"
 .I '$D(LINKARR(TF)) D
 ..S IEN=0 F  S IEN=$O(^DGCN(391.91,"C",TFIEN,IEN)) Q:'IEN  D
 ...I TF=200 S DODHOST=DODHOST+1 Q
 ...S DFN=$$GET1^DIQ(391.91,IEN_",",.01,"I")
 ...S BADTF=BADTF+1
 ...S ^XTMP("RGMT","MONT","BAD TF",DFN)=$$GET1^DIQ(4,TFIEN_",",.01)_"^"_IEN
 ...I DEV,'$D(RGHLMQ) W !?3,"** Bad TF of ",$P(^DIC(4,TFIEN,0),"^")," for DFN #",DFN," - (TF IEN #",IEN,")"
 ;
6 ;
 I DEV,'$D(RGHLMQ) W !,"=> Check 5   : Checking for non-VAxxx links with INSTITUTION defined..."
 S LINK=""
 F  S LINK=$O(^HLCS(870,"B",LINK)) Q:LINK=""  I $E(LINK,1,2)'="VA"!($E(LINK,1,4)="VAFC") D
 .Q:LINK="MPIVA"
 .S LINKIEN=$O(^HLCS(870,"B",LINK,0))
 .S LINKINST=$$GET1^DIQ(870,LINKIEN_",",.02,"I")
 .Q:'LINKINST
 .Q:LINKINST'=+$$SITE^VASITE()
 .S NONVA=NONVA+1
 .S ^XTMP("RGMT","MONT","NONVA LINK WITH INSTITUTION",LINK)=""
 .S TXT="   ** Non-VA Link with LOCAL INSTITUTION defined - "_LINK D TXT
 ;
DONE ;
 S TXT="" D TXT
 ;
 S TXT="==============================" D TXT
 S TXT="Check 1: "_MULTID_" VA MPI/PD Links with multiple ""D"" xref in ^DIC(4 for Station #." D TXT
 S TXT="Check 2: "_NOINST_" VA MPI/PD Links without an INSTITUTION defined." D TXT
 S TXT="Check 3: "_BADINST_" VA MPI/PD Links with incorrect INSTITUTION defined."  D TXT
 S TXT="Check 4: "_BADTF_" patients with invalid Treating Facilities. => FHIE Pts: "_DODHOST D TXT
 S TXT="Check 5: "_NONVA_" non-VA MPI/PD Links with an INSTITUTION of local site defined." D TXT
 ;
 K BADINST,BADTF,CNT,DEV,DFN,IEN,LINK,LINKARR,LINKIEN,LINKINST,LOCSITE,LOCSTA,MULTID
 K NOINST,NONVA,QQ,TF,TFIEN,TFIFN,TXT,TXTCNT,VALSTA,XCNT,DODHOST
 Q
 ;
TXT ;
 S TXTCNT=TXTCNT+1
 I DEV,'$D(RGHLMQ) W !,TXT
 I $D(RGHLMQ) S ^XTMP("RGMT","HLMQMONT",LOCSITE,TXTCNT)=TXT
 Q
 ;
LINKS ;
MPIVA ;;MPI^200M
VAALX ;;ALEXANDRIA^502
VAALT ;;ALTOONA^503
VAAMA ;;AMARILLO HCS^504
VAANC ;;ANCHORAGE^463
VAANN ;;ANN ARBOR^506
VAASH ;;ASHEVILLE^637
VAATG ;;ATLANTA^508
VAAUG ;;AUGUSTA^509
VABAC ;;BATTLE CREEK^515
VABAY ;;BAY PINES^516
VABEC ;;BECKLEY^517
VABED ;;BEDFORD^518
VABIL ;;BILOXI^520
VABIR ;;BIRMINGHAM^521
VABHH ;;BLACK HILLS HCS^568
VABOI ;;BOISE^531
VABOS ;;BOSTON HCS^523
VABRX ;;BRONX^526
VABUT ;;BUTLER^529
VACAH ;;CENTRAL ALABAMA HCS^619
VALIT ;;CENTRAL ARKANSAS HCS^598
VAFRE ;;CENTRAL CALIFORNIA HCS^570
VAOMA ;;CENTRAL PLAINS HCS^636
VACTX ;;CENTRAL TEXAS HCS^674
VACHA ;;CHARLESTON^534
VACHY ;;CHEYENNE^442
VACHS ;;CHICAGO HCS^537
VACLL ;;CHILLICOTHE^538
VACIN ;;CINCINNATI^539
VACLA ;;CLARKSBURG^540
VACLE ;;CLEVELAND^541
VACOA ;;COATESVILLE^542
VACMS ;;COLUMBIA^544
VACOS ;;COLUMBUS^757
VACON ;;CONNECTICUT HCS^689
VADAY ;;DAYTON^552
VADEN ;;DENVER^554
VADET ;;DETROIT^553
VADUB ;;DUBLIN^557
VADUR ;;DURHAM^558
VAELP ;;EL PASO^756
VAERI ;;ERIE^562
VAFAR ;;FARGO^437
VAFNC ;;FAYETTEVILLE^565
VAFAV ;;FAYETTEVILLE^564
VAJAC ;;G. V. (SONNY) MONTGOMERY^586
VAGRJ ;;GRAND JUNCTION^575
VAHAM ;;HAMPTON^590
VAHIN ;;HINES^578
VAHON ;;HONOLULU VAMROC^459
VAHOU ;;HOUSTON^580
VAHVH ;;HUDSON VALLEY HCS^620
VAHUN ;;HUNTINGTON^581
VADAN ;;ILLIANA HEALTH CARE SYSTEM^550
VAIND ;;INDIANAPOLIS^583
VAIRO ;;IRON MOUNTAIN^585
VALAS ;;LAS VEGAS^593
VALEB ;;LEBANON^595
VALEX ;;LEXINGTON^596
VALOM ;;LOMA LINDA^605
VALON ;;LONG BEACH HCS^600
VALOU ;;LOUISVILLE^603
VAMAD ;;MADISON^607
VAMAN ;;MANCHESTER^608
VAMPI ;;MANILA^358
VAMWV ;;MARTINSBURG^613
VAMAR ;;MARYLAND HCS^512
VAMEM ;;MEMPHIS^614
VAMIA ;;MIAMI^546
VAMIW ;;MILWAUKEE^695
VAMIN ;;MINNEAPOLIS^618
VAFHM ;;MONTANA HCS^436
VAMOU ;;MOUNTAIN HOME^621
VAMUS ;;MUSKOGEE^623
VAGAI ;;N. FLORIDA/S. GEORGIA HCS^573
VANJH ;;NEW JERSEY HCS^561
VAALB ;;NEW MEXICO HCS^501
VANOL ;;NEW ORLEANS^629
VANYN ;;NEW YORK HARBOR HCS^630
VANCH ;;NORTH CHICAGO^556
VANTH ;;NORTH TEXAS HCS^549
VANHM ;;NORTHAMPTON^631
VAPRE ;;NORTHERN ARIZONA HCS^649
VAMAC ;;NORTHERN CALIFORNIA HCS^612
VANIN ;;NORTHERN INDIANA HCS^610
VANOP ;;NORTHPORT^632
VAOKL ;;OKLAHOMA CITY^635
VAPAL ;;PALO ALTO HCS^640
VAPHI ;;PHILADELPHIA^642
VAPHO ;;PHOENIX^644
VAPTH ;;PITTSBURGH HCS^646
VAPOR ;;PORTLAND^648
VAPRO ;;PROVIDENCE^650
VAPUG ;;PUGET SOUND HCS^663
VARIC ;;RICHMOND^652
VAROS ;;ROSEBURG^653
VASAG ;;SAGINAW^655
VASAM ;;SALEM^658
VASBY ;;SALISBURY^659
VASLC ;;SALT LAKE CITY HCS^660
VASDC ;;SAN DIEGO HCS^664
VASFC ;;SAN FRANCISCO^662
VASAJ ;;SAN JUAN^672
VASHE ;;SHERIDAN^666
VASHR ;;SHREVEPORT^667
VAREN ;;SIERRA NEVADA HCS^654
VASUX ;;SIOUX FALLS^438
VASTX ;;SOUTH TEXAS HCS^671
VATUC ;;SOUTHERN ARIZONA HCS^678
VASPO ;;SPOKANE^668
VASTC ;;ST. CLOUD^656
VATAM ;;TAMPA^673
VANAS ;;TENNESSEE VALLEY HCS^626
VATOG ;;TOGUS^402
VATOM ;;TOMAH^676
VATUA ;;TUSCALOOSA^679
VAWNY ;;UPSTATE NEW YORK HCS^528
VASTL ;;VA HEARTLAND - EAST, VISN 15^657
VAKAN ;;VA HEARTLAND - WEST, VISN 15^589
VAWWW ;;WALLA WALLA^687
VAWAS ;;WASHINGTON^688
VAWLA ;;WEST LA VAMC^691
VAWPB ;;WEST PALM BEACH^548
VABIG ;;WEST TEXAS HCS^519
VAWCO ;;WHITE CITY^692
VAWRJ ;;WHITE RIVER JCT^405
VAWBP ;;WILKES BARRE^693
VAWIM ;;WILMINGTON^460
 ;;***
