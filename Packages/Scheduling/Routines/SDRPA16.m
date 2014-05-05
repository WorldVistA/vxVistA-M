SDRPA16 ;BP-OIFO/ESW - UTILITY ; 6/9/04 8:30am
 ;;5.3;Scheduling;**376**;Aug 13, 1993
EN(ST) ;
 N SR,II,STR,SA,STE,GG,SS,SQS,ER,SB,SM,SQ
 F II=1:1 S STR=$P($T(@ST+II),";;",2) Q:+STR'=ST  D
 .S SA=$P(STR,";",2) Q:SA'="B"
 .S SB=$P(STR,";",3),SM=$P(STR,";",4) D
 ..F GG=1:1 S STE=$P($T(@ST+II+GG),";;",2) Q:+STE'=ST!($P(STE,";",2)="B")  D
 ...S ER=$P(STE,";",3) S SQS=$P(STE,";",4) F SS=1:1 S SQ=$P(SQS,",",SS) Q:SQ=""  D PR^SD376P(SB,SM,ER,SQ)
 ..;update batch acknowledgement
 ..S ER="" S SQ=$O(^SDWL(409.6,"AMSG",SM,"")) Q:SQ=""  D PR^SD376P(SB,SM,ER,SQ)
 Q
664 ;;
 ;;664;B;66428349836;66444721425
 ;;664;ER;400;503,
 ;;664;B;66428368268;66444741152
 ;;664;ER;350;1452,
 ;;664;B;66428368989;66444741953
 ;;664;ER;350;4755,
 ;;664;ER;400;519,
 ;;664;B;66428375804;66444748723
 ;;664;ER;350;3217,
 ;;664;B;66428383184;66444755266
 ;;664;ER;400;597,
 ;;664;B;66428386245;66444758330
 ;;664;ER;400;534,
666 ;;
 ;;666;B;6669797547;66610952037
 ;;666;ER;350;3244,
667 ;;
 ;;667;B;66713122874;66738994513
 ;;667;ER;350;184,
 ;;667;B;66713133064;66739009240
 ;;667;ER;350;1647,
 ;;667;B;66713155082;66739041926
 ;;667;ER;350;3290,
668 ;;
 ;;668;B;66810464290;66817886230
 ;;668;ER;350;4754,
 ;;668;B;66810466850;66817889332
 ;;668;ER;350;2207,
 ;;668;B;66810474924;66817900838
 ;;668;ER;350;4754,
 ;;668;B;66810475458;66817901533
 ;;668;ER;350;2153,
 ;;668;B;66810488617;66817919364
 ;;668;ER;350;4754,
 ;;668;B;66810488785;66817919561
 ;;668;ER;350;2140,
671 ;;
 ;;671;B;67132981217;67159117106
 ;;671;ER;350;833,2035,3868,
 ;;671;B;67132987909;67159125467
 ;;671;ER;350;4073,
 ;;671;B;67133036913;67159200628
 ;;671;ER;350;643,1237,
 ;;671;B;67133041690;67159206920
 ;;671;ER;350;4773,
 ;;671;B;67133042133;67159207528
 ;;671;ER;350;3832,
 ;;671;B;67133045694;67159211695
 ;;671;ER;350;2299,
 ;;671;B;67133046570;67159212323
 ;;671;ER;350;4381,
 ;;671;B;67133047309;67159213658
 ;;671;ER;350;3620,
 ;;671;B;67133048613;67159215258
 ;;671;ER;350;1902,
 ;;671;B;67133050070;67159216787
 ;;671;ER;350;1954,
 ;;671;B;67133050455;67159217438
 ;;671;ER;350;4661,
 ;;671;B;67133058683;67159227958
 ;;671;ER;350;3228,
 ;;671;B;67133098028;67159287146
 ;;671;ER;400;4927,4928,4929,4930,4931,
 ;;671;B;67133111103;67159303091
 ;;671;ER;350;2048,
 ;;671;B;67133133529;67159334455
 ;;671;ER;200;301,
 ;;671;B;67133137194;67159341590
 ;;671;ER;200;2161,
 ;;671;B;67133141268;67159347813
 ;;671;ER;200;3057,3865,
 ;;671;B;67133213967;67159469557
 ;;671;ER;200;3679,
 ;;671;ER;350;224,611,3475,4651,
672 ;;
 ;;672;B;67229599250;67247153270
 ;;672;ER;350;991,
 ;;672;B;67229626113;67247191361
 ;;672;ER;350;1503,
 ;;672;B;67229626569;67247192113
 ;;672;ER;350;2080,
 ;;672;B;67229627431;67247194800
 ;;672;ER;350;3597,
 ;;672;B;67229629617;67247197857
 ;;672;ER;350;135,
 ;;672;B;67229641291;67247214235
 ;;672;ER;350;2778,
 ;;672;B;67229652973;67247229333
 ;;672;ER;350;4323,
 ;;672;B;67229690512;67247282637
 ;;672;ER;400;4959,
673 ;;
 ;;673;B;67356533673;67315769886
 ;;673;ER;350;4512,
 ;;673;B;67356593151;67315849807
 ;;673;ER;350;2927,
 ;;673;B;67356619775;67315884695
 ;;673;ER;350;3404,
 ;;673;B;67356621574;67315887362
 ;;673;ER;350;862,
678 ;;
 ;;678;B;6784062356;67841327096
 ;;678;ER;350;2133,
 ;;678;B;6784064606;67841330669
 ;;678;ER;350;2680,
688 ;;
 ;;688;B;68830770661;68845659219
 ;;688;ER;350;3609,
 ;;688;B;68830785983;68845682134
 ;;688;ER;400;238,
 ;;688;B;68830816605;68845723675
 ;;688;ER;400;527,
689 ;;
 ;;689;B;68926206064;68946706330
 ;;689;ER;350;3394,
 ;;689;B;68926226721;68946729321
 ;;689;ER;350;1414,
 ;;689;B;68926228196;68946731460
 ;;689;ER;350;687,
 ;;689;B;68926229578;68946732672
 ;;689;ER;350;3689,
 ;;689;B;68926230786;68946733904
 ;;689;ER;350;1692,
 ;;689;B;68926231927;68946735396
 ;;689;ER;350;677,
 ;;689;B;68926235812;68946739193
 ;;689;ER;350;447,
 ;;689;B;68926238671;68946741989
 ;;689;ER;350;1617,
 ;;689;B;68926240742;68946744272
 ;;689;ER;350;1941,
691 ;;
 ;;691;B;69115467595;69199974034
 ;;691;ER;350;3515,
 ;;691;B;69115473812;691100057312
 ;;691;ER;350;569,791,2239,3291,
 ;;691;B;69115473843;691100057730
 ;;691;ER;350;3558,
 ;;691;B;69115474016;691100059938
 ;;691;ER;350;4558,
 ;;691;ER;400;1900,
 ;;691;B;69115474436;691100065253
 ;;691;ER;350;2084,
 ;;691;B;69115475068;691100072896
 ;;691;ER;350;2292,
 ;;691;B;69115475285;691100076254
 ;;691;ER;350;2492,
 ;;691;B;69115477604;691100106895
 ;;691;ER;400;4378,
695 ;;
 ;;695;B;69547025705;69566977886
 ;;695;ER;700;747,
 ;;695;B;69547184760;69567085569
 ;;695;ER;350;1522,
 ;;695;B;69547228336;69567113862
 ;;695;ER;350;4578,
 ;;695;B;69547301923;69567168441
 ;;695;ER;350;3721,
 ;;695;B;69547309355;69567175635
 ;;695;ER;350;4755,
 ;;695;B;69547362795;69567213604
 ;;695;ER;350;966,
 ;;695;B;69547380956;69567225219
 ;;695;ER;350;2566,
 ;;695;B;69547650192;69567407656
 ;;695;ER;350;4474,
756 ;;
 ;;756;B;75611363180;75615911397
 ;;756;ER;700;2781,
