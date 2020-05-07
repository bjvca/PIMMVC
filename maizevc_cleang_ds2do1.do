use "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds1.dta" 
doedit "C:\Users\Samuel\Documents\iFPRI\maize\mz_cleaning1.do" 

use "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", clear

label variable start "survey start time"
label variable end "survey end time"
label variable today "date of interview"
label variable deviceid "devise id"
label variable subscriberid "subscriber id"
label variable simserial "sim serial"
label variable phonenumber "phone numer"
label variable enumerator "enumerator"
label variable date "date of "
label variable hhmaizesub "hh subcounty location"
label variable hhmaizedistrict "hh district location"
label variable hhmaizeparish "hh paarish location"
label variable hhmaizevillage "hh village location"
label variable hhconsent "hh respondent consent"
label variable hhphone "hh contact phone"
tab hhmaizedistrict
tab hhmaizesub
ed metainstanceid _uuid
sort metainstanceid
gen index= _n
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds1.dta", replace

*xxxxxxxxxxxxxxxxxxxxxxxxx
doedit "C:\Users\Samuel\Documents\iFPRI\maize\mz_cleaning1.do" 
use "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", clear
ed
compress
label variable q1 "hh/hh head [yes=1/0]"
label variable q2 "hh/make decisions [yes=1/0]"
label variable q3 "hm/other knwledgeable person [yes=1/0]"
label variable q4 "hm/grew mz- 2018A [yes=1/0]"
label variable q4 "hh/grew mz -2018A [yes=1/0]"

label variable hhmaizeparish "hh parish location"
label variable hhphone "h: contact phone"
label variable hhconsent "h: respondent consent"
label variable hhmaizedistrict "h: district location"
label variable hhmaizesub "h: subcounty location"
label variable hhmaizeparish "h: parish location"
label variable hhmaizevillage "h: village location"

label variable hhq5 "h: respo name"
label variable hhq6 "h: have a cellphone"
rename hhphone hhphoneq7
rename hhconsent hhconsentq8
rename hhmaizedistrict hhmaizedistrictq9
rename hhmaizesub hhmaizesubq10
rename hhmaizeparish hhmaizeparishq11
rename hhmaizevillage hhmaizevillageq12

label variable hhq5 "hh/respo name"
label variable hhq6 "hh/have a cellphone"
label variable hhphoneq7 "hh/contact phone"
label variable hhconsentq8 "hh/respondent consent"
label variable hhconsentq8 "hh/respondent consent [yes=1/0]"

label variable hhmaizedistrictq9 "hh/district location"
label variable hhmaizesubq10 "hh/subcounty location"
label variable hhmaizeparishq11 "hh/parish location"
label variable hhmaizevillageq12 "hh/village location"

label variable hhmaizeq13 "hh/dist to n.tarmac (km)"
label variable hhmaizeq14 "hm/dist to n.marram"
label variable hhmaizeq13 "hm/dist to n.tarmac (km)"
label variable hhmaizeq15 "hm/dist to n.mill"

label variable hhmaizeq16 "hm/mz mills in villa"
label variable hhmaizeq17 "hm/dist to n.agroinput shop"
label variable hhmaizeq18 "hm/numb. of agroinpt shops "
label variable hhmaizeq19 "hm/dist. to n.neighbor (km)"
label variable hhmaizeq20 "hm/dist to n.trading center"
label variable hhmaizeq21 "hm/dist to LC1"

label variable hhmaizeq22 "hm/mob. cp network at home"
label variable hhmaizeq22a1 "hm/mob. nw: 1=good reception"
label variable hhmaizeq22a2 "hm/mob. nw: 2=poor reception"
label variable hhmaizeq22a3 "hm/mob. nw: 3=no nw coverage
label variable hhmaizeq22a98 "hm/mob. nw: 98=na
label variable hhmaizeq23 "hm/respo rship to hhhead (1=spouse; 2=child; 3=sib; 4=parent; 5=oth rel; 6=nonrel)"
label variable hhmaizeq23 "hm/respo rship to hhhead (1=spou; 2=child; 3=sib; 4=parent; 5=oth rel; 6=nonrel)"
label variable hhmaizeq24 "hm/respo age"
label variable hhmaizeq25 "hm/respo gender (1=m; 2=f)"
label variable hhmaizeq26 "hm/respo marital sta (1=marr; 2=W; 3=D; 4=S; 5=Single)"

sort hhmaizeq26
replace hhmaizeq26 = "1" in 4
replace hhmaizeq26 = "1" in 5
replace hhmaizeq26 = "1" in 6
replace hhmaizeq26 = "1" in 7
replace hhmaizeq26 = "1" in 8
replace hhmaizeq26 = "1" in 9
replace hhmaizeq26 = "1" in 10
replace hhmaizeq26 = "1" in 11
replace hhmaizeq26 = "1" in 12
replace hhmaizeq26 = "1" in 13
replace hhmaizeq26 = "1" in 14
replace hhmaizeq26 = "1" in 15
replace hhmaizeq26 = "1" in 16
replace hhmaizeq26 = "1" in 17
replace hhmaizeq26 = "1" in 18
replace hhmaizeq26 = "1" in 19
replace hhmaizeq26 = "1" in 20
replace hhmaizeq26 = "1" in 21
replace hhmaizeq26 = "1" in 22
replace hhmaizeq26 = "1" in 23
replace hhmaizeq26 = "1" in 24
replace hhmaizeq26 = "1" in 1320
replace hhmaizeq26 = "1" in 1321
replace hhmaizeq26 = "2" in 1322
replace hhmaizeq26 = "2" in 1323
replace hhmaizeq26 = "2" in 1324
replace hhmaizeq26 = "2" in 1325
replace hhmaizeq26 = "2" in 1326
replace hhmaizeq26 = "2" in 1327
replace hhmaizeq26 = "2" in 1328
replace hhmaizeq26 = "2" in 1329
replace hhmaizeq26 = "2" in 1330
replace hhmaizeq26 = "2" in 1331
replace hhmaizeq26 = "2" in 1332
replace hhmaizeq26 = "2" in 1333
replace hhmaizeq26 = "2" in 1334
replace hhmaizeq26 = "2" in 1335
replace hhmaizeq26 = "2" in 1336
replace hhmaizeq26 = "2" in 1337
replace hhmaizeq26 = "2" in 1338
replace hhmaizeq26 = "2" in 1339
replace hhmaizeq26 = "2" in 1340
replace hhmaizeq26 = "2" in 1341
replace hhmaizeq26 = "2" in 1342
replace hhmaizeq26 = "2" in 1343
replace hhmaizeq26 = "2" in 1344
replace hhmaizeq26 = "2" in 1345
replace hhmaizeq26 = "2" in 1346
replace hhmaizeq26 = "2" in 1347
replace hhmaizeq26 = "2" in 1348
replace hhmaizeq26 = "2" in 1421
replace hhmaizeq26 = "2" in 1422
replace hhmaizeq26 = "2" in 1423
replace hhmaizeq26 = "2" in 1424
replace hhmaizeq26 = "2" in 1425
replace hhmaizeq26 = "2" in 1426
replace hhmaizeq26 = "2" in 1427
replace hhmaizeq26 = "2" in 1428
replace hhmaizeq26 = "2" in 1429
replace hhmaizeq26 = "3" in 1430
replace hhmaizeq26 = "3" in 1431
replace hhmaizeq26 = "3" in 1432
replace hhmaizeq26 = "3" in 1433
replace hhmaizeq26 = "3" in 1434
replace hhmaizeq26 = "3" in 1435
replace hhmaizeq26 = "3" in 1436
replace hhmaizeq26 = "3" in 1437
replace hhmaizeq26 = "3" in 1438
replace hhmaizeq26 = "3" in 1439
replace hhmaizeq26 = "5" in 1493
replace hhmaizeq26 = "5" in 1494
replace hhmaizeq26 = "5" in 1495
replace hhmaizeq26 = "5" in 1496
replace hhmaizeq26 = "5" in 1497
replace hhmaizeq26 = "5" in 1498
replace hhmaizeq26 = "5" in 1499
replace hhmaizeq26 = "5" in 1500
replace hhmaizeq26 = "5" in 1501
encode hhmaizeq26, generate(married) label(1)
destring, replace
destring, replace
destring hhmaizeq26, replace
destring hhmaizeq26, replace force float

label variable hhmaizeq28 "hm/hh size"
label variable hhmaizeq28a "hm/ hh females"
label variable hhmaizeq28b "hh/hh members <15"
label variable hhmaizeq28b "hm/hh members <15"
label variable hhmaizeq28c "hm/hh f members <15"

label variable hhmaizeq29 "hm/hh key income source"
label variable hhmaizeother_q29 "hm/hh key income sorc specify 99"

label variable hhmaizeq27 "hm/respo educ level (1=nil; 2=some pri; 3=fin pr; 4=some sec; 5=fin sec; 6=higher; 99=oth)"
rename hhmaizeq27 hhmaizeq27x
egen hhmaizeq27=group( hhmaizeq27x )
gen hhmaizeq27y=hhmaizeq27, before( hhmaizeq28 )
label variable hhmaizeq27y "hm/respo educ level (1=nil; 2=some pri; 3=fin pr; 4=some sec; 5=fin sec; 6=higher; 99=oth)"
ed hhmaizeq27x hhmaizeq27y hhmaizeq27
sort hhmaizeq27x
drop hhmaizeq27
drop hhmaizeq27x
rename hhmaizeq27y hhmaizeq27
ed
rename hhmaizeq25 hhmaizeq25x
gen hhmaizeq25=., after( hhmaizeq25x )
label variable hhmaizeq25 "hm/respo gender (1=m; 2=f)"
egen hhmaizeq25xx =group( hhmaizeq25x )
replace hhmaizeq25= hhmaizeq25xx
ed hhmaizeq25x hhmaizeq25 hhmaizeq25xx
sort hhmaizeq25x
drop hhmaizeq25x hhmaizeq25xx
replace hhmaizeq25=0 if hhmaizeq25==1
replace hhmaizeq25=1 if hhmaizeq25==2
label variable hhmaizeq25 "hm/respo gender (1=m; 0=f)"
ed
rename q1 q1x
gen q1=., after( q1x )
egen q1xx =group( q1x )
replace q1= q1xx
drop q1x
drop q1xx
label variable q1 "hh head (yes=1)"
label variable q1 "hh head (yes=2)"
ed q1
ed
replace q1=0 if q1==1
replace q1=1 if q1==2
label variable q1 "hh head (yes=1)"
ed
rename q2 q2x
gen q2=., after(q2x)
egen q2xx =group( q2x )
replace q2= q2xx
sort q2x

ed q2x q2xx q2
replace q2=0 if q2==1
replace q2=1 if q2==2
replace q2=98 if q2==3
label variable q2 "hh. make decisions [yes=1, 0]"
drop q2x
drop q2xx
label variable q2 "hh/make decisions (yes=1)"
label variable q1 "hh/ head (yes=1)"
label variable q1 "hh/hh head (yes=1)"
ed
label variable q3 "hh/hv other knwledgeable person (yes=1)"
rename q3 q3x
gen q3=., after(q3x)
egen q3xx =group( q3x )
replace q3= q3xx
drop q3xx
sort q3x
ed q3x q3
tab1 q3x q3
label variable q3 "other knwledgeable person (yes=1)"
label variable q3 "h: other knwledgeable person (yes=1)"
label variable q3 "hh/hv other knwledgeable person (yes=1)"
drop q3x
label variable q3 "hh/hv other knwledgeable person (yes=1; 2=na)"
replace q3=98 if q3==2
label variable q3 "hh/hv other knwledgeable person (yes=1; 98=na)"
ed
rename q4 q4x
egen q4xx =group( q4x )
gen q4= q4xx, after( q4x )
label variable q4 "grew mz (2018A; yes=1)"
label variable q4 "hh/grew mz (2018A; yes=1)"
drop q4x q4xx
sort hhphoneq7
replace hhphoneq7 = "99999999999" in 1266
replace hhphoneq7 = "999999999" in 1266
rename hhq6 hhq6x
egen hhq6xx =group( hhq6x )
gen hhq6= hhq6xx, after( hhq6x )
label variable hhq6 "hh/have a cellphone"
sort hhq6x
replace hhq6=0 if hhq6==1
replace hhq6=1 if hhq6==2
drop hhq6x hhq6xx
order index
label variable index "respo number"
rename hhconsentq8 hhconsentq8x
egen hhconsentq8xx =group( hhconsentq8x )
gen hhconsentq8= hhconsentq8xx, after( hhconsentq8x)
label variable hhconsentq8 "hh/respondent consent"
label variable hhconsentq8 "hh/respondent consent [yes=1]"
drop hhconsentq8x hhconsentq8xx
sort hhmaizedistrictq9
rename hhmaizedistrictq9 hhmaizedistrictq9s
egen hhmaizedistrictq9xx =group( hhmaizedistrictq9s )
gen hhmaizedistrictq9= hhmaizedistrictq9xx, before( hhmaizedistrictq9s )
label variable hhmaizedistrictq9 "hh/district location"
label variable hhmaizedistrictq9s "hh/district location (string)"
sort hhmaizeq22
sort hhmaizeq29
rename hhmaizeq29 hhmaizeq29x
egen hhmaizeq29xx =group( hhmaizeq29x )
gen hhmaizeq29= hhmaizeq29xx,after( hhmaizeq29x )
label variable hhmaizeq29 "hm/hh key income source"
replace hhmaizeq29=99 if hhmaizeq29==1
replace hhmaizeq29=1 if hhmaizeq29==2
replace hhmaizeq29=2 if hhmaizeq29==3
replace hhmaizeq29=3 if hhmaizeq29==4
replace hhmaizeq29=4 if hhmaizeq29==5
replace hhmaizeq29=5 if hhmaizeq29==6
replace hhmaizeq29=6 if hhmaizeq29==7
replace hhmaizeq29=7 if hhmaizeq29==8
replace hhmaizeq29=8 if hhmaizeq29==9
replace hhmaizeq29=9 if hhmaizeq29==10
sort hhmaizeq29
drop hhmaizeq29x hhmaizeq29xx

ed
sort hhmaizeq29
replace hhmaizeq29 = 6 in 1505
replace hhmaizeq29 = 6 in 1507
replace hhmaizeq29 = 6 in 1512
replace hhmaizeq29 = 6 in 1513
replace hhmaizeq29 = 6 in 1515
replace hhmaizeq29 = 6 in 1517
replace hhmaizeq29 = 6 in 1519
replace hhmaizeq29 = 6 in 1520
replace hhmaizeq29 = 6 in 1521
replace hhmaizeq29 = 6 in 1523
replace hhmaizeq29 = 7 in 1524
replace hhmaizeq29 = 8 in 1521
replace hhmaizeq29 = 8 in 1517
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace

sort hhmaizeq29
replace hhmaizeq29 = 6 in 1520

save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace
label variable hhmaizeq30 "hm/rooms in house"
label variable hhmaizeq31a "hm/ hh lighting"
label variable hhmaizeq31a "hm/ hh lightg=electr"
label variable hhmaizeq31b "hm/ hh lightg=gen"
label variable hhmaizeq31c "hm/ hh lightg=solar"
label variable hhmaizeq31d "hm/ hh lightg=kero"
label variable hhmaizeq31e "hm/ hh lightg=candle"
label variable hhmaizeq31f "hm/ hh lightg=torch"
label variable hhmaizeq31g "hm/ hh lightg=biogas"
rename hhmaizeq3196 hhmaizeq31h
label variable hhmaizeq31h "hm/ hh lightg=oth"
label variable hhmaizeq31oth "hm/hh lightgother specified"

rename hhmaizeother_q31 hhmaizeq31oth
label variable hhmaizeq31oth "hm/hh lightg=other specified"

sort hhmaizeq31a
rename hhmaizeq31a hhmaizeq31ax
egen hhmaizeq31axx =group( hhmaizeq31ax )
gen hhmaizeq31a= hhmaizeq31axx, after( hhmaizeq31ax )
replace hhmaizeq31a=0 if hhmaizeq31a==1
replace hhmaizeq31a=1 if hhmaizeq31a==2
drop hhmaizeq31ax hhmaizeq31axx

do "C:\Users\Samuel\AppData\Local\Temp\STD02000000.tmp"
label variable hhmaizeq31a "hm/ hh lightg=electr"
label variable hhmaizeq31b "hm/ hh lightg=gen"
label variable hhmaizeq31c "hm/ hh lightg=solar"
label variable hhmaizeq31d "hm/ hh lightg=kero"
label variable hhmaizeq31e "hm/ hh lightg=candle"
label variable hhmaizeq31f "hm/ hh lightg=torch"
label variable hhmaizeq31g "hm/ hh lightg=biogas"
label variable hhmaizeq31h "hm/ hh lightg=oth"
sort hhmaizeq31oth

save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace
rename hhmaizeq32 hhmaizeq32x
egen hhmaizeq32xx =group( hhmaizeq32x)
sort hhmaizeq32x
gen hhmaizeq32= hhmaizeq32xx, after( hhmaizeq32x )
label variable hhmaizeq32x "hm/roofing material of hse"
label variable hhmaizeq32 "hm/roofing material of hse"
drop hhmaizeq32x hhmaizeq32xx
sort hhmaizeq33
label variable hhmaizeq33 "hm/started growg mz"

sort hhmaizeq34
label variable hhmaizeq34 "hm/also grow bns (yes=1)"
rename hhmaizeq34 hhmaizeq34x
egen hhmaizeq34xx =group( hhmaizeq34x )
gen hhmaizeq34= hhmaizeq34xx, after( hhmaizeq34x )
label variable hhmaizeq34 "hm/also grow bns (yes=1)"
label variable hhmaizeq25 "hm/respo gender [male=1/0]"
label variable hhmaizeq34 "hm/also grow bns [yes=1/0]"

sort hhmaizeq34x
replace hhmaizeq34=0 if hhmaizeq34==1
replace hhmaizeq34=1 if hhmaizeq34==2
drop hhmaizeq34x hhmaizeq34xx
label variable hhmaizeq35 "hm/maize/q35"
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace
label variable hhmaizeq35 "hm/started growg bns"
label variable hhmaizeq36 "hm/tot land size (ac)"
label variable hhmaizeq37 "hm/belong to a fg"
label variable hhmaizeq38 "hm/whn joined fg"
rename hhmaizeq37 hhmaizeq37x
egen hhmaizeq37xx =group( hhmaizeq37x )
gen hhmaizeq37= hhmaizeq37xx, after( hhmaizeq37x )
label variable hhmaizeq37 "hm/belong to a fg"
replace hhmaizeq37=0 if hhmaizeq37==1
replace hhmaizeq37=1 if hhmaizeq37==2
drop hhmaizeq37x hhmaizeq37xx
label variable hhmaizeq35 "hm/whn started growg bns"
label variable hhmaizeq33 "hm/whn started growg mz"
label variable hhmaizeq39a "hm/aware of var1=longe10H"
rename hhmaizeq3998 hhmaizeq39m
do "C:\Users\Samuel\AppData\Local\Temp\STD02000000.tmp"
sort hhmaizeq39a
sort hhmaizeq39a
rename hhmaizeq39a hhmaizeq39ax
egen hhmaizeq39axx =group( hhmaizeq39ax )
gen hhmaizeq39a= hhmaizeq39axx, after( hhmaizeq39ax )
replace hhmaizeq39a=0 if hhmaizeq39a==1
replace hhmaizeq39a=1 if hhmaizeq39a==2
drop hhmaizeq39ax hhmaizeq39axx
do "C:\Users\Samuel\AppData\Local\Temp\STD02000000.tmp"
do "C:\Users\Samuel\AppData\Local\Temp\STD02000000.tmp"
label variable hhmaizeq40 "hm/number of mz plots"

save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace
label variable hhmaizeplot_count "hm/plot"
label variable hhmaizeplot_count "hm/count of plots"
label variable hhmaizeplot_count "hm/plot count"
label variable hhmaizeplot1q41 "hm/mz plot1 name"
label variable hhmaizeplot1plot_num "hm/mz plot1 number"
label variable hhmaizeplot1plot_num "hm/mz plot1 size"
label variable hhmaizeplot1plot_num "hm/mz plot1 number"
label variable hhmaizeplot1q42 "hm/mz plot1 size"

label variable hhmaizeplot2plot_num "hm/mz plot2 number"
sort hhmaizeplot2q42
replace hhmaizeplot2q42 = "" in 559
replace hhmaizeplot2q42 = "" in 560
replace hhmaizeplot2q42 = "" in 561
replace hhmaizeplot2q42 = "" in 562
replace hhmaizeplot2q42 = "" in 563
replace hhmaizeplot2q42 = "" in 564
replace hhmaizeplot2q42 = "" in 565
replace hhmaizeplot2q42 = "" in 566
replace hhmaizeplot2q42 = "" in 567
replace hhmaizeplot2q42 = "" in 568
replace hhmaizeplot2q42 = "" in 569
replace hhmaizeplot2q42 = "" in 570
replace hhmaizeplot2q42 = "" in 571
replace hhmaizeplot2q42 = "" in 572
replace hhmaizeplot2q42 = "" in 573
replace hhmaizeplot2q42 = "" in 574
replace hhmaizeplot2q42 = "" in 575
drop in 1528/1578

destring hhmaizeplot2q42, replace force float
destring hhmaizeplot2plot_num, replace force float
replace hhmaizeplot2plot_num=98 if hhmaizeplot2plot_num==.

sort hhmaizeplot3plot_num
sort hhmaizeplot3plot_num
drop in 1528/1684
destring hhmaizeplot3plot_num, replace force float
destring hhmaizeplot3q42, replace force float
replace hhmaizeplot3plot_num=98 if hhmaizeplot3plot_num==.


sort hhmaizeplot4plot_num
sort hhmaizeplot4plot_num
destring hhmaizeplot4plot_num, replace force float
destring hhmaizeplot4q42, replace force float
replace hhmaizeplot4plot_num=98 if hhmaizeplot4plot_num==.

sort hhmaizeplot5plot_num
destring hhmaizeplot5plot_num, replace force float
destring hhmaizeplot5q42, replace force float
replace hhmaizeplot5plot_num=98 if hhmaizeplot5plot_num==.

sort hhmaizeplot6plot_num
destring hhmaizeplot6plot_num, replace force float
destring hhmaizeplot6q42, replace force float
replace hhmaizeplot6plot_num=98 if hhmaizeplot6plot_num==.
sort hhmaizeplot_calc1

label variable hhmaizeplot_calc1 "hm/maize plot_calc1"
label variable hhmaizeplot_calc2 "hm/maize plot_calc2"

sort hhmaizeplot_select_name
sort hhmaizeplot_select_name

label variable hhmaizeplot_select_name "hh/selected maize plot name"
label variable hhmaizeplot_select "hm/maize/plot_select"
label variable hhmaizeplot_select_name "hm/selected maize plot name"
label variable hhmaizeplot_select_area "hm/selected mz plot_area"
label variable hhmaizeplot_select_name "hm/selected mz plot_name"

replace hhmaizeplot_select_area = "98" in 1485
replace hhmaizeplot_select_area = "98" in 1486
replace hhmaizeplot_select_area = "98" in 1487
replace hhmaizeplot_select_area = "98" in 1488
replace hhmaizeplot_select_area = "98" in 1489
replace hhmaizeplot_select_area = "98" in 1490
destring hhmaizeplot_select_area, replace force float

label variable hhmaizemaizenq43 "hm/whn startd cultvtg plot"
label variable hhmaizeorder "hm/maize/order"
destring hhmaizeplot_select_area, replace ignore(`"/"') force float
destring hhmaizemaizenq43, replace ignore(`"/"') force float
destring hhmaizeq33 , replace ignore(`"/"') force float
destring hhmaizeq35, replace ignore(`"/"') force float
destring hhmaizeq38, replace ignore(`"/"') force float
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace

*xxx xx
rename hhmaizeq31a hhmaizeq31ax
egen hhmaizeq31axx =group( hhmaizeq31ax )
gen hhmaizeq31a= hhmaizeq31axx, after( hhmaizeq31ax )
replace hhmaizeq31a=0 if hhmaizeq31a==1
replace hhmaizeq31a=1 if hhmaizeq31a==2
drop hhmaizeq31ax hhmaizeq31axx

rename hhmaizeq31b hhmaizeq31bx
egen hhmaizeq31bxx =group( hhmaizeq31bx )
gen hhmaizeq31b= hhmaizeq31bxx, after( hhmaizeq31bx )
replace hhmaizeq31b=0 if hhmaizeq31b==1
replace hhmaizeq31b=1 if hhmaizeq31b==2
drop hhmaizeq31bx hhmaizeq31bxx

rename hhmaizeq31c hhmaizeq31cx
egen hhmaizeq31cxx =group( hhmaizeq31cx )
gen hhmaizeq31c= hhmaizeq31cxx, after( hhmaizeq31cx )
replace hhmaizeq31c=0 if hhmaizeq31c==1
replace hhmaizeq31c=1 if hhmaizeq31c==2
drop hhmaizeq31cx hhmaizeq31cxx

rename hhmaizeq31d hhmaizeq31dx
egen hhmaizeq31dxx =group( hhmaizeq31dx )
gen hhmaizeq31d= hhmaizeq31dxx, after( hhmaizeq31dx )
replace hhmaizeq31d=0 if hhmaizeq31d==1
replace hhmaizeq31d=1 if hhmaizeq31d==2
drop hhmaizeq31dx hhmaizeq31dxx

rename hhmaizeq31e hhmaizeq31ex
egen hhmaizeq31exx =group( hhmaizeq31ex )
gen hhmaizeq31e= hhmaizeq31exx, after( hhmaizeq31ex )
replace hhmaizeq31e=0 if hhmaizeq31e==1
replace hhmaizeq31e=1 if hhmaizeq31e==2
drop hhmaizeq31ex hhmaizeq31exx

rename hhmaizeq31f hhmaizeq31fx
egen hhmaizeq31fxx =group( hhmaizeq31fx )
gen hhmaizeq31f= hhmaizeq31fxx, after( hhmaizeq31fx )
replace hhmaizeq31f=0 if hhmaizeq31f==1
replace hhmaizeq31f=1 if hhmaizeq31f==2
drop hhmaizeq31fx hhmaizeq31fxx

rename hhmaizeq31g hhmaizeq31gx
egen hhmaizeq31gxx =group( hhmaizeq31gx )
gen hhmaizeq31g= hhmaizeq31gxx, after( hhmaizeq31gx )
replace hhmaizeq31g=0 if hhmaizeq31g==1
replace hhmaizeq31g=1 if hhmaizeq31g==2
drop hhmaizeq31gx hhmaizeq31gxx

rename hhmaizeq31h hhmaizeq31hx
egen hhmaizeq31hxx =group( hhmaizeq31hx )
gen hhmaizeq31h= hhmaizeq31hxx, after( hhmaizeq31hx )
replace hhmaizeq31h=0 if hhmaizeq31h==1
replace hhmaizeq31h=1 if hhmaizeq31h==2
drop hhmaizeq31hx hhmaizeq31hxx

*xxxx variety awareness
label variable hhmaizeq39a "hm/aware of var1=longe10H"
label variable hhmaizeq39b "hm/aware of var2=longe7H"
label variable hhmaizeq39c "hm/aware of var3=longe7R"
label variable hhmaizeq39d "hm/aware of var4=bazooka"
label variable hhmaizeq39e "hm/aware of var5=longe6H"
label variable hhmaizeq39f "hm/aware of var6=longe5"
label variable hhmaizeq39g "hm/aware of var7=longe4"
label variable hhmaizeq39h "hm/aware of var8=panner"
label variable hhmaizeq39i "hm/aware of var9=wama"
label variable hhmaizeq39j "hm/aware of var10=kh series"
label variable hhmaizeq39k "hm/aware of var11=land races"
label variable hhmaizeq39l "hm/aware of var12=oth"
label variable hhmaizeq39m "hm/aware of var13=don't knw"

sort hhmaizeq39a
rename hhmaizeq39a hhmaizeq39ax
egen hhmaizeq39axx =group( hhmaizeq39ax )
gen hhmaizeq39a= hhmaizeq39axx, after( hhmaizeq39ax )
replace hhmaizeq39a=0 if hhmaizeq39a==1
replace hhmaizeq39a=1 if hhmaizeq39a==2
drop hhmaizeq39ax hhmaizeq39axx

sort hhmaizeq39b
rename hhmaizeq39b hhmaizeq39bx
egen hhmaizeq39bxx =group( hhmaizeq39bx )
gen hhmaizeq39b= hhmaizeq39bxx, after( hhmaizeq39bx )
replace hhmaizeq39b=0 if hhmaizeq39b==1
replace hhmaizeq39b=1 if hhmaizeq39b==2
drop hhmaizeq39bx hhmaizeq39bxx

sort hhmaizeq39c
rename hhmaizeq39c hhmaizeq39cx
egen hhmaizeq39cxx =group( hhmaizeq39cx )
gen hhmaizeq39c= hhmaizeq39cxx, after( hhmaizeq39cx )
replace hhmaizeq39c=0 if hhmaizeq39c==1
replace hhmaizeq39c=1 if hhmaizeq39c==2
drop hhmaizeq39cx hhmaizeq39cxx

sort hhmaizeq39d
rename hhmaizeq39d hhmaizeq39dx
egen hhmaizeq39dxx =group( hhmaizeq39dx )
gen hhmaizeq39d= hhmaizeq39dxx, after( hhmaizeq39dx )
replace hhmaizeq39d=0 if hhmaizeq39d==1
replace hhmaizeq39d=1 if hhmaizeq39d==2
drop hhmaizeq39dx hhmaizeq39dxx

sort hhmaizeq39e
rename hhmaizeq39e hhmaizeq39ex
egen hhmaizeq39exx =group( hhmaizeq39ex )
gen hhmaizeq39e= hhmaizeq39exx, after( hhmaizeq39ex )
replace hhmaizeq39e=0 if hhmaizeq39e==1
replace hhmaizeq39e=1 if hhmaizeq39e==2
drop hhmaizeq39ex hhmaizeq39exx

sort hhmaizeq39f
rename hhmaizeq39f hhmaizeq39fx
egen hhmaizeq39fxx =group( hhmaizeq39fx )
gen hhmaizeq39f= hhmaizeq39fxx, after( hhmaizeq39fx )
replace hhmaizeq39f=0 if hhmaizeq39f==1
replace hhmaizeq39f=1 if hhmaizeq39f==2
drop hhmaizeq39fx hhmaizeq39fxx

sort hhmaizeq39g
rename hhmaizeq39g hhmaizeq39gx
egen hhmaizeq39gxx =group( hhmaizeq39gx )
gen hhmaizeq39g= hhmaizeq39gxx, after( hhmaizeq39gx )
replace hhmaizeq39g=0 if hhmaizeq39g==1
replace hhmaizeq39g=1 if hhmaizeq39g==2
drop hhmaizeq39gx hhmaizeq39gxx

sort hhmaizeq39h
rename hhmaizeq39h hhmaizeq39hx
egen hhmaizeq39hxx =group( hhmaizeq39hx )
gen hhmaizeq39h= hhmaizeq39hxx, after( hhmaizeq39hx )
replace hhmaizeq39h=0 if hhmaizeq39h==1
replace hhmaizeq39h=1 if hhmaizeq39h==2
drop hhmaizeq39hx hhmaizeq39hxx

sort hhmaizeq39i
rename hhmaizeq39i hhmaizeq39ix
egen hhmaizeq39ixx =group( hhmaizeq39ix )
gen hhmaizeq39i= hhmaizeq39ixx, after( hhmaizeq39ix )
replace hhmaizeq39i=0 if hhmaizeq39i==1
replace hhmaizeq39i=1 if hhmaizeq39i==2
drop hhmaizeq39ix hhmaizeq39ixx

sort hhmaizeq39j
rename hhmaizeq39j hhmaizeq39jx
egen hhmaizeq39jxx =group( hhmaizeq39jx )
gen hhmaizeq39j= hhmaizeq39jxx, after( hhmaizeq39jx )
replace hhmaizeq39j=0 if hhmaizeq39j==1
replace hhmaizeq39j=1 if hhmaizeq39j==2
drop hhmaizeq39jx hhmaizeq39jxx

sort hhmaizeq39k
rename hhmaizeq39k hhmaizeq39kx
egen hhmaizeq39kxx =group( hhmaizeq39kx )
gen hhmaizeq39k= hhmaizeq39kxx, after( hhmaizeq39kx )
replace hhmaizeq39k=0 if hhmaizeq39k==1
replace hhmaizeq39k=1 if hhmaizeq39k==2
drop hhmaizeq39kx hhmaizeq39kxx

sort hhmaizeq39l
rename hhmaizeq39l hhmaizeq39lx
egen hhmaizeq39lxx =group( hhmaizeq39lx )
gen hhmaizeq39l= hhmaizeq39lxx, after( hhmaizeq39lx )
replace hhmaizeq39l=0 if hhmaizeq39l==1
replace hhmaizeq39l=1 if hhmaizeq39l==2
drop hhmaizeq39lx hhmaizeq39lxx

sort hhmaizeq39m
rename hhmaizeq39m hhmaizeq39mx
egen hhmaizeq39mxx =group( hhmaizeq39mx )
gen hhmaizeq39m= hhmaizeq39mxx, after( hhmaizeq39mx )
replace hhmaizeq39m=0 if hhmaizeq39m==1
replace hhmaizeq39m=1 if hhmaizeq39m==2
drop hhmaizeq39mx hhmaizeq39mxx

**xxx maize plots
label variable hhmaizeplot1q41 "hm/mz plot1 name"
label variable hhmaizeplot1plot_num "hm/mz plot1 number"
label variable hhmaizeplot1q42 "hm/mz plot1 size(ac)"

label variable hhmaizeplot2q41 "hm/mz plot2 name"
label variable hhmaizeplot2plot_num "hm/mz plot2 number"
label variable hhmaizeplot2q42 "hm/mz plot2 size(ac)"

label variable hhmaizeplot3q41 "hm/mz plot3 name"
label variable hhmaizeplot3plot_num "hm/mz plot3 number"
label variable hhmaizeplot3q42 "hm/mz plot3 size(ac)"

label variable hhmaizeplot4q41 "hm/mz plot4 name"
label variable hhmaizeplot4plot_num "hm/mz plot4 number"
label variable hhmaizeplot4q42 "hm/mz plot4 size(ac)"


label variable hhmaizeplot5q41 "hm/mz plot5 name"
label variable hhmaizeplot5plot_num "hm/mz plot5 number"
label variable hhmaizeplot5q42 "hm/mz plot5 size(ac)"

label variable hhmaizeplot6q41 "hm/mz plot6 name"
label variable hhmaizeplot6plot_num "hm/mz plot6 number"
label variable hhmaizeplot6q42 "hm/mz plot6 size(ac)"

*xxx destringing dates
destring hhmaizemaizenq43, replace ignore(`"/"') force float
destring hhmaizemaizenq43, replace ignore(`"/"') force float
destring hhmaizemaizenq43, replace ignore(`"/"') force float
destring hhmaizebeansbeanq71, replace ignore(`"/"') force float
destring hhmaizebeansbeanq72, replace ignore(`"/"') force float

label variable hhmaizemaizenq44 "hm/dist (from hh) to mz plot"
label variable hhmaizemaizenq44 "hm/dist (in km from hh) to mz plot"
label variable hhmaizemaizenq44 "hm/dist (in km from hh) to mz plot"
label variable hhmaizemaizenq45 "hm/mz plot soil fertility"
label variable hhmaizemaizenq45 "hm/rating of mz plot soil fertility"
label variable hhmaizemaizenq46 "hm/intercropped in 2018A"

sort hhmaizemaizenq45
replace hhmaizemaizenq45 = "98" in 1524
replace hhmaizemaizenq45 = "98" in 1525
replace hhmaizemaizenq45 = "98" in 1526
replace hhmaizemaizenq45 = "98" in 1527

destring hhmaizemaizenq45, replace force float

*xxx q46
rename hhmaizemaizenq46 hhmaizemaizenq46x
egen hhmaizemaizenq46xx =group( hhmaizemaizenq46x)
gen hhmaizemaizenq46=hhmaizemaizenq46xx, after(hhmaizemaizenq46x)
label variable hhmaizemaizenq46 "hm/intercropped in 2018A"
label variable hhmaizemaizenq46 "hm/intercropped in 2018A (yes=1)"

sort hhmaizemaizenq46
replace hhmaizemaizenq46 = 98 in 1
replace hhmaizemaizenq46 = 98 in 2
replace hhmaizemaizenq46=0 if hhmaizemaizenq46==2
replace hhmaizemaizenq46=1 if hhmaizemaizenq46==3
drop hhmaizemaizenq46x hhmaizemaizenq46xx

label variable hhmaizemaizenq46a1 "hh/maize intercrop1=beans"
label variable hhmaizemaizenq46a96 "hh/maize/maizen/q46a7"
rename hhmaizemaizenq46a96 hhmaizemaizenq46a7
sort hhmaizemaizenq46a1
rename hhmaizemaizenq46a1 hhmaizemaizenq46a1x
egen hhmaizemaizenq46a1xx= group( hhmaizemaizenq46a1x )
gen hhmaizemaizenq46a1= hhmaizemaizenq46a1xx, after( hhmaizemaizenq46a1x )
replace hhmaizemaizenq46a1=0 if hhmaizemaizenq46a1==1
replace hhmaizemaizenq46a1=1 if hhmaizemaizenq46a1==2
replace hhmaizemaizenq46a1=98 if hhmaizemaizenq46a1==3
drop hhmaizemaizenq46a1x hhmaizemaizenq46a1xx

*xxx intercrops
label variable hhmaizemaizenq46a1 "hm/maize intercrop1=beans"
label variable hhmaizemaizenq46a2 "hm/maize intercrop2=soy"
label variable hhmaizemaizenq46a3 "hm/maize intercrop3=gnuts"
label variable hhmaizemaizenq46a4 "hm/maize intercrop4=cassav"
label variable hhmaizemaizenq46a5 "hm/maize intercrop5=millet"
label variable hhmaizemaizenq46a6 "hm/maize intercrop6=sorghm"
label variable hhmaizemaizenq46a7 "hm/maize intercrop7=oth"

sort hhmaizemaizenq46a1
rename hhmaizemaizenq46a1 hhmaizemaizenq46a1x
egen hhmaizemaizenq46a1xx= group( hhmaizemaizenq46a1x )
gen hhmaizemaizenq46a1= hhmaizemaizenq46a1xx, after( hhmaizemaizenq46a1x )
replace hhmaizemaizenq46a1=0 if hhmaizemaizenq46a1==1
replace hhmaizemaizenq46a1=1 if hhmaizemaizenq46a1==2
replace hhmaizemaizenq46a1=98 if hhmaizemaizenq46a1==3
drop hhmaizemaizenq46a1x hhmaizemaizenq46a1xx

sort hhmaizemaizenq46a2
rename hhmaizemaizenq46a2 hhmaizemaizenq46a2x
egen hhmaizemaizenq46a2xx= group( hhmaizemaizenq46a2x )
gen hhmaizemaizenq46a2= hhmaizemaizenq46a2xx, after( hhmaizemaizenq46a2x )
replace hhmaizemaizenq46a2=0 if hhmaizemaizenq46a2==1
replace hhmaizemaizenq46a2=1 if hhmaizemaizenq46a2==2
replace hhmaizemaizenq46a2=98 if hhmaizemaizenq46a2==3
drop hhmaizemaizenq46a2x hhmaizemaizenq46a2xx

sort hhmaizemaizenq46a3
rename hhmaizemaizenq46a3 hhmaizemaizenq46a3x
egen hhmaizemaizenq46a3xx= group( hhmaizemaizenq46a3x )
gen hhmaizemaizenq46a3= hhmaizemaizenq46a3xx, after( hhmaizemaizenq46a3x )
replace hhmaizemaizenq46a3=0 if hhmaizemaizenq46a3==1
replace hhmaizemaizenq46a3=1 if hhmaizemaizenq46a3==2
replace hhmaizemaizenq46a3=98 if hhmaizemaizenq46a3==3
drop hhmaizemaizenq46a3x hhmaizemaizenq46a3xx

sort hhmaizemaizenq46a4
rename hhmaizemaizenq46a4 hhmaizemaizenq46a4x
egen hhmaizemaizenq46a4xx= group( hhmaizemaizenq46a4x )
gen hhmaizemaizenq46a4= hhmaizemaizenq46a4xx, after( hhmaizemaizenq46a4x )
replace hhmaizemaizenq46a4=0 if hhmaizemaizenq46a4==1
replace hhmaizemaizenq46a4=1 if hhmaizemaizenq46a4==2
replace hhmaizemaizenq46a4=98 if hhmaizemaizenq46a4==3
drop hhmaizemaizenq46a4x hhmaizemaizenq46a4xx

sort hhmaizemaizenq46a5
rename hhmaizemaizenq46a5 hhmaizemaizenq46a5x
egen hhmaizemaizenq46a5xx= group( hhmaizemaizenq46a5x )
gen hhmaizemaizenq46a5= hhmaizemaizenq46a5xx, after( hhmaizemaizenq46a5x )
replace hhmaizemaizenq46a5=0 if hhmaizemaizenq46a5==1
replace hhmaizemaizenq46a5=1 if hhmaizemaizenq46a5==2
replace hhmaizemaizenq46a5=98 if hhmaizemaizenq46a5==3
drop hhmaizemaizenq46a5x hhmaizemaizenq46a5xx

sort hhmaizemaizenq46a6
rename hhmaizemaizenq46a6 hhmaizemaizenq46a6x
egen hhmaizemaizenq46a6xx= group( hhmaizemaizenq46a6x )
gen hhmaizemaizenq46a6= hhmaizemaizenq46a6xx, after( hhmaizemaizenq46a6x )
replace hhmaizemaizenq46a6=0 if hhmaizemaizenq46a6==1
replace hhmaizemaizenq46a6=1 if hhmaizemaizenq46a6==2
replace hhmaizemaizenq46a6=98 if hhmaizemaizenq46a6==3
drop hhmaizemaizenq46a6x hhmaizemaizenq46a6xx

sort hhmaizemaizenq46a7
rename hhmaizemaizenq46a7 hhmaizemaizenq46a7x
egen hhmaizemaizenq46a7xx= group( hhmaizemaizenq46a7x )
gen hhmaizemaizenq46a7= hhmaizemaizenq46a7xx, after( hhmaizemaizenq46a7x )
replace hhmaizemaizenq46a7=0 if hhmaizemaizenq46a7==1
replace hhmaizemaizenq46a7=1 if hhmaizemaizenq46a7==2
replace hhmaizemaizenq46a7=98 if hhmaizemaizenq46a7==3
drop hhmaizemaizenq46a7x hhmaizemaizenq46a7xx


label variable hhmaizemaizenq46b "hm/percent of mz in intercrop"
replace hhmaizemaizenq46b = "" in 1080
replace hhmaizemaizenq46b = "" in 1081
replace hhmaizemaizenq46b = "" in 1082
replace hhmaizemaizenq46b = "" in 1083
replace hhmaizemaizenq46b = "" in 1084
replace hhmaizemaizenq46b = "" in 1085
replace hhmaizemaizenq46b = "" in 1086
replace hhmaizemaizenq46b = "" in 1087
replace hhmaizemaizenq46b = "" in 1088
replace hhmaizemaizenq46b = "" in 1089
replace hhmaizemaizenq46b = "" in 1090
replace hhmaizemaizenq46b = "" in 1091
replace hhmaizemaizenq46b = "" in 1092
replace hhmaizemaizenq46b = "" in 1093
replace hhmaizemaizenq46b = "" in 1095
replace hhmaizemaizenq46b = "" in 1094
replace hhmaizemaizenq46b = "" in 1096
replace hhmaizemaizenq46b = "" in 1097
replace hhmaizemaizenq46b = "" in 1098
replace hhmaizemaizenq46b = "" in 1099
replace hhmaizemaizenq46b = "" in 1100
replace hhmaizemaizenq46b = "" in 1101
replace hhmaizemaizenq46b = "" in 1102
replace hhmaizemaizenq46b = "" in 1103
replace hhmaizemaizenq46b = "" in 1104
destring hhmaizemaizenq46b, replace force float
replace hhmaizemaizenq46b=100 if hhmaizemaizenq46==0

label variable hhmaizemaizenq47 "hm/grew maize on plot in 2017"
label variable hhmaizemaizenq47 "hm/grew maize on plot in 2017 (yes=1)"
rename hhmaizemaizenq47 hhmaizemaizenq47x
egen hhmaizemaizenq47xx=group( hhmaizemaizenq47x )
gen hhmaizemaizenq47= hhmaizemaizenq47xx, after( hhmaizemaizenq47x )
label variable hhmaizemaizenq47 "hm/grew maize on plot in 2017 (yes=1)"
sort hhmaizemaizenq47
replace hhmaizemaizenq47=98 if hhmaizemaizenq47==1
replace hhmaizemaizenq47=0 if hhmaizemaizenq47==2
replace hhmaizemaizenq47=1 if hhmaizemaizenq47==3
tab hhmaizemaizenq47
drop hhmaizemaizenq47x hhmaizemaizenq47xx
tab hhmaizemaizenq47

label variable hhmaizemaizenq48 "hm/maize var planted on plot"
rename hhmaizemaizenq48 hhmaizemaizenq48x
egen hhmaizemaizenq48xx=group( hhmaizemaizenq48x )
gen hhmaizemaizenq48= hhmaizemaizenq48xx, after( hhmaizemaizenq48x )
label variable hhmaizemaizenq48 "hm/maize var planted on plot"
sort hhmaizemaizenq48
drop hhmaizemaizenq48x hhmaizemaizenq48xx
label variable hhmaizemaizenq48 "hm/maize var planted on plot xx"

*xxx source of maize seed 
label variable hhmaizemaizenq49 "hm/source of maize seed"
rename hhmaizemaizenother_q49 hhmaizemaizenq49_oth
label variable hhmaizemaizenq49_oth "hm/other source of maize seed"
rename hhmaizemaizenq49 hhmaizemaizenq49x
egen hhmaizemaizenq49xx=group( hhmaizemaizenq49x )
gen hhmaizemaizenq49= hhmaizemaizenq49xx, after( hhmaizemaizenq49x )
sort hhmaizemaizenq49
replace hhmaizemaizenq49=98 if hhmaizemaizenq49==1
drop hhmaizemaizenq49x hhmaizemaizenq49xx
label variable hhmaizemaizenq49 "hm/source of maize seed"

replace hhmaizemaizenq49=98 if hhmaizemaizenq49==1
replace hhmaizemaizenq49=1 if hhmaizemaizenq49==2
replace hhmaizemaizenq49=2 if hhmaizemaizenq49==3
replace hhmaizemaizenq49=3 if hhmaizemaizenq49==4
replace hhmaizemaizenq49=4 if hhmaizemaizenq49==5
replace hhmaizemaizenq49=5 if hhmaizemaizenq49==6
replace hhmaizemaizenq49=6 if hhmaizemaizenq49==7
replace hhmaizemaizenq49=7 if hhmaizemaizenq49==8
replace hhmaizemaizenq49=8 if hhmaizemaizenq49==9
replace hhmaizemaizenq49=9 if hhmaizemaizenq49==10
replace hhmaizemaizenq49=10 if hhmaizemaizenq49==11
replace hhmaizemaizenq49=11 if hhmaizemaizenq49==12
replace hhmaizemaizenq49=12 if hhmaizemaizenq49==13

save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace
sort hhmaizemaizenq50
replace hhmaizemaizenq50 = "g" in 5
replace hhmaizemaizenq50 = "g" in 6
replace hhmaizemaizenq50 = "g" in 7
replace hhmaizemaizenq50 = "g" in 8
replace hhmaizemaizenq50 = "g" in 9
replace hhmaizemaizenq50 = "g" in 10
replace hhmaizemaizenq50 = "g" in 11
replace hhmaizemaizenq50 = "g" in 13
replace hhmaizemaizenq50 = "g" in 14
replace hhmaizemaizenq50 = "g" in 12
replace hhmaizemaizenq50 = "g" in 15
replace hhmaizemaizenq50 = "g" in 16
replace hhmaizemaizenq50 = "g" in 17
replace hhmaizemaizenq50 = "g" in 18
replace hhmaizemaizenq50 = "g" in 19
replace hhmaizemaizenq50 = "g" in 20
replace hhmaizemaizenq50 = "g" in 21
replace hhmaizemaizenq50 = "g" in 22
replace hhmaizemaizenq50 = "g" in 23
replace hhmaizemaizenq50 = "g98" in 25
replace hhmaizemaizenq50 = "g" in 24
replace hhmaizemaizenq50 = "g" in 25
replace hhmaizemaizenq50 = "g" in 26

rename hhmaizemaizenq50 hhmaizemaizenq50x
egen hhmaizemaizenq50xx=group( hhmaizemaizenq50x )
gen hhmaizemaizenq50= hhmaizemaizenq50xx, after( hhmaizemaizenq50x)
label variable hhmaizemaizenq50x "hm/freq of seed recycling"
replace hhmaizemaizenq50= hhmaizemaizenq50xx
sort hhmaizemaizenq50
replace hhmaizemaizenq50=98 if hhmaizemaizenq50==8
replace hhmaizemaizenq50=97 if hhmaizemaizenq50==7
label variable hhmaizemaizenq50 "hm/freq of seed recycling"
drop hhmaizemaizenq50x hhmaizemaizenq50xx

label variable hhmaizemaizenq51a "hm/seed rating on yield (1-5)"
label variable hhmaizemaizenother_q51 "hh/maize/maizen/q51"
rename hhmaizemaizenother_q51 hhmaizemaizenq51_oth
rename hhmaizemaizenq51_oth hhmaizemaizenq51k_oth
rename hhmaizemaizenq51k_oth hhmaizemaizenq51k

replace hhmaizemaizenq51k = "x" in 1216
replace hhmaizemaizenq51k = "x" in 1217
replace hhmaizemaizenq51k = "x" in 1218
replace hhmaizemaizenq51k = "x" in 1219
replace hhmaizemaizenq51k = "x" in 1220


*xxx seed rating on attributes q51
label variable hhmaizemaizenq51a "hm/seed rating on yield (1-5)"
label variable hhmaizemaizenq51b "hm/seed rating on drought tol (1-5)"
label variable hhmaizemaizenq51c "hm/seed rating on pest&dse tol (1-5)"
label variable hhmaizemaizenq51d "hm/seed rating on early maturity (1-5)"
label variable hhmaizemaizenq51e "hm/seed rating on high price/mktg/dd (1-5)"
label variable hhmaizemaizenq51f "hm/seed rating on taste/nutritn (1-5)"
label variable hhmaizemaizenq51g "hm/seed rating on low price (1-5)"
label variable hhmaizemaizenq51h "hm/seed rating on availability (1-5)"
label variable hhmaizemaizenq51i "hm/seed rating on germintn rate (1-5)"
label variable hhmaizemaizenq51j "hm/seed rating on experience (1-5)"
label variable hhmaizemaizenq51k "hm/seed rating on other reason (1-5)"

*xxx reason not satisfied with seed var planted q52
label variable hhmaizemaizenq52 "hm/satisfied with mz seed qty"
sort hhmaizemaizenq52
sort hhmaizemaizenq51k hhmaizemaizenq51j

sort hhmaizemaizenq52
rename hhmaizemaizenq52 hhmaizemaizenq52x
egen hhmaizemaizenq52xx=group( hhmaizemaizenq52x )
gen hhmaizemaizenq52= hhmaizemaizenq52xx, after ( hhmaizemaizenq52x)
label variable hhmaizemaizenq52 "hm/satisfied with mz seed qty"
label variable hhmaizemaizenq52 "hm/satisfied with mz seed qty [yes=1/0]"
replace hhmaizemaizenq52=0 if hhmaizemaizenq52==1
replace hhmaizemaizenq52=1 if hhmaizemaizenq52==2
drop hhmaizemaizenq52x hhmaizemaizenq52xx

sort hhmaizemaizenq52a
rename hhmaizemaizenq52a hhmaizemaizenq52ax
egen hhmaizemaizenq52axx=group( hhmaizemaizenq52ax)
gen hhmaizemaizenq52a= hhmaizemaizenq52axx, after ( hhmaizemaizenq52ax)
label variable hhmaizemaizenq52a "hm/satisfied with mz seed qty [yes=1/0]"
*label variable hhmaizemaizenq52a "hm/was seed fake? [yes=1/0]"

replace hhmaizemaizenq52a=98 if hhmaizemaizenq52a==1
replace hhmaizemaizenq52a=0 if hhmaizemaizenq52a==2
replace hhmaizemaizenq52a=1 if hhmaizemaizenq52a==3
replace hhmaizemaizenq52a=99 if hhmaizemaizenq52a==4
label variable hhmaizemaizenq52a "hm/satisfied with mz seed qty [yes=1/0] (99=na)"
drop hhmaizemaizenq52ax hhmaizemaizenq52axx

rename hhmaizemaizenq52b1 hhmaizemaizenq52b1x
rename hhmaizemaizenq52b2 hhmaizemaizenq52b2x
rename hhmaizemaizenq52b3 hhmaizemaizenq52b3x
rename hhmaizemaizenq52b4 hhmaizemaizenq52b4x
rename hhmaizemaizenq52b5 hhmaizemaizenq52b5x
rename hhmaizemaizenq52b6 hhmaizemaizenq52b6x
rename hhmaizemaizenq52b7 hhmaizemaizenq52b7x
rename hhmaizemaizenq52b8 hhmaizemaizenq52b8x
rename hhmaizemaizenq52b9 hhmaizemaizenq52b9x
rename hhmaizemaizenq52b10 hhmaizemaizenq52b10x
rename hhmaizemaizenq52b11 hhmaizemaizenq52b11x

*xxx making reasons dummies (for dissatifaction on seed)
egen hhmaizemaizenq52b1xx=group( hhmaizemaizenq52b1x )
egen hhmaizemaizenq52b2xx=group( hhmaizemaizenq52b2x )
egen hhmaizemaizenq52b3xx=group( hhmaizemaizenq52b3x )
egen hhmaizemaizenq52b4xx=group( hhmaizemaizenq52b4x )
egen hhmaizemaizenq52b5xx=group( hhmaizemaizenq52b5x )
egen hhmaizemaizenq52b6xx=group( hhmaizemaizenq52b6x )
egen hhmaizemaizenq52b7xx=group( hhmaizemaizenq52b7x )
egen hhmaizemaizenq52b8xx=group( hhmaizemaizenq52b8x )
egen hhmaizemaizenq52b9xx=group( hhmaizemaizenq52b9x )
egen hhmaizemaizenq52b10xx=group( hhmaizemaizenq52b10x )
egen hhmaizemaizenq52b11xx=group( hhmaizemaizenq52b11x )

gen hhmaizemaizenq52b1= hhmaizemaizenq52b1xx, after(hhmaizemaizenq52b1x)
gen hhmaizemaizenq52b2= hhmaizemaizenq52b2xx, after(hhmaizemaizenq52b2x)
gen hhmaizemaizenq52b3= hhmaizemaizenq52b3xx, after(hhmaizemaizenq52b3x)
gen hhmaizemaizenq52b4= hhmaizemaizenq52b4xx, after(hhmaizemaizenq52b4x)
gen hhmaizemaizenq52b5= hhmaizemaizenq52b5xx, after(hhmaizemaizenq52b5x)
gen hhmaizemaizenq52b6= hhmaizemaizenq52b6xx, after(hhmaizemaizenq52b6x)
gen hhmaizemaizenq52b7= hhmaizemaizenq52b7xx, after(hhmaizemaizenq52b7x)
gen hhmaizemaizenq52b8= hhmaizemaizenq52b8xx, after(hhmaizemaizenq52b8x)
gen hhmaizemaizenq52b9= hhmaizemaizenq52b9xx, after(hhmaizemaizenq52b9x)
gen hhmaizemaizenq52b10= hhmaizemaizenq52b10xx, after(hhmaizemaizenq52b10x)
gen hhmaizemaizenq52b11= hhmaizemaizenq52b11xx, after(hhmaizemaizenq52b11x)

replace hhmaizemaizenq52b1=0 if hhmaizemaizenq52b1==1
replace hhmaizemaizenq52b1=1 if hhmaizemaizenq52b1==2
replace hhmaizemaizenq52b1=99 if hhmaizemaizenq52b1==3
replace hhmaizemaizenq52b2=0 if hhmaizemaizenq52b2==1
replace hhmaizemaizenq52b2=1 if hhmaizemaizenq52b2==2
replace hhmaizemaizenq52b2=99 if hhmaizemaizenq52b2==3
replace hhmaizemaizenq52b3=0 if hhmaizemaizenq52b3==1
replace hhmaizemaizenq52b3=1 if hhmaizemaizenq52b3==2
replace hhmaizemaizenq52b3=99 if hhmaizemaizenq52b3==3
replace hhmaizemaizenq52b4=0 if hhmaizemaizenq52b4==1
replace hhmaizemaizenq52b4=1 if hhmaizemaizenq52b4==2
replace hhmaizemaizenq52b4=99 if hhmaizemaizenq52b4==3
replace hhmaizemaizenq52b5=0 if hhmaizemaizenq52b5==1
replace hhmaizemaizenq52b5=1 if hhmaizemaizenq52b5==2
replace hhmaizemaizenq52b5=99 if hhmaizemaizenq52b5==3
replace hhmaizemaizenq52b6=0 if hhmaizemaizenq52b6==1
replace hhmaizemaizenq52b6=1 if hhmaizemaizenq52b6==2
replace hhmaizemaizenq52b6=99 if hhmaizemaizenq52b6==3
replace hhmaizemaizenq52b7=0 if hhmaizemaizenq52b7==1
replace hhmaizemaizenq52b7=1 if hhmaizemaizenq52b7==2
replace hhmaizemaizenq52b7=99 if hhmaizemaizenq52b7==3
replace hhmaizemaizenq52b8=0 if hhmaizemaizenq52b8==1
replace hhmaizemaizenq52b8=1 if hhmaizemaizenq52b8==2
replace hhmaizemaizenq52b8=99 if hhmaizemaizenq52b8==3
replace hhmaizemaizenq52b9=0 if hhmaizemaizenq52b9==1
replace hhmaizemaizenq52b9=1 if hhmaizemaizenq52b9==2
replace hhmaizemaizenq52b9=99 if hhmaizemaizenq52b9==3
replace hhmaizemaizenq52b10=0 if hhmaizemaizenq52b10==1
replace hhmaizemaizenq52b10=1 if hhmaizemaizenq52b10==2
replace hhmaizemaizenq52b10=99 if hhmaizemaizenq52b10==3
replace hhmaizemaizenq52b11=0 if hhmaizemaizenq52b11==1
replace hhmaizemaizenq52b11=1 if hhmaizemaizenq52b11==2
replace hhmaizemaizenq52b11=99 if hhmaizemaizenq52b11==3

drop hhmaizemaizenq52b1x hhmaizemaizenq52b1xx
drop hhmaizemaizenq52b2x hhmaizemaizenq52b2xx
drop hhmaizemaizenq52b3x hhmaizemaizenq52b3xx
drop hhmaizemaizenq52b4x hhmaizemaizenq52b4xx
drop hhmaizemaizenq52b5x hhmaizemaizenq52b5xx
drop hhmaizemaizenq52b6x hhmaizemaizenq52b6xx
drop hhmaizemaizenq52b7x hhmaizemaizenq52b7xx
drop hhmaizemaizenq52b8x hhmaizemaizenq52b8xx
drop hhmaizemaizenq52b9x hhmaizemaizenq52b9xx
drop hhmaizemaizenq52b10x hhmaizemaizenq52b10xx
drop hhmaizemaizenq52b11x hhmaizemaizenq52b11xx

label variable hhmaizemaizenq52b1 "hm/reasn1 not satisfd wth seed=low yield"
label variable hhmaizemaizenq52b2 "hm/reasn2 not satisfd wth seed=need more water"
label variable hhmaizemaizenq52b3 "hm/reasn3 not satisfd wth seed=less tol to p&d"
label variable hhmaizemaizenq52b4 "hm/reasn4 not satisfd wth seed=slow maturity"
label variable hhmaizemaizenq52b5 "hm/reasn5 not satisfd wth seed=lower mktvalue"
label variable hhmaizemaizenq52b6 "hm/reasn6 not satisfd wth seed=poor taste"
label variable hhmaizemaizenq52b7 "hm/reasn7 not satisfd wth seed=lower germintn rate"
label variable hhmaizemaizenq52b8 "hm/reasn8 not satisfd wth seed=poor purity"
label variable hhmaizemaizenq52b9 "hm/reasn9 not satisfd wth seed=reqs much labor"
label variable hhmaizemaizenq52b10 "hm/reasn10 not satisfd wth seed=needs more comp inputs"
label variable hhmaizemaizenq52b11 "hm/reasn11 not satisfd wth seed=other reason"

rename hhmaizemaizenq52b96 hhmaizemaizenq52b11

sort hhmaizemaizenq52b1
rename hhmaizemaizenq52b1 hhmaizemaizenq52b1x
egen hhmaizemaizenq52b1xx=group( hhmaizemaizenq52b1x )
gen hhmaizemaizenq52b1= hhmaizemaizenq52b1xx, after(hhmaizemaizenq52b1x)
replace hhmaizemaizenq52b1=0 if hhmaizemaizenq52b1==1
replace hhmaizemaizenq52b1=1 if hhmaizemaizenq52b1==2
replace hhmaizemaizenq52b1=99 if hhmaizemaizenq52b1==3
drop hhmaizemaizenq52b1x hhmaizemaizenq52b1xx

rename hhmaizemaizenother_q52b hhmaizemaizenq52b_oth
rename hhmaizemaizenq52b_oth hhmaizemaizenq52b11_oth
label variable hhmaizemaizenq52b11_oth "hm/ reasn11 not satisfd with seed specified"
label variable hhmaizemaizenq52b11_oth "hm/ reasn11 not satisfd wth seed specified"

sort hhmaizemaizenq53
rename hhmaizemaizenq53 hhmaizemaizenq53x
egen hhmaizemaizenq53xx=group( hhmaizemaizenq53x )
gen hhmaizemaizenq53= hhmaizemaizenq53xx, after(hhmaizemaizenq53x)
replace hhmaizemaizenq53=0 if hhmaizemaizenq53==1
replace hhmaizemaizenq53=1 if hhmaizemaizenq53==2
drop hhmaizemaizenq53x hhmaizemaizenq53xx
label variable hhmaizemaizenq53 "hm/wd use the seed again [yes=1/0]"

label variable hhmaizemaizenq53 "hm/wd use the seed again in future"
label variable hhmaizemaizenq53 "hm/wd use the seed again [yes=1/0]"
label variable hhmaizemaizenq54 "hm/qty of maize seed planted on this plot"
label variable hhmaizemaizenq54 "hm/qty of maize seed planted on this plot [kg]"
label variable hhmaizemaizenq55 "hm/cost of maize seed [free=0]"

label variable hhmaizemaizenq56 "hm/maize"
label variable hhmaizemaizenq56 "hm/maize spacing [1=broadcast; 2=2.5x2ft; 2.5x1ft]"
label variable hhmaizemaizenq56 "hm/maize spacing [1=broadcast; 2=2.5x2ft; 3=2.5x1ft]"
sort hhmaizemaizenq56
replace hhmaizemaizenq56 = "d" in 6
replace hhmaizemaizenq56 = "d" in 7
replace hhmaizemaizenq56 = "d" in 8
replace hhmaizemaizenq56 = "d" in 9
replace hhmaizemaizenq56 = "d" in 10
replace hhmaizemaizenq56 = "d" in 11
replace hhmaizemaizenq56 = "d" in 12
replace hhmaizemaizenq56 = "d" in 13
replace hhmaizemaizenq56 = "d" in 14
replace hhmaizemaizenq56 = "d" in 15
replace hhmaizemaizenq56 = "d" in 16
replace hhmaizemaizenq56 = "d" in 17
replace hhmaizemaizenq56 = "d" in 18
replace hhmaizemaizenq56 = "d" in 19
replace hhmaizemaizenq56 = "d" in 20
replace hhmaizemaizenq56 = "d" in 21
replace hhmaizemaizenq56 = "d" in 22
replace hhmaizemaizenq56 = "d" in 23
replace hhmaizemaizenq56 = "d" in 24
replace hhmaizemaizenq56 = "d" in 25
replace hhmaizemaizenq56 = "d" in 26

rename hhmaizemaizenq56 hhmaizemaizenq56x
egen hhmaizemaizenq56xx=group( hhmaizemaizenq56x )
gen hhmaizemaizenq56= hhmaizemaizenq56xx, after(hhmaizemaizenq56x)
label variable hhmaizemaizenq56 "hm/maize spacing [1=broadcast; 2=2.5x2ft; 3=2.5x1ft]"
replace hhmaizemaizenq56=96 if hhmaizemaizenq56==4
drop hhmaizemaizenq56x hhmaizemaizenq56xx

sort hhmaizemaizenq57
replace hhmaizemaizenq57 = "98" in 1445
replace hhmaizemaizenq57 = "98" in 1446
replace hhmaizemaizenq57 = "98" in 1447
replace hhmaizemaizenq57 = "98" in 1448
replace hhmaizemaizenq57 = "98" in 1449
replace hhmaizemaizenq57 = "98" in 1450
replace hhmaizemaizenq57 = "99" in 1440
replace hhmaizemaizenq57 = "99" in 1441
replace hhmaizemaizenq57 = "99" in 1442
replace hhmaizemaizenq57 = "99" in 1443
replace hhmaizemaizenq57 = "99" in 1444
label variable hhmaizemaizenq57 "hm/if not broadcast, seed# per hill [98=na]"
destring hhmaizemaizenq57 , replace force float

sort hhmaizemaizenq58
label variable hhmaizemaizenq58 "hm/applied org-manure b4 plantg [yes=1/0] 2018A"

replace hhmaizemaizenq58 = "z" in 6
replace hhmaizemaizenq58 = "z" in 7
replace hhmaizemaizenq58 = "z" in 8
replace hhmaizemaizenq58 = "z" in 9
replace hhmaizemaizenq58 = "z" in 10
replace hhmaizemaizenq58 = "z" in 11
replace hhmaizemaizenq58 = "z" in 12
replace hhmaizemaizenq58 = "z" in 13
replace hhmaizemaizenq58 = "z" in 14

rename hhmaizemaizenq58 hhmaizemaizenq58x
egen hhmaizemaizenq58xx=group( hhmaizemaizenq58x )
gen hhmaizemaizenq58= hhmaizemaizenq58xx, after( hhmaizemaizenq58x )
replace hhmaizemaizenq58=0 if hhmaizemaizenq58==1
replace hhmaizemaizenq58=1 if hhmaizemaizenq58==2
replace hhmaizemaizenq58=96 if hhmaizemaizenq58==3
label variable hhmaizemaizenq58 "hm/applied orgmanure b4 plantg [yes=1/0] 2018A (96=dont knw)"
drop hhmaizemaizenq58x
drop hhmaizemaizenq58xx
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace

*XXX 20200506
label variable hhmaizemaizenq59 "hm/applied DAP/NPK [yes=1/0]"
label variable hhmaizemaizenq59 "hm/applied DAP/NPK [yes=1, 0]"

sort hhmaizemaizenq59
rename hhmaizemaizenq59 hhmaizemaizenq59x
egen hhmaizemaizenq59xx=group( hhmaizemaizenq59x )
gen hhmaizemaizenq59= hhmaizemaizenq59xx, after( hhmaizemaizenq59x )

ed
replace hhmaizemaizenq59=0 if hhmaizemaizenq59==1
replace hhmaizemaizenq59=1 if hhmaizemaizenq59==2
drop hhmaizemaizenq59x hhmaizemaizenq59xx
save "C:\Users\Samuel\Documents\iFPRI\maize\maizevc_linked_ds2.dta", replace

label variable hhmaizemaizenq59a "hm/qty of fertzer applied (kg)"
sort hhmaizemaizenq59a
replace hhmaizemaizenq59a = "98" in 293
replace hhmaizemaizenq59a = "998" in 293
replace hhmaizemaizenq59a = "998" in 294
replace hhmaizemaizenq59a = "998" in 295
replace hhmaizemaizenq59a = "998" in 296
replace hhmaizemaizenq59a = "998" in 297
replace hhmaizemaizenq59a = "998" in 298
replace hhmaizemaizenq59a = "998" in 299
replace hhmaizemaizenq59a = "998" in 300
replace hhmaizemaizenq59a = "998" in 301
replace hhmaizemaizenq59a = "998" in 302
replace hhmaizemaizenq59a = "998" in 303
replace hhmaizemaizenq59a = "998" in 304
replace hhmaizemaizenq59a = "998" in 305
replace hhmaizemaizenq59a = "998" in 306
drop in 1528/1552
destring hhmaizemaizenq59a, replace force float
label variable hhmaizemaizenq59a "hm/qty of dap/npk applied (kg)"

label variable hhmaizemaizenq60 "hm/applied UREA [yes=1, 0]"
sort hhmaizemaizenq60
replace hhmaizemaizenq60 = "z" in 6
replace hhmaizemaizenq60 = "z" in 7
replace hhmaizemaizenq60 = "z" in 8
replace hhmaizemaizenq60 = "z" in 9
replace hhmaizemaizenq60 = "z" in 10
replace hhmaizemaizenq60 = "z" in 11
replace hhmaizemaizenq60 = "z" in 12
replace hhmaizemaizenq60 = "z" in 13
replace hhmaizemaizenq60 = "z" in 14

rename hhmaizemaizenq60 hhmaizemaizenq60x
egen hhmaizemaizenq60xx=group( hhmaizemaizenq60x )
gen hhmaizemaizenq60= hhmaizemaizenq60xx, after( hhmaizemaizenq60xx)
drop hhmaizemaizenq60
gen hhmaizemaizenq60= hhmaizemaizenq60xx, after( hhmaizemaizenq60x)
replace hhmaizemaizenq60=0 if hhmaizemaizenq60==1
replace hhmaizemaizenq60=1 if hhmaizemaizenq60==2
replace hhmaizemaizenq60=98 if hhmaizemaizenq60==3
label variable hhmaizemaizenq60 "hm/applied UREA [yes=1, 0]"
drop hhmaizemaizenq60x hhmaizemaizenq60xx

label variable hhmaizemaizenq60a "hm/qty of urea applied (kg)"
destring hhmaizemaizenq60a , replace force float
replace hhmaizemaizenq60a=998 if hhmaizemaizenq60a==0| hhmaizemaizenq60a==98
replace hhmaizemaizenq60a=0 if hhmaizemaizenq60a==998
label variable hhmaizemaizenq61 "hm/freq. of mz weeding 2018A"
label variable hhmaizemaizenq62 "hm/duratn to 1st mz weeding"
label variable hhmaizemaizenq62 "hm/duratn to 1st mz weeding (days)"
destring hhmaizemaizenq62 , replace force float

sort hhmaizemaizenq63
replace hhmaizemaizenq63 = "z" in 6
replace hhmaizemaizenq63 = "z" in 7
replace hhmaizemaizenq63 = "z" in 8
replace hhmaizemaizenq63 = "z" in 9
replace hhmaizemaizenq63 = "z" in 10
replace hhmaizemaizenq63 = "z" in 11
replace hhmaizemaizenq63 = "z" in 12
replace hhmaizemaizenq63 = "z" in 13

rename hhmaizemaizenq63 hhmaizemaizenq63x
egen hhmaizemaizenq63xx=group( hhmaizemaizenq63x )
gen hhmaizemaizenq63= hhmaizemaizenq63xx, after( hhmaizemaizenq63x )
replace hhmaizemaizenq63=0 if hhmaizemaizenq63==1
replace hhmaizemaizenq63=1 if hhmaizemaizenq63==2
replace hhmaizemaizenq63=98 if hhmaizemaizenq63==3
drop hhmaizemaizenq63x hhmaizemaizenq63xx

label variable hhmaizemaizenq63 "hm/used pestcide/herbcide [yes=1/0; 2=dontknw]"
label variable hhmaizemaizenq63 "hm/used pestcide/herbcide [yes=1/0; 96=dontknw]"
label variable hhmaizemaizenq64 "hm/maize harvest (bags)"
label variable hhmaizemaizenq65 "hm/ref unit of measure per bag"
label variable hhmaizemaizenq65 "hm/ref kg per bag /unit of measure per bag"
label variable hhmaizemaizenq65 "hm/kg per bag /ref unit of measure per bag"

*xxx maize yield
gen hhmaizemaizenq645=., after( hhmaizemaizenq65 )
replace hhmaizemaizenq645= hhmaizemaizenq64* hhmaizemaizenq65

rename hhmaizemaizenq64 hhmaizemaizenq64a
rename hhmaizemaizenq645 hhmaizemaizenq64b

gen hhmaizemaizenq64c=., after( hhmaizemaizenq64b )
gen hhmaizemaizenq64ax=., before( hhmaizemaizenq64a )
rename hhmaizemaizenq64ax hhmaizemaizenq64x
replace hhmaizemaizenq64x= hhmaizeplot_select_area*0.404686
replace hhmaizemaizenq64c= hhmaizemaizenq64b/ hhmaizemaizenq64x

label variable hhmaizemaizenq64x "hm/maize area (ha) planted for selected plot"
label variable hhmaizemaizenq645 "hm/maize harvest in Kgs from selected plot"
label variable hhmaizemaizenq64c "hm/maize yield (Kgs/ha)"

sum hhmaizemaizenq64x hhmaizemaizenq64b hhmaizemaizenq64c
tab1 hhmaizemaizenq64x hhmaizemaizenq64b hhmaizemaizenq64c if hhmaizemaizenq64c==0

sort hhmaizemaizenq64c

rename hhmaizeq6698 hhmaizeq6616

sort hhmaizeq661
rename hhmaizeq661 hhmaizeq661x
egen hhmaizeq661xx=group( hhmaizeq661x )
gen hhmaizeq661= hhmaizeq661xx, after(hhmaizeq661x)
sort hhmaizeq661x
sort hhmaizeq661
replace hhmaizeq661=0 if hhmaizeq661==1
replace hhmaizeq661=1 if hhmaizeq661==2
drop hhmaizeq661x hhmaizeq661xx

label variable hhmaizebeansq67 "hb/number of bean plots"
sort hhmaizebeansq67
replace hhmaizebeansq67 = "98" in 1111
replace hhmaizebeansq67 = "98" in 1112
replace hhmaizebeansq67 = "98" in 1113
replace hhmaizebeansq67 = "98" in 1114
replace hhmaizebeansq67 = "98" in 1115
replace hhmaizebeansq67 = "98" in 1116
replace hhmaizebeansq67 = "98" in 1117
replace hhmaizebeansq67 = "98" in 1118
replace hhmaizebeansq67 = "98" in 1119
replace hhmaizebeansq67 = "98" in 1120
replace hhmaizebeansq67 = "98" in 1121
replace hhmaizebeansq67 = "98" in 1122
replace hhmaizebeansq67 = "98" in 1123
replace hhmaizebeansq67 = "98" in 1124
replace hhmaizebeansq67 = "98" in 1125
replace hhmaizebeansq67 = "98" in 1126
replace hhmaizebeansq67 = "98" in 1127
replace hhmaizebeansq67 = "98" in 1128
replace hhmaizebeansq67 = "98" in 1129
replace hhmaizebeansq67 = "98" in 1130
replace hhmaizebeansq67 = "98" in 1131
replace hhmaizebeansq67 = "98" in 1132
replace hhmaizebeansq67 = "98" in 1133
replace hhmaizebeansq67 = "98" in 1134
replace hhmaizebeansq67 = "98" in 1135
replace hhmaizebeansq67 = "98" in 1136
replace hhmaizebeansq67 = "98" in 1137
replace hhmaizebeansq67 = "98" in 1138
replace hhmaizebeansq67 = "98" in 1139
replace hhmaizebeansq67 = "98" in 1140
replace hhmaizebeansq67 = "98" in 1141
replace hhmaizebeansq67 = "98" in 1142
replace hhmaizebeansq67 = "98" in 1143

destring hhmaizebeansq67 , replace force float
destring hhmaizebeansplotb_count , replace force float

label variable hhmaizebeansplotb_count "hb/maize/beans/plotb_count"
label variable hhmaizebeansplotb_count "hb/bean plot #"
label variable hhmaizebeansplotb_count "hb/bean plot count"
label variable hhmaizebeansplotb1plot_num1 "hb/bean plot1"
label variable hhmaizebeansplotb1plot_num1 "hb/bean plot #1"

replace hhmaizebeansplotb1plot_num1 = "98" in 12
replace hhmaizebeansplotb1plot_num1 = "98" in 11
replace hhmaizebeansplotb1plot_num1 = "98" in 9
replace hhmaizebeansplotb1plot_num1 = "98" in 6
replace hhmaizebeansplotb1plot_num1 = "98" in 8

label variable hhmaizebeansplotb1q69 "hb/bean plot1 size (ac)"
label variable hhmaizebeansplotb1plot_num1 "hb/bean plot1 name ( #1)"
label variable hhmaizebeansplotb1q69 "hb/bean plot1 area (ac)"
sort hhmaizebeansplotb1q69








*xxxx q66
rename hhmaizeq661 hhmaizeq661x
rename hhmaizeq662 hhmaizeq662x
rename hhmaizeq663 hhmaizeq663x
rename hhmaizeq664 hhmaizeq664x
rename hhmaizeq665 hhmaizeq665x
rename hhmaizeq666 hhmaizeq666x
rename hhmaizeq667 hhmaizeq667x
rename hhmaizeq668 hhmaizeq668x
rename hhmaizeq669 hhmaizeq669x
rename hhmaizeq6610 hhmaizeq6610x
rename hhmaizeq6611 hhmaizeq6611x
rename hhmaizeq6612 hhmaizeq6612x
rename hhmaizeq6613 hhmaizeq6613x
rename hhmaizeq6614 hhmaizeq6614x
rename hhmaizeq6615 hhmaizeq6615x
rename hhmaizeq6616 hhmaizeq6616x

egen hhmaizeq661xx=group( hhmaizeq661x )
egen hhmaizeq662xx=group( hhmaizeq662x )
egen hhmaizeq663xx=group( hhmaizeq663x )
egen hhmaizeq664xx=group( hhmaizeq664x )
egen hhmaizeq665xx=group( hhmaizeq665x )
egen hhmaizeq666xx=group( hhmaizeq666x )
egen hhmaizeq667xx=group( hhmaizeq667x )
egen hhmaizeq668xx=group( hhmaizeq668x )
egen hhmaizeq669xx=group( hhmaizeq669x )
egen hhmaizeq6610xx=group( hhmaizeq6610x )
egen hhmaizeq6611xx=group( hhmaizeq6611x )
egen hhmaizeq6612xx=group( hhmaizeq6612x )
egen hhmaizeq6613xx=group( hhmaizeq6613x )
egen hhmaizeq6614xx=group( hhmaizeq6614x )
egen hhmaizeq6615xx=group( hhmaizeq6615x )
egen hhmaizeq6616xx=group( hhmaizeq6616x )

gen hhmaizeq661= hhmaizeq661xx, after(hhmaizeq661x)
gen hhmaizeq662= hhmaizeq662xx, after(hhmaizeq662x)
gen hhmaizeq663= hhmaizeq663xx, after(hhmaizeq663x)
gen hhmaizeq664= hhmaizeq664xx, after(hhmaizeq664x)
gen hhmaizeq665= hhmaizeq665xx, after(hhmaizeq665x)
gen hhmaizeq666= hhmaizeq666xx, after(hhmaizeq666x)
gen hhmaizeq667= hhmaizeq667xx, after(hhmaizeq667x)
gen hhmaizeq668= hhmaizeq668xx, after(hhmaizeq668x)
gen hhmaizeq669= hhmaizeq669xx, after(hhmaizeq669x)
gen hhmaizeq6610= hhmaizeq6610xx, after(hhmaizeq6610x)
gen hhmaizeq6611= hhmaizeq6611xx, after(hhmaizeq6611x)
gen hhmaizeq6612= hhmaizeq6612xx, after(hhmaizeq6612x)
gen hhmaizeq6613= hhmaizeq6613xx, after(hhmaizeq6613x)
gen hhmaizeq6614= hhmaizeq6614xx, after(hhmaizeq6614x)
gen hhmaizeq6615= hhmaizeq6615xx, after(hhmaizeq6615x)
gen hhmaizeq6616= hhmaizeq6616xx, after(hhmaizeq6616x)

replace hhmaizeq661=0 if hhmaizeq661==1
replace hhmaizeq662=0 if hhmaizeq662==1
replace hhmaizeq663=0 if hhmaizeq663==1
replace hhmaizeq664=0 if hhmaizeq664==1
replace hhmaizeq665=0 if hhmaizeq665==1
replace hhmaizeq666=0 if hhmaizeq666==1
replace hhmaizeq667=0 if hhmaizeq667==1
replace hhmaizeq668=0 if hhmaizeq668==1
replace hhmaizeq669=0 if hhmaizeq669==1
replace hhmaizeq6610=0 if hhmaizeq6610==1
replace hhmaizeq6611=0 if hhmaizeq6611==1
replace hhmaizeq6612=0 if hhmaizeq6612==1
replace hhmaizeq6613=0 if hhmaizeq6613==1
replace hhmaizeq6614=0 if hhmaizeq6614==1
replace hhmaizeq6615=0 if hhmaizeq6615==1
replace hhmaizeq6616=0 if hhmaizeq6616==1

replace hhmaizeq661=1 if hhmaizeq661==2
replace hhmaizeq662=1 if hhmaizeq662==2
replace hhmaizeq663=1 if hhmaizeq663==2
replace hhmaizeq664=1 if hhmaizeq664==2
replace hhmaizeq665=1 if hhmaizeq665==2
replace hhmaizeq666=1 if hhmaizeq666==2
replace hhmaizeq667=1 if hhmaizeq667==2
replace hhmaizeq668=1 if hhmaizeq668==2
replace hhmaizeq669=1 if hhmaizeq669==2
replace hhmaizeq6610=1 if hhmaizeq6610==2
replace hhmaizeq6611=1 if hhmaizeq6611==2
replace hhmaizeq6612=1 if hhmaizeq6612==2
replace hhmaizeq6613=1 if hhmaizeq6613==2
replace hhmaizeq6614=1 if hhmaizeq6614==2
replace hhmaizeq6615=1 if hhmaizeq6615==2
replace hhmaizeq6616=1 if hhmaizeq6616==2

drop hhmaizeq661x hhmaizeq661xx
drop hhmaizeq662x hhmaizeq662xx
drop hhmaizeq663x hhmaizeq663xx
drop hhmaizeq664x hhmaizeq664xx
drop hhmaizeq665x hhmaizeq665xx
drop hhmaizeq666x hhmaizeq666xx
drop hhmaizeq667x hhmaizeq667xx
drop hhmaizeq668x hhmaizeq668xx
drop hhmaizeq669x hhmaizeq669xx
drop hhmaizeq6610x hhmaizeq6610xx
drop hhmaizeq6611x hhmaizeq6611xx
drop hhmaizeq6612x hhmaizeq6612xx
drop hhmaizeq6613x hhmaizeq6613xx
drop hhmaizeq6614x hhmaizeq6614xx
drop hhmaizeq6615x hhmaizeq6615xx
drop hhmaizeq6616x hhmaizeq6616xx

label variable hhmaizeq661 "hm/bean var1 aware of=NABE4"
label variable hhmaizeq661 "hb/bean var1 aware of=NABE4 [yes=1/0]"
label variable hhmaizeq662 "hb/bean var2 aware of=NABE5 [yes=1/0]"
label variable hhmaizeq663 "hb/bean var3 aware of=NABE12C [yes=1/0]"
label variable hhmaizeq664 "hb/bean var4 aware of=NABE15 [yes=1/0]"
label variable hhmaizeq665 "hb/bean var5 aware of=NABE16 [yes=1/0]"
label variable hhmaizeq666 "hb/bean var6 aware of=NABE17 [yes=1/0]"
label variable hhmaizeq667 "hb/bean var7 aware of=NABE19 [yes=1/0]"
label variable hhmaizeq668 "hb/bean var8 aware of=NAROBN1 [yes=1/0]"
label variable hhmaizeq669 "hb/bean var9 aware of=NAROBN2 [yes=1/0]"
label variable hhmaizeq6610 "hb/bean var10 aware of=NAROBN3 [yes=1/0]"
label variable hhmaizeq6611 "hb/bean var11 aware of=NAROBN4C [yes=1/0]"
label variable hhmaizeq6612 "hb/bean var12 aware of=NAROBN5C [yes=1/0]"
label variable hhmaizeq6613 "hb/bean var13 aware of=LANDRACE [yes=1/0]"
label variable hhmaizeq6614 "hb/bean var14 aware of=DONTKNW (~NABE)"
label variable hhmaizeq6615 "hb/bean var15 aware of=DONTKNW (~NAROBN)"
label variable hhmaizeq6616 "hb/bean var16 aware of=DONTKNW (NO IDEA)"


label variable hhmaizebeansplotb2plot_num1 "hb/bean plot2 number (#1)"
label variable hhmaizebeansplotb3plot_num1 "hb/bean plot3 number (#1)"
label variable hhmaizebeansplotb4plot_num1 "hb/bean plot4 number (#1)"

label variable hhmaizebeansplotb2q69 "hb/bean plot2 area (ac)"
label variable hhmaizebeansplotb3q69 "hb/bean plot3 area (ac)"
label variable hhmaizebeansplotb4q69 "hb/bean plot4 area (ac)"

label variable hhmaizebeansplotb2q68 "hb/bean plot2 name"
label variable hhmaizebeansplotb3q68 "hb/bean plot3 name"
label variable hhmaizebeansplotb4q68 "hb/bean plot4 name"

destring hhmaizebeansplotb1plot_num1 , replace force float
destring hhmaizebeansplotb2plot_num1 , replace force float
destring hhmaizebeansplotb3plot_num1 , replace force float
destring hhmaizebeansplotb4plot_num1 , replace force float

destring hhmaizebeansplotb1q69 , replace force float
destring hhmaizebeansplotb2q69 , replace force float
destring hhmaizebeansplotb3q69 , replace force float
destring hhmaizebeansplotb4q69 , replace force float

label variable hhmaizebeansbeanq751 "hb/bean intercrop1=maize [yes=1, 0]"
label variable hhmaizebeansbeanq752 "hb/bean intercrop2=soybn [yes=1, 0]"
label variable hhmaizebeansbeanq753 "hb/bean intercrop3=gnuts [yes=1, 0]"
label variable hhmaizebeansbeanq754 "hb/bean intercrop4=cassv [yes=1, 0]"
label variable hhmaizebeansbeanq755 "hb/bean intercrop5=millt [yes=1, 0]"
label variable hhmaizebeansbeanq756 "hb/bean intercrop6=sorgh [yes=1, 0]"
label variable hhmaizebeansbeanq757 "hb/bean intercrop96=other [yes=1, 0]"

rename hhmaizebeansbeanq751 hhmaizebeansbeanq751x
rename hhmaizebeansbeanq752 hhmaizebeansbeanq752x
rename hhmaizebeansbeanq753 hhmaizebeansbeanq753x
rename hhmaizebeansbeanq754 hhmaizebeansbeanq754x
rename hhmaizebeansbeanq755 hhmaizebeansbeanq755x
rename hhmaizebeansbeanq756 hhmaizebeansbeanq756x
rename hhmaizebeansbeanq757 hhmaizebeansbeanq757x

egen hhmaizebeansbeanq751xx=group( hhmaizebeansbeanq751x )
egen hhmaizebeansbeanq752xx=group( hhmaizebeansbeanq752x )
egen hhmaizebeansbeanq753xx=group( hhmaizebeansbeanq753x )
egen hhmaizebeansbeanq754xx=group( hhmaizebeansbeanq754x )
egen hhmaizebeansbeanq755xx=group( hhmaizebeansbeanq755x )
egen hhmaizebeansbeanq756xx=group( hhmaizebeansbeanq756x )
egen hhmaizebeansbeanq757xx=group( hhmaizebeansbeanq757x )

gen hhmaizebeansbeanq751= hhmaizebeansbeanq751xx, after(hhmaizebeansbeanq751x )
gen hhmaizebeansbeanq752= hhmaizebeansbeanq752xx, after(hhmaizebeansbeanq752x )
gen hhmaizebeansbeanq753= hhmaizebeansbeanq753xx, after(hhmaizebeansbeanq753x )
gen hhmaizebeansbeanq754= hhmaizebeansbeanq754xx, after(hhmaizebeansbeanq754x )
gen hhmaizebeansbeanq755= hhmaizebeansbeanq755xx, after(hhmaizebeansbeanq755x )
gen hhmaizebeansbeanq756= hhmaizebeansbeanq756xx, after(hhmaizebeansbeanq756x )
gen hhmaizebeansbeanq757= hhmaizebeansbeanq757xx, after(hhmaizebeansbeanq757x )

replace hhmaizebeansbeanq751=0 if hhmaizebeansbeanq751==1
replace hhmaizebeansbeanq752=0 if hhmaizebeansbeanq752==1
replace hhmaizebeansbeanq753=0 if hhmaizebeansbeanq753==1
replace hhmaizebeansbeanq754=0 if hhmaizebeansbeanq754==1
replace hhmaizebeansbeanq755=0 if hhmaizebeansbeanq755==1
replace hhmaizebeansbeanq756=0 if hhmaizebeansbeanq756==1
replace hhmaizebeansbeanq757=0 if hhmaizebeansbeanq757==1

replace hhmaizebeansbeanq751=1 if hhmaizebeansbeanq751==2
replace hhmaizebeansbeanq752=1 if hhmaizebeansbeanq752==2
replace hhmaizebeansbeanq753=1 if hhmaizebeansbeanq753==2
replace hhmaizebeansbeanq754=1 if hhmaizebeansbeanq754==2
replace hhmaizebeansbeanq755=1 if hhmaizebeansbeanq755==2
replace hhmaizebeansbeanq756=1 if hhmaizebeansbeanq756==2
replace hhmaizebeansbeanq757=1 if hhmaizebeansbeanq757==2

replace hhmaizebeansbeanq751=98 if hhmaizebeansbeanq751==3
replace hhmaizebeansbeanq752=98 if hhmaizebeansbeanq752==3
replace hhmaizebeansbeanq753=98 if hhmaizebeansbeanq753==3
replace hhmaizebeansbeanq754=98 if hhmaizebeansbeanq754==3
replace hhmaizebeansbeanq755=98 if hhmaizebeansbeanq755==3
replace hhmaizebeansbeanq756=98 if hhmaizebeansbeanq756==3
replace hhmaizebeansbeanq757=98 if hhmaizebeansbeanq757==3

drop hhmaizebeansbeanq751x hhmaizebeansbeanq751xx hhmaizebeansbeanq752x hhmaizebeansbeanq752xx ///
hhmaizebeansbeanq753x hhmaizebeansbeanq753xx hhmaizebeansbeanq754x hhmaizebeansbeanq754xx ///
hhmaizebeansbeanq755x hhmaizebeansbeanq755xx hhmaizebeansbeanq756x hhmaizebeansbeanq756xx ///
hhmaizebeansbeanq757x hhmaizebeansbeanq757xx

label variable hhmaizebeansbeanother_q78a "hb/bean seed source1=own saved "
label variable hhmaizebeansbeanother_q78b "hb/bean seed source2=fellow farmer saved seed "
label variable hhmaizebeansbeanother_q78c "hb/bean seed source3=bought frm local mkt "
label variable hhmaizebeansbeanother_q78d "hb/bean seed source4=bought frm agroinpt shop "
label variable hhmaizebeansbeanother_q78e "hb/bean seed source5=owc/naads"
label variable hhmaizebeansbeanother_q78f "hb/bean seed source6=ngo"
label variable hhmaizebeansbeanother_q78g "hb/bean seed source7=seed comp"
label variable hhmaizebeansbeanother_q78h "hb/bean seed source8=LSB"
label variable hhmaizebeansbeanother_q78i "hb/bean seed source9=from research statn"
label variable hhmaizebeansbeanother_q78j "hb/bean seed source10= "
label variable hhmaizebeansbeanother_q78k "hb/bean seed source11= "
label variable hhmaizebeansbeanother_q78l "hb/bean seed source12= "
label variable hhmaizebeansbeanother_q78m "hb/bean seed source98=other source"


destring hhmaizebeansbeanother_q78a, replace force float
destring hhmaizebeansbeanother_q78b, replace force float
destring hhmaizebeansbeanother_q78c, replace force float
destring hhmaizebeansbeanother_q78d, replace force float
destring hhmaizebeansbeanother_q78e, replace force float
destring hhmaizebeansbeanother_q78f, replace force float
destring hhmaizebeansbeanother_q78g, replace force float
destring hhmaizebeansbeanother_q78h, replace force float
destring hhmaizebeansbeanother_q78i, replace force float
destring hhmaizebeansbeanother_q78j, replace force float
destring hhmaizebeansbeanother_q78k, replace force float
destring hhmaizebeansbeanother_q78l, replace force float
destring hhmaizebeansbeanother_q78m, replace force float

label variable hhmaizebeansbeanq80a "hb/reasn1 used this bn seed=high yield"
label variable hhmaizebeansbeanq80b "hb/reasn2 used this bn seed=drought tol"
label variable hhmaizebeansbeanq80c "hb/reasn3 used this bn seed=p&d tol"
label variable hhmaizebeansbeanq80d "hb/reasn4 used this bn seed=early maturity"
label variable hhmaizebeansbeanq80e "hb/reasn5 used this bn seed=higher mkt p; dd"
label variable hhmaizebeansbeanq80f "hb/reasn6 used this bn seed=good taste& nutr"
label variable hhmaizebeansbeanq80g "hb/reasn7 used this bn seed=low price/for free"
label variable hhmaizebeansbeanq80h "hb/reasn8 used this bn seed=availability"
label variable hhmaizebeansbeanq80i "hb/reasn9 used this bn seed=germintn rate"
label variable hhmaizebeansbeanq80j "hb/reasn10 used this bn seed=experience"
label variable hhmaizebeansbeanq80k "hb/reasn11 used this bn seed=cooks faster"

destring hhmaizebeansbeanq80a, replace force float
destring hhmaizebeansbeanq80b , replace force float
destring hhmaizebeansbeanq80c, replace force float
destring hhmaizebeansbeanq80d , replace force float
destring hhmaizebeansbeanq80e, replace force float
destring hhmaizebeansbeanq80f , replace force float
destring hhmaizebeansbeanq80g, replace force float
destring hhmaizebeansbeanq80h , replace force float
destring hhmaizebeansbeanq80i, replace force float
destring hhmaizebeansbeanq80j , replace force float
destring hhmaizebeansbeanq80k, replace force float


destring hhmaizebeansplotb1plot_num1 , replace force float
destring hhmaizebeansplotb1q69 , replace force float
label variable hhmaizebeansplotb_count "hb/bean plot count   (~98=na)"
label variable hhmaizebeansplotb1q68 "hb/bean plot1 name"
label variable hhmaizebeansplotb1plot_num1 "hb/bean plot1 number (#1)"
label variable hhmaizebeansplotb2plot_num1 "hb/bean plot2 number (#2)"
label variable hhmaizebeansplotb1plot_num1 "hb/bean plot1 number (#1)"
sort hhmaizebeansplotb2q69
drop in 1528/1810

label variable hhmaizebeansplotb2q69 "hb/maize/beans/plotb[2]/q69"
destring hhmaizebeansplotb2plot_num1 , replace force float
destring hhmaizebeansplotb2q69 , replace force float
sort hhmaizebeansplotb3plot_num1
sort hhmaizebeansplotb4plot_num1

label variable hhmaizebeansplot_calc3 "hb/beans plot_calc3"
label variable hhmaizebeansplot_calc4 "hb/beans plot_calc4"

sort hhmaizebeansplot_select1
destring hhmaizebeansplot_calc3, replace force float
destring hhmaizebeansplot_calc4 , replace force float

label variable hhmaizebeansplot_select1 "hb/selected bn plot #"
label variable hhmaizebeansplot_select_name1 "hb/selected bn plot name"
label variable hhmaizebeansplot_select_area2 "hb/selected bn plot area (ac)"
destring hhmaizebeansplot_select1 , replace force float
replace hhmaizebeansplot_select1 = 98 in 1110
destring hhmaizebeansplot_select_area2 , replace force float
destring hhmaizebeansorder1 , replace force float

label variable hhmaizebeansorder1 "hb/maize/beans/order1 xxx"

label variable hhmaizebeansbeanq70 "hb/selected bn plot=mz plot intercropped"
label variable hhmaizebeansbeanq70 "hb/selected bn plot=mz plot intercropped [yes=1/0]"

sort hhmaizebeansbeanq70
rename hhmaizebeansbeanq70 hhmaizebeansbeanq70x
egen hhmaizebeansbeanq70xx=group( hhmaizebeansbeanq70x )
gen hhmaizebeansbeanq70= hhmaizebeansbeanq70xx, after(hhmaizebeansbeanq70x )
replace hhmaizebeansbeanq70=0 if hhmaizebeansbeanq70==1
replace hhmaizebeansbeanq70=1 if hhmaizebeansbeanq70==2
replace hhmaizebeansbeanq70=98 if hhmaizebeansbeanq70==3
label variable hhmaizebeansbeanq70 "hb/selected bn plot=mz plot intercropped [yes=1/0]"
drop hhmaizebeansbeanq70x hhmaizebeansbeanq70xx
label variable hhmaizebeansbeanq71 "hb/when startd plant"
label variable hhmaizebeansbeanq71 "hb/when startd farmg on bn plot"

sort hhmaizebeansbeanq71
replace hhmaizebeansbeanq71=98 if hhmaizebeansbeanq71==.
replace hhmaizebeansbeanq72=98 if hhmaizebeansbeanq72==.
sort hhmaizebeansbeanq73
label variable hhmaizebeansbeanq71 "hb/if no, when startd farmg on bn plot"
label variable hhmaizebeansbeanq72 "hb/if no, how far is bn plot from hh (km)"
label variable hhmaizebeansbeanq73 "hb/if no, rating of bn plot soil fertility (1=v.poor~5=v.fertile)"
sort hhmaizebeansbeanq73
drop in 1528/1532
destring hhmaizebeansbeanq73 , replace force float

label variable hhmaizebeansbeanq74 "hb/if no, was bn plot intercropped in 2018A"
sort hhmaizebeansbeanq74
rename hhmaizebeansbeanq74 hhmaizebeansbeanq74x
egen hhmaizebeansbeanq74xx=group( hhmaizebeansbeanq74x )
gen hhmaizebeansbeanq74= hhmaizebeansbeanq74xx, after( hhmaizebeansbeanq74x )
label variable hhmaizebeansbeanq74 "hb/if no, was bn plot intercropped in 2018A"
replace hhmaizebeansbeanq74=0 if hhmaizebeansbeanq74==1
replace hhmaizebeansbeanq74=1 if hhmaizebeansbeanq74==2
replace hhmaizebeansbeanq74=98 if hhmaizebeansbeanq74==3
drop hhmaizebeansbeanq74x hhmaizebeansbeanq74xx
label variable hhmaizebeansbeanq751 "hb/maize/beans/bean/q75/1"

label variable hhmaizebeansbeanq751 "hb/bean intercrop1="
label variable hhmaizebeansbeanq751 "hb/bean intercrop1=maize [yes=1, 0]"

rename hhmaizebeansbeanq7596 hhmaizebeansbeanq757

sort hhmaizebeansbeanq751
rename hhmaizebeansbeanq751 hhmaizebeansbeanq751x
egen hhmaizebeansbeanq751xx=group( hhmaizebeansbeanq751x )
gen hhmaizebeansbeanq751= hhmaizebeansbeanq751xx, after(hhmaizebeansbeanq751x )
sort hhmaizebeansbeanq751
replace hhmaizebeansbeanq751=0 if hhmaizebeansbeanq751==1
drop hhmaizebeansbeanq751x hhmaizebeansbeanq751xx hhmaizebeansbeanq752x hhmaizebeansbeanq752xx hhmaizebeansbeanq753x hhmaizebeansbeanq753xx hhmaizebeansbeanq754x hhmaizebeansbeanq754xx hhmaizebeansbeanq755x hhmaizebeansbeanq755xx hhmaizebeansbeanq756x hhmaizebeansbeanq756xx hhmaizebeansbeanq757x hhmaizebeansbeanq757xx

sort hhmaizebeansbeanq756

sort hhmaizebeansbeanq76
rename hhmaizebeansbeanq76 hhmaizebeansbeanq76x
egen hhmaizebeansbeanq76xx=group( hhmaizebeansbeanq76x )
gen hhmaizebeansbeanq76= hhmaizebeansbeanq76xx, after(hhmaizebeansbeanq76x)
replace hhmaizebeansbeanq76=0 if hhmaizebeansbeanq76==1
replace hhmaizebeansbeanq76=1 if hhmaizebeansbeanq76==2
replace hhmaizebeansbeanq76=98 if hhmaizebeansbeanq76==3
drop hhmaizebeansbeanq76x hhmaizebeansbeanq76xx
label variable hhmaizebeansbeanq76 "hb/grew bns in same plot in 2017 [yes=1/0; 3=dk]"
label variable hhmaizebeansbeanq76 "hb/grew bns in same plot in 2017 [yes=1/0; 98=na]"

sort hhmaizebeansbeanq77
destring hhmaizebeansbeanq77, replace force float
label variable hhmaizebeansbeanq77 "hb/intercrop in bean plot"
label variable hhmaizebeansbeanq78 "hb/source of bean seed"
rename hhmaizebeansbeanq78 hhmaizebeansbeanq78x
egen hhmaizebeansbeanq78xx=group( hhmaizebeansbeanq78x )
gen hhmaizebeansbeanq78= hhmaizebeansbeanq78xx, after(hhmaizebeansbeanq78x)
sort hhmaizebeansbeanq78
replace hhmaizebeansbeanq78x = "z" in 1
replace hhmaizebeansbeanq78x = "z" in 2
replace hhmaizebeansbeanq78x = "z" in 3
egen hhmaizebeansbeanq78xx=group( hhmaizebeansbeanq78x )
gen hhmaizebeansbeanq78= hhmaizebeansbeanq78xx, after(hhmaizebeansbeanq78x)
sort hhmaizebeansbeanq78
replace hhmaizebeansbeanq78 = 96 in 1519
replace hhmaizebeansbeanq78 = 96 in 1520
replace hhmaizebeansbeanq78 = 96 in 1521
label variable hhmaizebeansbeanq78 "hb/source of bean seed"
replace hhmaizebeansbeanq78=98 if hhmaizebeansbeanq78==9
drop hhmaizebeansbeanq78x hhmaizebeansbeanq78xx

sort hhmaizebeansbeanother_q78a
label variable hhmaizebeansbeanother_q78a "hb/bean seed source1=own saved "
sort hhmaizebeansbeanother_q78a

replace hhmaizebeansbeanother_q78a = "0" in 7
replace hhmaizebeansbeanother_q78a = "0" in 8
replace hhmaizebeansbeanother_q78a = "0" in 9
replace hhmaizebeansbeanother_q78b = "0" in 7
replace hhmaizebeansbeanother_q78b = "0" in 8
replace hhmaizebeansbeanother_q78b = "0" in 9
replace hhmaizebeansbeanother_q78c = "0" in 7
replace hhmaizebeansbeanother_q78c = "0" in 8
replace hhmaizebeansbeanother_q78c = "0" in 9
drop in 1528/1677

destring hhmaizebeansbeanother_q78a, replace force float

rename hhmaizebeansbeanother_q7898 hhmaizebeansbeanother_q78m

sort hhmaizebeansbeanq79
replace hhmaizebeansbeanq79 = "98" in 1523
replace hhmaizebeansbeanq79 = "98" in 1524
replace hhmaizebeansbeanq79 = "98" in 1525
replace hhmaizebeansbeanq79 = "98" in 1526
replace hhmaizebeansbeanq79 = "98" in 1527

sort hhmaizebeansbeanq79
replace hhmaizebeansbeanq79 = "1" in 1123
replace hhmaizebeansbeanq79 = "1" in 1124
replace hhmaizebeansbeanq79 = "1" in 1125
replace hhmaizebeansbeanq79 = "1" in 1126
replace hhmaizebeansbeanq79 = "1" in 1127
replace hhmaizebeansbeanq79 = "1" in 1128
replace hhmaizebeansbeanq79 = "1" in 1141
replace hhmaizebeansbeanq79 = "1" in 1142
replace hhmaizebeansbeanq79 = "1" in 1143
replace hhmaizebeansbeanq79 = "1" in 1144
replace hhmaizebeansbeanq79 = "2" in 1145
replace hhmaizebeansbeanq79 = "2" in 1146
replace hhmaizebeansbeanq79 = "2" in 1147
replace hhmaizebeansbeanq79 = "2" in 1148
replace hhmaizebeansbeanq79 = "2" in 1149
replace hhmaizebeansbeanq79 = "2" in 1150
replace hhmaizebeansbeanq79 = "2" in 1151
replace hhmaizebeansbeanq79 = "2" in 1152
replace hhmaizebeansbeanq79 = "2" in 1153
replace hhmaizebeansbeanq79 = "3" in 1190
replace hhmaizebeansbeanq79 = "3" in 1191
replace hhmaizebeansbeanq79 = "3" in 1192
replace hhmaizebeansbeanq79 = "3" in 1193
replace hhmaizebeansbeanq79 = "3" in 1194
replace hhmaizebeansbeanq79 = "3" in 1195
replace hhmaizebeansbeanq79 = "3" in 1196
replace hhmaizebeansbeanq79 = "3" in 1197
replace hhmaizebeansbeanq79 = "3" in 1198
replace hhmaizebeansbeanq79 = "3" in 1199
replace hhmaizebeansbeanq79 = "3" in 1240
replace hhmaizebeansbeanq79 = "3" in 1241
replace hhmaizebeansbeanq79 = "3" in 1242
replace hhmaizebeansbeanq79 = "5" in 1285
replace hhmaizebeansbeanq79 = "5" in 1286
replace hhmaizebeansbeanq79 = "5" in 1287
replace hhmaizebeansbeanq79 = "5" in 1288
replace hhmaizebeansbeanq79 = "5" in 1289
replace hhmaizebeansbeanq79 = "5" in 1290
replace hhmaizebeansbeanq79 = "5" in 1291
replace hhmaizebeansbeanq79 = "5" in 1292
replace hhmaizebeansbeanq79 = "5" in 1293
replace hhmaizebeansbeanq79 = "5" in 1294
replace hhmaizebeansbeanq79 = "5" in 1295
replace hhmaizebeansbeanq79 = "5" in 1296
replace hhmaizebeansbeanq79 = "5" in 1297
replace hhmaizebeansbeanq79 = "5" in 1298
replace hhmaizebeansbeanq79 = "5" in 1299
replace hhmaizebeansbeanq79 = "5" in 1300
replace hhmaizebeansbeanq79 = "5" in 1301
replace hhmaizebeansbeanq79 = "5" in 1302
replace hhmaizebeansbeanq79 = "5" in 1303
replace hhmaizebeansbeanq79 = "6" in 1304
replace hhmaizebeansbeanq79 = "6" in 1305
replace hhmaizebeansbeanq79 = "6" in 1306
replace hhmaizebeansbeanq79 = "6" in 1307
replace hhmaizebeansbeanq79 = "6" in 1308
replace hhmaizebeansbeanq79 = "6" in 1309
replace hhmaizebeansbeanq79 = "6" in 1310
replace hhmaizebeansbeanq79 = "6" in 1311
replace hhmaizebeansbeanq79 = "6" in 1312
replace hhmaizebeansbeanq79 = "6" in 1313
replace hhmaizebeansbeanq79 = "6" in 1314
replace hhmaizebeansbeanq79 = "6" in 1315
replace hhmaizebeansbeanq79 = "6" in 1316
replace hhmaizebeansbeanq79 = "6" in 1525
replace hhmaizebeansbeanq79 = "98" in 1526
replace hhmaizebeansbeanq79 = "98" in 1527
destring hhmaizebeansbeanq79, replace float force

label variable hhmaizebeansbeanq79 "hb/if 78=a/b, freq. of recycling"
label variable hhmaizebeansbeanq79 "hb/if 78=a/b, what is the freq. of recycling"

label variable hhmaizebeansbeanq80a "hb/reasn1 use this seed=high yield"
label variable hhmaizebeansbeanq80a "hb/reasn1 use this bn seed=high yield"

sort hhmaizebeansbeanq80a
destring hhmaizebeansbeanq80a, replace force float
destring hhmaizebeansbeanq80b , replace force float

gen HHMAIZEBEANS=., before( hhmaizeq661 )
gen HHMAIZEMILLERQs=., before( hhmaizemiller1q98a )
gen HHMAIZETRADEQs=., before( hhmaizetrader1q102a )

label variable HHMAIZEBEANS "HH MZ- BEAN PRODUCTION"
label variable HHMAIZEMILLERQ "HH MAIZEMILLER RESPONSES"
label variable HHMAIZETRADEQs "HH MMAIZE TRADER RESPONSES"

sort hhmaizebeansbeanq81
encode hhmaizebeansbeanq81, gen( hhmaizebeansbeanq81x)
rename hhmaizebeansbeanq81 hhmaizebeansbeanq81xx
gen hhmaizebeansbeanq81= hhmaizebeansbeanq81x, after( hhmaizebeansbeanq81xx )
replace hhmaizebeansbeanq81=0 if hhmaizebeansbeanq81==1
replace hhmaizebeansbeanq81=1 if hhmaizebeansbeanq81==2
replace hhmaizebeansbeanq81=98 if hhmaizebeansbeanq81==3
label variable hhmaizebeansbeanq81 "hb/satisfiied with qty of bn seed [yes=1/0]"
drop hhmaizebeansbeanq81xx

sort hhmaizebeansbeanq81a
encode hhmaizebeansbeanq81a, gen( hhmaizebeansbeanq81ax)
rename hhmaizebeansbeanq81a hhmaizebeansbeanq81axx
gen hhmaizebeansbeanq81a= hhmaizebeansbeanq81ax, after( hhmaizebeansbeanq81axx )
replace hhmaizebeansbeanq81a=0 if hhmaizebeansbeanq81a==1
replace hhmaizebeansbeanq81a=1 if hhmaizebeansbeanq81a==2
replace hhmaizebeansbeanq81a=98 if hhmaizebeansbeanq81a==3
label variable hhmaizebeansbeanq81a "hb/if 81=no, was the seed fake [yes=1/0] 98=na"
drop hhmaizebeansbeanq81ax hhmaizebeansbeanq81axx

sort hhmaizebeansbeanq81b1
sort hhmaizebeansbeanq81b1
encode hhmaizebeansbeanq81b1, gen( hhmaizebeansbeanq81b1x)
rename hhmaizebeansbeanq81b1 hhmaizebeansbeanq81b1xx
gen hhmaizebeansbeanq81b1= hhmaizebeansbeanq81b1x, after( hhmaizebeansbeanq81b1xx )
replace hhmaizebeansbeanq81b1=0 if hhmaizebeansbeanq81b1==1
replace hhmaizebeansbeanq81b1=1 if hhmaizebeansbeanq81b1==2
replace hhmaizebeansbeanq81b1=98 if hhmaizebeansbeanq81b1==3
drop hhmaizebeansbeanq81b1x hhmaizebeansbeanq81b1xx

sort hhmaizebeansbeanq81b2
encode hhmaizebeansbeanq81b2, gen( hhmaizebeansbeanq81b2x)
rename hhmaizebeansbeanq81b2 hhmaizebeansbeanq81b2xx
gen hhmaizebeansbeanq81b2= hhmaizebeansbeanq81b2x, after( hhmaizebeansbeanq81b2xx )
replace hhmaizebeansbeanq81b2=0 if hhmaizebeansbeanq81b2==1
replace hhmaizebeansbeanq81b2=1 if hhmaizebeansbeanq81b2==2
replace hhmaizebeansbeanq81b2=98 if hhmaizebeansbeanq81b2==3
drop hhmaizebeansbeanq81b2x hhmaizebeansbeanq81b2xx

sort hhmaizebeansbeanq81b3
encode hhmaizebeansbeanq81b3, gen( hhmaizebeansbeanq81b3x)
rename hhmaizebeansbeanq81b3 hhmaizebeansbeanq81b3xx
gen hhmaizebeansbeanq81b3= hhmaizebeansbeanq81b3x, after( hhmaizebeansbeanq81b3xx )
replace hhmaizebeansbeanq81b3=0 if hhmaizebeansbeanq81b3==1
replace hhmaizebeansbeanq81b3=1 if hhmaizebeansbeanq81b3==2
replace hhmaizebeansbeanq81b3=98 if hhmaizebeansbeanq81b3==3
drop hhmaizebeansbeanq81b3x hhmaizebeansbeanq81b3xx

sort hhmaizebeansbeanq81b4
encode hhmaizebeansbeanq81b4, gen( hhmaizebeansbeanq81b4x)
rename hhmaizebeansbeanq81b4 hhmaizebeansbeanq81b4xx
gen hhmaizebeansbeanq81b4= hhmaizebeansbeanq81b4x, after( hhmaizebeansbeanq81b4xx )
replace hhmaizebeansbeanq81b4=0 if hhmaizebeansbeanq81b4==1
replace hhmaizebeansbeanq81b4=1 if hhmaizebeansbeanq81b4==2
replace hhmaizebeansbeanq81b4=98 if hhmaizebeansbeanq81b4==3
drop hhmaizebeansbeanq81b4x hhmaizebeansbeanq81b4xx

sort hhmaizebeansbeanq81b5
encode hhmaizebeansbeanq81b5, gen( hhmaizebeansbeanq81b5x)
rename hhmaizebeansbeanq81b5 hhmaizebeansbeanq81b5xx
gen hhmaizebeansbeanq81b5= hhmaizebeansbeanq81b5x, after( hhmaizebeansbeanq81b5xx )
replace hhmaizebeansbeanq81b5=0 if hhmaizebeansbeanq81b5==1
replace hhmaizebeansbeanq81b5=1 if hhmaizebeansbeanq81b5==2
replace hhmaizebeansbeanq81b5=98 if hhmaizebeansbeanq81b5==3
drop hhmaizebeansbeanq81b5x hhmaizebeansbeanq81b5xx

sort hhmaizebeansbeanq81b6
encode hhmaizebeansbeanq81b6, gen( hhmaizebeansbeanq81b6x)
rename hhmaizebeansbeanq81b6 hhmaizebeansbeanq81b6xx
gen hhmaizebeansbeanq81b6= hhmaizebeansbeanq81b6x, after( hhmaizebeansbeanq81b6xx )
replace hhmaizebeansbeanq81b6=0 if hhmaizebeansbeanq81b6==1
replace hhmaizebeansbeanq81b6=1 if hhmaizebeansbeanq81b6==2
replace hhmaizebeansbeanq81b6=98 if hhmaizebeansbeanq81b6==3
drop hhmaizebeansbeanq81b6x hhmaizebeansbeanq81b6xx

sort hhmaizebeansbeanq81b7
encode hhmaizebeansbeanq81b7, gen( hhmaizebeansbeanq81b7x)
rename hhmaizebeansbeanq81b7 hhmaizebeansbeanq81b7xx
gen hhmaizebeansbeanq81b7= hhmaizebeansbeanq81b7x, after( hhmaizebeansbeanq81b7xx )
replace hhmaizebeansbeanq81b7=0 if hhmaizebeansbeanq81b7==1
replace hhmaizebeansbeanq81b7=1 if hhmaizebeansbeanq81b7==2
replace hhmaizebeansbeanq81b7=98 if hhmaizebeansbeanq81b7==3
drop hhmaizebeansbeanq81b7x hhmaizebeansbeanq81b7xx

sort hhmaizebeansbeanq81b8
encode hhmaizebeansbeanq81b8, gen( hhmaizebeansbeanq81b8x)
rename hhmaizebeansbeanq81b8 hhmaizebeansbeanq81b8xx
gen hhmaizebeansbeanq81b8= hhmaizebeansbeanq81b8x, after( hhmaizebeansbeanq81b8xx )
replace hhmaizebeansbeanq81b8=0 if hhmaizebeansbeanq81b8==1
replace hhmaizebeansbeanq81b8=1 if hhmaizebeansbeanq81b8==2
replace hhmaizebeansbeanq81b8=98 if hhmaizebeansbeanq81b8==3
drop hhmaizebeansbeanq81b8x hhmaizebeansbeanq81b8xx

sort hhmaizebeansbeanq81b9
encode hhmaizebeansbeanq81b9, gen( hhmaizebeansbeanq81b9x)
rename hhmaizebeansbeanq81b9 hhmaizebeansbeanq81b9xx
gen hhmaizebeansbeanq81b9= hhmaizebeansbeanq81b9x, after( hhmaizebeansbeanq81b9xx )
replace hhmaizebeansbeanq81b9=0 if hhmaizebeansbeanq81b9==1
replace hhmaizebeansbeanq81b9=1 if hhmaizebeansbeanq81b9==2
replace hhmaizebeansbeanq81b9=98 if hhmaizebeansbeanq81b9==3
drop hhmaizebeansbeanq81b9x hhmaizebeansbeanq81b9xx

sort hhmaizebeansbeanq81b10
sort hhmaizebeansbeanq81b10
encode hhmaizebeansbeanq81b10, gen( hhmaizebeansbeanq81b10x)
rename hhmaizebeansbeanq81b10 hhmaizebeansbeanq81b10xx
gen hhmaizebeansbeanq81b10= hhmaizebeansbeanq81b10x, after( hhmaizebeansbeanq81b10xx )
replace hhmaizebeansbeanq81b10=0 if hhmaizebeansbeanq81b10==1
replace hhmaizebeansbeanq81b10=1 if hhmaizebeansbeanq81b10==2
replace hhmaizebeansbeanq81b10=98 if hhmaizebeansbeanq81b10==3
drop hhmaizebeansbeanq81b10x hhmaizebeansbeanq81b10xx

sort hhmaizebeansbeanq81b11
sort hhmaizebeansbeanq81b11
encode hhmaizebeansbeanq81b11, gen( hhmaizebeansbeanq81b11x)
rename hhmaizebeansbeanq81b11 hhmaizebeansbeanq81b11xx
gen hhmaizebeansbeanq81b11= hhmaizebeansbeanq81b11x, after( hhmaizebeansbeanq81b11xx )
replace hhmaizebeansbeanq81b11=0 if hhmaizebeansbeanq81b11==1
replace hhmaizebeansbeanq81b11=1 if hhmaizebeansbeanq81b11==2
replace hhmaizebeansbeanq81b11=98 if hhmaizebeansbeanq81b11==3
drop hhmaizebeansbeanq81b11x hhmaizebeansbeanq81b11xx
rename hhmaizebeansbeanother_q81b hhmaizebeansbeanq81b11_oth


label variable hhmaizebeansbeanq81b1 "hb/if 81=no, resn1 disatisfd wth bnseed=lower yiel
label variable hhmaizebeansbeanq81b2 "hb/if 81=no, resn2 disatisfd wth bnseed=need >water
label variable hhmaizebeansbeanq81b3 "hb/if 81=no, resn3 disatisfd wth bnseed=less p&d tol
label variable hhmaizebeansbeanq81b4 "hb/if 81=no, resn4 disatisfd wth bnseed=slow maturity
label variable hhmaizebeansbeanq81b5 "hb/if 81=no, resn5 disatisfd wth bnseed=lower mkt value
label variable hhmaizebeansbeanq81b6 "hb/if 81=no, resn6 disatisfd wth bnseed=poor taste
label variable hhmaizebeansbeanq81b7 "hb/if 81=no, resn7 disatisfd wth bnseed=low germtn %
label variable hhmaizebeansbeanq81b8 "hb/if 81=no, resn8 disatisfd wth bnseed=poor var purity
label variable hhmaizebeansbeanq81b9 "hb/if 81=no, resn9 disatisfd wth bnseed=need >labor
label variable hhmaizebeansbeanq81b10 "hb/if 81=no, resn10 disatisfd wth bnseed=need >comp inpts
label variable hhmaizebeansbeanq81b11 "hb/if 81=no, resn96 disatisfd wth bnseed=other reason
label variable hhmaizebeansbeanq81b11_oth "hb/if 81=no, resn96 disatisfd wth bnseed=other specifd

sort hhmaizebeansbeanq82
encode hhmaizebeansbeanq82, gen( hhmaizebeansbeanq82x)
rename hhmaizebeansbeanq82 hhmaizebeansbeanq82xx
gen hhmaizebeansbeanq82= hhmaizebeansbeanq82x, after( hhmaizebeansbeanq82xx )
replace hhmaizebeansbeanq82=0 if hhmaizebeansbeanq82==1
replace hhmaizebeansbeanq82=1 if hhmaizebeansbeanq82==2
replace hhmaizebeansbeanq82=98 if hhmaizebeansbeanq82==3
drop hhmaizebeansbeanq82xx
label variable hhmaizebeansbeanq82 "hb/wd you plant same bn seed in future [yes=1/0]"

sort hhmaizebeansbeanq83
replace hhmaizebeansbeanq83 = "998" in 1111
replace hhmaizebeansbeanq83 = "998" in 1112
replace hhmaizebeansbeanq83 = "998" in 1113
replace hhmaizebeansbeanq83 = "998" in 1114
replace hhmaizebeansbeanq83 = "998" in 1115
replace hhmaizebeansbeanq84 = "998" in 1111
sort hhmaizebeansbeanq84

destring hhmaizebeansbeanq83, replace force float
destring hhmaizebeansbeanq84, replace force float

label variable hhmaizebeansbeanq83 "hb/qty (kg) of bean seed planted on the selected plot"
label variable hhmaizebeansbeanq84 "hb/if 78 isnt=a, what was cost ofbean seed (ugx)"
label variable hhmaizebeansbeanq85 "hb/number of bean seeds per hill"
sort hhmaizebeansbeanq85
replace hhmaizebeansbeanq85 = "998" in 1111
destring hhmaizebeansbeanq85, replace force float

sort hhmaizebeansbeanq86
replace hhmaizebeansbeanq86 = "n/a" in 7

sort hhmaizebeansbeanq86
encode hhmaizebeansbeanq86, gen( hhmaizebeansbeanq86x)
rename hhmaizebeansbeanq86 hhmaizebeansbeanq86xx
gen hhmaizebeansbeanq86= hhmaizebeansbeanq86x, after( hhmaizebeansbeanq86xx )
replace hhmaizebeansbeanq86=0 if hhmaizebeansbeanq86==1
replace hhmaizebeansbeanq86=1 if hhmaizebeansbeanq86==2
replace hhmaizebeansbeanq86=98 if hhmaizebeansbeanq86==3
drop hhmaizebeansbeanq86xx

sort hhmaizebeansbeanq87
encode hhmaizebeansbeanq87, gen( hhmaizebeansbeanq87x)
rename hhmaizebeansbeanq87 hhmaizebeansbeanq87xx
gen hhmaizebeansbeanq87= hhmaizebeansbeanq87x, after( hhmaizebeansbeanq87xx )
replace hhmaizebeansbeanq87=0 if hhmaizebeansbeanq87==1
replace hhmaizebeansbeanq87=1 if hhmaizebeansbeanq87==2
replace hhmaizebeansbeanq87=98 if hhmaizebeansbeanq87==3
drop hhmaizebeansbeanq87x hhmaizebeansbeanq87xx

destring hhmaizebeansbeanq87a, replace force float

label variable hhmaizebeansbeanq86 "hb/if 70=no, did you app org manure on this bn plot [yes=1/0]"
label variable hhmaizebeansbeanq87 "hb/if 70=no, did you app DAP/NPK on this bn plot [yes=1/0]"
label variable hhmaizebeansbeanq87a "hb/if 70=1, qty of fertzer applied (kg)"

compress


sort hhmaizebeansbeanq88
encode hhmaizebeansbeanq88, gen( hhmaizebeansbeanq88x)
rename hhmaizebeansbeanq88 hhmaizebeansbeanq88xx
gen hhmaizebeansbeanq88= hhmaizebeansbeanq88x, after( hhmaizebeansbeanq88xx )
replace hhmaizebeansbeanq88=0 if hhmaizebeansbeanq88==1
replace hhmaizebeansbeanq88=1 if hhmaizebeansbeanq88==2
replace hhmaizebeansbeanq88=98 if hhmaizebeansbeanq88==3
drop hhmaizebeansbeanq88x hhmaizebeansbeanq88xx

sort hhmaizebeansbeanq88a
destring hhmaizebeansbeanq88a, replace force float

sort hhmaizebeansbeanq89
encode hhmaizebeansbeanq89, gen( hhmaizebeansbeanq89x)
rename hhmaizebeansbeanq89 hhmaizebeansbeanq89xx
gen hhmaizebeansbeanq89= hhmaizebeansbeanq89x, after( hhmaizebeansbeanq89xx )
replace hhmaizebeansbeanq89=0 if hhmaizebeansbeanq89==1
replace hhmaizebeansbeanq89=1 if hhmaizebeansbeanq89==2
replace hhmaizebeansbeanq89=98 if hhmaizebeansbeanq89==3
drop hhmaizebeansbeanq89x hhmaizebeansbeanq89xx

label variable hhmaizebeansbeanq88 "hb/applied urea [yes=1, 0]"
label variable hhmaizebeansbeanq88a "hb/if appld urea on bn plot, what qty (kg)"
label variable hhmaizebeansbeanq89 "hb/was any other fertzer used? [yes=1/0; 98=dk]"

sort hhmaizebeansbeanq90
sort hhmaizebeansbeanq90
destring hhmaizebeansbeanq90, replace force float
label variable hhmaizebeansbeanq90 "hb/if 70=0, freq of weeding in 2018A"

sort hhmaizebeansbeanq91
destring hhmaizebeansbeanq91, replace force float
label variable hhmaizebeansbeanq91 "hb/if 70=0, hw many days did it take to weedg frm plant'g"

sort hhmaizebeansbeanq92
encode hhmaizebeansbeanq92, gen( hhmaizebeansbeanq92x)
rename hhmaizebeansbeanq92 hhmaizebeansbeanq92xx
gen hhmaizebeansbeanq92= hhmaizebeansbeanq92x, after( hhmaizebeansbeanq92xx )
replace hhmaizebeansbeanq92=0 if hhmaizebeansbeanq92==1
replace hhmaizebeansbeanq92=1 if hhmaizebeansbeanq92==2
replace hhmaizebeansbeanq92=98 if hhmaizebeansbeanq92==3
drop hhmaizebeansbeanq92x hhmaizebeansbeanq92xx
label variable hhmaizebeansbeanq92 "hb/if 70=0, did you use any pesticide/fungcd/herbcd [yes=1/0]"


sort hhmaizebeansbeanq93
rename hhmaizebeansbeanq93 hhmaizebeansbeanq93a
gen hhmaizebeansbeanq93b=., after( hhmaizebeansbeanq94 )
gen hhmaizebeansbeanq93c=., after( hhmaizebeansbeanq93b )
gen hhmaizebeansbeanq93x=., before( hhmaizebeansbeanq93a )

destring hhmaizebeansbeanq93a, replace force float
destring hhmaizebeansbeanq94 , replace force float

label variable hhmaizebeansbeanq93 "hb/bean harvest (bags)"
label variable hhmaizebeansbeanq93b "hb/ bean harvest (kgs)"
label variable hhmaizebeansbeanq93c "hb/ bean yield (kgs/ha)"
label variable hhmaizebeansbeanq93x "hb/area under beans (ha) for selected plot"
label variable hhmaizebeansbeanq94 "hb/kgs in one bag of beans"

replace hhmaizebeansbeanq93b= hhmaizebeansbeanq93a* hhmaizebeansbeanq94
replace hhmaizebeansbeanq93b=. if hhmaizebeansbeanq94==998 & hhmaizebeansbeanq94!=.
replace hhmaizebeansbeanq93b=. if hhmaizebeansbeanq93a==999 & hhmaizebeansbeanq93a!=.
replace hhmaizebeansbeanq93b=. if hhmaizebeansbeanq93a==999 & hhmaizebeansbeanq93a!=.

*xxx bean yield
sort hhmaizebeansbeanq93b
replace hhmaizebeansbeanq93x= hhmaizebeansplot_select_area2 *0.404686
replace hhmaizebeansbeanq93c= hhmaizebeansbeanq93b/ hhmaizebeansbeanq93x
sum hhmaizebeansbeanq93x hhmaizebeansbeanq93c
sort hhmaizebeansbeanq93c

*xx HHMAIZEDISPOSAL=., before( hhmaizeq95 )
gen HHMAIZEDISPOSAL=., before( hhmaizeq95 )
label variable HHMAIZEDISPOSAL "HH MAIZE DISPOSITION & MKTING"
label variable hhmaizeq95 "hm/tot maize harvest (bags) in 2018A season"
label variable hhmaizeq96 "hm/kgs in one bag of maize"
label variable hhmaizeq97 "hm/qty of maize kept for seed (kgs)"
label variable hhmaizeq98 "hm/did take some maize to a miller [yes=1/0]"

sort hhmaizeq98
encode hhmaizeq98, gen( hhmaizeq98x)
rename hhmaizeq98 hhmaizeq98xx
gen hhmaizeq98= hhmaizeq98x, after( hhmaizeq98xx )
replace hhmaizeq98=0 if hhmaizeq98==1
replace hhmaizeq98=1 if hhmaizeq98==2
drop hhmaizeq98x hhmaizeq98xx

sort hhmaizemiller1q98a

label variable idmiller1 "hm/ miller1 id"
label variable idmiller2 "hm/ miller2 id"
label variable idmiller3 "hm/ miller3 id"

sort idmiller1
encode idmiller1, gen( idmiller1coded)
encode idmiller2, gen( idmiller2coded)
encode idmiller3, gen( idmiller3coded)

*mm1
label variable hhmaizemiller1q98a "hm/can you give miller name/details? [yes=1/0]"

label variable hhmaizemiller1q98b "hm/village mz miller1"
label variable hhmaizemiller1q98c "hm/subcounty mz miller1"
label variable hhmaizemiller1q98d "hm/distrct mz miller1"
label variable hhmaizemiller1q98e "hm/contact mz miller1"
label variable hhmaizemiller1q98f "hm/extra info to locate mz miller1"
label variable hhmaizemiller1q98g "hm/rate location mz miller1

exit
sort hhmaizemiller1q98g
replace hhmaizemiller1q98g = "98" in 1468
replace hhmaizemiller1q98g = "98" in 1469
replace hhmaizemiller1q98g = "98" in 1470
replace hhmaizemiller1q98g = "98" in 1471

destring hhmaizemiller1q98g, replace force float
destring hhmaizemiller1q98h, replace force float
destring hhmaizemiller1q98i, replace force float
destring hhmaizemiller1q98j, replace force float
destring hhmaizemiller1q98k, replace force float

label variable hhmaizemiller1q98h "hm/rate price mz miller1"
label variable hhmaizemiller1q98i "hm/rate quality of pdt mz miller1"
label variable hhmaizemiller1q98j "hm/rate quality of servc mz miller1"
label variable hhmaizemiller1q98k "hm/rate reputation mz miller1"
label variable hhmaizemiller1q98l "hm/when startd using mz miller1"

sort hhmaizemiller1q98m
encode hhmaizemiller1q98m, gen( hhmaizemiller1q98mx)
rename hhmaizemiller1q98m hhmaizemiller1q98mxx
gen hhmaizemiller1q98m= hhmaizemiller1q98mx, after( hhmaizemiller1q98mxx )
replace hhmaizemiller1q98m=0 if hhmaizemiller1q98m==1
replace hhmaizemiller1q98m=1 if hhmaizemiller1q98m==2
replace hhmaizemiller1q98m=98 if hhmaizemiller1q98m==3
drop hhmaizemiller1q98mx hhmaizemiller1q98mxx


label variable hhmaizemiller1q98m "hm/mz miller1 accepts in kind paymt"
label variable hhmaizemiller1q98n "hm/mz miller1 accepts later paymt"
label variable hhmaizemiller1q98o "hm/if 37=1, is mz miller1 part of the fga/coop [yes=1/0] dk="

*mm2
label variable hhmaizemiller2q99a "hm/can you give miller2 name/details? [yes=1/0]"

label variable hhmaizemiller2q99b "hm/village mz miller2"
label variable hhmaizemiller2q99c "hm/subcounty mz miller2"
label variable hhmaizemiller2q99d "hm/distrct mz miller2"
label variable hhmaizemiller2q99e "hm/contact mz miller2"
label variable hhmaizemiller2q99f "hm/extra info to locate mz miller2"
label variable hhmaizemiller2q99g "hm/rate location mz miller2"

exit
sort hhmaizemiller2q99g
replace hhmaizemiller2q99g = "98" in 
replace hhmaizemiller2q99g = "98" in 
replace hhmaizemiller2q99g = "98" in 
replace hhmaizemiller2q99g = "98" in 

destring hhmaizemiller2q99g, replace force float
destring hhmaizemiller2q99h, replace force float
destring hhmaizemiller2q99i, replace force float
destring hhmaizemiller2q99j, replace force float
destring hhmaizemiller2q99k, replace force float

label variable hhmaizemiller2q99h "hm/rate price mz miller2"
label variable hhmaizemiller2q99i "hm/rate quality of pdt mz miller2"
label variable hhmaizemiller2q99j "hm/rate quality of servc mz miller2"
label variable hhmaizemiller2q99k "hm/rate reputation mz miller2"
label variable hhmaizemiller2q99l "hm/when startd using mz miller2"

label variable hhmaizemiller2q99m "hm/mz miller2 accepts in kind paymt"
label variable hhmaizemiller2q99n "hm/mz miller2 accepts later paymt"
label variable hhmaizemiller2q99o "hm/if 37=1, is mz miller2 part of the fga/coop [yes=1/0] dk="

*mm3
label variable hhmaizemiller3q100a "hm/can you give miller3 name/details? [yes=1/0]"

label variable hhmaizemiller3q100b "hm/village mz miller3"
label variable hhmaizemiller3q100c "hm/subcounty mz miller3"
label variable hhmaizemiller3q100d "hm/distrct mz miller3"
label variable hhmaizemiller3q100e "hm/contact mz miller3"
label variable hhmaizemiller3q100f "hm/extra info to locate mz miller3"
label variable hhmaizemiller3q100g "hm/rate location mz miller3"

exit
sort hhmaizemiller3q100
replace hhmaizemiller3q100g = "98" in 1468
replace hhmaizemiller3q100g = "98" in 1469
replace hhmaizemiller3q100g = "98" in 1470
replace hhmaizemiller3q100g = "98" in 1471

destring hhmaizemiller3q100g, replace force float
destring hhmaizemiller3q100h, replace force float
destring hhmaizemiller3q100i, replace force float
destring hhmaizemiller3q100j, replace force float
destring hhmaizemiller3q100k, replace force float

label variable hhmaizemiller3q100h "hm/rate price mz miller3"
label variable hhmaizemiller3q100i "hm/rate quality of pdt mz miller3"
label variable hhmaizemiller3q100j "hm/rate quality of servc mz miller3"
label variable hhmaizemiller3q100k "hm/rate reputation mz miller3"
label variable hhmaizemiller3q100l "hm/when startd using mz miller3"

label variable hhmaizemiller3q100m "hm/mz miller3 accepts in kind paymt"
label variable hhmaizemiller3q100n "hm/mz miller3 accepts later paymt"
label variable hhmaizemiller3q100o "hm/if 37=3, is mz miller1 part of the fga/coop [yes=1/0] dk="

destring hhmaizemiller1q98l, replace ignore(`"/"') force float
destring hhmaizemiller1q98l, replace ignore(`"/"') force float
destring hhmaizemiller1q98l, replace ignore(`"/"') force float
