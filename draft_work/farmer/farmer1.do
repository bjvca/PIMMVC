import delimited "C:\Users\Ariong Richard\Dropbox (IFPRI)\Ariong_richard\Documents\Maize\MVC\work\farmer\farmers.csv", encoding(ISO-8859-2)

compress
label variable date "hh/date of interview"
label variable q1 "hh/are you the hh head"
label variable q2 "hh/do you take part in most hh decisions"
label variable q3 "hh/is there other person in the hh most knwledgeable about maize activities"
label variable q4 "hh/did you grow maize in season 2018A"

sort q1
rename q1 q1x
encode q1x, gen(q1)
order q1, after(q1x)
recode q1 1=0 2=1
label define q1 0 "No" 1 "Yes", replace
drop q1x

sort q2
rename q2 q2x
encode q2x, gen(q2)
order q2, after(q2x)
recode q2 1=0 2=1 3=99
label define q2 0 "No" 1 "Yes" 99 "NA", replace
drop q2x

sort q3
rename q3 q3x
encode q3x, gen(q3)
order q3, after(q3x)
recode q3 2=99
label define q3 0 "No" 1 "Yes" 99 "NA", replace
drop q3x

sort q4
rename q4 q4x
encode q4x, gen(q4)
order q4, after(q4x)
label define q4 0 "No" 1 "Yes", replace
drop q4x

label variable hhmaizeq13 "hh/distance to nearest tarmac (km)"
label variable hhmaizeq14 "hh/distance to nearest marram (km)"
label variable hhmaizeq15 "hh/distance to nearest mill (km)"

label variable hhmaizeq16 "hh/number of mz mills in this villa"
label variable hhmaizeq17 "hh/distance to nearest agroinput shop (km)"
label variable hhmaizeq18 "hh/number of agroinput shops in the village"
label variable hhmaizeq19 "hh/distance to nearest neighbor (km)"
label variable hhmaizeq20 "hh/distance to nearest trading center (km)"
label variable hhmaizeq21 "hh/distance to house of village LC1 chairman"

label variable hhmaizeq22 "hh/is mobile network coverage at the household"
label define hhmaizeq22 1 "Yes, with good reception" 2 "Yes, with bad reception" 3 "No coverage"

label variable hhmaizeq22a1 "hh/homestead mobile network: 1=MTN"
label variable hhmaizeq22a2 "hh/homestead mobile network: 2=AIRTEL"
label variable hhmaizeq22a3 "hh/homestead mobile network: 3=AFRICEL"
label variable hhmaizeq22a98 "hh/homestead mobile network: 98=DONTKNW"

sort hhmaizeq22a1
rename hhmaizeq22a1 hhmaizeq22a1x
encode hhmaizeq22a1x, gen(hhmaizeq22a1)
order hhmaizeq22a1, after(hhmaizeq22a1x)
recode hhmaizeq22a1 1=0 2=1 3=99
label define hhmaizeq22a1 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq22a1x

sort hhmaizeq22a2
rename hhmaizeq22a2 hhmaizeq22a2x
encode hhmaizeq22a2x, gen(hhmaizeq22a2)
order hhmaizeq22a2, after(hhmaizeq22a2x)
recode hhmaizeq22a2 1=0 2=1 3=99
label define hhmaizeq22a2 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq22a2x

sort hhmaizeq22a3
rename hhmaizeq22a3 hhmaizeq22a3x
encode hhmaizeq22a3x, gen(hhmaizeq22a3)
order hhmaizeq22a3, after(hhmaizeq22a3x)
recode hhmaizeq22a3 1=0 2=1 3=99
label define hhmaizeq22a3 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq22a3x

sort hhmaizeq22a98
rename hhmaizeq22a98 hhmaizeq22a98x
encode hhmaizeq22a98x, gen(hhmaizeq22a98)
order hhmaizeq22a98, after(hhmaizeq22a98x)
recode hhmaizeq22a98 1=0 2=1 3=99
label define hhmaizeq22a98 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq22a98x

sort hhmaizeq23
label variable hhmaizeq23 "hh/respondent r.ship to hh head"
rename hhmaizeq23 hhmaizeq23x
encode hhmaizeq23x, gen(hhmaizeq23)
order hhmaizeq23, after(hhmaizeq23x)
recode hhmaizeq23 7=99
label define hhmaizeq23 1 "spouse" 2 "child" 3 "sibling" 4 "parent" 5 "oth relative" 6 "non-relative" 99 "NA", replace
drop hhmaizeq23x

label variable hhmaizeq24 "hh/respodent age (years)"

label variable hhmaizeq25 "hh/respondent gender (1=F; 2=M)"
sort hhmaizeq25
rename hhmaizeq25 hhmaizeq25x
encode hhmaizeq25x, gen(hhmaizeq25)
order hhmaizeq25, after(hhmaizeq25x)
drop hhmaizeq25x

sort hhmaizeq26
label variable hhmaizeq26 "hh/respondent marital status"
rename hhmaizeq26 hhmaizeq26x
encode hhmaizeq26x, gen(hhmaizeq26)
order hhmaizeq26, after(hhmaizeq26x)
label define hhmaizeq26 1 "Married" 2 "Widowed" 3 "Divorced" 4 "Separated" 5 "Single", replace
drop hhmaizeq26x

sort hhmaizeq27
label variable hhmaizeq27 "hh/respondent education level
rename hhmaizeq27 hhmaizeq27x
encode hhmaizeq27x, gen( hhmaizeq27)
order hhmaizeq27, after(hhmaizeq27x)
label define hhmaizeq27 1 "no formal educ" 2 "some primary educ" 3 "finished primary educ" 4 "some secondary educ" 5 "finished secondary educ" 6 "higher than seconday educ" 7 "other", replace
drop hhmaizeq27x

label variable hhmaizeq28 "hh/number of hh members (hh size)"
label variable hhmaizeq28a "hh/number of females"
label variable hhmaizeq28b "hh/number of hh members <15"
label variable hhmaizeq28b "hh/number of hh members <15"
label variable hhmaizeq28c "hh/number of hh female members <15"

label variable hhmaizeq29 "hh/key hh income source"
label variable hhmaizeother_q29 "hh/hh key income source specify other"
sort hhmaizeq29
replace hhmaizeq29 = "z96" in 1
replace hhmaizeq29 = "z96" in 2
replace hhmaizeq29 = "z96" in 3
replace hhmaizeq29 = "z96" in 4
replace hhmaizeq29 = "z96" in 5
replace hhmaizeq29 = "z96" in 6
replace hhmaizeq29 = "z96" in 7
replace hhmaizeq29 = "z96" in 8
replace hhmaizeq29 = "z96" in 9
replace hhmaizeq29 = "z96" in 10
replace hhmaizeq29 = "z96" in 11
replace hhmaizeq29 = "z96" in 12
replace hhmaizeq29 = "z96" in 13
replace hhmaizeq29 = "z96" in 14
replace hhmaizeq29 = "z96" in 15
replace hhmaizeq29 = "z96" in 16
replace hhmaizeq29 = "z96" in 17
replace hhmaizeq29 = "z96" in 18
replace hhmaizeq29 = "z96" in 19
replace hhmaizeq29 = "z96" in 20
sort hhmaizeq29
label variable hhmaizeq29 "hh/respondent education level
rename hhmaizeq29 hhmaizeq29x
encode hhmaizeq29x, gen( hhmaizeq29)
order hhmaizeq29, after(hhmaizeq29x)
recode hhmaizeq29 9=96
label define hhmaizeq29 1 "Crop farming" 2 "Livestock farmg" 3 "Formal employmt with gvt" 4 "Formal employmt with private comp" 5 "Agricultiral wage labor" 6 "Self employed" 7 "Remittances" 8 "small business" 96 "Other", replace
drop hhmaizeq29x

sort hhmaizeq30
label variable hhmaizeq30 "hh/number of rooms in the main house"

label variable hhmaizeq31a "hh/house lighting=electricity"
label variable hhmaizeq31b "hh/house lighting=generator"
label variable hhmaizeq31c "hh/house lighting=solar"
label variable hhmaizeq31d "hh/house lighting=kerosen lamp"
label variable hhmaizeq31e "hh/house lighting=candle"
label variable hhmaizeq31f "hh/house lighting=torch"
label variable hhmaizeq31g "hh/house lighting=biogas"
rename hhmaizeq3196 hhmaizeq31h
rename hhmaizeother_q31 hhmaizeq31h_othe
label variable hhmaizeq31h "hh/household lighting=other"
label variable hhmaizeq31h_othe "hh/household lighting other specified"

sort hhmaizeq31a
rename hhmaizeq31a hhmaizeq31ax
encode hhmaizeq31ax, gen(hhmaizeq31a)
order hhmaizeq31a, after(hhmaizeq31ax)
recode hhmaizeq31a 1=0 2=1
label define hhmaizeq31a 0 "True/Yes" 1 "False/No", replace
drop hhmaizeq31ax

sort hhmaizeq31b
rename hhmaizeq31b hhmaizeq31bx
encode hhmaizeq31bx, gen(hhmaizeq31b)
order hhmaizeq31b, after(hhmaizeq31bx)
recode hhmaizeq31b 1=0 2=1
label define hhmaizeq31b 0 "No" 1 "Yes", replace
drop hhmaizeq31bx

sort hhmaizeq31c
rename hhmaizeq31c hhmaizeq31cx
encode hhmaizeq31cx, gen(hhmaizeq31c)
order hhmaizeq31c, after(hhmaizeq31cx)
recode hhmaizeq31c 1=0 2=1
label define hhmaizeq31c 0 "No" 1 "Yes", replace
drop hhmaizeq31cx

sort hhmaizeq31d
rename hhmaizeq31d hhmaizeq31dx
encode hhmaizeq31dx, gen(hhmaizeq31d)
order hhmaizeq31d, after(hhmaizeq31dx)
recode hhmaizeq31d 1=0 2=1
label define hhmaizeq31d 0 "No" 1 "Yes", replace
drop hhmaizeq31dx

sort hhmaizeq31e
rename hhmaizeq31e hhmaizeq31ex
encode hhmaizeq31ex, gen(hhmaizeq31e)
order hhmaizeq31e, after(hhmaizeq31ex)
recode hhmaizeq31e 1=0 2=1
label define hhmaizeq31e 0 "No" 1 "Yes", replace
drop hhmaizeq31ex

sort hhmaizeq31f
rename hhmaizeq31f hhmaizeq31fx
encode hhmaizeq31fx, gen(hhmaizeq31f)
order hhmaizeq31f, after(hhmaizeq31fx)
recode hhmaizeq31f 1=0 2=1
label define hhmaizeq31f 0 "No" 1 "Yes", replace
drop hhmaizeq31fx

sort hhmaizeq31g
rename hhmaizeq31g hhmaizeq31gx
encode hhmaizeq31gx, gen(hhmaizeq31g)
order hhmaizeq31g, after(hhmaizeq31gx)
recode hhmaizeq31g 1=0 2=1
label define hhmaizeq31g 0 "No" 1 "Yes", replace
drop hhmaizeq31gx

sort hhmaizeq31h
rename hhmaizeq31h hhmaizeq31hx
encode hhmaizeq31hx, gen(hhmaizeq31h)
order hhmaizeq31h, after(hhmaizeq31hx)
recode hhmaizeq31h 1=0 2=1
label define hhmaizeq31h 0 "No" 1 "Yes", replace
drop hhmaizeq31hx

sort hhmaizeq32
label variable hhmaizeq32 "hh/roofing material of main house"
rename hhmaizeq32 hhmaizeq32x
encode hhmaizeq32x, gen(hhmaizeq32)
order hhmaizeq32, after(hhmaizeq32x)
label define hhmaizeq32 1 "Grass" 2 "Iron sheets" 3 "Tiles" 4 "Other", replace
drop hhmaizeq32x

destring hhmaizeq33, replace ignore(`"/"') force float
label variable hhmaizeq33 "hm/when started growing maize"

sort hhmaizeq34
label variable hhmaizeq34 "hm/did you also grow beans in 2018A?"
rename hhmaizeq34 hhmaizeq34x
encode hhmaizeq34x, gen(hhmaizeq34)
order hhmaizeq34, after(hhmaizeq34x)
recode hhmaizeq34 1=0 2=1
label define hhmaizeq34 1 "Yes" 0 "No", replace
drop hhmaizeq34x

destring hhmaizeq35, replace ignore(`"/"') force float

label variable hhmaizeq35 "hm/when started growing beans"
label variable hhmaizeq36 "hm/total land size (ac)"
label variable hhmaizeq37 "hm/belong to a farmer group [fg]"

sort hhmaizeq37
rename hhmaizeq37 hhmaizeq37x
encode hhmaizeq37x, gen(hhmaizeq37)
order hhmaizeq37, after(hhmaizeq37x)
recode hhmaizeq37 1=0 2=1
label define hhmaizeq37 1 "Yes" 0 "No", replace
drop hhmaizeq37x

sort hhmaizeq38
destring hhmaizeq38, replace ignore(`"/"') force float

rename hhmaizeq3998 hhmaizeq39m
label variable hhmaizeq39a "hm/aware of variety1=longe10H"
label variable hhmaizeq39b "hm/aware of variety2=longe7H"
label variable hhmaizeq39c "hm/aware of variety3=longe7R"
label variable hhmaizeq39d "hm/aware of variety4=bazooka"
label variable hhmaizeq39e "hm/aware of variety5=longe6H"
label variable hhmaizeq39f "hm/aware of variety6=longe5"
label variable hhmaizeq39g "hm/aware of variety7=longe4"
label variable hhmaizeq39h "hm/aware of variety8=panner"
label variable hhmaizeq39i "hm/aware of variety9=wema"
label variable hhmaizeq39j "hm/aware of variety10=kh series"
label variable hhmaizeq39k "hm/aware of variety11=land races"
label variable hhmaizeq39l "hm/aware of variety96=oth"
label variable hhmaizeq39m "hm/aware of variety98=don't knw"

sort hhmaizeq39a
rename hhmaizeq39a hhmaizeq39ax
encode hhmaizeq39ax, gen(hhmaizeq39a)
order hhmaizeq39a, after(hhmaizeq39ax)
recode hhmaizeq39a 1=0 2=1
label define hhmaizeq39a 1 "True/Yes" 0 "False/No", replace
drop hhmaizeq39ax

rename hhmaizeq39b hhmaizeq39bx
rename hhmaizeq39c hhmaizeq39cx
rename hhmaizeq39d hhmaizeq39dx
rename hhmaizeq39e hhmaizeq39ex
rename hhmaizeq39f hhmaizeq39fx
rename hhmaizeq39g hhmaizeq39gx
rename hhmaizeq39h hhmaizeq39hx
rename hhmaizeq39i hhmaizeq39ix
rename hhmaizeq39j hhmaizeq39jx
rename hhmaizeq39k hhmaizeq39kx
rename hhmaizeq39l hhmaizeq39lx
rename hhmaizeq39m hhmaizeq39mx

encode hhmaizeq39bx, gen(hhmaizeq39b)
encode hhmaizeq39cx, gen(hhmaizeq39c)
encode hhmaizeq39dx, gen(hhmaizeq39d)
encode hhmaizeq39ex, gen(hhmaizeq39e)
encode hhmaizeq39fx, gen(hhmaizeq39f)
encode hhmaizeq39gx, gen(hhmaizeq39g)
encode hhmaizeq39hx, gen(hhmaizeq39h)
encode hhmaizeq39ix, gen(hhmaizeq39i)
encode hhmaizeq39jx, gen(hhmaizeq39j)
encode hhmaizeq39kx, gen(hhmaizeq39k)
encode hhmaizeq39lx, gen(hhmaizeq39l)
encode hhmaizeq39mx, gen(hhmaizeq39m)

order hhmaizeq39b, after(hhmaizeq39bx)
order hhmaizeq39c, after(hhmaizeq39cx)
order hhmaizeq39d, after(hhmaizeq39dx)
order hhmaizeq39e, after(hhmaizeq39ex)
order hhmaizeq39f, after(hhmaizeq39fx)
order hhmaizeq39g, after(hhmaizeq39gx)
order hhmaizeq39h, after(hhmaizeq39hx)
order hhmaizeq39i, after(hhmaizeq39ix)
order hhmaizeq39j, after(hhmaizeq39jx)
order hhmaizeq39k, after(hhmaizeq39kx)
order hhmaizeq39l, after(hhmaizeq39lx)
order hhmaizeq39m, after(hhmaizeq39mx)

recode hhmaizeq39b 1=0 2=1
recode hhmaizeq39c 1=0 2=1
recode hhmaizeq39d 1=0 2=1
recode hhmaizeq39e 1=0 2=1
recode hhmaizeq39f 1=0 2=1
recode hhmaizeq39g 1=0 2=1
recode hhmaizeq39h 1=0 2=1
recode hhmaizeq39i 1=0 2=1
recode hhmaizeq39j 1=0 2=1
recode hhmaizeq39k 1=0 2=1
recode hhmaizeq39l 1=0 2=1
recode hhmaizeq39m 1=0 2=1

label define hhmaizeq39b 1 "Yes" 0 "No", replace
label define hhmaizeq39c 1 "Yes" 0 "No", replace
label define hhmaizeq39d 1 "Yes" 0 "No", replace
label define hhmaizeq39e 1 "Yes" 0 "No", replace
label define hhmaizeq39f 1 "Yes" 0 "No", replace
label define hhmaizeq39g 1 "Yes" 0 "No", replace
label define hhmaizeq39h 1 "Yes" 0 "No", replace
label define hhmaizeq39i 1 "Yes" 0 "No", replace
label define hhmaizeq39j 1 "Yes" 0 "No", replace
label define hhmaizeq39k 1 "Yes" 0 "No", replace
label define hhmaizeq39l 1 "Yes" 0 "No", replace
label define hhmaizeq39m 1 "Yes" 0 "No", replace

drop hhmaizeq39bx hhmaizeq39cx hhmaizeq39dx hhmaizeq39ex hhmaizeq39fx hhmaizeq39gx hhmaizeq39hx hhmaizeq39ix hhmaizeq39jx hhmaizeq39kx hhmaizeq39lx hhmaizeq39mx

label variable hhmaizeq40 "hm/number of fields (plots) grew maize in season 2018A" 
label variable hhmaizeplot_count "hm/maize (mz) plot count"

label variable hhmaizeplot1plot_num "hm/mz plot1 number"
label variable hhmaizeplot1q42 "hm/mz plot1 size(ac)"

label variable hhmaizeplot2plot_num "hm/mz plot2 number"
label variable hhmaizeplot2q42 "hm/mz plot2 size(ac)"

label variable hhmaizeplot3plot_num "hm/mz plot3 number"
label variable hhmaizeplot3q42 "hm/mz plot3 size(ac)"

label variable hhmaizeplot4plot_num "hm/mz plot4 number"
label variable hhmaizeplot4q42 "hm/mz plot4 size(ac)"

label variable hhmaizeplot5plot_num "hm/mz plot5 number"
label variable hhmaizeplot5q42 "hm/mz plot5 size(ac)"

label variable hhmaizeplot6plot_num "hm/mz plot6 number"
label variable hhmaizeplot6q42 "hm/mz plot6 size(ac)"


sort hhmaizeplot2q42
destring hhmaizeplot2plot_num, replace force float
replace hhmaizeplot2plot_num=99 if hhmaizeplot2plot_num==.
destring hhmaizeplot2q42, replace force float

sort hhmaizeplot3plot_num
destring hhmaizeplot3plot_num, replace force float
replace hhmaizeplot3plot_num=99 if hhmaizeplot3plot_num==.
destring hhmaizeplot3q42, replace force float

sort hhmaizeplot4plot_num
destring hhmaizeplot4plot_num, replace force float
replace hhmaizeplot4plot_num=99 if hhmaizeplot4plot_num==.
destring hhmaizeplot4q42, replace force float

sort hhmaizeplot5plot_num
destring hhmaizeplot5plot_num, replace force float
replace hhmaizeplot5plot_num=99 if hhmaizeplot5plot_num==.
destring hhmaizeplot5q42, replace force float

sort hhmaizeplot6plot_num
destring hhmaizeplot6plot_num, replace force float
replace hhmaizeplot6plot_num=99 if hhmaizeplot6plot_num==.
destring hhmaizeplot6q42, replace force float

sort hhmaizeplot_calc1

label variable hhmaizeplot_calc1 "hm/maize plot_calc1"
label variable hhmaizeplot_calc2 "hm/maize plot_calc2"

label variable hhmaizeplot_select "hm/selected mz plot [#]"
label variable hhmaizeplot_select_area "hm/selected mz plot area (ac)"
destring hhmaizeplot_select_area, replace force float

label variable hhmaizeorder "hm/maize/order"

label variable hhmaizemaizenq43 "hm/when farmer started cultivating selected plot"
destring hhmaizemaizenq43, replace ignore(`"/"') force float
sort hhmaizemaizenq43

label variable hhmaizemaizenq44 "hm/distance (km) from hh to mz plot"
label variable hhmaizemaizenq45 "hm/rating of mz plot soil fertility"
label variable hhmaizemaizenq46 "hm/intercropped in 2018A"

sort hhmaizemaizenq45
replace hhmaizemaizenq45 = "99" in 1524
replace hhmaizemaizenq45 = "99" in 1525
replace hhmaizemaizenq45 = "99" in 1526
replace hhmaizemaizenq45 = "99" in 1527

sort hhmaizemaizenq45
rename hhmaizemaizenq45 hhmaizemaizenq45x
encode hhmaizemaizenq45x, gen(hhmaizemaizenq45)
order hhmaizemaizenq45, after(hhmaizemaizenq45x)
recode hhmaizemaizenq45 6=99
label define hhmaizemaizenq45 1 "Very poor soil fertility" 5 "Extreamly fertile soil" 99 "NA", replace
drop hhmaizemaizenq45x

sort hhmaizemaizenq46
label variable hhmaizemaizenq46 "hm/intercropped mz plot in 2018A"
rename hhmaizemaizenq46 hhmaizemaizenq46x
encode hhmaizemaizenq46x, gen(hhmaizemaizenq46)
order hhmaizemaizenq46, after(hhmaizemaizenq46x)
recode hhmaizemaizenq46 1=98 2=0 3=1
label define hhmaizemaizenq46 1 "Yes" 0 "No" 98 "Don't know", replace
drop hhmaizemaizenq46x

sort hhmaizemaizenq46a1
rename hhmaizemaizenq46a1 hhmaizemaizenq46a1x
egen hhmaizemaizenq46a1xx= group( hhmaizemaizenq46a1x )
gen hhmaizemaizenq46a1= hhmaizemaizenq46a1xx, after( hhmaizemaizenq46a1x )
replace hhmaizemaizenq46a1=0 if hhmaizemaizenq46a1==1
replace hhmaizemaizenq46a1=1 if hhmaizemaizenq46a1==2
replace hhmaizemaizenq46a1=98 if hhmaizemaizenq46a1==3
drop hhmaizemaizenq46a1x hhmaizemaizenq46a1xx

tostring hhmaizemaizenq46a1, replace force
sort hhmaizemaizenq46a1
label variable hhmaizemaizenq46a1 "hm/intercropped mz plot in season 2018A"
rename hhmaizemaizenq46a1 hhmaizemaizenq46a1x
encode hhmaizemaizenq46a1x, gen(hhmaizemaizenq46a1)
order hhmaizemaizenq46a1, after(hhmaizemaizenq46a1x)
recode hhmaizemaizenq46a1 1=0 2=1 3=98
label define hhmaizemaizenq46a1 1 "Yes" 0 "No" 98 "Don't know", replace
drop hhmaizemaizenq46a1x


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

sort hhmaizemaizenq46a96
rename hhmaizemaizenq46a96 hhmaizemaizenq46a96x
egen hhmaizemaizenq46a96xx= group( hhmaizemaizenq46a96x )
gen hhmaizemaizenq46a96= hhmaizemaizenq46a96xx, after( hhmaizemaizenq46a96x )
replace hhmaizemaizenq46a96=0 if hhmaizemaizenq46a96==1
replace hhmaizemaizenq46a96=1 if hhmaizemaizenq46a96==2
replace hhmaizemaizenq46a96=98 if hhmaizemaizenq46a96==3
drop hhmaizemaizenq46a96x hhmaizemaizenq46a96xx

label variable hhmaizemaizenq46a1 "hm/maize intercrop1=beans"
label variable hhmaizemaizenq46a2 "hm/maize intercrop2=soybean"
label variable hhmaizemaizenq46a3 "hm/maize intercrop3=gnuts"
label variable hhmaizemaizenq46a4 "hm/maize intercrop4=cassava"
label variable hhmaizemaizenq46a5 "hm/maize intercrop5=millet"
label variable hhmaizemaizenq46a6 "hm/maize intercrop6=sorghm"
label variable hhmaizemaizenq46a96 "hm/maize intercrop7=other"

sort hhmaizemaizenq46b
label variable hhmaizemaizenq46b "hm/percent of mz in intercrop"
destring hhmaizemaizenq46b, replace force float
replace hhmaizemaizenq46b=100 if hhmaizemaizenq46==0

sort hhmaizemaizenq47
replace hhmaizemaizenq47 = "z98" in 1
replace hhmaizemaizenq47 = "z98" in 2
replace hhmaizemaizenq47 = "z98" in 3
label variable hhmaizemaizenq47 "hm/grew maize on selected plot in previous yr2017 (Yes=1/0)"
rename hhmaizemaizenq47 hhmaizemaizenq47x
encode hhmaizemaizenq47x, gen(hhmaizemaizenq47)
order hhmaizemaizenq47, after(hhmaizemaizenq47x)
recode hhmaizemaizenq47 3=98 2=1 1=0
label define hhmaizemaizenq47 1 "Yes" 0 "No" 98 "Don't know", replace
drop hhmaizemaizenq47x

sort hhmaizemaizenq48
replace hhmaizemaizenq48 = "z98" in 1
replace hhmaizemaizenq48 = "z98" in 2
replace hhmaizemaizenq48 = "z98" in 3
replace hhmaizemaizenq48 = "z98" in 4
replace hhmaizemaizenq48 = "z98" in 5 
replace hhmaizemaizenq48 = "z98" in 6
replace hhmaizemaizenq48 = "z98" in 7
replace hhmaizemaizenq48 = "z98" in 8
replace hhmaizemaizenq48 = "z98" in 9
replace hhmaizemaizenq48 = "z98" in 10 
replace hhmaizemaizenq48 = "z98" in 21
replace hhmaizemaizenq48 = "z98" in 22
replace hhmaizemaizenq48 = "z98" in 23
replace hhmaizemaizenq48 = "z98" in 24
replace hhmaizemaizenq48 = "z98" in 25 
replace hhmaizemaizenq48 = "z98" in 26
replace hhmaizemaizenq48 = "z98" in 27
replace hhmaizemaizenq48 = "z98" in 28
replace hhmaizemaizenq48 = "z98" in 29
replace hhmaizemaizenq48 = "z98" in 30 

label variable hhmaizemaizenq48 "hm/maize variety planted on selected plot"
rename hhmaizemaizenq48 hhmaizemaizenq48x
encode hhmaizemaizenq48x, gen(hhmaizemaizenq48)
order hhmaizemaizenq48, after(hhmaizemaizenq48x)
recode hhmaizemaizenq48 12=96 13=98
label define hhmaizemaizenq48 1 "Longe10H" 2 "Longe7H" 3 "Longe7R" 4 "Bazooka" 5 "Longe6H" 6 "Longe5 (nalo)" 7 "Longe 4" 8 "Panner" 9 "Wema" 10 "KH Series" 11 "Land race" 96 "Other" 98 "Don't know", replace
drop hhmaizemaizenq48x

sort hhmaizemaizenq49
replace hhmaizemaizenq49 = "z96" in 1
replace hhmaizemaizenq49 = "z96" in 2
replace hhmaizemaizenq49 = "z96" in 3
replace hhmaizemaizenq49 = "z96" in 4
replace hhmaizemaizenq49 = "z96" in 5
replace hhmaizemaizenq49 = "z96" in 6
replace hhmaizemaizenq49 = "z96" in 7
replace hhmaizemaizenq49 = "z96" in 8
replace hhmaizemaizenq49 = "z96" in 9
replace hhmaizemaizenq49 = "z96" in 10
replace hhmaizemaizenq49 = "z96" in 11
replace hhmaizemaizenq49 = "z96" in 12
replace hhmaizemaizenq49 = "z96" in 13
replace hhmaizemaizenq49 = "z96" in 14
replace hhmaizemaizenq49 = "z96" in 15
replace hhmaizemaizenq49 = "z96" in 16
replace hhmaizemaizenq49 = "z96" in 17
replace hhmaizemaizenq49 = "z96" in 18
replace hhmaizemaizenq49 = "z96" in 19
replace hhmaizemaizenq49 = "z96" in 20
replace hhmaizemaizenq49 = "z96" in 21
replace hhmaizemaizenq49 = "z96" in 22
replace hhmaizemaizenq49 = "z96" in 23
replace hhmaizemaizenq49 = "z96" in 24

sort hhmaizemaizenq49
label variable hhmaizemaizenq49 "hm/source of maize seed"
rename hhmaizemaizenq49 hhmaizemaizenq49x
encode hhmaizemaizenq49x, gen(hhmaizemaizenq49)
order hhmaizemaizenq49, after(hhmaizemaizenq49x)
recode hhmaizemaizenq49 9=96
label define hhmaizemaizenq49 1 "Own saved seed" 2 "Farmer seed from fellowfarmer" 3 "Bought frm local grain mkt/shop" 4 "Bought frm input shop" 5 "OWC/NAADS" 6 "NGO" 7 "Seed company" 8 "LSB" 9 "Research orgzn" 96 "Other", replace
drop hhmaizemaizenq49x

rename hhmaizemaizenother_q49 hhmaizemaizenq49_oth
label variable hhmaizemaizenq49_oth "hm/other source of maize seed"


sort hhmaizemaizenq50
label variable hhmaizemaizenq50 "hm/frequency of seed recycling"
rename hhmaizemaizenq50 hhmaizemaizenq50x
encode hhmaizemaizenq50x, gen(hhmaizemaizenq50)
order hhmaizemaizenq50, after(hhmaizemaizenq50x)
recode hhmaizemaizenq50 1=98 2=1 3=2 4=3 5=4 6=5 7=6
recode hhmaizemaizenq50 8=99
label define hhmaizemaizenq50 1 "1st time use" 2 "2nd time use" 3 "3rd time use" 4 "4th time use" 5 "5th time used" 6 "More than 5 times" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq50x

rename hhmaizemaizenother_q51 hhmaizemaizenq51_oth
rename hhmaizemaizenq51_oth hhmaizemaizenq51k_oth
rename hhmaizemaizenq51k_oth hhmaizemaizenq51k

sort hhmaizemaizenq51k

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
label variable hhmaizemaizenq51k "hm/other reason why seed was used"

notes hhmaizemaizenq51a: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51b: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51c: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51d: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51e: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51f: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51g: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51h: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51i: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51j: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed
notes hhmaizemaizenq51k: This is on an idea of the reasons why farmer used this particular maize seed on selected plot. The rating is on a scale of 1 to 5, on this this dimension for the decision to use this seed

sort hhmaizemaizenq52
label variable hhmaizemaizenq52 "hm/were you satisfied with quality of mz seed material planted"
rename hhmaizemaizenq52 hhmaizemaizenq52x
encode hhmaizemaizenq52x, gen(hhmaizemaizenq52)
order hhmaizemaizenq52, after(hhmaizemaizenq52x)
recode hhmaizemaizenq52 1=0 2=1
label define hhmaizemaizenq52 1 "Yes" 0 "No", replace
drop hhmaizemaizenq52x

sort hhmaizemaizenq52a
label variable hhmaizemaizenq52a "hm/Were the seeds fake?"
rename hhmaizemaizenq52a hhmaizemaizenq52ax
encode hhmaizemaizenq52ax, gen(hhmaizemaizenq52a)
order hhmaizemaizenq52a, after(hhmaizemaizenq52ax)
recode hhmaizemaizenq52a 1=98 2=0 3=1 4=99
label define hhmaizemaizenq52a 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq52ax

notes hhmaizemaizenq52b1: Why not satisfied with the seed?
notes hhmaizemaizenq52b2: Why not satisfied with the seed?
notes hhmaizemaizenq52b3: Why not satisfied with the seed?
notes hhmaizemaizenq52b4: Why not satisfied with the seed?
notes hhmaizemaizenq52b5: Why not satisfied with the seed?
notes hhmaizemaizenq52b6: Why not satisfied with the seed?
notes hhmaizemaizenq52b7: Why not satisfied with the seed?
notes hhmaizemaizenq52b8: Why not satisfied with the seed?
notes hhmaizemaizenq52b9: Why not satisfied with the seed?
notes hhmaizemaizenq52b10: Why not satisfied with the seed?
notes hhmaizemaizenq52b96: Why not satisfied with the seed?

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
rename hhmaizemaizenq52b96 hhmaizemaizenq52b11x

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

label variable hhmaizemaizenq52b1 "hm/reasn1 not satisfd wth seed=low yield than expected"
label variable hhmaizemaizenq52b2 "hm/reasn2 not satisfd wth seed=need more water than expected"
label variable hhmaizemaizenq52b3 "hm/reasn3 not satisfd wth seed=less tol to p&d"
label variable hhmaizemaizenq52b4 "hm/reasn4 not satisfd wth seed=slower maturity"
label variable hhmaizemaizenq52b5 "hm/reasn5 not satisfd wth seed=lower mkt value"
label variable hhmaizemaizenq52b6 "hm/reasn6 not satisfd wth seed=Not taste as good"
label variable hhmaizemaizenq52b7 "hm/reasn7 not satisfd wth seed=lower germinatn rate"
label variable hhmaizemaizenq52b8 "hm/reasn8 not satisfd wth seed=poor variety purity"
label variable hhmaizemaizenq52b9 "hm/reasn9 not satisfd wth seed=requires much more labor"
label variable hhmaizemaizenq52b10 "hm/reasn10 not satisfd wth seed=needs more complementary inputs"
label variable hhmaizemaizenq52b11 "hm/reasn11 not satisfd wth seed=other reason"
label variable hhmaizemaizenq52b11 "hm/reasn11 not satisfd wth seed=other reason specified"

rename hhmaizemaizenother_q52b hhmaizemaizenq52b_oth
rename hhmaizemaizenq52b_oth hhmaizemaizenq52b11_oth
label variable hhmaizemaizenq52b11_oth "hm/ reasn11 not satisfd with seed specified"

sort hhmaizemaizenq53
label variable hhmaizemaizenq53 "hm/would you use the seed again?"
rename hhmaizemaizenq53 hhmaizemaizenq53x
encode hhmaizemaizenq53x, gen(hhmaizemaizenq53)
order hhmaizemaizenq53, after(hhmaizemaizenq53x)
recode hhmaizemaizenq53 1=0 2=1
label define hhmaizemaizenq53 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq53x

label variable hhmaizemaizenq53 "hm/would you use this seed again in future"
label variable hhmaizemaizenq54 "hm/quantity (kg) of seed planted on the selected mz plot"
label variable hhmaizemaizenq55 "hm/cost of maize seed [free=0]"
label variable hhmaizemaizenq56 "hm/maize plant spacing for the selected plot"

sort hhmaizemaizenq56
rename hhmaizemaizenq56 hhmaizemaizenq56x
encode hhmaizemaizenq56x, gen(hhmaizemaizenq56)
order hhmaizemaizenq56, after(hhmaizemaizenq56x)
recode hhmaizemaizenq56 1=96 2=1 3=2 4=3
label define hhmaizemaizenq56 1 "Broadcast" 2 "2.5x2 ft" 3 "2.5x1 ft" 96 "Other" 99 "NA", replace
drop hhmaizemaizenq56x

sort hhmaizemaizenq57
label variable hhmaizemaizenq57 "hm/seed number per hill (if not broadcast)"
destring hhmaizemaizenq57 , replace force float
replace hhmaizemaizenq57=99 if hhmaizemaizenq57==.

sort hhmaizemaizenq58
replace hhmaizemaizenq58 = "z98" in 1
replace hhmaizemaizenq58 = "z98" in 2
replace hhmaizemaizenq58 = "z98" in 3
replace hhmaizemaizenq58 = "z98" in 4
replace hhmaizemaizenq58 = "z98" in 5
replace hhmaizemaizenq58 = "z98" in 6
replace hhmaizemaizenq58 = "z98" in 7
replace hhmaizemaizenq58 = "z98" in 8
replace hhmaizemaizenq58 = "z98" in 9
label variable hhmaizemaizenq58 "hm/applied org-manure b4 planting for season 2018A"
rename hhmaizemaizenq58 hhmaizemaizenq58x
encode hhmaizemaizenq58x, gen(hhmaizemaizenq58)
order hhmaizemaizenq58, after(hhmaizemaizenq58x)
recode hhmaizemaizenq58 1=0 2=1 3=98
label define hhmaizemaizenq58 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq58x

sort hhmaizemaizenq59
label variable hhmaizemaizenq59 "hm/applied DAP/NPK on mz field [Yes=1, 0]"
rename hhmaizemaizenq59 hhmaizemaizenq59x
encode hhmaizemaizenq59x, gen(hhmaizemaizenq59)
order hhmaizemaizenq59, after(hhmaizemaizenq59x)
recode hhmaizemaizenq59 1=0 2=1 3=98
label define hhmaizemaizenq59 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq59x

sort hhmaizemaizenq59a
label variable hhmaizemaizenq59a "hm/quantity (kg) of fertilizer applied on mz field"
destring hhmaizemaizenq59a, replace force float
replace hhmaizemaizenq59a=0 if hhmaizemaizenq59a==.

sort hhmaizemaizenq60
replace hhmaizemaizenq60 = "z" in 1
replace hhmaizemaizenq60 = "z" in 2
replace hhmaizemaizenq60 = "z" in 3
replace hhmaizemaizenq60 = "z" in 4
replace hhmaizemaizenq60 = "z" in 5
replace hhmaizemaizenq60 = "z" in 6
replace hhmaizemaizenq60 = "z" in 7
replace hhmaizemaizenq60 = "z" in 8
replace hhmaizemaizenq60 = "z" in 9
label variable hhmaizemaizenq60 "hm/applied UREA on mz field [Yes=1, 0]"
rename hhmaizemaizenq60 hhmaizemaizenq60x
encode hhmaizemaizenq60x, gen(hhmaizemaizenq60)
order hhmaizemaizenq60, after(hhmaizemaizenq60x)
recode hhmaizemaizenq60 1=0 2=1 3=98
label define hhmaizemaizenq60 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq60x

sort hhmaizemaizenq60a 
label variable hhmaizemaizenq60a "hm/quantity of UREA applied (kg)"
destring hhmaizemaizenq60a , replace force float
replace hhmaizemaizenq60a=0 if hhmaizemaizenq60==0
replace hhmaizemaizenq60a=998 if hhmaizemaizenq60a==.
replace hhmaizemaizenq60a=998 if hhmaizemaizenq60a==999


label variable hhmaizemaizenq61 "hm/frequency of mz weeding 2018A"
label variable hhmaizemaizenq62 "hm/duration to 1st mz weeding (days)"
destring hhmaizemaizenq62 , replace force float

sort hhmaizemaizenq63
replace hhmaizemaizenq63 = "z" in 1
replace hhmaizemaizenq63 = "z" in 2
replace hhmaizemaizenq63 = "z" in 3
replace hhmaizemaizenq63 = "z" in 4
replace hhmaizemaizenq63 = "z" in 5
replace hhmaizemaizenq63 = "z" in 6
replace hhmaizemaizenq63 = "z" in 7
replace hhmaizemaizenq63 = "z" in 8

label variable hhmaizemaizenq63 "hm/used pestcide/herbcide?"
label variable hhmaizemaizenq64 "hm/maize harvest (bags)"
label variable hhmaizemaizenq65 "hm/kg per bag/ref. unit of measure per bag"

sort hhmaizemaizenq63
rename hhmaizemaizenq63 hhmaizemaizenq63x
encode hhmaizemaizenq63x, gen(hhmaizemaizenq63)
order hhmaizemaizenq63, after(hhmaizemaizenq63x)
recode hhmaizemaizenq63 1=0 2=1 3=98
label define hhmaizemaizenq63 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizemaizenq63x

gen hhmaizemaizenq65b=., after( hhmaizemaizenq65 )
replace hhmaizemaizenq65b= hhmaizemaizenq64* hhmaizemaizenq65
label variable hhmaizemaizenq65b "hm/maize harvest (Kgs) from selected plot"

gen hhmaizemaizenq65c=., after( hhmaizemaizenq65b )
replace hhmaizemaizenq65c= hhmaizeplot_select_area*0.404686
label variable hhmaizemaizenq65c "hm/maize area (ha) planted for selected plot"

gen hhmaizemaizenq65d=., after( hhmaizemaizenq65c )
replace hhmaizemaizenq65d= hhmaizemaizenq65b/ hhmaizemaizenq65c
label variable hhmaizemaizenq65d "hm/maize yield (Kgs/ha)"

sum hhmaizemaizenq65c hhmaizemaizenq65b hhmaizemaizenq65d

sort hhmaizeq661
rename hhmaizeq661 hhmaizeq661x
egen hhmaizeq661xx=group( hhmaizeq661x )
gen hhmaizeq661= hhmaizeq661xx, after(hhmaizeq661x)
sort hhmaizeq661x
sort hhmaizeq661
replace hhmaizeq661=0 if hhmaizeq661==1
replace hhmaizeq661=1 if hhmaizeq661==2
drop hhmaizeq661x hhmaizeq661xx

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
rename hhmaizeq6698 hhmaizeq6616
rename hhmaizeq6616 hhmaizeq6616x

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

sort hhmaizebeansq67
label variable hhmaizebeansq67 "hb/number of bean (bn) plots cultivated in season 2018A"
destring hhmaizebeansq67 , replace force float
replace hhmaizebeansq67=99 if hhmaizebeansq67==.

label variable hhmaizebeansplotb_count "hb/bean plot count"
destring hhmaizebeansplotb_count , replace force float

label variable hhmaizebeansplotb1plot_num1 "hb/bean plot1 number (P#1)"
label variable hhmaizebeansplotb2plot_num1 "hb/bean plot2 number (P#2)"
label variable hhmaizebeansplotb3plot_num1 "hb/bean plot3 number (P#3)"
label variable hhmaizebeansplotb4plot_num1 "hb/bean plot4 number (P#4)"

destring hhmaizebeansplotb1plot_num1 , replace force float
destring hhmaizebeansplotb2plot_num1 , replace force float
destring hhmaizebeansplotb3plot_num1 , replace force float
destring hhmaizebeansplotb4plot_num1 , replace force float

replace hhmaizebeansplotb1plot_num1=99 if hhmaizebeansplotb1plot_num1==.
replace hhmaizebeansplotb2plot_num1=99 if hhmaizebeansplotb2plot_num1==.
replace hhmaizebeansplotb3plot_num1=99 if hhmaizebeansplotb3plot_num1==.
replace hhmaizebeansplotb4plot_num1=99 if hhmaizebeansplotb4plot_num1==.

label variable hhmaizebeansplotb1q69 "hb/bean plot1 area (ac)"
label variable hhmaizebeansplotb2q69 "hb/bean plot2 area (ac)"
label variable hhmaizebeansplotb3q69 "hb/bean plot3 area (ac)"
label variable hhmaizebeansplotb4q69 "hb/bean plot4 area (ac)"

destring hhmaizebeansplotb1q69 , replace force float
destring hhmaizebeansplotb2q69 , replace force float
destring hhmaizebeansplotb3q69 , replace force float
destring hhmaizebeansplotb4q69 , replace force float

sort hhmaizebeansplotb3q69

label variable hhmaizebeansplot_select1 "hb/randomly selected bean plot"
label variable hhmaizebeansplot_select_area2 "hb/area of selected bean plot"
sort hhmaizebeansplot_select_area2
destring hhmaizebeansplot_select1, replace force float
destring hhmaizebeansplot_select_area2, replace force float
replace hhmaizebeansplot_select1=99 if hhmaizebeansplot_select1==.

label variable hhmaizebeansbeanq70 "hb/is selected bn plot same as the selected mz plot"
notes hhmaizebeansbeanq70: "is the randomly selected bn plot same as (intercropped) mz plot that was randomly picked for section of maize?"

sort hhmaizebeansbeanq70
rename hhmaizebeansbeanq70 hhmaizebeansbeanq70x
encode hhmaizebeansbeanq70x, gen(hhmaizebeansbeanq70)
order hhmaizebeansbeanq70, after(hhmaizebeansbeanq70x)
recode hhmaizebeansbeanq70 1=0 2=1 3=98
label define hhmaizebeansbeanq70 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
replace hhmaizebeansbeanq70=99 if hhmaizebeansbeanq70==98
drop hhmaizebeansbeanq70x

label variable hhmaizebeansbeanq71 "hb/when did you start farming on this (selected) bn plot?"
label variable hhmaizebeansbeanq72 "hb/est. distance (km) select bn plot from from residence?"

sort hhmaizebeansbeanq71
destring hhmaizebeansbeanq71, replace ignore(`"/"') force float
destring hhmaizebeansbeanq72, replace force float

destring hhmaizebeansbeanq73, replace force float
replace hhmaizebeansbeanq73=99 if hhmaizebeansbeanq73==.
labe variable hhmaizebeansbeanq73 "hb/on a scale of 1-5, rate the soil quality of bn plot"
notes hhmaizebeansbeanq73: for the scale of 1 to 5, 1 being very poor to 5 being extremely fertile

labe variable hhmaizebeansbeanq74 "hb/was this plot intercropped in season 2018A"
notes hhmaizebeansbeanq74: first season of 2018 (Jan-July)
sort hhmaizebeansbeanq74
rename hhmaizebeansbeanq74 hhmaizebeansbeanq74x
encode hhmaizebeansbeanq74x, gen(hhmaizebeansbeanq74)
order hhmaizebeansbeanq74, after(hhmaizebeansbeanq74x)
recode hhmaizebeansbeanq74 1=0 2=1 3=99
label define hhmaizebeansbeanq74 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizebeansbeanq74x

rename hhmaizebeansbeanq7596 hhmaizebeansbeanq757

notes hhmaizebeansbeanq751: crop intercropped with beans
notes hhmaizebeansbeanq752: crop intercropped with beans
notes hhmaizebeansbeanq753: crop intercropped with beans
notes hhmaizebeansbeanq754: crop intercropped with beans
notes hhmaizebeansbeanq755: crop intercropped with beans
notes hhmaizebeansbeanq756: crop intercropped with beans
notes hhmaizebeansbeanq757: crop intercropped with beans

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

label variable hhmaizebeansbeanq751 "hb/bean intercrop1=maize [yes=1, 0]"
label variable hhmaizebeansbeanq752 "hb/bean intercrop2=soybean [yes=1, 0]"
label variable hhmaizebeansbeanq753 "hb/bean intercrop3=gnuts [yes=1, 0]"
label variable hhmaizebeansbeanq754 "hb/bean intercrop4=cassava [yes=1, 0]"
label variable hhmaizebeansbeanq755 "hb/bean intercrop5=millet [yes=1, 0]"
label variable hhmaizebeansbeanq756 "hb/bean intercrop6=sorghm [yes=1, 0]"
label variable hhmaizebeansbeanq757 "hb/bean intercrop96=other crop [yes=1, 0]"

drop hhmaizebeansbeanq751x hhmaizebeansbeanq751xx hhmaizebeansbeanq752x hhmaizebeansbeanq752xx ///
hhmaizebeansbeanq753x hhmaizebeansbeanq753xx hhmaizebeansbeanq754x hhmaizebeansbeanq754xx ///
hhmaizebeansbeanq755x hhmaizebeansbeanq755xx hhmaizebeansbeanq756x hhmaizebeansbeanq756xx ///
hhmaizebeansbeanq757x hhmaizebeansbeanq757xx

sort hhmaizebeansbeanq76
label variable hhmaizebeansbeanq76 "hb/did you also grow beans on this selectd bn plot in 2017 (previous year)?"
rename hhmaizebeansbeanq76 hhmaizebeansbeanq76x
encode hhmaizebeansbeanq76x, gen(hhmaizebeansbeanq76)
order hhmaizebeansbeanq76, after(hhmaizebeansbeanq76x)
recode hhmaizebeansbeanq76 1=0 2=1 3=99
label define hhmaizebeansbeanq76 1 "Yes" 0 "No" 98 "Don't know" 99 "NA", replace
drop hhmaizebeansbeanq76x

label variable hhmaizebeansbeanq77 "hb/bean variety planted in season 2018A (in selected bn plot)"
sort hhmaizebeansbeanq77
rename hhmaizebeansbeanq77 hhmaizebeansbeanq77x
encode hhmaizebeansbeanq77x, gen(hhmaizebeansbeanq77)
order hhmaizebeansbeanq77, after(hhmaizebeansbeanq77x)
recode hhmaizebeansbeanq77 2=10 3=11 4=12 5=13 6=14 7=15 8=2 9=3 10=4 11=5 12=7 13=8 14=9 15=98 16=99
label define hhmaizebeansbeanq77 1 "NABE4, nambele long" 2 "NABE5" 3 "NABE12C" 4 "NABE15" 5"NABE16" 6 "NABE17" 7 "NABE19" 8 "NAROBEAN1" 9 "NAROBEAN2" 10 "NAROBEAN3" 11 "NAROBEAN4C" 12 "NAROBEAN5C" 13 "Land Race/ Old yellow" 14 "Don’t knw but old var" 15 "Don’t knw but new var" 98 "Don't know" 99 "NA", replace
drop hhmaizebeansbeanq77x

sort hhmaizebeansbeanq78 
replace hhmaizebeansbeanq78 = "z96" in 1
replace hhmaizebeansbeanq78 = "z96" in 2
replace hhmaizebeansbeanq78 = "z96" in 3
label variable hhmaizebeansbeanq78 "hb/Where did you obtain the bean seed from?"
rename hhmaizebeansbeanq78 hhmaizebeansbeanq78x
encode hhmaizebeansbeanq78x, gen(hhmaizebeansbeanq78)
order hhmaizebeansbeanq78, after(hhmaizebeansbeanq78x)
recode hhmaizebeansbeanq78 9=99 10=96
label define hhmaizebeansbeanq78 1 "Own saved seed" 2 "Farmer seed from fellowfarmer" 3 "Bought frm local grain mkt/shop" 4 "Bought frm input shop" 5 "OWC/NAADS" 6 "NGO" 7 "Seed company" 8 "LSB" 9 "Research orgzn" 96 "Other" 99 "NA", replace
drop hhmaizebeansbeanq78x

rename hhmaizebeansbeanother_q7898 hhmaizebeansbeanother_q78m

label variable hhmaizebeansbeanother_q78a "hb/bean seed source1=own saved "
label variable hhmaizebeansbeanother_q78b "hb/bean seed source2=fellow farmer saved seed "
label variable hhmaizebeansbeanother_q78c "hb/bean seed source3=bought frm local mkt/shop "
label variable hhmaizebeansbeanother_q78d "hb/bean seed source4=bought frm agroinput shop "
label variable hhmaizebeansbeanother_q78e "hb/bean seed source5=owc/naads"
label variable hhmaizebeansbeanother_q78f "hb/bean seed source6=ngo"
label variable hhmaizebeansbeanother_q78g "hb/bean seed source7=seed company"
label variable hhmaizebeansbeanother_q78h "hb/bean seed source8=LSB"
label variable hhmaizebeansbeanother_q78i "hb/bean seed source9=from research station"
label variable hhmaizebeansbeanother_q78j "hb/bean seed source10= "
label variable hhmaizebeansbeanother_q78k "hb/bean seed source11= "
label variable hhmaizebeansbeanother_q78l "hb/bean seed source12= "
label variable hhmaizebeansbeanother_q78m "hb/bean seed source98=other source"

sort hhmaizebeansbeanother_q78a

sort hhmaizebeansbeanq79
label variable hhmaizebeansbeanq79 "hb/if 78=a/b, what is the freq. of recycling"
rename hhmaizebeansbeanq79 hhmaizebeansbeanq79x
encode hhmaizebeansbeanq79x, gen(hhmaizebeansbeanq79)
order hhmaizebeansbeanq79, after(hhmaizebeansbeanq79x)
recode hhmaizebeansbeanq79 1=98 2=1 3=2 4=3 5=4 6=5 7=6 8=99 
label define hhmaizebeansbeanq79 1 "1st time used" 2 "2nd time used" 3 "3rd time used" 4 "4th time" 5 "5th time" 6 "More tha 5th time" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq79x

sort hhmaizebeansbeanq80a

label variable hhmaizebeansbeanq80a "hb/rating of bn seed on- high yield potential"
label variable hhmaizebeansbeanq80b "hb/rating of bn seed on- drought tolerance"
label variable hhmaizebeansbeanq80c "hb/rating of bn seed on- p&d tolerance"
label variable hhmaizebeansbeanq80d "hb/rating of bn seed on- early maturity"
label variable hhmaizebeansbeanq80e "hb/rating of bn seed on- higher mkt price; dd"
label variable hhmaizebeansbeanq80f "hb/rating of bn seed on- good taste & nutritin"
label variable hhmaizebeansbeanq80g "hb/rating of bn seed on- low price/for free"
label variable hhmaizebeansbeanq80h "hb/rating of bn seed on- availability"
label variable hhmaizebeansbeanq80i "hb/rating of bn seed on- germintn rate"
label variable hhmaizebeansbeanq80j "hb/rating of bn seed on- experience"
label variable hhmaizebeansbeanq80k "hb/rating of bn seed on- cooking faster"

notes hhmaizebeansbeanq80a: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80a: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80b: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80b: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80c: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80c: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80d: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80d: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80e: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80e: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80f: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80f: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80h: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80h: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80i: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80i: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80j: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80j: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

notes hhmaizebeansbeanq80k: Idea on the reasons why farmer used this particular bean seed on selected bean plot 
notes hhmaizebeansbeanq80k: Please rate, on a scale of 1 to 5, how important this dimension was for your decision to use this seed

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


gen HHMAIZEBEANS=., before( hhmaizeq661 )
gen HHMAIZEMILLER=., before( idmiller1)
gen HHMAIZETRADER=., before( hhmaizeq102)

label variable HHMAIZEBEANS "HH/MAIZE-BEAN PRODUCTION"
label variable HHMAIZEMILLER "HH/MAIZE MILLER"
label variable HHMAIZETRADER "HH/MAIZE TRADER"

sort hhmaizebeansbeanq81
encode hhmaizebeansbeanq81, gen( hhmaizebeansbeanq81x)
rename hhmaizebeansbeanq81 hhmaizebeansbeanq81xx
gen hhmaizebeansbeanq81= hhmaizebeansbeanq81x, after( hhmaizebeansbeanq81xx )
replace hhmaizebeansbeanq81=0 if hhmaizebeansbeanq81==1
replace hhmaizebeansbeanq81=1 if hhmaizebeansbeanq81==2
replace hhmaizebeansbeanq81=98 if hhmaizebeansbeanq81==3
label variable hhmaizebeansbeanq81 "hb/satisfiied with qty of bn seed [yes=1/0]"
drop hhmaizebeansbeanq81xx
drop hhmaizebeansbeanq81x

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

rename hhmaizebeansbeanq81b96 hhmaizebeansbeanq81b11
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
label variable hhmaizebeansbeanq82 "hb/wd you plant same bn seed in future"
rename hhmaizebeansbeanq82 hhmaizebeansbeanq82x
encode hhmaizebeansbeanq82x, gen(hhmaizebeansbeanq82)
order hhmaizebeansbeanq82, after(hhmaizebeansbeanq82x)
recode hhmaizebeansbeanq82 1=0 2=1 3=99 
label define hhmaizebeansbeanq82 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq82x

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
destring hhmaizebeansbeanq85, replace force float
label variable hhmaizebeansbeanq83 "hb/quantity (kg) of bn seed planted on the selected plot"
label variable hhmaizebeansbeanq84 "hb/if 78 isnt=a, what was cost of bean seed (ugx)"
label variable hhmaizebeansbeanq85 "hb/number of bean seeds per hill"

sort hhmaizebeansbeanq85
destring hhmaizebeansbeanq85, replace force float
replace hhmaizebeansbeanq85 = 99 if hhmaizebeansbeanq85==.

sort hhmaizebeansbeanq86
label variable hhmaizebeansbeanq86 "hb/if 70=no, did you app org manure on this bn plot [yes=1/0]"
rename hhmaizebeansbeanq86 hhmaizebeansbeanq86x
encode hhmaizebeansbeanq86x, gen(hhmaizebeansbeanq86)
order hhmaizebeansbeanq86, after(hhmaizebeansbeanq86x)
recode hhmaizebeansbeanq86 1=0 2=1 3=99 
recode hhmaizebeansbeanq86 0=98 1=0 2=1
recode hhmaizebeansbeanq86 4=99 99=1
label define hhmaizebeansbeanq86 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq86x


sort hhmaizebeansbeanq87
replace hhmaizebeansbeanq87 = "z98" in 1
replace hhmaizebeansbeanq87 = "z98" in 2
label variable hhmaizebeansbeanq87 "hb/if 70=no, did you app DAP/NPK on this bn plot [yes=1/0]"
rename hhmaizebeansbeanq87 hhmaizebeansbeanq87x
encode hhmaizebeansbeanq87x, gen(hhmaizebeansbeanq87)
order hhmaizebeansbeanq87, after(hhmaizebeansbeanq87x)
recode hhmaizebeansbeanq87 1=0 2=1 3=99 
label define hhmaizebeansbeanq87 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq87x

destring hhmaizebeansbeanq87a, replace force float
label variable hhmaizebeansbeanq87a "hb/if 70=yes, quantity (kg) of fertzer applied"
replace hhmaizebeansbeanq87a=0 if hhmaizebeansbeanq87==0


sort hhmaizebeansbeanq88
rename hhmaizebeansbeanq88 hhmaizebeansbeanq88x
encode hhmaizebeansbeanq88x, gen( hhmaizebeansbeanq88)
order hhmaizebeansbeanq88, after(hhmaizebeansbeanq88x)
recode hhmaizebeansbeanq88 1=98 2=0 3=1 4=99 
label define hhmaizebeansbeanq88 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
label variable hhmaizebeansbeanq88 "hb/if 70=no, did you app UREA on this bn plot [yes=1/0]"
drop hhmaizebeansbeanq88x

sort hhmaizebeansbeanq88a
destring hhmaizebeansbeanq88a, replace force float
replace hhmaizebeansbeanq88a=99 if hhmaizebeansbeanq88a==.
label variable hhmaizebeansbeanq88a "hb/if appld UREA on bn plot, what quantity (kg)"

sort hhmaizebeansbeanq89
label variable hhmaizebeansbeanq89 "hb/was any other fertzer used?"
rename hhmaizebeansbeanq89 hhmaizebeansbeanq89x
encode hhmaizebeansbeanq89x, gen( hhmaizebeansbeanq89)
order hhmaizebeansbeanq89, after(hhmaizebeansbeanq89x)
recode hhmaizebeansbeanq89 1=98 2=0 3=1 4=99 
label define hhmaizebeansbeanq89 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq89x

sort hhmaizebeansbeanq90
destring hhmaizebeansbeanq90, replace force float
replace hhmaizebeansbeanq90=99 if hhmaizebeansbeanq90==.
replace hhmaizebeansbeanq90=99 if hhmaizebeansbeanq90==999
label variable hhmaizebeansbeanq90 "hb/if 70=0, freq of weeding in 2018A"

sort hhmaizebeansbeanq91
destring hhmaizebeansbeanq91, replace force float
label variable hhmaizebeansbeanq91 "hb/from planting, how many days did it take to 1st weeding"

sort hhmaizebeansbeanq92
label variable hhmaizebeansbeanq92 "hb/if 70=0, did you use any pesticide/fungcd/herbcd"
rename hhmaizebeansbeanq92 hhmaizebeansbeanq92x
encode hhmaizebeansbeanq92x, gen( hhmaizebeansbeanq92)
order hhmaizebeansbeanq92, after(hhmaizebeansbeanq92x)
recode hhmaizebeansbeanq92 1=98 2=0 3=1 4=99 
label define hhmaizebeansbeanq92 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizebeansbeanq92x

label variable hhmaizebeansbeanq93 "hb/bean harvest (bags) from selected plot"
sort hhmaizebeansbeanq93
destring hhmaizebeansbeanq93, replace force float
destring hhmaizebeansbeanq94, replace force float
replace hhmaizebeansbeanq93=999 if hhmaizebeansbeanq93==.
replace hhmaizebeansbeanq94=999 if hhmaizebeansbeanq94==.

gen hhmaizebeansbeanq94b=., before(hhmaizeq95)
gen hhmaizebeansbeanq94c=., before(hhmaizeq95)
gen hhmaizebeansbeanq94d=., before(hhmaizeq95)

label variable hhmaizebeansbeanq94 "hb/kgs in one bag of beans"
label variable hhmaizebeansbeanq94b "hb/bean harvest (kgs)"
label variable hhmaizebeansbeanq94c "hb/area under beans (ha) for selected plot"
label variable hhmaizebeansbeanq94d "hb/bean yield (kgs/ha)"

replace hhmaizebeansbeanq94b= hhmaizebeansbeanq93* hhmaizebeansbeanq94 if hhmaizebeansbeanq94<=150 | hhmaizebeansbeanq93<=100

sort hhmaizebeansbeanq94b
replace hhmaizebeansbeanq94c= hhmaizebeansplot_select_area2 *0.404686
replace hhmaizebeansbeanq94d= hhmaizebeansbeanq94b/ hhmaizebeansbeanq94c
sum hhmaizebeansbeanq94b hhmaizebeansbeanq94d
sort hhmaizebeansbeanq94d

gen HHMAIZEDISPOSAL=., before( hhmaizeq95 )
label variable HHMAIZEDISPOSAL "HH MAIZE DISPOSITION & MKTING"
label variable hhmaizeq95 "hm/total maize harvest (bags) in season 2018A"
label variable hhmaizeq96 "hm/measure of mz bag (kgs per bag"
label variable hhmaizeq97 "hm/quantity (kgs) of maize kept for seed"
label variable hhmaizeq98 "hm/did you take some maize to a miller?"

sort hhmaizeq98
rename hhmaizeq98 hhmaizeq98x
encode hhmaizeq98x, gen( hhmaizeq98)
order hhmaizeq98, after( hhmaizeq98x)
recode hhmaizeq98 1=0 2=1
label define hhmaizeq98 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq98x 

sort idmiller1

label variable idmiller1 "hm/ miller1 id"
label variable idmiller2 "hm/ miller2 id"
label variable idmiller3 "hm/ miller3 id"

encode idmiller1, gen( idmiller1coded)
encode idmiller2, gen( idmiller2coded)
encode idmiller3, gen( idmiller3coded)
order idmiller1coded, after (idmiller1)
order idmiller2coded, after (idmiller2)
order idmiller3coded, after (idmiller3)

label variable hhmaizemiller1q98d "hm/district location of maize miller1"
label variable hhmaizemiller2q99d "hm/district location of maize miller2"
label variable hhmaizemiller3q100d "hm/district location of maize miller3"

sort hhmaizemiller1q98g
destring hhmaizemiller1q98g, replace force float
destring hhmaizemiller1q98h, replace force float
destring hhmaizemiller1q98i, replace force float
destring hhmaizemiller1q98j, replace force float
destring hhmaizemiller1q98k, replace force float

replace hhmaizemiller1q98g =99 if hhmaizemiller1q98g==.
replace hhmaizemiller1q98h =99 if hhmaizemiller1q98h==.
replace hhmaizemiller1q98i =99 if hhmaizemiller1q98i==.
replace hhmaizemiller1q98j =99 if hhmaizemiller1q98j==.
replace hhmaizemiller1q98k =99 if hhmaizemiller1q98k==.

label variable hhmaizemiller1q98g "hm/rating on location of mz miller1 (1-5)"
label variable hhmaizemiller1q98h "hm/rating on price of mz miller1"
label variable hhmaizemiller1q98i "hm/rating on quality of pdt- mz miller1"
label variable hhmaizemiller1q98j "hm/rating on quality of servc- mz miller1"
label variable hhmaizemiller1q98k "hm/rating on reputation of mz miller1"

note hhmaizemiller1q98g: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller1q98h: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller1q98i: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller1q98j: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller1q98k: Please rate this miller on a scale of 5 on this attribute

label variable hhmaizemiller1q98l "hm/when did you start using mz miller1"
destring hhmaizemiller1q98l, replace ignore(`"/"') force float

sort hhmaizemiller1q98m
label variable hhmaizemiller1q98m "hm/mz miller1 accepts in-kind paymt"
rename hhmaizemiller1q98m hhmaizemiller1q98mx
encode hhmaizemiller1q98mx, gen(hhmaizemiller1q98m)
order hhmaizemiller1q98m, after(hhmaizemiller1q98mx)
recode hhmaizemiller1q98m 1=98 2=0 3=1 4=99
label define hhmaizemiller1q98m 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizemiller1q98mx

sort hhmaizemiller1q98n
label variable hhmaizemiller1q98n "hm/mz miller1 accepts later payment"
rename hhmaizemiller1q98n hhmaizemiller1q98nx
encode hhmaizemiller1q98nx, gen(hhmaizemiller1q98n)
order hhmaizemiller1q98n, after(hhmaizemiller1q98nx)
recode hhmaizemiller1q98n 1=98 2=0 3=1 4=99
label define hhmaizemiller1q98n 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizemiller1q98nx

sort hhmaizemiller1q98o
label variable hhmaizemiller1q98o "hm/if 37=1, is mz miller1 part of the fga/coop"
rename hhmaizemiller1q98o hhmaizemiller1q98ox
encode hhmaizemiller1q98ox, gen(hhmaizemiller1q98o)
order hhmaizemiller1q98o, after(hhmaizemiller1q98ox)
recode hhmaizemiller1q98o 1=98 2=0 3=1 4=99
label define hhmaizemiller1q98o 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizemiller1q98ox

sort hhmaizeq99
label variable hhmaizeq99 "hm/do you know any other miller [yes=1/0]"
rename hhmaizeq99 hhmaizeq99x
encode hhmaizeq99x, gen(hhmaizeq99)
order hhmaizeq99, after(hhmaizeq99x)
recode hhmaizeq99 1=0 2=1
label define hhmaizemiller1q99 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq99x



sort idmiller2
destring hhmaizemiller2q99g, replace force float
destring hhmaizemiller2q99h, replace force float
destring hhmaizemiller2q99i, replace force float
destring hhmaizemiller2q99j, replace force float
destring hhmaizemiller2q99k, replace force float

label variable hhmaizemiller2q99g "hm/rating on location of mz miller2 (1-5)"
label variable hhmaizemiller2q99h "hm/rating on price of mz miller2"
label variable hhmaizemiller2q99i "hm/rating on quality of pdt- mz miller2"
label variable hhmaizemiller2q99j "hm/rating on quality of servc- mz miller2"
label variable hhmaizemiller2q99k "hm/rating on reputation of mz miller2"

note hhmaizemiller2q99g: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller2q99h: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller2q99i: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller2q99j: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller2q99k: Please rate this miller on a scale of 5 on this attribute

label variable hhmaizemiller2q99l "hm/did you ever use mz mill2?"
rename hhmaizemiller2q99l hhmaizemiller2q99lx
encode hhmaizemiller2q99lx, gen(hhmaizemiller2q99l)
order hhmaizemiller2q99l, after(hhmaizemiller2q99lx)
recode hhmaizemiller2q99l 1=0 2=1
label define hhmaizemiller2q99l 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizemiller2q99lx

sort hhmaizeq100
label variable hhmaizeq100 "hm/do you know any other maize miller(3)"
rename hhmaizeq100 hhmaizeq100x
encode hhmaizeq100x, gen( hhmaizeq100)
order hhmaizeq100, after(hhmaizeq100x)
recode hhmaizeq100 1=0 2=1
recode hhmaizeq100 3=99
label define hhmaizeq100 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq100x 


sort idmiller3
label variable hhmaizemiller3q100g "hm/rate location mz miller3"

sort hhmaizemiller3q100g
destring hhmaizemiller3q100g, replace force float
destring hhmaizemiller3q100h, replace force float
destring hhmaizemiller3q100i, replace force float
destring hhmaizemiller3q100j, replace force float
destring hhmaizemiller3q100k, replace force float

label variable hhmaizemiller3q100g "hm/rating on location of mz miller3 (1-5)"
label variable hhmaizemiller3q100h "hm/rating on price of mz miller3"
label variable hhmaizemiller3q100i "hm/rating on quality of pdt- mz miller3"
label variable hhmaizemiller3q100j "hm/rating on quality of servc- mz miller3"
label variable hhmaizemiller3q100k "hm/rating on reputation of mz miller3"

note hhmaizemiller3q100g: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller3q100h: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller3q100i: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller3q100j: Please rate this miller on a scale of 5 on this attribute
note hhmaizemiller3q100k: Please rate this miller on a scale of 5 on this attribute

label variable hhmaizemiller3q100l "hm/did you ever use mz miller3?"
rename hhmaizemiller3q100l hhmaizemiller3q100lx
encode hhmaizemiller3q100lx, gen(hhmaizemiller3q100l)
order hhmaizemiller3q100l, after(hhmaizemiller3q100lx)
recode hhmaizemiller3q100l 1=0 2=1 3=99
label define hhmaizemiller3q100l 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizemiller3q100lx 

label variable hhmaizeq101a "hm/if sold, how many mz bags?"
label variable hhmaizeq101b "hm/if sold, how many mz transactns (#)?"

sort hhmaizeq101
label variable hhmaizeq101 "hm/did you sell any maize of the total harvest?"
rename hhmaizeq101 hhmaizeq101x
encode hhmaizeq101x, gen(hhmaizeq101)
order hhmaizeq101, after(hhmaizeq101x)
recode hhmaizeq101 1=0 2=1 3=99
label define hhmaizeq101 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq101x 

sort hhmaizeq101a
destring hhmaizeq101a, replace force float
destring hhmaizeq101b, replace force float
destring hhmaizetransaction_count, replace force float
label variable hhmaizeq101a	"hm/How much maize was sold in total (in bags)?"
label variable hhmaizeq101b	"hm/How many separate transactions took place? (# of times sold mz)"
label variable hhmaizetransaction_count "hm/number of mz transactions (count)"


sort hhmaizetransaction1price
label variable hhmaizetransaction1price "hm/mz transactn1- calculated price"
label variable hhmaizetransaction1q101c "hm/mz transactn1- when transaction was done"
label variable hhmaizetransaction1q101f "hm/mz transactn1- price confirm"
label variable hhmaizetransaction1q101d "hm/mz transactn1- quantity sold (bags)"
label variable hhmaizetransaction1q101e "hm/mz transactn1- value of sale (ugx)"

destring hhmaizetransaction1price, replace force float
destring hhmaizetransaction1q101f, replace force float
destring hhmaizetransaction1q101d, replace force float
destring hhmaizetransaction1q101e, replace force float

label variable hhmaizetransaction2price "hm/mz transactn2- calculated price"
label variable hhmaizetransaction2q101c "hm/mz transactn2- when transaction was done"
label variable hhmaizetransaction2q101f "hm/mz transactn2- price confirm"
label variable hhmaizetransaction2q101d "hm/mz transactn2- quantity sold (bags)"
label variable hhmaizetransaction2q101e "hm/mz transactn2- value of sale (ugx)"

destring hhmaizetransaction2price, replace force float
destring hhmaizetransaction2q101f, replace force float
destring hhmaizetransaction2q101d, replace force float
destring hhmaizetransaction2q101e, replace force float

label variable hhmaizetransaction3price "hm/mz transactn3- calculated price"
label variable hhmaizetransaction3q101c "hm/mz transactn3- when transaction was done"
label variable hhmaizetransaction3q101d "hm/mz transactn3- quantity sold (bags)"
label variable hhmaizetransaction3q101e "hm/mz transactn3- value of sale (ugx)"
label variable hhmaizetransaction3q101f "hm/mz transactn3- price confirm"

destring hhmaizetransaction3price, replace force float
destring hhmaizetransaction3q101f, replace force float
destring hhmaizetransaction3q101d, replace force float
destring hhmaizetransaction3q101e, replace force float

label variable hhmaizetransaction4price "hm/mz transactn4- calculated price"
label variable hhmaizetransaction4q101c "hm/mz transactn4- when transaction was done"
label variable hhmaizetransaction4q101d "hm/mz transactn4- quantity sold (bags)"
label variable hhmaizetransaction4q101e "hm/mz transactn4- value of sale (ugx)"
label variable hhmaizetransaction4q101f "hm/mz transactn4- price confirm"

destring hhmaizetransaction4price, replace force float
destring hhmaizetransaction4q101f, replace force float
destring hhmaizetransaction4q101d, replace force float
destring hhmaizetransaction4q101e, replace force float

label variable hhmaizetransaction5price "hm/mz transactn5- calculated price"
label variable hhmaizetransaction5q101c "hm/mz transactn5- when transaction was done"
label variable hhmaizetransaction5q101d "hm/mz transactn5- quantity sold (bags)"
label variable hhmaizetransaction5q101e "hm/mz transactn5- value of sale (ugx)"
label variable hhmaizetransaction5q101f "hm/mz transactn5- price confirm"

destring hhmaizetransaction5price, replace force float
destring hhmaizetransaction5q101f, replace force float
destring hhmaizetransaction5q101d, replace force float
destring hhmaizetransaction5q101e, replace force float

label variable hhmaizetransaction6price "hm/mz transactn6- calculated price"
label variable hhmaizetransaction6q101c "hm/mz transactn6- when transaction was done"
label variable hhmaizetransaction6q101d "hm/mz transactn6- quantity sold (bags)"
label variable hhmaizetransaction6q101e "hm/mz transactn6- value of sale (ugx)"
label variable hhmaizetransaction6q101f "hm/mz transactn6- price confirm"

destring hhmaizetransaction6price, replace force float
destring hhmaizetransaction6q101f, replace force float
destring hhmaizetransaction6q101d, replace force float
destring hhmaizetransaction6q101e, replace force float

label variable hhmaizetransaction7price "hm/mz transactn7- calculated price"
label variable hhmaizetransaction7q101c "hm/mz transactn7- when transaction was done"
label variable hhmaizetransaction7q101d "hm/mz transactn7- quantity sold (bags)"
label variable hhmaizetransaction7q101e "hm/mz transactn7- value of sale (ugx)"
label variable hhmaizetransaction7q101f "hm/mz transactn7- price confirm"

destring hhmaizetransaction7price, replace force float
destring hhmaizetransaction7q101f, replace force float
destring hhmaizetransaction7q101d, replace force float
destring hhmaizetransaction7q101e, replace force float

label variable hhmaizetransaction8price "hm/mz transactn8- calculated price"
label variable hhmaizetransaction8q101c "hm/mz transactn8- when transaction was done"
label variable hhmaizetransaction8q101d "hm/mz transactn8- quantity sold (bags)"
label variable hhmaizetransaction8q101e "hm/mz transactn8- value of sale (ugx)"
label variable hhmaizetransaction8q101f "hm/mz transactn8- price confirm"

destring hhmaizetransaction8price, replace force float
destring hhmaizetransaction8q101f, replace force float
destring hhmaizetransaction8q101d, replace force float
destring hhmaizetransaction8q101e, replace force float

label variable hhmaizetransaction9price "hm/mz transactn9- calculated price"
label variable hhmaizetransaction9q101c "hm/mz transactn9- when transaction was done"
label variable hhmaizetransaction9q101d "hm/mz transactn9- quantity sold (bags)"
label variable hhmaizetransaction9q101e "hm/mz transactn9- value of sale (ugx)"
label variable hhmaizetransaction9q101f "hm/mz transactn9- price confirm"

destring hhmaizetransaction9price, replace force float
destring hhmaizetransaction9q101f, replace force float
destring hhmaizetransaction9q101d, replace force float
destring hhmaizetransaction9q101e, replace force float

replace hhmaizetransaction1price=99 if hhmaizetransaction1price==.
replace hhmaizetransaction2price=99 if hhmaizetransaction2price==.
replace hhmaizetransaction3price=99 if hhmaizetransaction3price==.
replace hhmaizetransaction4price=99 if hhmaizetransaction4price==.
replace hhmaizetransaction5price=99 if hhmaizetransaction5price==.
replace hhmaizetransaction6price=99 if hhmaizetransaction6price==.
replace hhmaizetransaction7price=99 if hhmaizetransaction7price==.
replace hhmaizetransaction8price=99 if hhmaizetransaction8price==.
replace hhmaizetransaction9price=99 if hhmaizetransaction9price==.

label variable hhmaizetransaction4other_q101f1 "ht/sold to other trader sold to"
label variable hhmaizetransaction1other_q101f1 "ht/to whom else did you sell"
label variable hhmaizetransaction2other_q101f "ht/whom eslse did you sell"

label variable idtrader1 "id/maize trader1"

sort hhmaizeq101g
label variable hhmaizeq101g "hm/>=1 transactions was with a village buyer/agent"
rename hhmaizeq101g hhmaizeq101gx
encode hhmaizeq101gx, gen(hhmaizeq101g)
order hhmaizeq101g, after(hhmaizeq101gx)
recode hhmaizeq101g 1=0 2=1 3=99
label define hhmaizeq101g 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq101gx 

note hhmaizeq101g: can you confirm from the farmer that one or more transactions was with [ii Village middlemen/Private/independent maize store/buyer/Agent of independent maize store/buyer]?

label variable hhmaizeq102 "hm/can you give the details of trader"
notes hhmaizeq102: If any quantity sold to [ii Village middlemen/Private/independent maize store/buyer/Agent of independent maize store/buyer] in any transaction, can you give me the name of trader, location and contact info?

sort hhmaizeq102
label variable hhmaizeq102 "hm/did you sell any maize of the total harvest?"
rename hhmaizeq102 hhmaizeq102x
encode hhmaizeq102x, gen(hhmaizeq102)
order hhmaizeq102, after(hhmaizeq102x)
recode hhmaizeq102 1=0 2=1 3=99
label define hhmaizeq102 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq102x 

label variable idtrader1 "ht/id trader1"
label variable idtrader2 "ht/id trader2"
label variable idtrader3 "ht/id trader3"

sort idtrader1
rename idtrader1 idtrader1x
encode idtrader1x, gen(idtrader1)
order idtrader1, after(idtrader1x)
label variable idtrader1 "hm/id trader1 coded"
rename idtrader1 idtrader1xx
rename idtrader1x idtrader1
rename idtrader1xx idtrader1x

label variable hhmaizetrader1q102d "hm/district location of maize trader1"

label variable hhmaizetrader1q102g "ht/trader1-rating on location/ease of reach"
label variable hhmaizetrader1q102h "ht/trader1-rating on price"
label variable hhmaizetrader1q102i "ht/trader1-rating on quality of service (prompt pay, take any qtty, provides bags)"
label variable hhmaizetrader1q102j "ht/trader1-rating on honesty (does not try to cheat a farmer)"
label variable hhmaizetrader1q102k "ht/trader1-rating on reputation (what others think of trader)"
label variable hhmaizetrader1q102l "ht/trader1-accepts small quantities"
label variable hhmaizetrader1q102m "ht/trader1-accepts poor quality maize"
label variable hhmaizetrader1q102n "ht/trader1-provides credit/advances"
label variable hhmaizetrader1q102o "ht/trader1-year when first sold to this trader"
label variable hhmaizetrader1q102p "ht/trader1-provides some storge & handling inptuts"
label variable hhmaizetrader1q102q "ht/trader1-provides training on mz prodn & ph handling"
label variable hhmaizetrader1q102r "ht/trader1-provides farm inputs (like seed,fert,chems)"

notes hhmaizetrader1q102g: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader1q102h: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader1q102i: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader1q102j: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader1q102k: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader1q102l: Does this trader accept small quantities (eg even one kg)?
notes hhmaizetrader1q102m: Does this trader also accept poor quality maize?
notes hhmaizetrader1q102n: Does this trader provide credit or give advances eg buys/pays before harvest)
notes hhmaizetrader1q102o: Duration doing business with this trader
notes hhmaizetrader1q102p: Trader provides storage and handling related inputs such as bags, tarpaulins
notes hhmaizetrader1q102q: Trader provide training on maize farming or maize storage and handling
notes hhmaizetrader1q102r: Trader provide you with farming inputs such as maize seed, fertilizer and chemicals

notes hhmaizetrader1q102g: 1 means Not easy to reach at all, 5 means very easy to reach
notes hhmaizetrader1q102h: 1 means Pays at very low price, 5 means pays very high price
notes hhmaizetrader1q102i: 1 means very poor quality service, 5 means very high quality service
notes hhmaizetrader1q102j: 1 means completely not to be trusted, 5 means extreamly honest business person
notes hhmaizetrader1q102k: 1 means Others think he is a lousy businessperson, 5 others think he is a fantastic businessperson

label variable hhmaizetrader1q102s "ht/maize trader1-if 37=yes, is associated with the cooperative or organization you are part of 
notes hhmaizetrader1q102s: Trader is a memebr of a farmer assocaition/ cooperative
sort hhmaizetrader1q102g

destring hhmaizetrader1q102g, replace force float
destring hhmaizetrader1q102h, replace force float
destring hhmaizetrader1q102i, replace force float
destring hhmaizetrader1q102j, replace force float
destring hhmaizetrader1q102k, replace force float

sort hhmaizetrader1q102l
rename hhmaizetrader1q102l hhmaizetrader1q102lx
encode hhmaizetrader1q102lx, gen(hhmaizetrader1q102l)
order hhmaizetrader1q102l, after(hhmaizetrader1q102lx)
recode hhmaizetrader1q102l 4=98 5=99
label define hhmaizetrader1q102l 1 "Yes, always" 2 "Yes, sometimes" 3 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102lx

sort hhmaizetrader1q102m
rename hhmaizetrader1q102m hhmaizetrader1q102mx
encode hhmaizetrader1q102mx, gen(hhmaizetrader1q102m)
order hhmaizetrader1q102m, after(hhmaizetrader1q102mx)
recode hhmaizetrader1q102m 4=98 5=99
label define hhmaizetrader1q102m 1 "Yes, always" 2 "Yes, sometimes" 3 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102mx

sort hhmaizetrader1q102n
rename hhmaizetrader1q102n hhmaizetrader1q102nx
encode hhmaizetrader1q102nx, gen(hhmaizetrader1q102n)
order hhmaizetrader1q102n, after(hhmaizetrader1q102nx)
recode hhmaizetrader1q102n 4=98 5=99
label define hhmaizetrader1q102n 1 "Yes, always" 2 "Yes, sometimes" 3 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102nx

destring hhmaizetrader1q102o, replace ignore(`"/"') force float

sort hhmaizetrader1q102p
rename hhmaizetrader1q102p hhmaizetrader1q102px
encode hhmaizetrader1q102px, gen(hhmaizetrader1q102p)
order hhmaizetrader1q102p, after(hhmaizetrader1q102px)
recode hhmaizetrader1q102p 1=98 2=0 3=1 4=99
label define hhmaizetrader1q102p 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102px

sort hhmaizetrader1q102q
rename hhmaizetrader1q102q hhmaizetrader1q102qx
encode hhmaizetrader1q102qx, gen(hhmaizetrader1q102q)
order hhmaizetrader1q102q, after(hhmaizetrader1q102qx)
recode hhmaizetrader1q102q 1=98 2=0 3=1 4=99
label define hhmaizetrader1q102q 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102qx

sort hhmaizetrader1q102r
rename hhmaizetrader1q102r hhmaizetrader1q102rx
encode hhmaizetrader1q102rx, gen(hhmaizetrader1q102r)
order hhmaizetrader1q102r, after(hhmaizetrader1q102rx)
recode hhmaizetrader1q102r 1=98 2=0 3=1 4=99
label define hhmaizetrader1q102r 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102rx

sort hhmaizetrader1q102s
rename hhmaizetrader1q102s hhmaizetrader1q102sx
encode hhmaizetrader1q102sx, gen(hhmaizetrader1q102s)
order hhmaizetrader1q102s, after(hhmaizetrader1q102sx)
recode hhmaizetrader1q102s 1=98 2=0 3=1 4=99
label define hhmaizetrader1q102s 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader1q102sx




sort hhmaizeq103
label variable hhmaizeq103 "ht/do you knw any other mz trader(T#2)"

rename hhmaizeq103 hhmaizeq103x
encode hhmaizeq103x, gen(hhmaizeq103)
order hhmaizeq103, after(hhmaizeq103x)
recode hhmaizeq103 1=0 2=1 3=99
label define hhmaizeq103 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq103x 

sort idtrader2
rename idtrader2 idtrader2x
encode idtrader2x, gen(idtrader2)
order idtrader2, after(idtrader2x)
rename idtrader2x idtrader2xx
rename idtrader2 idtrader2x
rename idtrader2xx idtrader2
label variable idtrader2 "hm/id trader2 coded"

label variable hhmaizetrader2q103d "hm/district location of maize trader2"

label variable hhmaizetrader2q103g "ht/trader2-rating on location/ease of reach"
label variable hhmaizetrader2q103h "ht/trader2-rating on price"
label variable hhmaizetrader2q103i "ht/trader2-rating on quality of service (prompt pay, take any qtty, provides bags)"
label variable hhmaizetrader2q103j "ht/trader2-rating on honesty (does not try to cheat a farmer)"
label variable hhmaizetrader2q103k "ht/trader2-rating on reputation (what others think of trader)"
label variable hhmaizetrader2q103l "ht/trader2-accepts small quantities"

notes hhmaizetrader2q103g: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader2q103h: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader2q103i: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader2q103j: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader2q103k: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader2q103l: Does this trader accept small quantities (eg even one kg)?

notes hhmaizetrader2q103g: 1 means Not easy to reach at all, 5 means very easy to reach
notes hhmaizetrader2q103h: 1 means Pays at very low price, 5 means pays very high price
notes hhmaizetrader2q103i: 1 means very poor quality service, 5 means very high quality service
notes hhmaizetrader2q103j: 1 means completely not to be trusted, 5 means extreamly honest business person
notes hhmaizetrader2q103k: 1 means Others think he is a lousy businessperson, 5 others think he is a fantastic businessperson

destring hhmaizetrader2q103g, replace force float
destring hhmaizetrader2q103h, replace force float
destring hhmaizetrader2q103i, replace force float
destring hhmaizetrader2q103j, replace force float
destring hhmaizetrader2q103k, replace force float

sort hhmaizetrader2q103l
rename hhmaizetrader2q103l hhmaizetrader2q103lx
encode hhmaizetrader2q103lx, gen(hhmaizetrader2q103l)
order hhmaizetrader2q103l, after(hhmaizetrader2q103lx)
recode hhmaizetrader2q103l 3=99 1=0 2=1
label define hhmaizetrader2q103l 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader2q103lx




sort hhmaizeq104
label variable hhmaizeq104 "ht/do you knw any other mz trader(T#3)"

rename hhmaizeq104 hhmaizeq104x
encode hhmaizeq104x, gen(hhmaizeq104)
order hhmaizeq104, after(hhmaizeq104x)
recode hhmaizeq104 1=0 2=1 3=99
label define hhmaizeq104 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq104x 

sort idtrader3
rename idtrader3 idtrader3x
encode idtrader3x, gen(idtrader3)
order idtrader3, after(idtrader3x)
rename idtrader3x idtrader3xx
rename idtrader3 idtrader3x
rename idtrader3xx idtrader3

label variable idtrader3 "hm/id trader3"
label variable idtrader3x "hm/id trader3 coded"

label variable hhmaizetrader3q104d "hm/district location of maize trader2"

label variable hhmaizetrader3q104g "ht/trader3-rating on location/ease of reach"
label variable hhmaizetrader3q104h "ht/trader3-rating on price"
label variable hhmaizetrader3q104i "ht/trader3-rating on quality of service (prompt pay, take any qtty, provides bags)"
label variable hhmaizetrader3q104j "ht/trader3-rating on honesty (does not try to cheat a farmer)"
label variable hhmaizetrader3q104k "ht/trader3-rating on reputation (what others think of trader)"
label variable hhmaizetrader3q104l "ht/trader3-accepts small quantities"

notes hhmaizetrader3q104g: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader3q104h: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader3q104i: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader3q104j: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader3q104k: Please rate this trader on a five stars scale on this attribute
notes hhmaizetrader3q104l: Does this trader accept small quantities (eg even one kg)?

notes hhmaizetrader3q104g: 1 means Not easy to reach at all, 5 means very easy to reach
notes hhmaizetrader3q104h: 1 means Pays at very low price, 5 means pays very high price
notes hhmaizetrader3q104i: 1 means very poor quality service, 5 means very high quality service
notes hhmaizetrader3q104j: 1 means completely not to be trusted, 5 means extreamly honest business person
notes hhmaizetrader3q104k: 1 means Others think he is a lousy businessperson, 5 others think he is a fantastic businessperson

destring hhmaizetrader3q104g, replace force float
destring hhmaizetrader3q104h, replace force float
destring hhmaizetrader3q104i, replace force float
destring hhmaizetrader3q104j, replace force float
destring hhmaizetrader3q104k, replace force float

sort hhmaizetrader3q104l
rename hhmaizetrader3q104l hhmaizetrader3q104lx
encode hhmaizetrader3q104lx, gen(hhmaizetrader3q104l)
order hhmaizetrader3q104l, after(hhmaizetrader3q104lx)
recode hhmaizetrader3q104l 3=99 1=0 2=1
label define hhmaizetrader3q104l 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizetrader3q104lx

sort hhmaizeq105
label variable hhmaizeq105 "ht/estimate # of these maize traders/middlemen buy mz in your village/neighborhood"



gen GENERALSEED=., before(hhmaizeq106)
label variable GENERALSEED "GENRAL SEED"

label variable hhmaizeq106 "hs/have bought imprvd (maize/bean) seed"
rename hhmaizeq106 hhmaizeq106x
encode hhmaizeq106x, gen(hhmaizeq106)
order hhmaizeq106, after(hhmaizeq106x)
recode hhmaizeq106 1=0 2=1
label define hhmaizeq106 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq106x


label variable hhmaizeq107q "hs/if 106=yes, got from neighbor?"
label variable hhmaizeq107b "hs/if 106=yes, bought from local mkt?"
label variable hhmaizeq107c "hs/if 106=yes, bought from agroinpt shop?"
label variable hhmaizeq107d "hs/if 106=yes, got from ow/naads?"
label variable hhmaizeq107e "hs/if 106=yes, got from ngo?"
label variable hhmaizeq107f "hs/if 106=yes, got from seed company?"
label variable hhmaizeq107g "hs/if 106=yes, got from local seed business?"
label variable hhmaizeq107h "hs/if 106=yes, got from resarch station?"
label variable hhmaizeq10796 "hs/if 106=yes, got from other source?"

notes hhmaizeq107q: source of seed
notes hhmaizeq107b: source of seed
notes hhmaizeq107c: source of seed
notes hhmaizeq107d: source of seed
notes hhmaizeq107e: source of seed
notes hhmaizeq107f: source of seed
notes hhmaizeq107g: source of seed
notes hhmaizeq107h: source of seed
notes hhmaizeq10796: other source of seed

rename hhmaizeq107q hhmaizeq107qx
encode hhmaizeq107qx, gen(hhmaizeq107q)
order hhmaizeq107q, after(hhmaizeq107qx)
recode hhmaizeq107q 1=0 2=1 3=99
label define hhmaizeq107q 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label variable hhmaizeq107q "hs/if 106=yes, got from neighbor?"
drop hhmaizeq107qx

rename hhmaizeq107b hhmaizeq107bx
rename hhmaizeq107c hhmaizeq107cx
rename hhmaizeq107d hhmaizeq107dx
rename hhmaizeq107e hhmaizeq107ex
rename hhmaizeq107f hhmaizeq107fx
rename hhmaizeq107g hhmaizeq107gx
rename hhmaizeq107h hhmaizeq107hx
rename hhmaizeq10796 hhmaizeq10796x

encode hhmaizeq107bx, gen(hhmaizeq107b)
encode hhmaizeq107cx, gen(hhmaizeq107c)
encode hhmaizeq107dx, gen(hhmaizeq107d)
encode hhmaizeq107ex, gen(hhmaizeq107e)
encode hhmaizeq107fx, gen(hhmaizeq107f)
encode hhmaizeq107gx, gen(hhmaizeq107g)
encode hhmaizeq107hx, gen(hhmaizeq107h)
encode hhmaizeq10796x, gen(hhmaizeq10796)

order hhmaizeq107b, after(hhmaizeq107bx)
order hhmaizeq107c, after(hhmaizeq107cx)
order hhmaizeq107d, after(hhmaizeq107dx)
order hhmaizeq107e, after(hhmaizeq107ex)
order hhmaizeq107f, after(hhmaizeq107fx)
order hhmaizeq107g, after(hhmaizeq107gx)
order hhmaizeq107h, after(hhmaizeq107hx)
order hhmaizeq10796, after(hhmaizeq10796x)

recode hhmaizeq107b 1=0 2=1 3=99
recode hhmaizeq107c 1=0 2=1 3=99
recode hhmaizeq107d 1=0 2=1 3=99
recode hhmaizeq107e 1=0 2=1 3=99
recode hhmaizeq107f 1=0 2=1 3=99
recode hhmaizeq107g 1=0 2=1 3=99
recode hhmaizeq107h 1=0 2=1 3=99
recode hhmaizeq10796 1=0 2=1 3=99

label define hhmaizeq107b 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107c 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107d 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107e 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107f 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107g 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq107h 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
label define hhmaizeq10796 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace

label variable hhmaizeq107q "hs/if 106=yes, got from neighbor?"
label variable hhmaizeq107b "hs/if 106=yes, bought from local mkt?"
label variable hhmaizeq107c "hs/if 106=yes, bought from agroinpt shop?"
label variable hhmaizeq107d "hs/if 106=yes, got from ow/naads?"
label variable hhmaizeq107e "hs/if 106=yes, got from ngo?"
label variable hhmaizeq107f "hs/if 106=yes, got from seed company?"
label variable hhmaizeq107g "hs/if 106=yes, got from local seed business?"
label variable hhmaizeq107h "hs/if 106=yes, got from resarch station?"
label variable hhmaizeq10796 "hs/if 106=yes, got from other source?"

drop hhmaizeq107bx hhmaizeq107cx hhmaizeq107dx hhmaizeq107ex hhmaizeq107fx hhmaizeq107gx hhmaizeq107hx hhmaizeq10796x

rename hhmaizeq108 hhmaizeq108x
encode hhmaizeq108x, gen(hhmaizeq108)
order hhmaizeq108, after(hhmaizeq108x)
recode hhmaizeq108 1=0 2=1 3=99
label define hhmaizeq108 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
label variable hhmaizeq108 "hs/can you provide details of agroinput shop? (S#1)"
drop hhmaizeq108x

label variable idagro1 "hs/id agroinput1"
note idagro1: agroinput dealer #1

sort idagro1
rename idagro1 idagro1x
encode idagro1x, gen(idagro1)
order idagro1, after(idagro1x)
rename idagro1x idagro1xx
rename idagro1 idagro1x
rename idagro1xx idagro1
label variable idagro1x "hs/id agroinput1 coded"

label variable hhmaizeagro1q108e "hs/agroinputdealer1-district"
label variable hhmaizeagro1q108h "hs/agroinputdealer1-rating on location"
label variable hhmaizeagro1q108i "hs/agroinputdealer1-rating on price"
label variable hhmaizeagro1q108j "hs/agroinputdealer1-rating on quality of seed/pdts"
label variable hhmaizeagro1q108k "hs/agroinputdealer1-rating on stock availability"
label variable hhmaizeagro1q108l "hs/agroinputdealer1-rating on reputation"

notes hhmaizeagro1q108h: Location – close to clients, in a convenient location? 
notes hhmaizeagro1q108h: 1 extremely inaccessible – 5 very good location and accessible

notes hhmaizeagro1q108i: Price – competitive pricing, discounts? 
notes hhmaizeagro1q108i: 1 way too expensive, 5 extremely cheap

notes hhmaizeagro1q108j: Quality of seed – good products, no fake seed 
notes hhmaizeagro1q108j: 1 very poor quality-often fake, 5 excellent quality

notes hhmaizeagro1q108k: Stock – availability of seeds at all time 
notes hhmaizeagro1q108k: 1 always out of stock and sells in only large quantities, 5 always has stock and accepts to sell in smaller quantities

notes hhmaizeagro1q108l: Reputation – others are recommending him 
notes hhmaizeagro1q108l: 1. they think is a lousy agro-dealer, 5. they think it is an excellent agro-dealer

label variable hhmaizeagro1q108m "hs/agro1- since when have you been buying frrm this shop? year"
label variable hhmaizeagro1q108n "hs/agro1-if with a problem, can you take the seed back & be refunded? (insurance)"
label variable hhmaizeagro1q108o "hs/agro1-does agro-dealer give seed on credit (pay after hvst)?"
label variable hhmaizeagro1q108p "hs/agro1-does dealer offer train'g on seed varieties?"

destring hhmaizeagro1q108h, replace force float
destring hhmaizeagro1q108i, replace force float
destring hhmaizeagro1q108j, replace force float
destring hhmaizeagro1q108k, replace force float
destring hhmaizeagro1q108l, replace force float

replace hhmaizeagro1q108h=99 if hhmaizeagro1q108h==.
replace hhmaizeagro1q108i=99 if hhmaizeagro1q108i==.
replace hhmaizeagro1q108j=99 if hhmaizeagro1q108j==.
replace hhmaizeagro1q108k=99 if hhmaizeagro1q108k==.
replace hhmaizeagro1q108l=99 if hhmaizeagro1q108l==.

destring hhmaizeagro1q108m, replace ignore(`"/"') force float

sort hhmaizeagro1q108n
rename hhmaizeagro1q108n hhmaizeagro1q108nx
encode hhmaizeagro1q108nx, gen(hhmaizeagro1q108n)
order hhmaizeagro1q108n, after(hhmaizeagro1q108nx)
recode hhmaizeagro1q108n 1=0 2=1 3=99
recode hhmaizeagro1q108n 1=0 99=1 0=98 4=99
label define hhmaizeagro1q108n 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeagro1q108nx

sort hhmaizeagro1q108o
rename hhmaizeagro1q108o hhmaizeagro1q108ox
encode hhmaizeagro1q108ox, gen(hhmaizeagro1q108o)
order hhmaizeagro1q108o, after(hhmaizeagro1q108ox)
recode hhmaizeagro1q108o 1=0 2=1 3=99
label define hhmaizeagro1q108o 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeagro1q108ox

sort hhmaizeagro1q108p
rename hhmaizeagro1q108p hhmaizeagro1q108px
encode hhmaizeagro1q108px, gen(hhmaizeagro1q108p)
order hhmaizeagro1q108p, after(hhmaizeagro1q108px)
recode hhmaizeagro1q108p 1=98 2=0 3=1 4=99
label define hhmaizeagro1q108p 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeagro1q108px


label variable hhmaizeq109 "hs/do you know of any other input shop (S#2)?"
label variable idagro2 "hs/id agroinput2"
note idagro2: agroinput dealer #2

sort hhmaizeq109
rename hhmaizeq109 hhmaizeq109x
encode hhmaizeq109x, gen(hhmaizeq109)
order hhmaizeq109, after(hhmaizeq109x)
recode hhmaizeq109 1=0 2=1 3=99
label define hhmaizeq109 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq109x


sort idagro2
rename idagro2 idagro2x
encode idagro2x, gen(idagro2)
order idagro2, after(idagro2x)
rename idagro2x idagro2xx
rename idagro2 idagro2x
rename idagro2xx idagro2
label variable idagro2x "hs/id agroinput2 coded"

label variable hhmaizeagro2q109e "hs/agroinputdealer2-district"
label variable hhmaizeagro2q109h "hs/agroinputdealer2-rating on location"
label variable hhmaizeagro2q109i "hs/agroinputdealer2-rating on price"
label variable hhmaizeagro2q109j "hs/agroinputdealer2-rating on quality of seed/pdts"
label variable hhmaizeagro2q109k "hs/agroinputdealer2-rating on stock availability"
label variable hhmaizeagro2q109l "hs/agroinputdealer2-rating on reputation"

notes hhmaizeagro2q109h: Location – close to clients, in a convenient location? 
notes hhmaizeagro2q109h: 1 extremely inaccessible – 5 very good location and accessible

notes hhmaizeagro2q109i: Price – competitive pricing, discounts? 
notes hhmaizeagro2q109i: 1 way too expensive, 5 extremely cheap

notes hhmaizeagro2q109j: Quality of seed – good products, no fake seed 
notes hhmaizeagro2q109j: 1 very poor quality-often fake, 5 excellent quality

notes hhmaizeagro2q109k: Stock – availability of seeds at all time 
notes hhmaizeagro2q109k: 1 always out of stock and sells in only large quantities, 5 always has stock and accepts to sell in smaller quantities

notes hhmaizeagro2q109l: Reputation – others are recommending him 
notes hhmaizeagro2q109l: 1. they think is a lousy agro-dealer, 5. they think it is an excellent agro-dealer

destring hhmaizeagro2q109h, replace force float
destring hhmaizeagro2q109i, replace force float
destring hhmaizeagro2q109j, replace force float
destring hhmaizeagro2q109k, replace force float
destring hhmaizeagro2q109l, replace force float

replace hhmaizeagro2q109h=99 if hhmaizeagro2q109h==.
replace hhmaizeagro2q109i=99 if hhmaizeagro2q109i==.
replace hhmaizeagro2q109j=99 if hhmaizeagro2q109j==.
replace hhmaizeagro2q109k=99 if hhmaizeagro2q109k==.
replace hhmaizeagro2q109l=99 if hhmaizeagro2q109l==.

label variable hhmaizeagro2q110 "Have you ever bought seed at this agro-input(2) shop?"
sort hhmaizeagro2q110
rename hhmaizeagro2q110 hhmaizeagro2q110x
encode hhmaizeagro2q110x, gen(hhmaizeagro2q110)
order hhmaizeagro2q110, after(hhmaizeagro2q110x)
recode hhmaizeagro2q110 1=0 2=1 3=99
label define hhmaizeagro2q110 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeagro2q110x
label variable hhmaizeagro2q110 "Have you ever bought seed at this agro-input(2) shop?"


label variable hhmaizeq111 "hs/do you know of any other input shop (S#3)?"
label variable idagro3 "hs/id agroinput3"
note idagro3: agroinput dealer #3

sort hhmaizeq111
rename hhmaizeq111 hhmaizeq111x
encode hhmaizeq111x, gen(hhmaizeq111)
order hhmaizeq111, after(hhmaizeq111x)
recode hhmaizeq111 1=0 2=1 3=99
label define hhmaizeq111 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq111x

sort idagro3
rename idagro3 idagro3x
encode idagro3x, gen(idagro3)
order idagro3, after(idagro3x)
rename idagro3x idagro3xx
rename idagro3 idagro3x
rename idagro3xx idagro3
label variable idagro3x "hs/id agroinput3 coded"

label variable hhmaizeagro3q111e "hs/agroinputdealer3-district"
label variable hhmaizeagro3q111h "hs/agroinputdealer3-rating on location"
label variable hhmaizeagro3q111i "hs/agroinputdealer3-rating on price"
label variable hhmaizeagro3q111j "hs/agroinputdealer3-rating on quality of seed/pdts"
label variable hhmaizeagro3q111k "hs/agroinputdealer3-rating on stock availability"
label variable hhmaizeagro3q111l "hs/agroinputdealer3-rating on reputation"

notes hhmaizeagro3q111h: Location – close to clients, in a convenient location? 
notes hhmaizeagro3q111h: 1 extremely inaccessible – 5 very good location and accessible

notes hhmaizeagro3q111i: Price – competitive pricing, discounts? 
notes hhmaizeagro3q111i: 1 way too expensive, 5 extremely cheap

notes hhmaizeagro3q111j: Quality of seed – good products, no fake seed 
notes hhmaizeagro3q111j: 1 very poor quality-often fake, 5 excellent quality

notes hhmaizeagro3q111k: Stock – availability of seeds at all time 
notes hhmaizeagro3q111k: 1 always out of stock and sells in only large quantities, 5 always has stock and accepts to sell in smaller quantities

notes hhmaizeagro3q111l: Reputation – others are recommending him 
notes hhmaizeagro3q111l: 1. they think is a lousy agro-dealer, 5. they think it is an excellent agro-dealer

destring hhmaizeagro3q111h, replace force float
destring hhmaizeagro3q111i, replace force float
destring hhmaizeagro3q111j, replace force float
destring hhmaizeagro3q111k, replace force float
destring hhmaizeagro3q111l, replace force float

replace hhmaizeagro3q111h=99 if hhmaizeagro3q111h==.
replace hhmaizeagro3q111i=99 if hhmaizeagro3q111i==.
replace hhmaizeagro3q111j=99 if hhmaizeagro3q111j==.
replace hhmaizeagro3q111k=99 if hhmaizeagro3q111k==.
replace hhmaizeagro3q111l=99 if hhmaizeagro3q111l==.

sort hhmaizeagro3q112
label variable hhmaizeagro3q112 "hs/Have you ever bought seed at this agro-input(2) shop?"
rename hhmaizeagro3q112 hhmaizeagro3q112x
encode hhmaizeagro3q112x, gen(hhmaizeagro3q112)
order hhmaizeagro3q112, after(hhmaizeagro3q112x)
recode hhmaizeagro3q112 1=0 2=1 3=99
label define hhmaizeagro3q112 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeagro3q112x

sort hhmaizeq114 
label variable hhmaizeq114 "hs/did you ever buy seed that appeared to be counterfeit?"
rename hhmaizeq114 hhmaizeq114x
encode hhmaizeq114x, gen(hhmaizeq114)
order hhmaizeq114, after(hhmaizeq114x)
recode hhmaizeq114 5=99
label define hhmaizeq114 1 "Never" 2 "Only once" 3 "A few times" 4 "Always when I buy seed, it's fake" 98 "Dont know" 99 "NA", replace
drop hhmaizeq114x


sort hhmaizeq115 
label variable hhmaizeq115 "hs/as a farmer, can you differentiate btn quality & counterfeit seed?"
rename hhmaizeq115 hhmaizeq115x
encode hhmaizeq115x, gen(hhmaizeq115)
order hhmaizeq115, after(hhmaizeq115x)
recode hhmaizeq115 7=99
label define hhmaizeq115 1 "Yes, I can tell from package" 2 "Yes, but only after opening & inspecting packg" 3 "Yes, but only after germinatn test" 4 "Yes, but only after monitoring field performc" 5 "Yes, but only after inspecting the harvest" 6 "No,  It is impossible to know if seed is real or fake" 98 "Dont know" 99 "NA", replace
drop hhmaizeq115x

 
sort hhmaizeq116
label variable hhmaizeq116 "hp/For maize you sold/plan to sell, do you sort maize cobs before threshing?"
rename hhmaizeq116 hhmaizeq116x
encode hhmaizeq116x, gen(hhmaizeq116)
order hhmaizeq116, after(hhmaizeq116x)
recode hhmaizeq116 1=0 2=1 3=99
label define hhmaizeq116 1 "Yes" 0 "No" 98 "Dont know" 99 "NA", replace
drop hhmaizeq116x


label variable hhmaizeq117a "hp/thresh maize by beating with sticks"
label variable hhmaizeq117b "hp/thresh maize using hands"
label variable hhmaizeq117c "hp/thresh maize using a hand powered thresher"
label variable hhmaizeq117d "hp/thresh maize using a machine powered thresher"
label variable hhmaizeq11796 "hp/thresh maize using other means"

notes hhmaizeq117a: "hp/for maize you sold/plan to sell, how do you thresh (maize)?"
notes hhmaizeq117b: "hp/for maize you sold/plan to sell, how do you thresh (maize)?"
notes hhmaizeq117c: "hp/for maize you sold/plan to sell, how do you thresh (maize)?"
notes hhmaizeq117d: "hp/for maize you sold/plan to sell, how do you thresh (maize)?"
notes hhmaizeq11796: "hp/for maize you sold/plan to sell, how do you thresh (maize)?"

sort hhmaizeq117a
label variable hhmaizeq117a "hp/thresh maize by beating with sticks"
rename hhmaizeq117a hhmaizeq117ax
encode hhmaizeq117ax, gen(hhmaizeq117a)
order hhmaizeq117a, after(hhmaizeq117ax)
recode hhmaizeq117a 1=0 2=1 3=99
label define hhmaizeq117a 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq117ax

sort hhmaizeq117b
label variable hhmaizeq117b "hp/thresh maize using hands"
rename hhmaizeq117b hhmaizeq117bx
encode hhmaizeq117bx, gen(hhmaizeq117b)
order hhmaizeq117b, after(hhmaizeq117bx)
recode hhmaizeq117b 1=0 2=1 3=99
label define hhmaizeq117b 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq117bx


sort hhmaizeq117c
label variable hhmaizeq117c "hp/thresh maize using a hand powered thresher"
rename hhmaizeq117c hhmaizeq117cx
encode hhmaizeq117cx, gen(hhmaizeq117c)
order hhmaizeq117c, after(hhmaizeq117cx)
recode hhmaizeq117c 1=0 2=1 3=99
label define hhmaizeq117c 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq117cx

sort hhmaizeq117d
label variable hhmaizeq117d "hp/thresh maize using a machine powered thresher"
rename hhmaizeq117d hhmaizeq117dx
encode hhmaizeq117dx, gen(hhmaizeq117d)
order hhmaizeq117d, after(hhmaizeq117dx)
recode hhmaizeq117d 1=0 2=1 3=99
label define hhmaizeq117d 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq117dx

sort hhmaizeq11796
label variable hhmaizeq11796 "hp/thresh maize using other means"
rename hhmaizeq11796 hhmaizeq11796x
encode hhmaizeq11796x, gen(hhmaizeq11796)
order hhmaizeq11796, after(hhmaizeq11796x)
recode hhmaizeq11796 1=0 2=1 3=99
label define hhmaizeq11796 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq11796x

sort hhmaizeq118 
label variable hhmaizeq118 "hp/for maize you sold/plan to sell, how do you dry your maize?"
rename hhmaizeq118 hhmaizeq118x
encode hhmaizeq118x, gen(hhmaizeq118)
order hhmaizeq118, after(hhmaizeq118x)
recode hhmaizeq118 4=99
label define hhmaizeq118 1 "On ground" 2 "On a concrete floor/tarred area" 3 "On tarpaulin" 98 "Dont know" 99 "NA", replace
drop hhmaizeq118x


sort hhmaizeq119a 
label variable hhmaizeq119a "hp/store maize using gunny bags on the floor"
rename hhmaizeq119a hhmaizeq119ax
encode hhmaizeq119ax, gen(hhmaizeq119a)
order hhmaizeq119a, after(hhmaizeq119ax)
recode hhmaizeq119a 1=0 2=1 3=99
label define hhmaizeq119a 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119ax

label variable hhmaizeq119b "hp/store maize using gunny bags on palates"
rename hhmaizeq119b hhmaizeq119bx
encode hhmaizeq119bx, gen(hhmaizeq119b)
order hhmaizeq119b, after(hhmaizeq119bx)
recode hhmaizeq119b 1=0 2=1 3=99
label define hhmaizeq119b 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119bx

label variable hhmaizeq119c "hp/store maize using pics bags on the floor"
rename hhmaizeq119c hhmaizeq119cx
encode hhmaizeq119cx, gen(hhmaizeq119c)
order hhmaizeq119c, after(hhmaizeq119cx)
recode hhmaizeq119c 1=0 2=1 3=99
label define hhmaizeq119c 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119cx

label variable hhmaizeq119d "hp/store maize using pics bags on the palates"
rename hhmaizeq119d hhmaizeq119dx
encode hhmaizeq119dx, gen(hhmaizeq119d)
order hhmaizeq119d, after(hhmaizeq119dx)
recode hhmaizeq119d 1=0 2=1 3=99
label define hhmaizeq119d 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119dx

label variable hhmaizeq119e "hp/store maize using hermetic container"
rename hhmaizeq119e hhmaizeq119ex
encode hhmaizeq119ex, gen(hhmaizeq119e)
order hhmaizeq119e, after(hhmaizeq119ex)
recode hhmaizeq119e 1=0 2=1 3=99
label define hhmaizeq119e 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119ex

label variable hhmaizeq119f "hp/store maize using plastic containers"
rename hhmaizeq119f hhmaizeq119fx
encode hhmaizeq119fx, gen(hhmaizeq119f)
order hhmaizeq119f, after(hhmaizeq119fx)
recode hhmaizeq119f 1=0 2=1 3=99
label define hhmaizeq119f 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119fx

label variable hhmaizeq119g "hp/store maize using baskets"
rename hhmaizeq119g hhmaizeq119gx
encode hhmaizeq119gx, gen(hhmaizeq119g)
order hhmaizeq119g, after(hhmaizeq119gx)
recode hhmaizeq119g 1=0 2=1 3=99
label define hhmaizeq119g 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119gx

label variable hhmaizeq119h "hp/store maize using pots"
rename hhmaizeq119h hhmaizeq119hx
encode hhmaizeq119hx, gen(hhmaizeq119h)
order hhmaizeq119h, after(hhmaizeq119hx)
recode hhmaizeq119h 1=0 2=1 3=99
label define hhmaizeq119h 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119hx

label variable hhmaizeq119i "hp/store maize using traditional granary"
rename hhmaizeq119i hhmaizeq119ix
encode hhmaizeq119ix, gen(hhmaizeq119i)
order hhmaizeq119i, after(hhmaizeq119ix)
recode hhmaizeq119i 1=0 2=1 3=99
label define hhmaizeq119i 1 "True" 0 "False" 98 "Dont know" 99 "NA", replace
drop hhmaizeq119ix

notes hhmaizeq119a: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119b: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119c: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119d: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119e: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119f: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119g: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119h: "hp/for maize you sold or plan to sell, how do you store your maize?"
notes hhmaizeq119i: "hp/for maize you sold or plan to sell, how do you store your maize?"

label variable hhmaizeq120 "hp/What is your storage capacity? How many kgs can you store at your homestead?"


compress
sort id



