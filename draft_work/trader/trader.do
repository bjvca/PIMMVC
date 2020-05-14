import delimited C:\Users\richard\Documents\iFPRI\maize\PIMMVC-master\PIMMVC-master\data\public\trader\trader.csv
sort idtrader
compress
label variable date "date of interview"
label variable q1 "Did you, during the first season of 2018, trade or transport maize as a business?"
label variable q1 "Did you, in first season of 2018, trade or transport maize as a business?"

label variable hhmaizeq6 "age of respondent"

rename hhmaizeq7 hhmaizeq7x
encode hhmaizeq7x, gen(hhmaizeq7)
order hhmaizeq7, after(hhmaizeq7x)
label variable hhmaizeq7 "gender of respondent"
label define hhmaizeq7 1 "Female" 2 "Male", replace
note hhmaizeq7: sex of respondent
drop hhmaizeq7x 

sort hhmaizeq8
rename hhmaizeq8 hhmaizeq8x
encode hhmaizeq8x, gen(hhmaizeq8)
order hhmaizeq8, after (hhmaizeq8x)
label variable hhmaizeq8 "marital status of respondent"
label define hhmaizeq8 1 "married" 2 "widowed" 3 "separated" 4 "single", replace
note hhmaizeq8: "marital state"
drop hhmaizeq8x 

rename hhmaizeq9 hhmaizeq9x
encode hhmaizeq9x, gen(hhmaizeq9)
label variable hhmaizeq9 "level of education of respondent?"
note hhmaizeq9: "education"
label define hhmaizeq9 1 "no formal education" 2 "some primary" 3 "finished primary" 4 "some sec" 5 "finished sec" 6 "higher than sec" 7 "other", replace
order hhmaizeq9, after (hhmaizeq8)
drop hhmaizeq9x

label variable hhmaizeq10 "When did you start trading agricultural commodities as a business? (year)"
label variable hhmaizeq11 "When did you start trading in maize? (year)"

exit
*make sure your dates are separated by / and if separated by -, please repalce the foward slash with a dash "-" then execute the entire do to the last line
destring hhmaizeq10, replace ignore(`"/"') force float
destring hhmaizeq11, replace ignore(`"/"') force float

label variable hhmaizeq12 "Do you trade other commodities than maize?"
rename hhmaizeq12 hhmaizeq12x
sort hhmaizeq12x
encode hhmaizeq12x, gen(hhmaizeq12)
order hhmaizeq12, after(hhmaizeq12x)
recode hhmaizeq12 1=0 2=1
label define hhmaizeq12 0 "No" 1 "Yes", replace
drop hhmaizeq12x

label variable hhmaizeq13a "other commodity1 traded=beans"
label variable hhmaizeq13b "other commodity2 traded=coffee"
label variable hhmaizeq13c "other commodity3 traded=soybean"
label variable hhmaizeq13d "other commodity4 traded=groundnuts"
label variable hhmaizeq13e "other commodity5 traded=cassava"
label variable hhmaizeq13f "other commodity6 traded=banana"
label variable hhmaizeq13g "other commodity7 traded=pumpkin"
label variable hhmaizeq13h "other commodity8 traded=watermelon"
label variable hhmaizeq13i "other commodity9 traded=simsim"
label variable hhmaizeq13j "other commodity10 traded=sorghum/millet"
label variable hhmaizeq13k "other commodity11 traded=rice"

rename hhmaizeq13a hhmaizeq13ax
sort hhmaizeq13ax
encode hhmaizeq13ax, gen(hhmaizeq13a)
order hhmaizeq13a, after(hhmaizeq13ax)
drop hhmaizeq13ax

rename hhmaizeq13b hhmaizeq13bx
rename hhmaizeq13c hhmaizeq13cx
rename hhmaizeq13d hhmaizeq13dx
rename hhmaizeq13e hhmaizeq13ex
rename hhmaizeq13f hhmaizeq13fx
rename hhmaizeq13g hhmaizeq13gx
rename hhmaizeq13h hhmaizeq13hx
rename hhmaizeq13i hhmaizeq13ix
rename hhmaizeq13j hhmaizeq13jx
rename hhmaizeq13k hhmaizeq13kx

encode hhmaizeq13bx, gen(hhmaizeq13b)
encode hhmaizeq13cx, gen(hhmaizeq13c)
encode hhmaizeq13dx, gen(hhmaizeq13d)
encode hhmaizeq13ex, gen(hhmaizeq13e)
encode hhmaizeq13fx, gen(hhmaizeq13f)
encode hhmaizeq13gx, gen(hhmaizeq13g)
encode hhmaizeq13hx, gen(hhmaizeq13h)
encode hhmaizeq13ix, gen(hhmaizeq13i)
encode hhmaizeq13jx, gen(hhmaizeq13j)
encode hhmaizeq13kx, gen(hhmaizeq13k)

order hhmaizeq13b, after(hhmaizeq13bx)
order hhmaizeq13c, after(hhmaizeq13cx)
order hhmaizeq13d, after(hhmaizeq13dx)
order hhmaizeq13e, after(hhmaizeq13ex)
order hhmaizeq13f, after(hhmaizeq13fx)
order hhmaizeq13g, after(hhmaizeq13gx)
order hhmaizeq13h, after(hhmaizeq13hx)
order hhmaizeq13i, after(hhmaizeq13ix)
order hhmaizeq13j, after(hhmaizeq13jx)
order hhmaizeq13k, after(hhmaizeq13kx)

recode hhmaizeq13a 1=0 2=1 3=96
recode hhmaizeq13b 1=0 2=1 3=96
recode hhmaizeq13c 1=0 2=1 3=96
recode hhmaizeq13d 1=0 2=1 3=96
recode hhmaizeq13e 1=0 2=1 3=96
recode hhmaizeq13f 1=0 2=1 3=96
recode hhmaizeq13g 1=0 2=1 3=96
recode hhmaizeq13h 1=0 2=1 3=96
recode hhmaizeq13i 1=0 2=1 3=96
recode hhmaizeq13j 1=0 2=1 3=96
recode hhmaizeq13k 1=0 2=1 3=96

label define hhmaizeq13a 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13b 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13c 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13d 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13e 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13f 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13g 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13h 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13i 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13j 0 "FALSE" 1 "TRUE" 96 "NA", replace
label define hhmaizeq13k 0 "FALSE" 1 "TRUE" 96 "NA", replace

drop hhmaizeq13bx hhmaizeq13cx hhmaizeq13dx hhmaizeq13ex hhmaizeq13fx hhmaizeq13gx hhmaizeq13hx hhmaizeq13ix hhmaizeq13jx hhmaizeq13kx 

label variable hhmaizeq14 "Value-wise, what was your total maize trade compared to other crops over the last year (%)?"
destring hhmaizeq14, replace force
replace hhmaizeq14=999 if hhmaizeq14==.

label variable hhmaizeq14b "Do you also mill and sell the flour from the maize you buy?"
sort hhmaizeq14b
rename hhmaizeq14b hhmaizeq14bx
encode hhmaizeq14bx, gen(hhmaizeq14b)
order hhmaizeq14b, after(hhmaizeq14bx)
recode hhmaizeq14b 1=0 2=1
label define hhmaizeq14b 0 "No" 1 "Yes", replace
drop hhmaizeq14bx

label variable hhmaizeq14c "What is the percentage (%) of the maize that you buy do you mill and sell as flour?"
sort hhmaizeq14c
destring hhmaizeq14c, replace force
replace hhmaizeq14c=999 if hhmaizeq14c==.

label variable hhmaizevalueq15a "Directly from farmers at the farm gate in villages "
label variable hhmaizevalueq15b "From assemblers in the village (farmers that buy from other farmers in the village and bulk);"
label variable hhmaizevalueq15c "From producer associations % procured"
label variable hhmaizevalueq15d "From other traders"
label variable hhmaizevalueq15e "From wholesalers"

note hhmaizevalueq15a: "Where do you normally buy your maize and how important is this source? Can we get also a sense of the importance (eg percentage procured)"
note hhmaizevalueq15b: "Where do you normally buy your maize and how important is this source? Can we get also a sense of the importance (eg percentage procured)"
note hhmaizevalueq15c: "Where do you normally buy your maize and how important is this source? Can we get also a sense of the importance (eg percentage procured)"
note hhmaizevalueq15d: "Where do you normally buy your maize and how important is this source? Can we get also a sense of the importance (eg percentage procured)"
note hhmaizevalueq15e: "Where do you normally buy your maize and how important is this source? Can we get also a sense of the importance (eg percentage procured)"


label variable hhmaizevalue2q16a "Percent(%) of tot sold directly to consumers in villages"
label variable hhmaizevalue2q16b "Percent(%) of tot sold to retail shops (who sell further to consumers)"
label variable hhmaizevalue2q16c "Percent(%) of tot sold to (larger) broker/trader/wholesaler"
label variable hhmaizevalue2q16d "Percent(%) of tot sold to processor or miller"
label variable hhmaizevalue2q16e "Percent(%) of tot sold to exporter(s)"
label variable hhmaizevalue2q16f "Percent(%) of tot sold in the market  during a market day"
label variable hhmaizevalue2q16g "Percent(%) of tot sold to cooperatives"
label variable hhmaizevalue2q16h "Percent(%) of tot sold to institutions (schools, prisons,...)"
label variable hhmaizevalue2q16i "Percent(%) of tot sold to Government"
label variable hhmaizevalue2q16j "Percent(%) of tot sold to NGO (WFP)"

notes hhmaizevalue2q16a: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16b: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16c: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16d: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16e: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16f: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16g: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16h: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16i: Where do you normally sell  your maize  and how important is this buyer in terms of %
notes hhmaizevalue2q16j: Where do you normally sell  your maize  and how important is this buyer in terms of %

sort hhmaizevalue2q16a
destring hhmaizevalue2q16a, replace force
destring hhmaizevalue2q16b, replace force
destring hhmaizevalue2q16c, replace force
destring hhmaizevalue2q16d, replace force
destring hhmaizevalue2q16e, replace force
destring hhmaizevalue2q16f, replace force
destring hhmaizevalue2q16g, replace force
destring hhmaizevalue2q16h, replace force
destring hhmaizevalue2q16i, replace force
destring hhmaizevalue2q16j, replace force

replace hhmaizevalue2q16a=999 if hhmaizevalue2q16a==.
replace hhmaizevalue2q16b=999 if hhmaizevalue2q16b==.
replace hhmaizevalue2q16c=999 if hhmaizevalue2q16c==.
replace hhmaizevalue2q16d=999 if hhmaizevalue2q16d==.
replace hhmaizevalue2q16e=999 if hhmaizevalue2q16e==.
replace hhmaizevalue2q16f=999 if hhmaizevalue2q16f==.
replace hhmaizevalue2q16g=999 if hhmaizevalue2q16g==.
replace hhmaizevalue2q16h=999 if hhmaizevalue2q16h==.
replace hhmaizevalue2q16i=999 if hhmaizevalue2q16i==.
replace hhmaizevalue2q16j=999 if hhmaizevalue2q16j==.

label variable hhmaizeq17 "Do you know the final destination of most of the maize you handle? where does it end up?"
label variable hhmaizeq17aa "mz destn1=Shops in this and neighboring district"
label variable hhmaizeq17ab "mz destn2=Consumers/traders/processors in Kampala"
label variable hhmaizeq17ac "mz destn3=Consumers/traders/processors in neighboring towns and other towns within Uganda (except Kampala)"
label variable hhmaizeq17ad "mz destn4=Consumers/traders/processors outside of Uganda (export)"
label variable hhmaizeq17a96 "mz destn96=Other"
label variable hhmaizeother_q17a "mz destn96=Other specify"

notes hhmaizeq17: maize destination
notes hhmaizeq17aa: what destination do you known for the maize you sell
notes hhmaizeq17ab: what destination do you known for the maize you sell
notes hhmaizeq17ac: what destination do you known for the maize you sell
notes hhmaizeq17ad: what destination do you known for the maize you sell
notes hhmaizeq17a96: what destination do you known for the maize you sell

sort hhmaizeq17
rename hhmaizeq17 hhmaizeq17x
encode hhmaizeq17x, gen(hhmaizeq17)
order hhmaizeq17, after(hhmaizeq17x)
recode hhmaizeq17 1=0 2=1 3=99 
label define hhmaizeq17 0 "No" 1 "Yes" 99"NA", replace
drop hhmaizeq17x

sort hhmaizeq17aa
rename hhmaizeq17aa hhmaizeq17aax
encode hhmaizeq17aax, gen(hhmaizeq17aa)
order hhmaizeq17aa, after(hhmaizeq17aax)
recode hhmaizeq17aa 1=0 2=1 3=99
label define hhmaizeq17aa 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq17aax

sort hhmaizeq17ab
rename hhmaizeq17ab hhmaizeq17abx
encode hhmaizeq17abx, gen(hhmaizeq17ab)
order hhmaizeq17ab, after(hhmaizeq17abx)
recode hhmaizeq17ab 1=0 2=1 3=99
label define hhmaizeq17ab 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq17abx

sort hhmaizeq17ac
rename hhmaizeq17ac hhmaizeq17acx
encode hhmaizeq17acx, gen(hhmaizeq17ac)
order hhmaizeq17ac, after(hhmaizeq17acx)
recode hhmaizeq17ac 1=0 2=1 3=99
label define hhmaizeq17ac 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq17acx

sort hhmaizeq17ad
rename hhmaizeq17ad hhmaizeq17adx
encode hhmaizeq17adx, gen(hhmaizeq17ad)
order hhmaizeq17ad, after(hhmaizeq17adx)
recode hhmaizeq17ad 1=0 2=1 3=99
label define hhmaizeq17ad 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq17adx

sort hhmaizeq17a96
rename hhmaizeq17a96 hhmaizeq17a96x
encode hhmaizeq17a96x, gen(hhmaizeq17a96)
order hhmaizeq17a96, after(hhmaizeq17a96x)
recode hhmaizeq17a96 1=0 2=1 3=99
label define hhmaizeq17a96 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq17a96x

sort hhmaizeq18
label variable hhmaizeq18 "When buying maize what transport means  do you mostly use?"
label variable hhmaizeother_q18 "Other transport specify"
notes hhmaizeq18: "Transport means mostly used when buying maize"
notes hhmaizeother_q18: "Transport means mostly used when buying maize"

sort hhmaizeq18
replace hhmaizeq18 = "z96" in 1
replace hhmaizeq18 = "z96" in 3
replace hhmaizeq18 = "z96" in 2
replace hhmaizeq18 = "z96" in 4
replace hhmaizeq18 = "z96" in 5
replace hhmaizeq18 = "z96" in 6
sort hhmaizeq18
rename hhmaizeq18 hhmaizeq18x
encode hhmaizeq18x, gen(hhmaizeq18)
order hhmaizeq18, after(hhmaizeq18x)
label define hhmaizeq18 1"On foot" 2 "Using wheelbarrow" 3 "Using bicycle" 4 "Using bodaboda" 5 "Using pick up car" 6 "Using a truck" 7 "Other transport means", replace
drop hhmaizeq18x

sort hhmaizeq19
replace hhmaizeq19 = "z96" in 1
replace hhmaizeq19 = "z96" in 3
replace hhmaizeq19 = "z96" in 2
replace hhmaizeq19 = "z96" in 4
replace hhmaizeq19 = "z96" in 5
replace hhmaizeq19 = "z96" in 6
replace hhmaizeq19 = "z96" in 7
replace hhmaizeq19 = "z96" in 8

sort hhmaizeq19
label variable hhmaizeq19 "When selliing maize what transport means do you mostly use?"
label variable hhmaizeother_q19 "Other transport specify"
notes hhmaizeq19: "Transport means when selling maize"
notes hhmaizeother_q19: "Transport means when selling maize"

rename hhmaizeq19 hhmaizeq19x
encode hhmaizeq19x, gen(hhmaizeq19)
order hhmaizeq19, after(hhmaizeq19x)
recode hhmaizeq19 1=3 2=4 3=5 4=6 6=7 5=98
label define hhmaizeq19 3 "Using bicycle" 4 "Using bodaboda" 5 "Using pick up car" 6 "Using a truck" 7 "Other transport means" 98 "NA", replace
drop hhmaizeq19x

rename hhmaizeradius hhmaizeradiusq20
sort hhmaizeradiusq20
label variable hhmaizeradiusq20 "What is the  action radius (for the maize that you buy) ?"
note hhmaizeradiusq20: "Action radius (for the maize that you buy) ?"
rename hhmaizeradiusq20 hhmaizeradiusq20x
encode hhmaizeradiusq20x, gen(hhmaizeradiusq20)
order hhmaizeradiusq20, after(hhmaizeradiusq20x)
label define hhmaizeradiusq20 1"Entire country" 2 "Various districts" 3 "One district" 4 "Various subcounties" 5 "One subcounty" 6 "Various parishes" 7 "One parish" 8 "Difft villages" 9 "One village" 10 "Only afew households", replace
drop hhmaizeradiusq20x

sort hhmaizeq21 
label variable hhmaizeq21 "How many other traders/transporters operate in the area you usually work (the radius of mz procuremt)"
note hhmaizeq21: "Within the radius maize is boought"
destring hhmaizeq21, replace force float
replace hhmaizeq21=998 if hhmaizeq21==.

label variable hhmaizeq22a "When maize prices go lowest, how many sellers (farmers, assemblers,..) do you visit to collect maize frm?"
label variable hhmaizeq22b "When maize prices go lowest, how much maize do you collect in total. (in kg)?"
label variable hhmaizeq22c "When maize prices go lowest, what was the lowest price you bought maize for?"
label variable hhmaizeq22d "When maize prices go lowest, how many buyers do you deliver to?"
label variable hhmaizeq22e "When maize prices go lowest, what was the lowest price you sold maize for per Kg?"
label variable hhmaizeq22f "When maize prices go lowest, do you have a minimum quantity requirement?"
label variable hhmaizeq22g "How many KGs are these?"

notes hhmaizeq22a: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22b: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22c: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22d: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22e: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22f: "For a typical day after harvest, when maize prices have reached their lowest levels" 
notes hhmaizeq22g: "For quantity of maize" 

sort hhmaizeq22d
destring hhmaizeq22d, replace force float
destring hhmaizeq22e, replace force float
replace hhmaizeq22d=998 if hhmaizeq22d==.

sort hhmaizeq22f
rename hhmaizeq22f hhmaizeq22fx
encode hhmaizeq22fx, gen(hhmaizeq22f)
order hhmaizeq22f, after(hhmaizeq22fx)
recode hhmaizeq22f 1=0 2=1
label define hhmaizeq22f 0 "No" 1 "Yes", replace
drop hhmaizeq22fx

sort hhmaizeq22g
destring hhmaizeq22g, replace force float

label variable hhmaizeq23a "During plantg & growg (2018B), how many sellers (farmers, assemblers,...) do you visit to collect maize from?"
label variable hhmaizeq23b "During plantg & growg (2018B), how much maize do you collect in total?"
label variable hhmaizeq23c "During plantg & growg (2018B), what was the highest price you bought maize?"
label variable hhmaizeq23d "During plantg & growg (2018B), how many buyers do you deliver to?"
label variable hhmaizeq23e "During plantg & growg (2018B), what was the highest price you sold maize for during low season (per Kg)?"
label variable hhmaizeq23f "During plantg & growg (2018B), do you have a minimum quantity requirement?"
label variable hhmaizeq23g "How many kg(s) is this?"

notes hhmaizeq23a: During planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23b: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23c: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23d: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23e: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23f: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels
notes hhmaizeq23g: On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels

sort hhmaizeq23d
destring hhmaizeq23d, replace force float
destring hhmaizeq23e, replace force float
destring hhmaizeq23g, replace force float
replace hhmaizeq23d=998 if hhmaizeq23d==.

sort hhmaizeq23f
rename hhmaizeq23f hhmaizeq23fx
encode hhmaizeq23fx, gen(hhmaizeq23f)
order hhmaizeq23f, after(hhmaizeq23fx)
recode hhmaizeq23f 1=0 2=1
label define hhmaizeq23f 0 "No" 1 "Yes", replace
drop hhmaizeq23fx

sort hhmaizeq24
rename hhmaizeq24 hhmaizeq24x
encode hhmaizeq24x, gen(hhmaizeq24)
order hhmaizeq24, after(hhmaizeq24x)
recode hhmaizeq24 1=0 2=1 3=98
label define hhmaizeq24 0 "No" 1 "Yes" 98 "NA", replace
drop hhmaizeq24x

label variable hhmaizeq24 "Do you also store maize?"
label variable hhmaizeq25a "Sell maize when my store is full"
label variable hhmaizeq25b "Sell maize when I have enough maize to organize a trip to the wholesaler, processor or any other buyer"
label variable hhmaizeq25c "Sell maize when the price of maize has increased"
label variable hhmaizeq25d "Sell maize when transport is available"
label variable hhmaizeq2596 "Sell maize when ~ Other reason"
label variable hhmaizeother_q25 "Sell maize when ~ Specify other reason"

notes hhmaizeq25a: After harvest, when maize prices are at their lowest levels, when do you sell?
notes hhmaizeq25b: After harvest, when maize prices are at their lowest levels, when do you sell?
notes hhmaizeq25c: After harvest, when maize prices are at their lowest levels, when do you sell?
notes hhmaizeq25d: After harvest, when maize prices are at their lowest levels, when do you sell?
notes hhmaizeq2596: After harvest, when maize prices are at their lowest levels, when do you sell?
notes hhmaizeother_q25: After harvest, when maize prices are at their lowest levels, when do you sell?

sort hhmaizeq25a
rename hhmaizeq25a hhmaizeq25ax
encode hhmaizeq25ax, gen(hhmaizeq25a)
order hhmaizeq25a, after(hhmaizeq25ax)
recode hhmaizeq25a 1=0 2=1
label define hhmaizeq25a 0 "No" 1 "Yes", replace
drop hhmaizeq25ax

rename hhmaizeq25b hhmaizeq25bx
rename hhmaizeq25c hhmaizeq25cx
rename hhmaizeq25d hhmaizeq25dx
rename hhmaizeq2596 hhmaizeq2596x

encode hhmaizeq25bx, gen(hhmaizeq25b)
encode hhmaizeq25cx, gen(hhmaizeq25c)
encode hhmaizeq25dx, gen(hhmaizeq25d)
encode hhmaizeq2596x, gen(hhmaizeq2596)

order hhmaizeq25b, after(hhmaizeq25bx)
order hhmaizeq25c, after(hhmaizeq25cx)
order hhmaizeq25d, after(hhmaizeq25dx)
order hhmaizeq2596, after(hhmaizeq2596x)

recode hhmaizeq25b 1=0 2=1
recode hhmaizeq25c 1=0 2=1
recode hhmaizeq25d 1=0 2=1
recode hhmaizeq2596 1=0 2=1

label define hhmaizeq25b 0 "No" 1 "Yes", replace
label define hhmaizeq25c 0 "No" 1 "Yes", replace
label define hhmaizeq25d 0 "No" 1 "Yes", replace
label define hhmaizeq2596 0 "No" 1 "Yes", replace

sort hhmaizeq25b
drop hhmaizeq25bx hhmaizeq25cx hhmaizeq25dx hhmaizeq2596x

sort hhmaizeq266
rename hhmaizeq266 hhmaizeq26a
label variable hhmaizeq26a "When maize prices have reached their highest levels, when do you sell"
label variable hhmaizeother_q26 "Specify other when you sell"
notes hhmaizeq26a: During planting and growing season, when does the trader sell
notes hhmaizeother_q26: During planting and growing season, when does the trader sell

label variable hhmaizeq26a "sell a-when my stor is full"
label variable hhmaizeq26b "sell b-When have enough maize to organize a trip to a buyer (wholesaler, processor,..)"
label variable hhmaizeq26c "sell c-When the price of maize has increased"
label variable hhmaizeq26d "sell d-When transport is available"
label variable hhmaizeq26e "sell e-When I find an appropriate buyer"
label variable hhmaizeq2696 "sell e-When I have other reason"

sort hhmaizeq26a
rename hhmaizeq26a hhmaizeq26ax
encode hhmaizeq26ax, gen(hhmaizeq26a)
order hhmaizeq26a, after(hhmaizeq26ax)
recode hhmaizeq26a 1=0 2=1 3=99
label define hhmaizeq26a 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq26ax

sort hhmaizeq26b
rename hhmaizeq26b hhmaizeq26bx
encode hhmaizeq26bx, gen(hhmaizeq26b)
order hhmaizeq26b, after(hhmaizeq26bx)
recode hhmaizeq26b 1=0 2=1 3=99
label define hhmaizeq26b 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq26bx

sort hhmaizeq26c
rename hhmaizeq26c hhmaizeq26cx
encode hhmaizeq26cx, gen(hhmaizeq26c)
order hhmaizeq26c, after(hhmaizeq26cx)
recode hhmaizeq26c 1=0 2=1 3=99
label define hhmaizeq26c 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq26cx

sort hhmaizeq26d
rename hhmaizeq26d hhmaizeq26dx
encode hhmaizeq26dx, gen(hhmaizeq26d)
order hhmaizeq26d, after(hhmaizeq26dx)
recode hhmaizeq26d 1=0 2=1 3=99
label define hhmaizeq26d 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq26dx

sort hhmaizeq26e
rename hhmaizeq26e hhmaizeq26ex
encode hhmaizeq26ex, gen(hhmaizeq26e)
order hhmaizeq26e, after(hhmaizeq26ex)
recode hhmaizeq26e 1=0 2=1 3=99
label define hhmaizeq26e 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq26ex

sort hhmaizeq2696
rename hhmaizeq2696 hhmaizeq2696x
encode hhmaizeq2696x, gen(hhmaizeq2696)
order hhmaizeq2696, after(hhmaizeq2696x)
recode hhmaizeq2696 1=0 2=1 3=99
label define hhmaizeq2696 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq2696x

label variable hhmaizeq27 "For maize you sold or plan to sell, how do you store your maize?"
label variable hhmaizeq28 "What is your storage capacity (kgs)?"
label variable hhmaizeq29a "services/asset from buyers a-Advances/credit?"
label variable hhmaizeq29b "services/asset from buyers b-Market information"
label variable hhmaizeq29c "services/asset from buyers c-Bags"
label variable hhmaizeq29d "services/asset from buyers d-Scales"
label variable hhmaizeq29e "services/asset from buyers e-Motorbike"
label variable hhmaizeq29f "services/asset from buyers f-Bicycle"
label variable hhmaizeq29g "services/asset from buyers g-technical training in PH handling (drying&storage)"
label variable hhmaizeq29h "services/asset from buyers h-Insurance"
label variable hhmaizeq29i "services/asset from buyers i-Receives nothing"
label variable hhmaizeq2996	"Other service/asset"
label variable hhmaizeother_q29	"Other service/asset specify"

notes hhmaizeq29a: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29b: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29c: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29d: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29e: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29f: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29g: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29h: Business receives this services/asset from the buyer(s)?
notes hhmaizeq29i: Business receives this services/asset from the buyer(s)?
notes hhmaizeq2996: Business receives this services/asset from the buyer(s)?
notes hhmaizeother_q29: Business receives this services/asset from the buyer(s)?

sort hhmaizeq27
rename hhmaizeq27 hhmaizeq27x
encode hhmaizeq27x, gen(hhmaizeq27)
order hhmaizeq27, after(hhmaizeq27x)
recode hhmaizeq27 6=99
label define hhmaizeq27 1 "Gunny bags on the floor" 2 "Gunny bags on palates" 3 "Pics bags on the floor" 4 "Pics bags on the palates" 5 "Hermetic container" 6 "Plastic containers" 7 "Baskets" 8 "pots" 9 "raditional granary" 10 "NA" 99 "Other", replace
drop hhmaizeq27x

destring hhmaizeq28, replace force float
replace hhmaizeq28=. if hhmaizeq28==.

sort hhmaizeq29a
rename hhmaizeq29a hhmaizeq29ax
encode hhmaizeq29ax, gen(hhmaizeq29a)
order hhmaizeq29a, after(hhmaizeq29ax)
recode hhmaizeq29a 1=0 2=1 3=99
label define hhmaizeq29a 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29ax

sort hhmaizeq29b
rename hhmaizeq29b hhmaizeq29bx
encode hhmaizeq29bx, gen(hhmaizeq29b)
order hhmaizeq29b, after(hhmaizeq29bx)
recode hhmaizeq29b 1=0 2=1 3=99
label define hhmaizeq29b 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29bx

sort hhmaizeq29c
rename hhmaizeq29c hhmaizeq29cx
encode hhmaizeq29cx, gen(hhmaizeq29c)
order hhmaizeq29c, after(hhmaizeq29cx)
recode hhmaizeq29c 1=0 2=1 3=99
label define hhmaizeq29c 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29cx

sort hhmaizeq29d
rename hhmaizeq29d hhmaizeq29dx
encode hhmaizeq29dx, gen(hhmaizeq29d)
order hhmaizeq29d, after(hhmaizeq29dx)
recode hhmaizeq29d 1=0 2=1 3=99
label define hhmaizeq29d 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29dx

sort hhmaizeq29e
rename hhmaizeq29e hhmaizeq29ex
encode hhmaizeq29ex, gen(hhmaizeq29e)
order hhmaizeq29e, after(hhmaizeq29ex)
recode hhmaizeq29e 1=0 2=1 3=99
label define hhmaizeq29e 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29ex

sort hhmaizeq29f
rename hhmaizeq29f hhmaizeq29fx
encode hhmaizeq29fx, gen(hhmaizeq29f)
order hhmaizeq29f, after(hhmaizeq29fx)
recode hhmaizeq29f 1=0 2=1 3=99
label define hhmaizeq29f 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29fx

sort hhmaizeq29g
rename hhmaizeq29g hhmaizeq29gx
encode hhmaizeq29gx, gen(hhmaizeq29g)
order hhmaizeq29g, after(hhmaizeq29gx)
recode hhmaizeq29g 1=0 2=1 3=99
label define hhmaizeq29g 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29gx

sort hhmaizeq29h
rename hhmaizeq29h hhmaizeq29hx
encode hhmaizeq29hx, gen(hhmaizeq29h)
order hhmaizeq29h, after(hhmaizeq29hx)
recode hhmaizeq29h 1=0 2=1 3=99
label define hhmaizeq29h 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29hx

sort hhmaizeq29i
rename hhmaizeq29i hhmaizeq29ix
encode hhmaizeq29ix, gen(hhmaizeq29i)
order hhmaizeq29i, after(hhmaizeq29ix)
recode hhmaizeq29i 1=0 2=1 3=99
label define hhmaizeq29i 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq29ix

sort hhmaizeq2996
rename hhmaizeq2996 hhmaizeq2996x
encode hhmaizeq2996x, gen(hhmaizeq2996)
order hhmaizeq2996, after(hhmaizeq2996x)
recode hhmaizeq2996 1=0 2=1 3=99
label define hhmaizeq2996 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq2996x

sort hhmaizeq30a
label variable hhmaizeq30a	"provide a-Inputs (seed, fertilizer)"
label variable hhmaizeq30b	"provide b-Tarpaulins for drying of maize "
label variable hhmaizeq30c	"provide c-PICS bags "
label variable hhmaizeq30d	"provide d-Gunny bags "
label variable hhmaizeq30e	"provide e-Technical assistance or training in PH handling (drying & storage) "
label variable hhmaizeq30f	"provide f-Credit "
label variable hhmaizeq30g	"provide g-Provide nothing "
label variable hhmaizeq3096	"provide 96-Other Service "
label variable hhmaizeother_q30	"provide 96-Other service specified "

notes hhmaizeq30a: Service provided to the sellers/farmers
notes hhmaizeq30b: Service provided to the sellers/farmers
notes hhmaizeq30c: Service provided to the sellers/farmers
notes hhmaizeq30d: Service provided to the sellers/farmers
notes hhmaizeq30e: Service provided to the sellers/farmers
notes hhmaizeq30f: Service provided to the sellers/farmers
notes hhmaizeq30g: Service provided to the sellers/farmers
notes hhmaizeq3096: Service provided to the sellers/farmers
notes hhmaizeother_q30: Service provided to the sellers/farmers

sort hhmaizeq30a
rename hhmaizeq30a hhmaizeq30ax
encode hhmaizeq30ax, gen(hhmaizeq30a)
order hhmaizeq30a, after(hhmaizeq30ax)
recode hhmaizeq30a 1=0 2=1 3=99
label define hhmaizeq30a 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30ax

sort hhmaizeq30b
rename hhmaizeq30b hhmaizeq30bx
encode hhmaizeq30bx, gen(hhmaizeq30b)
order hhmaizeq30b, after(hhmaizeq30bx)
recode hhmaizeq30b 1=0 2=1 3=99
label define hhmaizeq30b 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30bx

sort hhmaizeq30c
rename hhmaizeq30c hhmaizeq30cx
encode hhmaizeq30cx, gen(hhmaizeq30c)
order hhmaizeq30c, after(hhmaizeq30cx)
recode hhmaizeq30c 1=0 2=1 3=99
label define hhmaizeq30c 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30cx

sort hhmaizeq30d
rename hhmaizeq30d hhmaizeq30dx
encode hhmaizeq30dx, gen(hhmaizeq30d)
order hhmaizeq30d, after(hhmaizeq30dx)
recode hhmaizeq30d 1=0 2=1 3=99
label define hhmaizeq30d 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30dx

sort hhmaizeq30e
rename hhmaizeq30e hhmaizeq30ex
encode hhmaizeq30ex, gen(hhmaizeq30e)
order hhmaizeq30e, after(hhmaizeq30ex)
recode hhmaizeq30e 1=0 2=1 3=99
label define hhmaizeq30e 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30ex

sort hhmaizeq30f
rename hhmaizeq30f hhmaizeq30fx
encode hhmaizeq30fx, gen(hhmaizeq30f)
order hhmaizeq30f, after(hhmaizeq30fx)
recode hhmaizeq30f 1=0 2=1 3=99
label define hhmaizeq30f 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30fx

sort hhmaizeq30g
rename hhmaizeq30g hhmaizeq30gx
encode hhmaizeq30gx, gen(hhmaizeq30g)
order hhmaizeq30g, after(hhmaizeq30gx)
recode hhmaizeq30g 1=0 2=1 3=99
label define hhmaizeq30g 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq30gx

sort hhmaizeq3096
rename hhmaizeq3096 hhmaizeq3096x
encode hhmaizeq3096x, gen(hhmaizeq3096)
order hhmaizeq3096, after(hhmaizeq3096x)
recode hhmaizeq3096 1=0 2=1 3=99
label define hhmaizeq3096 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq3096x

label variable hhmaizeq31 "Do you have weighing scales?"
label variable hhmaizeq32 "Is the scale you use currently certified?"
label variable hhmaizeq33 "Do farmers trust your scales?"
label variable hhmaizeq34 "Do you give better prices for large quantities?"
label variable hhmaizeq34b "What do you consider a large quantity?"
label variable hhmaizeq35 "Do you give better prices for better quality maize (sorted-no stones, sand, etc)?"

sort hhmaizeq31
rename hhmaizeq31 hhmaizeq31x
encode hhmaizeq31x, gen(hhmaizeq31)
order hhmaizeq31, after(hhmaizeq31x)
recode hhmaizeq31 1=0 2=1 3=99
label define hhmaizeq31 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq31x

sort hhmaizeq32
rename hhmaizeq32 hhmaizeq32x
encode hhmaizeq32x, gen(hhmaizeq32)
order hhmaizeq32, after(hhmaizeq32x)
recode hhmaizeq32 1=0 2=1 3=99
label define hhmaizeq32 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq32x

sort hhmaizeq33
rename hhmaizeq33 hhmaizeq33x
encode hhmaizeq33x, gen(hhmaizeq33)
order hhmaizeq33, after(hhmaizeq33x)
recode hhmaizeq33 5=99
label define hhmaizeq33 1 "Yes, all of them" 2 "Yes, most of them" 3 "No, but what can they do" 4 "no, farmers insist on using their own" 99"NA", replace
drop hhmaizeq33x

sort hhmaizeq34
rename hhmaizeq34 hhmaizeq34x
encode hhmaizeq34x, gen(hhmaizeq34)
order hhmaizeq34, after(hhmaizeq34x)
recode hhmaizeq34 1=0 2=1 3=99
label define hhmaizeq34 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq34x

sort hhmaizeq34b
destring hhmaizeq34b, replace force float
replace hhmaizeq34b=. if hhmaizeq34b==.

sort hhmaizeq35
rename hhmaizeq35 hhmaizeq35x
encode hhmaizeq35x, gen(hhmaizeq35)
order hhmaizeq35, after(hhmaizeq35x)
recode hhmaizeq35 1=0 2=1 3=99
label define hhmaizeq35 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq35x

label variable hhmaizeq36a "Do dry it further"
label variable hhmaizeq36b "Do sorting out broken/rotten grains"
label variable hhmaizeq36c "Do sorting out broken combs"
label variable hhmaizeq36d "Do sieving it to remove sand and dust"
label variable hhmaizeq36e "Do keep and sell it as procured"
label variable hhmaizeq3696 "Do other activity"
label variable hhmaizeother_q36 "Other activity specified"

notes hhmaizeq36a: "How the trader handles the maize bought before selling?"
notes hhmaizeq36b: "How the trader handles the maize bought before selling?"
notes hhmaizeq36c: "How the trader handles the maize bought before selling?"
notes hhmaizeq36d: "How the trader handles the maize bought before selling?"
notes hhmaizeq36e: "How the trader handles the maize bought before selling?"
notes hhmaizeq3696: "How the trader handles the maize bought before selling?"

sort hhmaizeq36a
rename hhmaizeq36a hhmaizeq36ax
encode hhmaizeq36ax, gen(hhmaizeq36a)
order hhmaizeq36a, after(hhmaizeq36ax)
recode hhmaizeq36a 1=0 2=1 3=99
label define hhmaizeq36a 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq36ax

sort hhmaizeq36b
rename hhmaizeq36b hhmaizeq36bx
encode hhmaizeq36bx, gen(hhmaizeq36b)
order hhmaizeq36b, after(hhmaizeq36bx)
recode hhmaizeq36b 1=0 2=1 3=99
label define hhmaizeq36b 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq36bx

sort hhmaizeq36c
rename hhmaizeq36c hhmaizeq36cx
encode hhmaizeq36cx, gen(hhmaizeq36c)
order hhmaizeq36c, after(hhmaizeq36cx)
recode hhmaizeq36c 1=0 2=1 3=99
label define hhmaizeq36c 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq36cx

sort hhmaizeq36d
rename hhmaizeq36d hhmaizeq36dx
encode hhmaizeq36dx, gen(hhmaizeq36d)
order hhmaizeq36d, after(hhmaizeq36dx)
recode hhmaizeq36d 1=0 2=1 3=99
label define hhmaizeq36d 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq36dx

sort hhmaizeq36e
rename hhmaizeq36e hhmaizeq36ex
encode hhmaizeq36ex, gen(hhmaizeq36e)
order hhmaizeq36e, after(hhmaizeq36ex)
recode hhmaizeq36e 1=0 2=1 3=99
label define hhmaizeq36e 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq36ex

sort hhmaizeq3696
rename hhmaizeq3696 hhmaizeq3696x
encode hhmaizeq3696x, gen(hhmaizeq3696)
order hhmaizeq3696, after(hhmaizeq3696x)
recode hhmaizeq3696 1=0 2=1 3=99
label define hhmaizeq3696 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq3696x

label variable hhmaizeq37 "Do you usually get better prices for better quality maize (well sorted- no stones, sand, etc) that you sell?"
label variable hhmaizeq38 "Do you often buy the maize when it is still in the garden?"
label variable hhmaizeq39a "1/3 most serious risk1-storage?"
label variable hhmaizeq39b "1/3 most serious risk2-Price fluctuations"
label variable hhmaizeq39c "1/3 most serious risk3-Thieft?"
label variable hhmaizeq39d "1/3 most serious risk4-Fire"
label variable hhmaizeq39e "1/3 most serious risk5-Pests"
label variable hhmaizeq3996 "1/3 most serious Other risk"
label variable hhmaizeother_q39 "1/3 most serious other risk specified"

sort hhmaizeq37
rename hhmaizeq37 hhmaizeq37x
encode hhmaizeq37x, gen(hhmaizeq37)
order hhmaizeq37, after(hhmaizeq37x)
recode hhmaizeq37 1=0 2=1 3=99
label define hhmaizeq37 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq37x

sort hhmaizeq38
rename hhmaizeq38 hhmaizeq38x
encode hhmaizeq38x, gen(hhmaizeq38)
order hhmaizeq38, after(hhmaizeq38x)
recode hhmaizeq38 1=0 2=1 3=99
label define hhmaizeq38 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq38x

notes hhmaizeq39a: "One of three most serious risks for the maize enterprise"
notes hhmaizeq39b: "One of three most serious risks for the maize enterprise"
notes hhmaizeq39c: "One of three most serious risks for the maize enterprise"
notes hhmaizeq39d: "One of three most serious risks for the maize enterprise"
notes hhmaizeq39e: "One of three most serious risks for the maize enterprise"
notes hhmaizeq3996: "One of three most serious risks for the maize enterprise"
notes hhmaizeother_q39: "One of three most serious risks for the maize enterprise"

sort hhmaizeq39a
rename hhmaizeq39a hhmaizeq39ax
encode hhmaizeq39ax, gen(hhmaizeq39a)
order hhmaizeq39a, after(hhmaizeq39ax)
recode hhmaizeq39a 1=0 2=1 3=99
label define hhmaizeq39a 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq39ax

sort hhmaizeq39b
rename hhmaizeq39b hhmaizeq39bx
encode hhmaizeq39bx, gen(hhmaizeq39b)
order hhmaizeq39b, after(hhmaizeq39bx)
recode hhmaizeq39b 1=0 2=1 3=99
label define hhmaizeq39b 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq39bx

sort hhmaizeq39c
rename hhmaizeq39c hhmaizeq39cx
encode hhmaizeq39cx, gen(hhmaizeq39c)
order hhmaizeq39c, after(hhmaizeq39cx)
recode hhmaizeq39c 1=0 2=1 3=99
label define hhmaizeq39c 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq39cx

sort hhmaizeq39d
rename hhmaizeq39d hhmaizeq39dx
encode hhmaizeq39dx, gen(hhmaizeq39d)
order hhmaizeq39d, after(hhmaizeq39dx)
recode hhmaizeq39d 1=0 2=1 3=99
label define hhmaizeq39d 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq39dx

sort hhmaizeq39e
rename hhmaizeq39e hhmaizeq39ex
encode hhmaizeq39ex, gen(hhmaizeq39e)
order hhmaizeq39e, after(hhmaizeq39ex)
recode hhmaizeq39e 1=0 2=1 3=99
label define hhmaizeq39e 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq39ex

sort hhmaizeq3996
rename hhmaizeq3996 hhmaizeq3996x
encode hhmaizeq3996x, gen(hhmaizeq3996)
order hhmaizeq3996, after(hhmaizeq3996x)
recode hhmaizeq3996 1=0 2=1 3=99
label define hhmaizeq3996 0 "No" 1 "Yes" 99 "NA", replace
drop hhmaizeq3996x

sort hhmaizeq40
label variable hhmaizeq40 "Do you make agreements on a maximum buyer price with other traders?"
label variable hhmaizeq40a "Self rating on location: also see notes 
label variable hhmaizeq40b "Self rating on price: also see notes
label variable hhmaizeq40c "Self rating on Quality of service: also see notes
label variable hhmaizeq40d "Self rating on honesty: also see notes
label variable hhmaizeq40e "Self rating on reputation: also see notes

notes hhmaizeq40: agreement with other traders
notes hhmaizeq40a: Self rating as a business on this particular trait
notes hhmaizeq40a: "according to your own opinion, are you easy to reach (eg did he come by your door, can you call him,…) (1. Not easy to reach at all, 5. 40b. Very easy to reach)"

notes hhmaizeq40b: Self rating as a business on this particular trait
notes hhmaizeq40b: "according to your own opinion, do you pay a competitive price (1. Pays a very low price to farmers, 5. Pays very high price to farmers)"

notes hhmaizeq40c: Self rating as a business on this particular trait
notes hhmaizeq40c: "according to your own opinion, do you provide a good service to farmers (eg prompt payment, accepts small amounts, provides bags) (1. Very poor quality of service, 5. Very high quality of services)"

notes hhmaizeq40d: Self rating as a business on this particular trait
notes hhmaizeq40d: "according to your own opinion, are you an honest businessman who does not try to cheat the farmer? (1. Completely not to be trusted, 5. Extremely honest businessman/woman)"

notes hhmaizeq40e: Self rating as a business on this particular trait
notes hhmaizeq40e: "according to your own opinion, what do others think you as a trader? (1. Others think he is a lousy businessmen, 5. Others think he is a fantastic businessman)"

compress
sort idtrader


