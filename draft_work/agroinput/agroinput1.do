import delimited "C:\Users\Ariong Richard\Dropbox (IFPRI)\Ariong_richard\Documents\Maize\MVC\work\agroinput\agroinput.csv", encoding(ISO-8859-2)


compress
label variable date "ag/date of interview"

sort q1
rename q1 q1x
encode q1x, gen(q1)
order q1, after(q1x)
recode q1 1=0 2=1
label define q1 1 "Yes" 0 "No", replace
label variable q1 "ag/are you talking to the shop owner?"
note q1: Busines owner
drop q1x 

rename q2 q2x
encode q2x, gen(q2)
order q2, after(q2x)
recode q2 2=99
label define q2 1 "Yes" 0 "No" 99 "NA", replace
label variable q2 "ag/are you the person responsible for routine activities of the business?"
note q2: Business operator
drop q2x 

rename q3 q3x
encode q3x, gen(q3)
order q3, after(q3x)
recode q3 2=99
label define q3 1 "Yes" 0 "No" 99 "NA", replace
label variable q3 "ag/can I talk to someone responsible for the shop activities?"
note q3: Business operator
drop q3x 

rename idagro idagrox
encode idagrox, gen(idagro)
order idagro, after(idagrox)
rename idagrox idagroxx
rename idagro idagrox
rename idagroxx idagro
label variable idagro "ag/id agroinput dealer"
label variable idagrox "ag/id agroinput dealer coded"

rename hhmaizeq6 hhmaizeq6x
encode hhmaizeq6x, gen(hhmaizeq6)
order hhmaizeq6, after(hhmaizeq6x)
recode hhmaizeq6 1=2 2=3 3=4
label define hhmaizeq6 1 "Village" 2 "Trading center" 3 "Town" 4 "District capital" 99 "NA", replace
label variable hhmaizeq6 "ag/location of agro-input shop"
note hhmaizeq6: Business location
drop hhmaizeq6x 

label variable hhmaizeq6a "ag/distance (km) of agro-input shop to nearest tarmac rd"
label variable hhmaizeq6b "ag/distance (km) of agro-input shop to nearest murram rd"
label variable hhmaizeq6c "ag/distance (km) of agro-input shop to nearest competitor"
note hhmaizeq6c: competitors as other shop that sells improved maize or bean seed varieties
label variable hhmaizeq6d "ag/number of other agro-input shops in the neigbourhood"

tostring hhmaizeq6e, replace
rename hhmaizeq6e hhmaizeq6ex
encode hhmaizeq6ex, gen(hhmaizeq6e)
order hhmaizeq6e, after(hhmaizeq6ex)
recode hhmaizeq6e 3=4 4=5 5=96
label define hhmaizeq6e 1 "Director/owner" 2 "Manager" 3 "Secretary" 4 "Treasurer" 5 "Sales person" 96 "Other" 99 "NA", replace
label variable hhmaizeq6e "ag/What is your role in the business?"
note hhmaizeq6e: Business role
drop hhmaizeq6ex 
label variable hhmaizeother_q6e "ag/specify the your other role in the business?"

rename hhmaizeq6f hhmaizeq6fx
encode hhmaizeq6fx, gen(hhmaizeq6f)
order hhmaizeq6f, after(hhmaizeq6fx)
recode hhmaizeq6f 1=0 2=1 3=99
label define hhmaizeq6f 1 "Yes" 0 "No" 99 "NA", replace
label variable hhmaizeq6f "ag/Are you a relative of the owner?"
note hhmaizeq6f: Operator relation to business owner
drop hhmaizeq6fx 

label variable hhmaizeq6g "ag/age of respondent"
label variable hhmaizeq7 "ag/gender of respondent"

rename hhmaizeq7 hhmaizeq7x
encode hhmaizeq7x, gen(hhmaizeq7)
order hhmaizeq7, after(hhmaizeq7x)
label define hhmaizeq7 1 "Female" 2 "Male" 96 "Other" 99 "NA", replace
label variable hhmaizeq7 "ag/gender of respondent"
note hhmaizeq7: sex business operator
drop hhmaizeq7x 

rename hhmaizeq8 hhmaizeq8x
encode hhmaizeq8x, gen(hhmaizeq8)
order hhmaizeq8, after(hhmaizeq8x)
recode hhmaizeq8 2=4 3=5 
label define hhmaizeq8 1 "Married" 2 "Widowed" 3 "Divorced" 4 "Separated" 5 "Single" 96 "Other" 99 "NA", replace
label variable hhmaizeq8 "ag/Marital status of respondent"
note hhmaizeq8: Marital status
drop hhmaizeq8x 

rename hhmaizeq9 hhmaizeq9x
encode hhmaizeq9x, gen(hhmaizeq9)
order hhmaizeq9, after(hhmaizeq9x)
recode hhmaizeq9 7=96
label define hhmaizeq9 1 "No formal education" 2 "Some primary" 3 "Finished primary" 4 "Some secondary" 5 "Finished secondary" 6 "Higher than secondary" 96 "Other" 99 "NA", replace
label variable hhmaizeq9 "ag/Level of education of respondent"
note hhmaizeq9: Education status
drop hhmaizeq9x 

rename hhmaizeq10 hhmaizeq10x
encode hhmaizeq10x, gen(hhmaizeq10)
order hhmaizeq10, after(hhmaizeq10x)
recode hhmaizeq10 1=0 2=1
label define hhmaizeq10 0 "No" 1 "Yes", replace
label variable hhmaizeq10 "ag/Is this a specialized farm input shop that only sells farm inputs?"
note hhmaizeq10: Business products
drop hhmaizeq10x 

rename hhmaizeq11 hhmaizeq11x
encode hhmaizeq11x, gen(hhmaizeq11)
order hhmaizeq11, after(hhmaizeq11x)
recode hhmaizeq11 1=0 2=1
label define hhmaizeq11 0 "No" 1 "Yes", replace
label variable hhmaizeq11 "ag/Can we take a picture of your shop?"
note hhmaizeq11: Business premise
drop hhmaizeq11x 

destring hhmaizeq12, replace ignore(`"/"') force float 
label variable hhmaizeq12 "ag/When did you start this business/input shop?"

rename hhmaizeq13 hhmaizeq13x
encode hhmaizeq13x, gen(hhmaizeq13)
order hhmaizeq13, after(hhmaizeq13x)
recode hhmaizeq13 1=0 2=1
recode hhmaizeq13 3=1 1=0
label define hhmaizeq13 0 "No" 1 "Yes", replace
label variable hhmaizeq13 "ag/Is the business registered as seed dealer with UNADA" 
notes hhmaizeq13: UNADA-Uganda National Agro-input Dealers Association
note hhmaizeq13: Business registration
drop hhmaizeq13x 

rename hhmaizeq14 hhmaizeq14x
encode hhmaizeq14x, gen(hhmaizeq14)
order hhmaizeq14, after(hhmaizeq14x)
recode hhmaizeq14 1=0 2=1
label define hhmaizeq14 0 "No" 1 "Yes", replace
label variable hhmaizeq14 "ag/Does this business have a Trading license issued by the LG"
note hhmaizeq14: LG - Local government
drop hhmaizeq14x 

rename hhmaizeq15 hhmaizeq15x
encode hhmaizeq15x, gen(hhmaizeq15)
order hhmaizeq15, after(hhmaizeq15x)
recode hhmaizeq15 1=98 2=0 3=1
label define hhmaizeq15 0 "No" 1 "Yes" 98 "Don't know", replace
label variable hhmaizeq15 "ag/Is this business a member of any other professional association?"
note hhmaizeq15: Membership to an organized body
drop hhmaizeq15x 

label variable hhmaizeq15a "ag/Specify which association or organization"

rename hhmaizeq16 hhmaizeq16x
encode hhmaizeq16x, gen(hhmaizeq16)
order hhmaizeq16, after(hhmaizeq16x)
recode hhmaizeq16 1=0 2=1
label define hhmaizeq16 0 "No" 1 "Yes" 98 "Don't know", replace
label variable hhmaizeq16 "ag/Do you have other outlets?"
note hhmaizeq16: Business branches
drop hhmaizeq16x 

destring hhmaizeq16a, replace force float
replace hhmaizeq16a=99 if hhmaizeq16a==.
label variable hhmaizeq16a "ag/How many other outlets do you have?"

rename hhmaizeq17a hhmaizeq17ax
encode hhmaizeq17ax, gen(hhmaizeq17a)
order hhmaizeq17a, after(hhmaizeq17ax)
recode hhmaizeq17a 1=0 2=1
label define hhmaizeq17a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq17a "ag/deal in input1- Machinery"
note hhmaizeq17a: Inpiuts sold
drop hhmaizeq17ax 

rename hhmaizeq17b hhmaizeq17bx
encode hhmaizeq17bx, gen(hhmaizeq17b)
order hhmaizeq17b, after(hhmaizeq17bx)
recode hhmaizeq17b 1=0 2=1
label define hhmaizeq17b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq17b "ag/deal in input2- Equipment"
note hhmaizeq17b: Inpiuts sold
drop hhmaizeq17bx 

rename hhmaizeq17c hhmaizeq17cx
encode hhmaizeq17cx, gen(hhmaizeq17c)
order hhmaizeq17c, after(hhmaizeq17cx)
recode hhmaizeq17c 1=0 2=1
label define hhmaizeq17c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq17c "ag/deal in input3- Seeds"
note hhmaizeq17c: Inpiuts sold
drop hhmaizeq17cx 

rename hhmaizeq17d hhmaizeq17dx
encode hhmaizeq17dx, gen(hhmaizeq17d)
order hhmaizeq17d, after(hhmaizeq17dx)
recode hhmaizeq17d 1=0 2=1
label define hhmaizeq17d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq17d "ag/deal in input4- Chemicals"
note hhmaizeq17d: Inpiuts sold
drop hhmaizeq17dx 

rename hhmaizeq17e hhmaizeq17ex
encode hhmaizeq17ex, gen(hhmaizeq17e)
order hhmaizeq17e, after(hhmaizeq17ex)
recode hhmaizeq17e 1=0 2=1
label define hhmaizeq17e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq17e "ag/deal in input5- Fertilizers"
note hhmaizeq17e: Inpiuts sold
drop hhmaizeq17ex 

rename hhmaizeq1796 hhmaizeq1796x
encode hhmaizeq1796x, gen(hhmaizeq1796)
order hhmaizeq1796, after(hhmaizeq1796x)
recode hhmaizeq1796 1=0 2=1
label define hhmaizeq1796 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq1796 "ag/deal in input6- Other"
note hhmaizeq1796: Inpiuts sold
drop hhmaizeq1796x 
label variable hhmaizeother_q17 "ag/other input specify"

rename hhmaizeq18a hhmaizeq18ax
encode hhmaizeq18ax, gen(hhmaizeq18a)
order hhmaizeq18a, after(hhmaizeq18ax)
recode hhmaizeq18a 1=0 2=1
label define hhmaizeq18a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18a "ag/sell seed type1- Hybrid maize seed"
note hhmaizeq18a: Type of seed sold
drop hhmaizeq18ax 

rename hhmaizeq18b hhmaizeq18bx
encode hhmaizeq18bx, gen(hhmaizeq18b)
order hhmaizeq18b, after(hhmaizeq18bx)
recode hhmaizeq18b 1=0 2=1
label define hhmaizeq18b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18b "ag/sell seed type2- OPV Maize seed"
note hhmaizeq18b: Type of seed sold
drop hhmaizeq18bx 

rename hhmaizeq18c hhmaizeq18cx
encode hhmaizeq18cx, gen(hhmaizeq18c)
order hhmaizeq18c, after(hhmaizeq18cx)
recode hhmaizeq18c 1=0 2=1
label define hhmaizeq18c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18c "ag/sell seed type3- Vegetable seeds"
note hhmaizeq18c: Type of seed sold
drop hhmaizeq18cx 

rename hhmaizeq18d hhmaizeq18dx
encode hhmaizeq18dx, gen(hhmaizeq18d)
order hhmaizeq18d, after(hhmaizeq18dx)
recode hhmaizeq18d 1=0 2=1
label define hhmaizeq18d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18d "ag/sell seed type4- Ornamental seed"
note hhmaizeq18d: Type of seed sold
drop hhmaizeq18dx 

rename hhmaizeq18e hhmaizeq18ex
encode hhmaizeq18ex, gen(hhmaizeq18e)
order hhmaizeq18e, after(hhmaizeq18ex)
recode hhmaizeq18e 1=0 2=1
label define hhmaizeq18e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18e "ag/sell seed type5- Pasture seed"
note hhmaizeq18e: Type of seed sold
drop hhmaizeq18ex 

rename hhmaizeq18f hhmaizeq18fx
encode hhmaizeq18fx, gen(hhmaizeq18f)
order hhmaizeq18f, after(hhmaizeq18fx)
recode hhmaizeq18f 1=0 2=1
label define hhmaizeq18f 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18f "ag/sell seed type6- Rice seed"
note hhmaizeq18f: Type of seed sold
drop hhmaizeq18fx 

rename hhmaizeq18g hhmaizeq18gx
encode hhmaizeq18gx, gen(hhmaizeq18g)
order hhmaizeq18g, after(hhmaizeq18gx)
recode hhmaizeq18g 1=0 2=1
label define hhmaizeq18g 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18g "ag/sell seed type1- Bean seed"
note hhmaizeq18g: Type of seed sold
drop hhmaizeq18gx 

rename hhmaizeq18h hhmaizeq18hx
encode hhmaizeq18hx, gen(hhmaizeq18h)
order hhmaizeq18h, after(hhmaizeq18hx)
recode hhmaizeq18h 1=0 2=1
label define hhmaizeq18h 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq18h "ag/sell seed type1- Don't sell seed"
note hhmaizeq18h: Type of seed sold
drop hhmaizeq18hx 

rename hhmaizeq1896 hhmaizeq1896x
encode hhmaizeq1896x, gen(hhmaizeq1896)
order hhmaizeq1896, after(hhmaizeq1896x)
recode hhmaizeq1896 1=0 2=1
label define hhmaizeq1896 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq1896 "ag/sell seed type1- Other maize seed"
note hhmaizeq1896: Type of seed sold
drop hhmaizeq1896x 
label variable hhmaizeother_q18 "ag/other input specify"

label variable hhmaizeq19 "ag/number of hybrid maize seed varieties sold in season 2018A"
notes hhmaizeq19: Stock and turnover about the first season of 2018

rename hhmaizeseed1q20 hhmaizeseed1q20x
encode hhmaizeseed1q20x, gen(hhmaizeseed1q20)
order hhmaizeseed1q20, after(hhmaizeseed1q20x)
recode hhmaizeseed1q20 1=4 2=8 3=1 4=2 5=3 7=99
label define hhmaizeseed1q20 1 "Longe 10H" 2 "Longe 7H" 3 "Longe_7R" 4 "Bazooka" 5 "Longe 6H" 6 "Panner" 7 "Wema" 8 "KH series" 99 "NA", replace
note hhmaizeseed1q20: Seed sold
drop hhmaizeseed1q20x 

label variable hhmaizeseed1q20  "ag/hybrid seed var1- name of maize hybrid variety"
notes hhmaizeseed1q20: of the top 3 most popular hybrid seed sold by agroinput dealr in first season of 2018
label variable hhmaizeseed1q21  "ag/hybrid seed var1- price per kg at beginning of season 2018A"
destring hhmaizeseed1q21, replace force float
replace hhmaizeseed1q21=99 if hhmaizeseed1q21==.
label variable hhmaizeseed1q22  "ag/hybird seed var1- total quantity (Kg) sold in season 2018A"
destring hhmaizeseed1q22, replace force float
replace hhmaizeseed1q22=999 if hhmaizeseed1q22==.
label variable hhmaizeseed1seed_num  "ag/hybrid seed var1- seed number (for season 2018A)"
destring hhmaizeseed1seed_num, replace force float
replace hhmaizeseed1seed_num=99 if hhmaizeseed1seed_num==.


rename hhmaizeseed2q20 hhmaizeseed2q20x
encode hhmaizeseed2q20x, gen(hhmaizeseed2q20)
order hhmaizeseed2q20, after(hhmaizeseed2q20x)
recode hhmaizeseed2q20 1=4 2=8 3=1 4=5 5=2 6=3 7=99
label define hhmaizeseed2q20 1 "Longe 10H" 2 "Longe 7H" 3 "Longe_7R" 4 "Bazooka" 5 "Longe 6H" 6 "Panner" 7 "Wema" 8 "KH series" 99 "NA", replace
note hhmaizeseed2q20: Maize seed variety sold
drop hhmaizeseed2q20x 

label variable hhmaizeseed2q20  "ag/seed var2- name of maize hybrid variety"
notes hhmaizeseed2q20: of the top 3 most popular hybrid seed sold by agroinput dealr in first season of 2018
label variable hhmaizeseed2q21  "ag/seed var2- price per kg at beginning of season 2018A"
destring hhmaizeseed2q21, replace force float
replace hhmaizeseed2q21=99 if hhmaizeseed2q21==.
label variable hhmaizeseed2q22  "ag/seed var2- total quantity (Kg) sold in season 2018A"
destring hhmaizeseed2q22, replace force float
replace hhmaizeseed2q22=999 if hhmaizeseed2q22==.
label variable hhmaizeseed2seed_num  "ag/seed var2- seed number (for season 2018A)"
destring hhmaizeseed2seed_num, replace force float
replace hhmaizeseed2seed_num=99 if hhmaizeseed2seed_num==.

rename hhmaizeseed3q20 hhmaizeseed3q20x
encode hhmaizeseed3q20x, gen(hhmaizeseed3q20)
order hhmaizeseed3q20, after(hhmaizeseed3q20x)
recode hhmaizeseed3q20 1=4 2=8 3=5 4=2 5=3 6=99
label define hhmaizeseed3q20 1 "Longe 10H" 2 "Longe 7H" 3 "Longe_7R" 4 "Bazooka" 5 "Longe 6H" 6 "Panner" 7 "Wema" 8 "KH series" 99 "NA", replace
note hhmaizeseed3q20: Maize seed variety sold
drop hhmaizeseed3q20x 

label variable hhmaizeseed3q20  "ag/seed var3- name of maize hybrid seed variety"
notes hhmaizeseed3q20: of the top 3 most popular hybrid seed sold by agroinput dealr in first season of 2018
label variable hhmaizeseed3q21  "ag/seed var3- price per kg at beginning of season 2018A"
destring hhmaizeseed3q21, replace force float
replace hhmaizeseed3q21=99 if hhmaizeseed3q21==.
label variable hhmaizeseed3q22  "ag/seed var3- total quantity (Kg) sold in season 2018A"
destring hhmaizeseed3q22, replace force float
replace hhmaizeseed3q22=999 if hhmaizeseed3q22==.
label variable hhmaizeseed3seed_num  "ag/seed var3- seed number (for season 2018A)"
destring hhmaizeseed3seed_num, replace force float
replace hhmaizeseed3seed_num=99 if hhmaizeseed3seed_num==.

label variable hhmaizeseed_select "ag/selected hybrid seed var number (out of the 3 top selling)"
notes hhmaizeseed_select: randomly selected hybrid maize seed variety out of the top three varieties
label variable hhmaizeseed_select_name "ag/selected hybrid maize seed variety (out of the 3)"
notes hhmaizeseed_select_name: of the randomly selected hybird maize seed variety out of the top three varieties
label variable hhmaizeseed_select_price "ag/price of selected hybrid mz seed variety"
notes hhmaizeseed_select_price: of the randomly selected hybid maize seed variety out of the top three varieties

rename hhmaizeseed_select_name hhmaizeseed_select_namex
encode hhmaizeseed_select_namex, gen(hhmaizeseed_select_name)
order hhmaizeseed_select_name, after(hhmaizeseed_select_namex)
recode hhmaizeseed_select_name 1=4 2=8 3=1 4=5 5=2 6=3 7=99
label define hhmaizeseed_select_name 1 "Longe 10H" 2 "Longe 7H" 3 "Longe_7R" 4 "Bazooka" 5 "Longe 6H" 6 "Panner" 7 "Wema" 8 "KH series" 99 "NA", replace
note hhmaizeseed_select_name: for the randomly selected maize HYBRID seed variety sold among the top 3 listed
sort hhmaizeseed_select_name
drop hhmaizeseed_select_namex 

destring hhmaizeseed_select_price, replace force float
replace hhmaizeseed_select_price=99 if hhmaizeseed_select_price==.

rename hhmaizesellq23 hhmaizesellq23x
encode hhmaizesellq23x, gen(hhmaizesellq23)
order hhmaizesellq23, after(hhmaizesellq23x)
recode hhmaizesellq23 1=0 2=1 3=99
recode hhmaizesellq23 0=1 1=99
label define hhmaizesellq23 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizesellq23 "ag/was the selected H variety certified seed"
note hhmaizesellq23: on the selected hybrid mz seed variety
drop hhmaizesellq23x 

rename hhmaizesellq24 hhmaizesellq24x
encode hhmaizesellq24x, gen(hhmaizesellq24)
order hhmaizesellq24, after(hhmaizesellq24x)
recode hhmaizesellq24 2=99
label define hhmaizesellq24 1 "Sealed and labeled" 2 "Sealed but not labeled" 3 "Unsealed but labeled" 4 "Unsealed and unlabeled" 96 "Other" 99 "NA", replace
label variable hhmaizesellq24 "ag/How was  the selected H seed variety packaged when you obtained/purchased it?"
note hhmaizesellq24: on the selected hybrid maize seed variety
drop hhmaizesellq24x 

label variable hhmaizesellq25 "ag/How much (kgs) of selected H seed var was stocked in season 2018A"
destring hhmaizesellq25, replace force float
replace hhmaizesellq25=99 if hhmaizesellq25==.
note hhmaizesellq25: on the selected hybrid maize seed variety

label variable hhmaizesellq26 "ag/How much (kgs) of selected H seed var was carried over from prev. season (2017) into 2018A"
destring hhmaizesellq26, replace force float
replace hhmaizesellq26=99 if hhmaizesellq26==.
note hhmaizesellq26: on the selected hybrid maize seed variety

label variable hhmaizesellq27 "ag/How much (kgs) of selected seed var was lost/wasted in season 2018A"
destring hhmaizesellq27, replace force float
replace hhmaizesellq27=999 if hhmaizesellq27==.
note hhmaizesellq27: on the selected hybrid maize seed variety

label variable hhmaizesellq28 "ag/How much (kgs) of selected H seed var was still in stock at start of season 2018B"
destring hhmaizesellq28, replace force float
replace hhmaizesellq28=999 if hhmaizesellq28==.
note hhmaizesellq28: on the selected hybrid maize seed variety

label variable hhmaizesellq29 "ag/What is the smallest package size (kgs) of selected H seed var did you sell 2018A?"
destring hhmaizesellq29, replace force float
replace hhmaizesellq29=999 if hhmaizesellq29==.
note hhmaizesellq29: on the selected hybrid maize seed variety

rename hhmaizesellq30 hhmaizesellq30x
encode hhmaizesellq30x, gen(hhmaizesellq30)
order hhmaizesellq30, after(hhmaizesellq30x)
recode hhmaizesellq30 1=0 2=1 3=99
label define hhmaizesellq30 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizesellq30 "ag/Did you repackage the selected H mz seed var when clients wanted smaller packages?"
note hhmaizesellq30: on the selected hybrid maize seed variety
drop hhmaizesellq30x 

label variable hhmaizesellq30b "ag/What was the smallest repackage (kgs) sold in season 2018A"
destring hhmaizesellq30b, replace force float
replace hhmaizesellq30b=999 if hhmaizesellq30b==.
note hhmaizesellq30b: on the selected hybrid maize seed variety

rename hhmaizesellq31 hhmaizesellq31x
encode hhmaizesellq31x, gen(hhmaizesellq31)
order hhmaizesellq31, after(hhmaizesellq31x)
recode hhmaizesellq31 1=96 2=1 3=2 6=99
label define hhmaizesellq31 1 "Agro input wholesaler" 2 "Seed company" 3 "Research station" 4 "Local Seed Business" 5 "Individual seed producer" 96 "Other" 99 "NA", replace
label variable hhmaizesellq31 "ag/from whom did you directly buy the selected H mz seed var 2018A?"
note hhmaizesellq31: on the selected hybrid maize seed variety
drop hhmaizesellq31x 
label variable hhmaizesellother_q31 "ag/from whom other did you directly buy the selected H mz seed var 2018A?"
sort hhmaizesellq31

label variable hhmaizesellq32 "ag/What was the cost (per kg) of selected H mz seed var from the source in 2018A"
destring hhmaizesellq32, replace force float
replace hhmaizesellq32=999 if hhmaizesellq32==.
note hhmaizesellq32: on the selected hybrid maize seed variety

rename hhmaizesellq33 hhmaizesellq33x
encode hhmaizesellq33x, gen(hhmaizesellq33)
order hhmaizesellq33, after(hhmaizesellq33x)
recode hhmaizesellq33 1=0 2=1 3=99
label define hhmaizesellq33 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizesellq33 "ag/Did you ever run out stock of selected H seed var during season 2018A?"
note hhmaizesellq33: disappointed clients on the selected hybrid maize seed variety
drop hhmaizesellq33x 

label variable hhmaizesellq33a "ag/how often did you run out of stock for selected H seed var during season2018A"
destring hhmaizesellq33a, replace force float
replace hhmaizesellq33a=99 if hhmaizesellq33a==.
note hhmaizesellq33a: on the selected hybrid maize seed variety

label variable hhmaizesellq33b "ag/How long (days) did it on average take to restock for selected H seed var in season2018A"
destring hhmaizesellq33b, replace force float
replace hhmaizesellq33b=99 if hhmaizesellq33b==.
note hhmaizesellq33b: on the selected hybrid maize seed variety

rename hhmaizesellq34 hhmaizesellq34x
encode hhmaizesellq34x, gen(hhmaizesellq34)
order hhmaizesellq34, after(hhmaizesellq34x)
recode hhmaizesellq34 1=0 2=1 3=99
label define hhmaizesellq34 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizesellq34 "ag/Did you offer discounts for large quantities of selected H seed var in season 2018A?"
note hhmaizesellq34: on the selected hybrid maize seed variety
drop hhmaizesellq34x 

rename hhmaizesellq34b hhmaizesellq34bx
encode hhmaizesellq34bx, gen(hhmaizesellq34b)
order hhmaizesellq34b, after(hhmaizesellq34bx)
recode hhmaizesellq34b 1=0 2=1 3=99
label define hhmaizesellq34b 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizesellq34b "ag/do you charge more per kg if customers only want to buy 1 kg?"
note hhmaizesellq34b: on the selected hybrid maize seed variety
drop hhmaizesellq34bx 

label variable hhmaizeq35 "ag/number of OPV maize seed varieties sold in season 2018A"
notes hhmaizeq35: Stock and turnover about OPV maize seed for first season of 2018 (2018A)

label variable hhmaizeopv1q38 "ag/Total quantity (kgs) sold of OPV mz seed var1 in season2018A"
destring hhmaizeopv1q38, replace force float


label variable hhmaizeopv1opv_num  "ag/opv seed var1- seed number (for season 2018A)"
destring hhmaizeopv1opv_num, replace force float
replace hhmaizeopv1opv_num=99 if hhmaizeopv1opv_num==.

label variable hhmaizeopv1q36  "ag/opv seed var1- name of maize hybrid variety"
notes hhmaizeopv1q36: of the top 3 most popular seed sold by agroinput dealr in first season of 2018

label variable hhmaizeopv1q37  "ag/opv seed var1- price per kg at beginning of season 2018A"
destring hhmaizeopv1q37, replace force float
replace hhmaizeopv1q37=99 if hhmaizeopv1q37==.

label variable hhmaizeopv2q38  "ag/opv seed var1- total quantity (Kg) sold in season 2018A"
destring hhmaizeopv2q38, replace force float
replace hhmaizeopv2q38=999 if hhmaizeopv2q38==.

rename hhmaizeopv1q36 hhmaizeopv1q36x
encode hhmaizeopv1q36x, gen(hhmaizeopv1q36)
order hhmaizeopv1q36, after(hhmaizeopv1q36x)
recode hhmaizeopv1q36 1=5 2=1 3=2 4=99
label define hhmaizeopv1q36 1 "Longe4" 2 "Longe5" 3 "Land races (local seed)" 4 "MM2" 5 "DK" 99 "NA", replace
note hhmaizeopv1q36: opv seed sold
drop hhmaizeopv1q36x 

label variable hhmaizeopv2opv_num  "ag/opv seed var2- seed number (for season 2018A)"
destring hhmaizeopv2opv_num, replace force float
replace hhmaizeopv2opv_num=99 if hhmaizeopv2opv_num==.

label variable hhmaizeopv2q36  "ag/opv seed var2- name of maize hybrid variety"
notes hhmaizeopv2q36: of the top 3 most popular seed sold by agroinput dealr in first season of 2018

label variable hhmaizeopv2q37  "ag/opv seed var2- price per kg at beginning of season 2018A"
destring hhmaizeopv2q37, replace force float
replace hhmaizeopv2q37=99 if hhmaizeopv2q37==.

label variable hhmaizeopv2q38  "ag/opv seed var2- total quantity (Kg) sold in season 2018A"
destring hhmaizeopv2q38, replace force float
replace hhmaizeopv2q38=999 if hhmaizeopv2q38==.

rename hhmaizeopv2q36 hhmaizeopv2q36x
encode hhmaizeopv2q36x, gen(hhmaizeopv2q36)
order hhmaizeopv2q36, after(hhmaizeopv2q36x)
recode hhmaizeopv2q36 1=5 2=3 3=1 4=2 5=4 6=99
label define hhmaizeopv2q36 1 "Longe4" 2 "Longe5" 3 "Land races (local seed)" 4 "MM2" 5 "DK" 99 "NA", replace
note hhmaizeopv2q36: opv seed sold
drop hhmaizeopv2q36x 

label variable hhmaizeopv3opv_num  "ag/opv seed var3- seed number (for season 2018A)"
destring hhmaizeopv3opv_num, replace force float
replace hhmaizeopv3opv_num=99 if hhmaizeopv3opv_num==.

label variable hhmaizeopv3q36  "ag/opv seed var3- name of maize hybrid variety"
notes hhmaizeopv3q36: of the top 3 most popular seed sold by agroinput dealr in first season of 2018

label variable hhmaizeopv3q37  "ag/opv seed var3- price per kg at beginning of season 2018A"
destring hhmaizeopv3q37, replace force float
replace hhmaizeopv3q37=99 if hhmaizeopv3q37==.

label variable hhmaizeopv3q38  "ag/opv seed var3- total quantity (Kg) sold in season 2018A"
destring hhmaizeopv3q38, replace force float
replace hhmaizeopv3q38=999 if hhmaizeopv3q38==.

rename hhmaizeopv3q36 hhmaizeopv3q36x
encode hhmaizeopv3q36x, gen(hhmaizeopv3q36)
order hhmaizeopv3q36, after(hhmaizeopv3q36x)
recode hhmaizeopv3q36 1=5 2=4 3=99
label define hhmaizeopv3q36 1 "Longe4" 2 "Longe5" 3 "Land races (local seed)" 4 "MM2" 5 "DK" 99 "NA", replace
note hhmaizeopv3q36: opv seed sold
drop hhmaizeopv3q36x 

label variable hhmaizeorder1 "ag/hh.maize.order1"

label variable hhmaizeopv_select "ag/selected opv seed var number (out of the listed top 3 selling)"
notes hhmaizeopv_select: randomly selected opv maize seed variety out of the top three varieties

label variable hhmaizeopv_select_name "ag/selected opv maize seed variety (out of the listed 3)"
notes hhmaizeopv_select_name: of the randomly selected opv maize seed variety out of the top three varieties

label variable hhmaizeopv_select_price "ag/price of selected opv mz seed variety"
notes hhmaizeopv_select_price: of the randomly selected opv maize seed variety out of the top three varieties

rename hhmaizeopv_select_name hhmaizeopv_select_namex
encode hhmaizeopv_select_namex, gen(hhmaizeopv_select_name)
order hhmaizeopv_select_name, after(hhmaizeopv_select_namex)
recode hhmaizeopv_select_name 1=4 2=1 3=2 5=99
label define hhmaizeopv_select_name 1 "Longe4" 2 "Longe5" 3 "Land races (local seed)" 4 "MM2" 5 "DK" 99 "NA", replace
note hhmaizeopv_select_name: for the randomly selected maize opv seed variety sold among the top 3 listed
sort hhmaizeopv_select_name
drop hhmaizeopv_select_namex 

destring hhmaizeopv_select_price, replace force float
replace hhmaizeopv_select_price=99 if hhmaizeopv_select_price==.

rename hhmaizeopq39 hhmaizeopq39x
encode hhmaizeopq39x, gen(hhmaizeopq39)
order hhmaizeopq39, after(hhmaizeopq39x)
recode hhmaizeopq39 1=0 2=1 3=99
label define hhmaizeopq39 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeopq39 "ag/was the selected opv variety certified seed"
note hhmaizeopq39: on the selected opv mz seed variety
drop hhmaizeopq39x 

rename hhmaizeopq40 hhmaizeopq40x
encode hhmaizeopq40x, gen(hhmaizeopq40)
order hhmaizeopq40, after(hhmaizeopq40x)
recode hhmaizeopq40 3=99
label define hhmaizeopq40 1 "Sealed and labeled" 2 "Sealed but not labeled" 3 "Unsealed but labeled" 4 "Unsealed and unlabeled" 96 "Other" 99 "NA", replace
label variable hhmaizeopq40 "ag/How was  the selected opv seed variety packaged when you obtained/purchased it?"
note hhmaizeopq40: on the selected opv maize seed variety
drop hhmaizeopq40x 

label variable hhmaizeopq41 "ag/How much (kgs) of selected opv seed var was stocked in season 2018A"
destring hhmaizeopq41, replace force float
replace hhmaizeopq41=99 if hhmaizesellq25==.
note hhmaizeopq41: on the selected opv maize seed variety

label variable hhmaizeopq42 "ag/How much (kgs) of selected opv seed var was carried over from prev. season (2017) into 2018A"
destring hhmaizeopq42, replace force float
replace hhmaizeopq42=999 if hhmaizeopq42==.
note hhmaizeopq42: on the selected opv maize seed variety

label variable hhmaizeopq43 "ag/How much (kgs) of selected opv seed var was lost/wasted in 2018A"
destring hhmaizeopq43, replace force float
replace hhmaizeopq43=999 if hhmaizeopq43==.
note hhmaizeopq43: on the selected opv maize seed variety

label variable hhmaizeopq44 "ag/How much (kgs) of selected opv seed var was still in stock at start of season 2018B"
destring hhmaizeopq44, replace force float
replace hhmaizeopq44=999 if hhmaizeopq44==.
note hhmaizeopq44: on the selected opv maize seed variety

label variable hhmaizeopq45 "ag/What is the smallest package(kgs) of selected opv seed var did you sell 2018A?"
destring hhmaizeopq45, replace force float
replace hhmaizeopq45=999 if hhmaizeopq45==.
note hhmaizeopq45: on the selected opv maize seed variety

rename hhmaizeopq46 hhmaizeopq46x
encode hhmaizeopq46x, gen(hhmaizeopq46)
order hhmaizeopq46, after(hhmaizeopq46x)
recode hhmaizeopq46 1=0 2=1 3=99
label define hhmaizeopq46 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeopq46 "ag/Did you repackage the selected opv mz seed var when clients wanted smaller packages?"
note hhmaizeopq46: on the selected opv maize seed variety
drop hhmaizeopq46x 

label variable hhmaizeopq46b "ag/What was the smallest repackage (kgs) sold in season 2018A"
destring hhmaizeopq46b, replace force float
replace hhmaizeopq46b=999 if hhmaizeopq46b==.
note hhmaizeopq46b: on the selected opv maize seed variety

rename hhmaizeopq47 hhmaizeopq47x
encode hhmaizeopq47x, gen(hhmaizeopq47)
order hhmaizeopq47, after(hhmaizeopq47x)
recode hhmaizeopq47 1=96 2=1 3=2 5=99
label define hhmaizeopq47 1 "Agro input wholesaler" 2 "Seed company" 3 "Research station" 4 "Local Seed Business" 5 "Individual seed producer" 96 "Other" 99 "NA", replace
label variable hhmaizeopq47 "ag/from whom did you directly buy the selected opv mz seed var 2018A?"
note hhmaizeopq47: on the selected hybrid maize seed variety
drop hhmaizeopq47x 
label variable hhmaizeopother_q47 "ag/from whom other did you directly buy the selected opv mz seed var 2018A?"

label variable hhmaizeopq48 "ag/What was the cost (per kg) of selected opv mz seed var from the source in 2018A"
destring hhmaizeopq48, replace force float
replace hhmaizeopq48=999 if hhmaizeopq48==.
note hhmaizeopq48: on the selected opv maize seed variety

rename hhmaizeopq49 hhmaizeopq49x
encode hhmaizeopq49x, gen(hhmaizeopq49)
order hhmaizeopq49, after(hhmaizeopq49x)
recode hhmaizeopq49 1=0 2=1 3=99
label define hhmaizeopq49 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeopq49 "ag/Did you ever run out stock of selected opv seed var during season 2018A?"
note hhmaizeopq49: disappointed clients on the selected opv maize seed variety
drop hhmaizeopq49x 

label variable hhmaizeopq49a "ag/how often did you run out of stock for selected opv seed var during season2018A"
destring hhmaizeopq49a, replace force float
replace hhmaizeopq49a=99 if hhmaizeopq49a==.
note hhmaizeopq49a: on the selected opv maize seed variety

label variable hhmaizeopq49b "ag/How long (days) did it on average take to restock for selected opv seed var in season2018A"
destring hhmaizeopq49b, replace force float
replace hhmaizeopq49b=999 if hhmaizeopq49b==.
note hhmaizeopq49b: on the selected opv maize seed variety

rename hhmaizeopq50 hhmaizeopq50x
encode hhmaizeopq50x, gen(hhmaizeopq50)
order hhmaizeopq50, after(hhmaizeopq50x)
recode hhmaizeopq50 1=0 2=1 3=99
label define hhmaizeopq50 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeopq50 "ag/Did you offer discounts for large quantities for selected opv var in 2018A"
note hhmaizeopq50: on the selected opv maize seed variety
drop hhmaizeopq50x

rename hhmaizeopq50b hhmaizeopq50bx
encode hhmaizeopq50bx, gen(hhmaizeopq50b)
order hhmaizeopq50b, after(hhmaizeopq50bx)
recode hhmaizeopq50b 1=0 2=1 3=99
label define hhmaizeopq50b 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeopq50b "ag/do you charge more per kg if customers only buys 1 kg?"
note hhmaizeopq50b: on the selected opv maize seed variety
drop hhmaizeopq50bx

rename hhmaizeq51 hhmaizeq51x
encode hhmaizeq51x, gen(hhmaizeq51)
order hhmaizeq51, after(hhmaizeq51x)
recode hhmaizeq51 1=0 2=1
label define hhmaizeq51 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq51 "ag/Do you also sell bean seed varieties?"
drop hhmaizeq51x 

destring hhmaizebeq51a, replace force float
replace hhmaizebeq51a=99 if hhmaizebeq51a==.
label variable hhmaizebeq51a "ag/How many bean seed varieties did you sell in season 2018A?"

destring hhmaizebebean1bean_num, replace force float
replace hhmaizebebean1bean_num=99 if hhmaizebebean1bean_num==.
label variable hhmaizebebean1bean_num "ag/bean seed variety #1 sold in season 2018A?"
notes hhmaizebebean1bean_num: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean1q54, replace force float
replace hhmaizebebean1q54=99 if hhmaizebebean1q54==.
label variable hhmaizebebean1q54 "ag/Total quantity (kgs) of bean var1 sold in season 2018A"
notes hhmaizebebean1q54: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean1q53, replace force float
replace hhmaizebebean1q53=99 if hhmaizebebean1q53==.
label variable hhmaizebebean1q53 "ag/Price per kilogram of bean var1 in season 2018A"
notes hhmaizebebean1q53: of the top 3 bean varieties sold in season 2018A

rename hhmaizebebean1q52 hhmaizebebean1q52x
encode hhmaizebebean1q52x, gen(hhmaizebebean1q52)
order hhmaizebebean1q52, after(hhmaizebebean1q52x)
label variable hhmaizebebean1q52 "ag/Name of bean seed variety1"
drop hhmaizebebean1q52x 

destring hhmaizebebean2bean_num, replace force float
replace hhmaizebebean2bean_num=99 if hhmaizebebean2bean_num==.
label variable hhmaizebebean2bean_num "ag/bean seed variety #2 sold in season 2018A?"
notes hhmaizebebean2bean_num: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean2q54, replace force float
replace hhmaizebebean2q54=99 if hhmaizebebean2q54==.
label variable hhmaizebebean2q54 "ag/Total quantity (kgs) of bean var2 sold in season 2018A"
notes hhmaizebebean2q54: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean2q53, replace force float
replace hhmaizebebean2q53=99 if hhmaizebebean2q53==.
label variable hhmaizebebean2q53 "ag/Price per kilogram of bean var2 in season 2018A"
notes hhmaizebebean2q53: of the top 3 bean varieties sold in season 2018A

rename hhmaizebebean2q52 hhmaizebebean2q52x
encode hhmaizebebean2q52x, gen(hhmaizebebean2q52)
order hhmaizebebean2q52, after(hhmaizebebean2q52x)
label variable hhmaizebebean2q52 "ag/Name of bean seed variety2"
drop hhmaizebebean2q52x 

destring hhmaizebebean3bean_num, replace force float
replace hhmaizebebean3bean_num=99 if hhmaizebebean3bean_num==.
label variable hhmaizebebean3bean_num "ag/bean seed variety #3 sold in season 2018A?"
notes hhmaizebebean3bean_num: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean3q54, replace force float
replace hhmaizebebean3q54=99 if hhmaizebebean3q54==.
label variable hhmaizebebean3q54 "ag/Total quantity (kgs) of bean var3 sold in season 2018A"
notes hhmaizebebean3q54: of the top 3 bean varieties sold in season 2018A

destring hhmaizebebean3q53, replace force float
replace hhmaizebebean3q53=99 if hhmaizebebean3q53==.
label variable hhmaizebebean3q53 "ag/Price per kilogram of bean var3 in season 2018A"
notes hhmaizebebean3q53: of the top 3 bean varieties sold in season 2018A

rename hhmaizebebean3q52 hhmaizebebean3q52x
encode hhmaizebebean3q52x, gen(hhmaizebebean3q52)
order hhmaizebebean3q52, after(hhmaizebebean3q52x)
label variable hhmaizebebean3q52 "ag/Name of bean seed variety3"
drop hhmaizebebean3q52x 

destring hhmaizebebean_select, replace force float
replace hhmaizebebean_select=99 if hhmaizebebean_select==.
label variable hhmaizebebean_select "ag/selected variety # of the top 3 identified for season 2018A"
notes hhmaizebebean_select: randomly selected bean var of the top 3 bean varieties sold in season 2018A

 
rename hhmaizebebean_select_name hhmaizebebean_select_namex
encode hhmaizebebean_select_namex, gen(hhmaizebebean_select_name)
order hhmaizebebean_select_name, after(hhmaizebebean_select_namex)
recode hhmaizebebean_select_name 4=99
label define hhmaizebebean_select_name 99 "NA", replace
label variable hhmaizebebean_select_name "ag/Name of randomly selected bean seed variety"
drop hhmaizebebean_select_namex 

destring hhmaizebebean_select_price, replace force float
replace hhmaizebebean_select_price=99 if hhmaizebebean_select_price==.
label variable hhmaizebebean_select_price "ag/price of selected bean variety"

rename hhmaizebesaleq55 hhmaizebesaleq55x
encode hhmaizebesaleq55x, gen(hhmaizebesaleq55)
order hhmaizebesaleq55, after(hhmaizebesaleq55x)
recode hhmaizebesaleq55 1=0 2=1 3=99
label define hhmaizebesaleq55 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq55 "ag/was this selected bean variety certified?"
notes hhmaizebesaleq55: "on the selected bean variety" 
drop hhmaizebesaleq55x 

rename hhmaizebesaleq55b hhmaizebesaleq55bx
encode hhmaizebesaleq55bx, gen(hhmaizebesaleq55b)
order hhmaizebesaleq55b, after(hhmaizebesaleq55bx)
recode hhmaizebesaleq55b 1=0 2=1 3=99
label define hhmaizebesaleq55b 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq55b "ag/was this selected bean variety qds?"
notes hhmaizebesaleq55b: "on the selected bean variety" 
drop hhmaizebesaleq55bx 

rename hhmaizebesaleq56 hhmaizebesaleq56x
encode hhmaizebesaleq56x, gen(hhmaizebesaleq56)
order hhmaizebesaleq56, after(hhmaizebesaleq56x)
recode hhmaizebesaleq56 3=4 4=99
label define hhmaizebesaleq56 1 "Sealed and labeled" 2 "Sealed but not labeled" 3 "Unsealed but labeled" 4 "Unsealed and unlabeled" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq56 "ag/How was the selected bean var packaged when you obtained it?"
notes hhmaizebesaleq56: "on the selected bean variety" 
drop hhmaizebesaleq56x 

destring hhmaizebesaleq57, replace force float
replace hhmaizebesaleq57=99 if hhmaizebesaleq57==.
label variable hhmaizebesaleq57 "ag/how much (kgs) of selected bean var was stocked in 2018A"
notes hhmaizebesaleq57: "on the selected bean variety" 

destring hhmaizebesaleq58, replace force float
replace hhmaizebesaleq58=99 if hhmaizebesaleq58==.
label variable hhmaizebesaleq58 "ag/how much (kgs) of selected bean var was carry over stock frm 2017"
notes hhmaizebesaleq58: "on the selected bean variety from previous year, 2017, into 2018" 

destring hhmaizebesaleq59, replace force float
replace hhmaizebesaleq59=99 if hhmaizebesaleq59==.
label variable hhmaizebesaleq59 "ag/how much (kgs) of selected bean var was lost/wasted in 2018A"
notes hhmaizebesaleq59: "on the selected bean variety in season 2018A" 

destring hhmaizebesaleq60, replace force float
replace hhmaizebesaleq60=99 if hhmaizebesaleq60==.
label variable hhmaizebesaleq60 "ag/quantity (kgs) of selected bean var still in stock at start of season 2018A"
notes hhmaizebesaleq60: "on the selected bean variety in season 2018A" 

destring hhmaizebesaleq61, replace force float
replace hhmaizebesaleq61=999 if hhmaizebesaleq61==.
label variable hhmaizebesaleq61 "ag/smallest package (kgs) of selected bean var sold in season 2018A"
notes hhmaizebesaleq61: "on the selected bean variety in season 2018A" 

rename hhmaizebesaleq62 hhmaizebesaleq62x
encode hhmaizebesaleq62x, gen(hhmaizebesaleq62)
order hhmaizebesaleq62, after(hhmaizebesaleq62x)
recode hhmaizebesaleq62 1=0 2=1 3=99
label define hhmaizebesaleq62 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq62 "ag/did you repackage the selected bean variety in smaller packages?"
notes hhmaizebesaleq62: "on the selected bean variety" 
drop hhmaizebesaleq62x 

destring hhmaizebesaleq62b, replace force float
replace hhmaizebesaleq62b=99 if hhmaizebesaleq62b==.
label variable hhmaizebesaleq62b "ag/smallest repackage (kgs) of selected bean var sold in season 2018A"
notes hhmaizebesaleq62b: "on the selected bean variety in first season of 2018" 

rename hhmaizebesaleq63 hhmaizebesaleq63x
encode hhmaizebesaleq63x, gen(hhmaizebesaleq63)
order hhmaizebesaleq63, after(hhmaizebesaleq63x)
recode hhmaizebesaleq63 3=4 4=5 5=99
label define hhmaizebesaleq63 1 "Agro input wholesaler" 2 "Seed company" 3 "Research station" 4 "Local Seed Business" 5 "Individual seed producer" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq63 "ag/who was your direct supplier of the selected bean variety in 2018A?"
notes hhmaizebesaleq63: "on the selected bean variety" 
drop hhmaizebesaleq63x 
label variable hhmaizebesaleother_q63 "ag/who was other direct supplier of the selected bean variety in 2018A?"

destring hhmaizebesaleq64, replace force float
replace hhmaizebesaleq64=99 if hhmaizebesaleq64==.
label variable hhmaizebesaleq64 "ag/what was the cost (per kg) of selected bean var from where you bought it in 2018A"
notes hhmaizebesaleq64: "on the selected bean variety in first season of 2018" 

rename hhmaizebesaleq65 hhmaizebesaleq65x
encode hhmaizebesaleq65x, gen(hhmaizebesaleq65)
order hhmaizebesaleq65, after(hhmaizebesaleq65x)
recode hhmaizebesaleq65 1=0 2=1 3=99
label define hhmaizebesaleq65 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq65 "ag/did you run out of stock for this bean variety in 2018A?"
notes hhmaizebesaleq65: "on the selected bean variety" 
drop hhmaizebesaleq65x 

destring hhmaizebesaleq65a, replace force float
replace hhmaizebesaleq65a=99 if hhmaizebesaleq65a==.
label variable hhmaizebesaleq65a "ag/how often did you run out of stock for selected bean var in 2018A"
notes hhmaizebesaleq65a: "on the selected bean variety in first season of 2018" 

destring hhmaizebesaleq65b, replace force float
replace hhmaizebesaleq65b=99 if hhmaizebesaleq65b==.
label variable hhmaizebesaleq65b "ag/how long did it take to restock for selected bean var in 2018A"
notes hhmaizebesaleq65b: "days on average for the selected bean variety in first season of 2018" 

rename hhmaizebesaleq66 hhmaizebesaleq66x
encode hhmaizebesaleq66x, gen(hhmaizebesaleq66)
order hhmaizebesaleq66, after(hhmaizebesaleq66x)
recode hhmaizebesaleq66 1=0 2=1 3=99
label define hhmaizebesaleq66 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq66 "ag/did you offer discounts to large quantity buyers of the selected bean variety in 2018A?"
notes hhmaizebesaleq66: "on the selected bean variety in season 2018A" 
drop hhmaizebesaleq66x 

rename hhmaizebesaleq66b hhmaizebesaleq66bx
encode hhmaizebesaleq66bx, gen(hhmaizebesaleq66b)
order hhmaizebesaleq66b, after(hhmaizebesaleq66bx)
recode hhmaizebesaleq66b 1=0 2=1 3=99
label define hhmaizebesaleq66b 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizebesaleq66b "ag/do you charge more (per kg) for buyers of 1kg for this bean variety?"
notes hhmaizebesaleq66b: "on the selected bean variety in season 2018A" 
drop hhmaizebesaleq66bx 

tostring hhmaizeq67, replace
rename hhmaizeq67 hhmaizeq67x
encode hhmaizeq67x, gen(hhmaizeq67)
order hhmaizeq67, after(hhmaizeq67x)
label define hhmaizeq67 1 "No" 2 "Yes but only to some" 3 "Yes but to any who wants" 96 "Other" 99 "NA", replace
label variable hhmaizeq67 "ag/do you offer extension/training to your clients?"
drop hhmaizeq67x 

tostring hhmaizeq68, replace
rename hhmaizeq68 hhmaizeq68x
encode hhmaizeq68x, gen(hhmaizeq68)
order hhmaizeq68, after(hhmaizeq68x)
label define hhmaizeq68 1 "No" 2 "Yes but only to some" 3 "Yes but to any who wants" 96 "Other" 99 "NA", replace
label variable hhmaizeq68 "ag/do you provide seed on credit to your clients?"
drop hhmaizeq68x 

label variable hhmaizeq68a "ag/do these farmers need to pay interest?"
label variable hhmaizeq68b "ag/What percentage (%) do they have to pay?"
destring hhmaizeq68a, replace force float
destring hhmaizeq68b, replace force float
replace hhmaizeq68a=99 if hhmaizeq68a==.
replace hhmaizeq68b=99 if hhmaizeq68b==.

rename hhmaizeq69 hhmaizeq69x
encode hhmaizeq69x, gen(hhmaizeq69)
order hhmaizeq69, after(hhmaizeq69x)
recode hhmaizeq69 1=96 2=1 3=2 4=3 5=4 6=5
label define hhmaizeq69 1 "Refund all money back" 2 "Money is partly refunded" 3 "Gets discount on next purchase" 4 "Gets new seed" 5 "Nothing: goods sold cannot be returned" 96 "Other" 99 "NA", replace
label variable hhmaizeq69 "ag/What normally happens if farmers are not happy with seed bought?"
drop hhmaizeq69x 
label variable hhmaizeother_q69 "ag/specify what other normally happens if farmers are not happy with seed bought?"

rename hhmaizeq70 hhmaizeq70x
encode hhmaizeq70x, gen(hhmaizeq70)
order hhmaizeq70, after(hhmaizeq70x)
recode hhmaizeq70 1=0 2=1 3=99
label define hhmaizeq70 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq70 "ag/do you actively promote/advertise your seed?"
drop hhmaizeq70x 

rename hhmaizeq70ba hhmaizeq70bax
encode hhmaizeq70bax, gen(hhmaizeq70ba)
order hhmaizeq70ba, after(hhmaizeq70bax)
recode hhmaizeq70ba 1=0 2=1 3=99
label define hhmaizeq70ba 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70ba "ag/promote seed by (a) field days "
drop hhmaizeq70bax 

rename hhmaizeq70bb hhmaizeq70bbx
encode hhmaizeq70bbx, gen(hhmaizeq70bb)
order hhmaizeq70bb, after(hhmaizeq70bbx)
recode hhmaizeq70bb 1=0 2=1 3=99
label define hhmaizeq70bb 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70bb "ag/promote seed in (b) Seed markets "
drop hhmaizeq70bbx 

rename hhmaizeq70bc hhmaizeq70bcx
encode hhmaizeq70bcx, gen(hhmaizeq70bc)
order hhmaizeq70bc, after(hhmaizeq70bcx)
recode hhmaizeq70bc 1=0 2=1 3=99
label define hhmaizeq70bc 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70bc "ag/promote seed by (c) Demonstration"
drop hhmaizeq70bcx 

rename hhmaizeq70bd hhmaizeq70bdx
encode hhmaizeq70bdx, gen(hhmaizeq70bd)
order hhmaizeq70bd, after(hhmaizeq70bdx)
recode hhmaizeq70bd 1=0 2=1 3=99
label define hhmaizeq70bd 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70bd "ag/promote seed thru (d) Farmer group training"
drop hhmaizeq70bdx 

rename hhmaizeq70be hhmaizeq70bex
encode hhmaizeq70bex, gen(hhmaizeq70be)
order hhmaizeq70be, after(hhmaizeq70bex)
recode hhmaizeq70be 1=0 2=1 3=99
label define hhmaizeq70be 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70be "ag/promote seed thru (d) Farmer to farmer visits to the shop"
drop hhmaizeq70bex 

rename hhmaizeq70bf hhmaizeq70bfx
encode hhmaizeq70bfx, gen(hhmaizeq70bf)
order hhmaizeq70bf, after(hhmaizeq70bfx)
recode hhmaizeq70bf 1=0 2=1 3=99
label define hhmaizeq70bf 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70bf "ag/promote seed thru (d) Village seed agents"
drop hhmaizeq70bfx 

rename hhmaizeq70bg hhmaizeq70bgx
encode hhmaizeq70bgx, gen(hhmaizeq70bg)
order hhmaizeq70bg, after(hhmaizeq70bgx)
recode hhmaizeq70bg 1=0 2=1 3=99
label define hhmaizeq70bg 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70bg "ag/promote seed thru (d) Media (radio, tv)"
drop hhmaizeq70bgx 

rename hhmaizeq70b96 hhmaizeq70b96x
encode hhmaizeq70b96x, gen(hhmaizeq70b96)
order hhmaizeq70b96, after(hhmaizeq70b96x)
recode hhmaizeq70b96 1=0 2=1 3=99
label define hhmaizeq70b96 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq70b96 "ag/promote seed thru (d) other means"
drop hhmaizeq70b96x 
	
rename hhmaizeinstq71a hhmaizeinstq71ax
encode hhmaizeinstq71ax, gen(hhmaizeinstq71a)
order hhmaizeinstq71a, after(hhmaizeinstq71ax)
recode hhmaizeinstq71a 1=98 2=0 3=1
label define hhmaizeinstq71a 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71a "ag/comp or inst. gives seed on credit"
notes hhmaizeinstq71a: Does the company or institution where you obtain/buy your seed offer the above?
drop hhmaizeinstq71ax 

rename hhmaizeinstq71b hhmaizeinstq71bx
encode hhmaizeinstq71bx, gen(hhmaizeinstq71b)
order hhmaizeinstq71b, after(hhmaizeinstq71bx)
recode hhmaizeinstq71b 1=98 2=0 3=1
label define hhmaizeinstq71b 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71b "ag/comp or inst. provides you with training on how to handle seed"
notes hhmaizeinstq71b: Does the company or institution where you obtain/buy your seed offer the above?
notes hhmaizeinstq71b: e.g storing, planting
drop hhmaizeinstq71bx 

rename hhmaizeinstq71c hhmaizeinstq71cx
encode hhmaizeinstq71cx, gen(hhmaizeinstq71c)
order hhmaizeinstq71c, after(hhmaizeinstq71cx)
recode hhmaizeinstq71c 1=98 2=0 3=1
label define hhmaizeinstq71c 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71c "ag/comp or inst. provides you with training on materials for farmer training"
notes hhmaizeinstq71c: Does the company or institution where you obtain/buy your seed offer the above?
notes hhmaizeinstq71c: e.g manuals
drop hhmaizeinstq71cx 

rename hhmaizeinstq71d hhmaizeinstq71dx
encode hhmaizeinstq71dx, gen(hhmaizeinstq71d)
order hhmaizeinstq71d, after(hhmaizeinstq71dx)
recode hhmaizeinstq71d 1=98 2=0 3=1
recode hhmaizeinstq71d 98=0 0=1
label define hhmaizeinstq71d 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71d "ag/comp or inst. provides you with materials to promote yo' products'"
notes hhmaizeinstq71d: Does the company or institution where you obtain/buy your seed offer the above?
notes hhmaizeinstq71d: e.g brochures, posters
drop hhmaizeinstq71dx 

rename hhmaizeinstq71e hhmaizeinstq71ex
encode hhmaizeinstq71ex, gen(hhmaizeinstq71e)
order hhmaizeinstq71e, after(hhmaizeinstq71ex)
recode hhmaizeinstq71e 1=98 2=0 3=1
label define hhmaizeinstq71e 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71e "ag/comp or inst. offer transport service for the products"
notes hhmaizeinstq71e: Does the company or institution where you obtain/buy your seed offer the above?
notes hhmaizeinstq71e: e.g delivery to your shop
drop hhmaizeinstq71ex 

rename hhmaizeinstq71f hhmaizeinstq71fx
encode hhmaizeinstq71fx, gen(hhmaizeinstq71f)
order hhmaizeinstq71f, after(hhmaizeinstq71fx)
recode hhmaizeinstq71f 1=98 2=0 3=1
label define hhmaizeinstq71f 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeinstq71f "ag/comp or inst. provide moneyback guarantee (in case of bad quality seed)"
notes hhmaizeinstq71f: Does the company or institution where you obtain/buy your seed offer the above?
notes hhmaizeinstq71f: e.g delivery to your shop
drop hhmaizeinstq71fx 

rename hhmaizeq72 hhmaizeq72x
encode hhmaizeq72x, gen(hhmaizeq72)
order hhmaizeq72, after(hhmaizeq72x)
recode hhmaizeq72 1=98 2=0 3=1
label define hhmaizeq72 0 "No" 1 "Yes" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeq72 "ag/Do you get inspected?"
drop hhmaizeq72x 
	
rename hhmaizeq73 hhmaizeq73x
encode hhmaizeq73x, gen(hhmaizeq73)
order hhmaizeq73, after(hhmaizeq73x)
recode hhmaizeq73 1=2 2=3 3=4 4=5 5=6 6=99
label define hhmaizeq73 1 "Weekly" 2 "Monthly" 3 "four times a year" 4 "Twice a year" 5 "Yearly" 6 "Less than yearly" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeq73 "ag/how often do you get inspected?"
drop hhmaizeq73x 

rename hhmaizeq74 hhmaizeq74x
encode hhmaizeq74x, gen(hhmaizeq74)
order hhmaizeq74, after(hhmaizeq74x)
recode hhmaizeq74 1=96 2=98 3=1 4=3 5=4 6=99
label define hhmaizeq74 1 "UNADA" 2 "USTA" 3 "DAO/MAAIF" 4 "Seed company" 96 "Other" 98 "Don't know'" 99 "NA", replace
label variable hhmaizeq74 "ag/who does the inspection?"
drop hhmaizeq74x 
label variable hhmaizeother_q74 "ag/specify other inspector"

rename hhmaizeq75a hhmaizeq75ax
encode hhmaizeq75ax, gen(hhmaizeq75a)
order hhmaizeq75a, after(hhmaizeq75ax)
recode hhmaizeq75a 1=0 2=1 3=99
label define hhmaizeq75a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75a "ag/inspectors check for (a)Seed expiry date "
drop hhmaizeq75ax 

rename hhmaizeq75b hhmaizeq75bx
encode hhmaizeq75bx, gen(hhmaizeq75b)
order hhmaizeq75b, after(hhmaizeq75bx)
recode hhmaizeq75b 1=0 2=1 3=99
label define hhmaizeq75b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75b "ag/inspectors check for (b)Seed storage"
drop hhmaizeq75bx 

rename hhmaizeq75c hhmaizeq75cx
encode hhmaizeq75cx, gen(hhmaizeq75c)
order hhmaizeq75c, after(hhmaizeq75cx)
recode hhmaizeq75c 1=0 2=1 3=99
label define hhmaizeq75c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75c "ag/inspectors check for (c)Operation permit"
drop hhmaizeq75cx 

rename hhmaizeq75d hhmaizeq75dx
encode hhmaizeq75dx, gen(hhmaizeq75d)
order hhmaizeq75d, after(hhmaizeq75dx)
recode hhmaizeq75d 1=0 2=1 3=99
label define hhmaizeq75d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75d "ag/inspectors check for (d)Seed class sticker"
drop hhmaizeq75dx 

rename hhmaizeq75e hhmaizeq75ex
encode hhmaizeq75ex, gen(hhmaizeq75e)
order hhmaizeq75e, after(hhmaizeq75ex)
recode hhmaizeq75e 1=0 2=1 3=99
label define hhmaizeq75e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75e "ag/inspectors check for (e)Seed packaging"
drop hhmaizeq75ex 

rename hhmaizeq75f hhmaizeq75fx
encode hhmaizeq75fx, gen(hhmaizeq75f)
order hhmaizeq75f, after(hhmaizeq75fx)
recode hhmaizeq75f 1=0 2=1 3=99
label define hhmaizeq75f 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75f "ag/inspectors check for (f)Seed lot"
drop hhmaizeq75fx 

rename hhmaizeq75g hhmaizeq75gx
encode hhmaizeq75gx, gen(hhmaizeq75g)
order hhmaizeq75g, after(hhmaizeq75gx)
recode hhmaizeq75g 1=0 2=1 3=99
label define hhmaizeq75g 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75g "ag/inspectors check for (g)Seed germination"
drop hhmaizeq75gx 

rename hhmaizeq75h hhmaizeq75hx
encode hhmaizeq75hx, gen(hhmaizeq75h)
order hhmaizeq75h, after(hhmaizeq75hx)
recode hhmaizeq75h 1=0 2=1 3=99
label define hhmaizeq75h 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75h "ag/inspectors check for (h)Seed moisture cont't"
drop hhmaizeq75hx 

rename hhmaizeq75i hhmaizeq75ix
encode hhmaizeq75ix, gen(hhmaizeq75i)
order hhmaizeq75i, after(hhmaizeq75ix)
recode hhmaizeq75i 1=0 2=1 3=99
label define hhmaizeq75i 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq75i "ag/inspectors check for (i)Seed purity"
drop hhmaizeq75ix 

rename hhmaizeq7596 hhmaizeq7596x
encode hhmaizeq7596x, gen(hhmaizeq7596)
order hhmaizeq7596, after(hhmaizeq7596x)
recode hhmaizeq7596 1=0 2=1 3=99
label define hhmaizeq7596 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq7596 "ag/inspectors check for (96)Other seed aspect"
drop hhmaizeq7596x 
label variable hhmaizeother_q75 "ag/specify other inspection aspect"

       
rename hhmaizeq76a hhmaizeq76ax
encode hhmaizeq76ax, gen(hhmaizeq76a)
order hhmaizeq76a, after(hhmaizeq76ax)
recode hhmaizeq76a 1=0 2=1
label define hhmaizeq76a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76a "ag/challenge faced(a) Limited storage "
drop hhmaizeq76ax 

rename hhmaizeq76b hhmaizeq76bx
encode hhmaizeq76bx, gen(hhmaizeq76b)
order hhmaizeq76b, after(hhmaizeq76bx)
recode hhmaizeq76b 1=0 2=1
label define hhmaizeq76b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76b "ag/challenge(b) High taxes"
drop hhmaizeq76bx 

rename hhmaizeq76c hhmaizeq76cx
encode hhmaizeq76cx, gen(hhmaizeq76c)
order hhmaizeq76c, after(hhmaizeq76cx)
recode hhmaizeq76c 1=0 2=1
label define hhmaizeq76c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76c "ag/challenge(c) Limited/No Access to credit"
drop hhmaizeq76cx 

rename hhmaizeq76d hhmaizeq76dx
encode hhmaizeq76dx, gen(hhmaizeq76d)
order hhmaizeq76d, after(hhmaizeq76dx)
recode hhmaizeq76d 1=0 2=1
label define hhmaizeq76d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76d "ag/challenge(d) Counterfeit products"
drop hhmaizeq76dx 

rename hhmaizeq76e hhmaizeq76ex
encode hhmaizeq76ex, gen(hhmaizeq76e)
order hhmaizeq76e, after(hhmaizeq76ex)
recode hhmaizeq76e 1=0 2=1
label define hhmaizeq76e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76e "ag/challenge(e) Limited Availability of seed"
drop hhmaizeq76ex 

rename hhmaizeq76f hhmaizeq76fx
encode hhmaizeq76fx, gen(hhmaizeq76f)
order hhmaizeq76f, after(hhmaizeq76fx)
recode hhmaizeq76f 1=0 2=1
label define hhmaizeq76f 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76f "ag/challenge(f) Limited Information about new varieties"
drop hhmaizeq76fx 

rename hhmaizeq76g hhmaizeq76gx
encode hhmaizeq76gx, gen(hhmaizeq76g)
order hhmaizeq76g, after(hhmaizeq76gx)
recode hhmaizeq76g 1=0 2=1
label define hhmaizeq76g 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76g "ag/challenge(g) Limited demand for seed"
drop hhmaizeq76gx 

rename hhmaizeq76h hhmaizeq76hx
encode hhmaizeq76hx, gen(hhmaizeq76h)
order hhmaizeq76h, after(hhmaizeq76hx)
recode hhmaizeq76h 1=0 2=1
label define hhmaizeq76h 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76h "ag/challenge(h) Limited demand for improved seed"
drop hhmaizeq76hx 

rename hhmaizeq76i hhmaizeq76ix
encode hhmaizeq76ix, gen(hhmaizeq76i)
order hhmaizeq76i, after(hhmaizeq76ix)
recode hhmaizeq76i 1=0 2=1
label define hhmaizeq76i 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq76i "ag/challenge(i) Supply of free agro-inputs to farmers by gvt or NGOs"
drop hhmaizeq76ix 

rename hhmaizeq7696 hhmaizeq7696x
encode hhmaizeq7696x, gen(hhmaizeq7696)
order hhmaizeq7696, after(hhmaizeq7696x)
recode hhmaizeq7696 1=0 2=1
label define hhmaizeq7696 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq7696 "ag/challenge(96) Other seed aspect"
drop hhmaizeq7696x 
label variable hhmaizeother_76 "ag/specify other chalenge"

 
rename hhmaizeq77 hhmaizeq77x
encode hhmaizeq77x, gen(hhmaizeq77)
order hhmaizeq77, after(hhmaizeq77x)
recode hhmaizeq77 1=0 2=1
label define hhmaizeq77 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq77 "ag/Did you receive any training on handling and storage of maize seed?"
drop hhmaizeq77x 

rename hhmaizeq77ba hhmaizeq77bax
encode hhmaizeq77bax, gen(hhmaizeq77ba)
order hhmaizeq77ba, after(hhmaizeq77bax)
recode hhmaizeq77ba 1=0 2=1 3=99
label define hhmaizeq77ba 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77ba "ag/got training from (a) UNADA"
notes hhmaizeq77ba: Source of training on handling and storage of maize seed
notes hhmaizeq77ba: UNADA - Uganda National Agro-dealers AssOCIATION
drop hhmaizeq77bax 

rename hhmaizeq77bb hhmaizeq77bbx
encode hhmaizeq77bbx, gen(hhmaizeq77bb)
order hhmaizeq77bb, after(hhmaizeq77bbx)
recode hhmaizeq77bb 1=0 2=1 3=99
label define hhmaizeq77bb 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77bb "ag/got training from (b) Seed company"
notes hhmaizeq77bb: Source of training on handling and storage of maize seed
notes hhmaizeq77bb: seed company eg. Victoria seed company
drop hhmaizeq77bbx 

rename hhmaizeq77bc hhmaizeq77bcx
encode hhmaizeq77bcx, gen(hhmaizeq77bc)
order hhmaizeq77bc, after(hhmaizeq77bcx)
recode hhmaizeq77bc 1=0 2=1 3=99
label define hhmaizeq77bc 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77bc "ag/got training from (c) DAO/MAAIF"
notes hhmaizeq77bc: Source of training on handling and storage of maize seed
notes hhmaizeq77bc: DAO- District Agricultural Officer
drop hhmaizeq77bcx 

rename hhmaizeq77bd hhmaizeq77bdx
encode hhmaizeq77bdx, gen(hhmaizeq77bd)
order hhmaizeq77bd, after(hhmaizeq77bdx)
recode hhmaizeq77bd 1=0 2=1 3=99
label define hhmaizeq77bd 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77bd "ag/got training from (d) NGOs/CBOs"
notes hhmaizeq77bd: Source of training on handling and storage of maize seed
notes hhmaizeq77bd: NGO- Non govt organization
drop hhmaizeq77bdx 

rename hhmaizeq77be hhmaizeq77bex
encode hhmaizeq77bex, gen(hhmaizeq77be)
order hhmaizeq77be, after(hhmaizeq77bex)
recode hhmaizeq77be 1=0 2=1 3=99
label define hhmaizeq77be 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77be "ag/got training from (e) Radio"
notes hhmaizeq77be: Source of training on handling and storage of maize seed
notes hhmaizeq77be: Mass media
drop hhmaizeq77bex 

rename hhmaizeq77bf hhmaizeq77bfx
encode hhmaizeq77bfx, gen(hhmaizeq77bf)
order hhmaizeq77bf, after(hhmaizeq77bfx)
recode hhmaizeq77bf 1=0 2=1 3=99
label define hhmaizeq77bf 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77bf "ag/got training from (f) USTA"
notes hhmaizeq77bf: Source of training on handling and storage of maize seed
notes hhmaizeq77bf: USTA - Uganda Seed Traders Association
drop hhmaizeq77bfx 

rename hhmaizeq77b96 hhmaizeq77b96x
encode hhmaizeq77b96x, gen(hhmaizeq77b96)
order hhmaizeq77b96, after(hhmaizeq77b96x)
recode hhmaizeq77b96 1=0 2=1 3=99
label define hhmaizeq77b96 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq77b96 "ag/got training from (96) other source"
notes hhmaizeq77b96: Source of training on handling and storage of maize seed
notes hhmaizeq77b96: Other source
drop hhmaizeq77b96x 
label variable hhmaizeother_q77b "ag/Specify other source of training"

rename hhmaizeq78 hhmaizeq78x
encode hhmaizeq78x, gen(hhmaizeq78)
order hhmaizeq78, after(hhmaizeq78x)
recode hhmaizeq78 1=96 2=1 3=2 4=3 5=4 6=5 7=6
label define hhmaizeq78 1 "All seed is always sold" 2 "Return to supplier" 3 "Sold at discount" 4 "Given away" 5 "Thrown away" 6 "Store and sell in the next season" 96 "Other" 99 "NA", replace
label variable hhmaizeq78 "ag/What usually happens if seed is not sold?"
drop hhmaizeq78x 
label variable hhmaizeother_q78 "ag/Specify other"

label variable hhmaizeq79 "ag/self-rating on: loaction of business (to clients)"
notes hhmaizeq79: Rate yoself on yo business location and reach by your customers
notes hhmaizeq79: 1 extremely inaccessible – 5 very good location and access

label variable hhmaizeq80 "ag/self-rating on: price of pdts (to clients)"
notes hhmaizeq80: Rate yoself on copetitive pricing
notes hhmaizeq80: 1 you are very expensive, 5 you are very competitively priced

label variable hhmaizeq81 "ag/self-rating on: quality of pdts (esp. seed)"
notes hhmaizeq81: Rate yoself on quality of seed especially
notes hhmaizeq81: 1 very poor quality-often fake, 5 excellent quality

label variable hhmaizeq82 "ag/self-rating on: availability of stock of pdts (esp. seed)"
notes hhmaizeq82: Rate yoself on stock availability and in quantities wanted by customers
notes hhmaizeq82: 1 always out of stock and sells in only large quantities, 5 always has stock and accepts to sell in smaller quantities

label variable hhmaizeq83 "ag/self-rating on: reputation "
notes hhmaizeq83: Rate yoself on what others think about your reputation
notes hhmaizeq83: 1 others think you are a lousy agro-dealer, 5. others think you are an excellent agro-dealer

compress
sort idagro
