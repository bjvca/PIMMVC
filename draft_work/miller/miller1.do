import delimited "C:\Users\Ariong Richard\Dropbox (IFPRI)\Ariong_richard\Documents\Maize\MVC\work\miller\millers.csv", encoding(ISO-8859-2) 

label variable date "m/date of interview"

compress
sort q1
rename q1 q1x
encode q1x, gen(q1)
order q1, after(q1x)
recode q1 1=0 2=1
label define q1 1 "Yes" 0 "No", replace
label variable q1 "mm/are you the business owner?"
note q1: Busines owner
drop q1x 

rename q2 q2x
encode q2x, gen(q2)
order q2, after(q2x)
recode q2 1=0 2=1 3=99
label define q2 1 "Yes" 0 "No" 99 "NA", replace
label variable q2 "mm/is the business owner the person responsible for routine activities of the business?"
note q2: Busines operator
drop q2x 

rename q3 q3x
encode q3x, gen(q3)
order q3, after(q3x)
recode q3 2=99
label define q3 1 "Yes" 0 "No" 99 "NA", replace
label variable q3 "mm/can I talk to someone responsible for shop activities?"
note q3: Busines operator
drop q3x 

rename idmiller idmillerx
encode idmillerx, gen(idmiller)
order idmiller, after(idmillerx)
rename idmillerx idmillerxx
rename idmiller idmillerx
rename idmillerxx idmiller
label variable idmiller "mm/id miller"
label variable idmillerx "mm/id miller coded"

rename hhmaizeq6 hhmaizeq6x
encode hhmaizeq6x, gen(hhmaizeq6)
order hhmaizeq6, after(hhmaizeq6x)
label define hhmaizeq6 1 "Village" 2 "Trading center" 3 "Town" 4 "District capital" 99 "NA", replace
label variable hhmaizeq6 "mm/business located?"
note hhmaizeq6: Business location
drop hhmaizeq6x 

label variable hhmaizeq6a "mm/distance (km) to nearest tarmac road"
note hhmaizeq6a: Business location to tarmac road

label variable hhmaizeq6b "mm/distance (km) to nearest murram road"
note hhmaizeq6b: Business location to murram road

label variable hhmaizeq6c "mm/distance (km) to nearest competitor"
note hhmaizeq6c: Business location to other miller

label variable hhmaizeq6d "mm/number of other mills in the neighborhood"
note hhmaizeq6d: Business competition in the area

tostring hhmaizeq6e, replace
sort hhmaizeq6e
rename hhmaizeq6e hhmaizeq6ex
encode hhmaizeq6ex, gen(hhmaizeq6e)
order hhmaizeq6e, after(hhmaizeq6ex)
recode hhmaizeq6e 3=4 4=5 5=96
label define hhmaizeq6e 1 "Director/owner" 2 "Manager" 3 "Operator" 4 "Treasurer" 5 "Sales person" 96 "Other" 99 "NA", replace
label variable hhmaizeq6e "mm/what is your role in the business?"
note hhmaizeq6e: Business role
drop hhmaizeq6ex 
label variable hhmaizeother_q6e "mm/specify other role in the business?"
replace hhmaizeq6e = 3 in 157
replace hhmaizeq6e = 3 in 158
replace hhmaizeq6e = 3 in 159
replace hhmaizeq6e = 3 in 160
replace hhmaizeq6e = 3 in 161
replace hhmaizeq6e = 3 in 164
replace hhmaizeq6e = 3 in 165
replace hhmaizeq6e = 3 in 166
replace hhmaizeq6e = 3 in 167
replace hhmaizeq6e = 3 in 169
replace hhmaizeq6e = 3 in 170
replace hhmaizeq6e = 3 in 172
replace hhmaizeq6e = 3 in 173
replace hhmaizeq6e = 3 in 174
replace hhmaizeq6e = 3 in 171
sort hhmaizeother_q6e

sort hhmaizeq6f
rename hhmaizeq6f hhmaizeq6fx
encode hhmaizeq6fx, gen(hhmaizeq6f)
order hhmaizeq6f, after(hhmaizeq6fx)
recode hhmaizeq6f 1=0 2=1 3=99
label define hhmaizeq6f 1 "Yes" 0 "No" 96 "Other" 99 "NA", replace
label variable hhmaizeq6f "mm/Are you a relative of the owner?"
note hhmaizeq6f: Business relation to owner
drop hhmaizeq6fx 

label variable hhmaizeq6g "m/Age of respondent"
note hhmaizeq6d: age

sort hhmaizeq7
rename hhmaizeq7 hhmaizeq7x
encode hhmaizeq7x, gen(hhmaizeq7)
order hhmaizeq7, after(hhmaizeq7x)
label define hhmaizeq7 1 "Female" 2 "Male" 96 "Other" 99 "NA", replace
label variable hhmaizeq7 "mm/gender of respondent"
note hhmaizeq7: sex
drop hhmaizeq7x 

sort hhmaizeq8
rename hhmaizeq8 hhmaizeq8x
encode hhmaizeq8x, gen(hhmaizeq8)
order hhmaizeq8, after(hhmaizeq8x)
label define hhmaizeq8 1 "Married" 2 "Widowed" 3 "Single" 96 "Other" 99 "NA", replace
label variable hhmaizeq8 "mm/Marital status of respondent"
note hhmaizeq8: Marital status
drop hhmaizeq8x 

sort hhmaizeq9
rename hhmaizeq9 hhmaizeq9x
encode hhmaizeq9x, gen(hhmaizeq9)
order hhmaizeq9, after(hhmaizeq9x)
label define hhmaizeq9 1 "No formal education" 2 "Some primary" 3 "Finished primary" 4 "Some secondary" 5 "Finished secondary" 6 "Higher than secondary" 96 "Other" 99 "NA", replace
label variable hhmaizeq9 "mm/Level of education of respondent"
note hhmaizeq9: Education status
drop hhmaizeq9x 

sort hhmaizeq10a
rename hhmaizeq10a hhmaizeq10ax
encode hhmaizeq10ax, gen(hhmaizeq10a)
order hhmaizeq10a, after(hhmaizeq10ax)
recode hhmaizeq10a 1=0
label define hhmaizeq10a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq10a "mm/process product1- Sesame"
note hhmaizeq10a: Processed products
drop hhmaizeq10ax 

rename hhmaizeq10b hhmaizeq10bx
rename hhmaizeq10c hhmaizeq10cx
rename hhmaizeq10d hhmaizeq10dx
rename hhmaizeq10e hhmaizeq10ex
rename hhmaizeq10f hhmaizeq10fx
rename hhmaizeq10g hhmaizeq10gx
rename hhmaizeq10h hhmaizeq10hx
rename hhmaizeq1096 hhmaizeq1096x

encode hhmaizeq10bx, gen(hhmaizeq10b)
encode hhmaizeq10cx, gen(hhmaizeq10c)
encode hhmaizeq10dx, gen(hhmaizeq10d)
encode hhmaizeq10ex, gen(hhmaizeq10e)
encode hhmaizeq10fx, gen(hhmaizeq10f)
encode hhmaizeq10gx, gen(hhmaizeq10g)
encode hhmaizeq10hx, gen(hhmaizeq10h)
encode hhmaizeq1096x, gen(hhmaizeq1096)

order hhmaizeq10b, after(hhmaizeq10bx)
order hhmaizeq10c, after(hhmaizeq10cx)
order hhmaizeq10d, after(hhmaizeq10dx)
order hhmaizeq10e, after(hhmaizeq10ex)
order hhmaizeq10f, after(hhmaizeq10fx)
order hhmaizeq10g, after(hhmaizeq10gx)
order hhmaizeq10h, after(hhmaizeq10hx)
order hhmaizeq1096, after(hhmaizeq1096x)

recode hhmaizeq10b 1=0 2=1
recode hhmaizeq10c 1=0 2=1
recode hhmaizeq10d 1=0 2=1
recode hhmaizeq10e 1=0 2=1
recode hhmaizeq10f 1=0 2=1
recode hhmaizeq10g 1=0 2=1
recode hhmaizeq10h 1=0 2=1
recode hhmaizeq1096 1=0 2=1

label define hhmaizeq10b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10f 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10g 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq10h 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label define hhmaizeq1096 0 "False" 1 "True" 96 "Other" 99 "NA", replace

label variable hhmaizeq10b "mm/process product1- Cassava"
label variable hhmaizeq10c "mm/process product2- Ground nut"
label variable hhmaizeq10d "mm/process product3- Soybean"
label variable hhmaizeq10e "mm/process product4- Sorghum/millet"
label variable hhmaizeq10f "mm/process product5- Maize"
label variable hhmaizeq10g "mm/process product6- Rice"
label variable hhmaizeq10h "mm/process product7- Maize bran"
label variable hhmaizeq1096 "mm/process product8- other"
label variable hhmaizeother_q10 "mm/process product8- other specify"

note hhmaizeq10b: Processed products
note hhmaizeq10c: Processed products
note hhmaizeq10d: Processed products
note hhmaizeq10e: Processed products
note hhmaizeq10f: Processed products
note hhmaizeq10g: Processed products
note hhmaizeq10h: Processed products
note hhmaizeq1096: Processed products
note hhmaizeother_q10: Processed products

drop hhmaizeq10bx hhmaizeq10cx hhmaizeq10dx hhmaizeq10ex hhmaizeq10fx hhmaizeq10gx hhmaizeq10hx hhmaizeq1096x

label variable hhmaizeq11 "mm/percentage of maize business compared to other crops"

rename hhmaizeq11b hhmaizeq11bx
encode hhmaizeq11bx, gen(hhmaizeq11b)
order hhmaizeq11b, after(hhmaizeq11bx)
recode hhmaizeq11b 1=0 2=1
label define hhmaizeq11b 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq11b "mm/can I take a picture of the mill?"
drop hhmaizeq11bx 

destring hhmaizeq12, replace ignore(`"/"') force float 
label variable hhmaizeq12 "mm/When did you start processing maize?"

rename hhmaizeq13 hhmaizeq13x
encode hhmaizeq13x, gen(hhmaizeq13)
order hhmaizeq13, after(hhmaizeq13x)
recode hhmaizeq13 1=0 2=1
label define hhmaizeq13 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq13 "mm/Does this mill belong to a farmer group, cooperative or association?"
drop hhmaizeq13x 

label variable hhmaizeq143 "mm/produce flour grade No3"

rename hhmaizeq141 hhmaizeq141x
encode hhmaizeq141x, gen(hhmaizeq141)
order hhmaizeq141, after(hhmaizeq141x)
recode hhmaizeq141 1=0 2=1
label define hhmaizeq141 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq141 "mm/produce flour grade No1"
note hhmaizeq141: Processed products
drop hhmaizeq141x 

rename hhmaizeq142 hhmaizeq142x
encode hhmaizeq142x, gen(hhmaizeq142)
order hhmaizeq142, after(hhmaizeq142x)
recode hhmaizeq142 1=0 2=1
label define hhmaizeq142 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq142 "mm/produce flour grade No2"
note hhmaizeq142: Processed products
drop hhmaizeq142x 

rename hhmaizeq143 hhmaizeq143x
encode hhmaizeq143x, gen(hhmaizeq143)
order hhmaizeq143, after(hhmaizeq143x)
recode hhmaizeq143 1=0 2=1
label define hhmaizeq143 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq143 "mm/produce flour grade No3"
note hhmaizeq143: Processed products
drop hhmaizeq143x 

destring hhmaizeq15, replace force float
replace hhmaizeq15=999 if hhmaizeq15==.
label variable hhmaizeq15 "mm/conversion rate(%) btn raw materials & finished product for grade 1"
destring hhmaizeq15b, replace force float
label variable hhmaizeq15b "mm/conversion rate(%) btn raw materials & finished product for grade 2"
replace hhmaizeq15b=999 if hhmaizeq15b==.

destring hhmaizeq16a, replace force float
replace hhmaizeq16a=999 if hhmaizeq16a==.
label variable hhmaizeq16a "mm/How long (in minutes) does it take to process one bag (100 kg) for grade1"
notes hhmaizeq16a: process means debranning and milling) for grade 1

destring hhmaizeq16b, replace force float
replace hhmaizeq16b=999 if hhmaizeq16b==.
label variable hhmaizeq16b "mm/How long (in minutes) does it take to process one bag (100 kg) for grade2"
notes hhmaizeq16b: process means debranning and milling) for grade 2

destring hhmaizeq16c, replace force float
replace hhmaizeq16c=999 if hhmaizeq16c==.
label variable hhmaizeq16c "mm/How long (in minutes) does it take to process one bag (100 kg) for grade3"
notes hhmaizeq16c: process means debranning and milling) for grade 3

label variable hhmaizeq17a "mm/Milling service (Do you mill someone else's maize and charge a fee for that)"
label variable hhmaizeq17b "mm/Milling service (Do you mill someone else's maize and charge a fee for that)"

notes hhmaizeq17a: Valuewise, please indicate the (%) importance of this activity in your business
notes hhmaizeq17b: Valuewise, please indicate the (%) importance of this activity in your business

destring hhmaizeq18a, replace force float
replace hhmaizeq18a=999 if hhmaizeq18a==.
label variable hhmaizeq18a "mm/fee charged for processing one kg of maize into grade 1"

destring hhmaizeq18b, replace force float
replace hhmaizeq18b=999 if hhmaizeq18b==.
label variable hhmaizeq18b "mm/fee charged for processing one kg of maize into grade 2"

destring hhmaizeq18c, replace force float
replace hhmaizeq18c=999 if hhmaizeq18c==.
label variable hhmaizeq18c "mm/fee charged for processing one kg of maize into grade 2"

notes hhmaizeq17a: Valuewise, please indicate the (%) importance of this activity in your business
notes hhmaizeq17b: Valuewise, please indicate the (%) importance of this activity in your business

destring hhmaizeq19a, replace force float
replace hhmaizeq19a=99 if hhmaizeq19a==.
label variable hhmaizeq19a "mm/current sales price (ugx) per kg for finished product/flour for grade 1"

destring hhmaizeq19b, replace force float
replace hhmaizeq19b=99 if hhmaizeq19b==.
label variable hhmaizeq19b "mm/current sales price (ugx) per kg for finished product/flour for grade 2"

destring hhmaizeq19c, replace force float
replace hhmaizeq19c=99 if hhmaizeq19c==.
label variable hhmaizeq19c "mm/current sales price (ugx) per kg for finished product/flour for grade 3"

destring hhmaizeq20, replace force float
replace hhmaizeq20=99 if hhmaizeq20==.
label variable hhmaizeq20 "mm/At what price do you currently buy maize per kg?"

destring hhmaizeq20, replace force float
replace hhmaizeq20=99 if hhmaizeq20==.

destring hhmaizeq21, replace force float
replace hhmaizeq21=99 if hhmaizeq21==999
label variable hhmaizeq21 "mm/What is your current sales unit price for by-products (mz bran)?" 
notes hhmaizeq21: price per kg of maize bran

rename hhmaizeq22a hhmaizeq22ax
encode hhmaizeq22ax, gen(hhmaizeq22a)
order hhmaizeq22a, after(hhmaizeq22ax)
recode hhmaizeq22a 1=0 2=1
label define hhmaizeq22a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq22a "mm/accept paymt by: Cash"
note hhmaizeq22a: accepted payment modality
drop hhmaizeq22ax 

rename hhmaizeq22b hhmaizeq22bx
encode hhmaizeq22bx, gen(hhmaizeq22b)
order hhmaizeq22b, after(hhmaizeq22bx)
recode hhmaizeq22b 1=0 2=1
label define hhmaizeq22b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq22b "mm/accept paymt by: by-product (bran)"
note hhmaizeq22b: accepted payment modality
drop hhmaizeq22bx 

rename hhmaizeq22c hhmaizeq22cx
encode hhmaizeq22cx, gen(hhmaizeq22c)
order hhmaizeq22c, after(hhmaizeq22cx)
recode hhmaizeq22c 1=0 2=1
label define hhmaizeq22c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq22c "mm/accept paymt by: maize grain"
note hhmaizeq22c: accepted payment modality
drop hhmaizeq22cx 

rename hhmaizeq2296 hhmaizeq2296x
encode hhmaizeq2296x, gen(hhmaizeq2296)
order hhmaizeq2296, after(hhmaizeq2296x)
recode hhmaizeq2296 1=0 2=1
label define hhmaizeq2296 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq2296 "mm/accept paymt by: other modality"
note hhmaizeq2296: accepted payment modality
drop hhmaizeq2296x 

rename hhmaizeq23 hhmaizeq23x
encode hhmaizeq23x, gen(hhmaizeq23)
order hhmaizeq23, after(hhmaizeq23x)
recode hhmaizeq23 1=0 2=1
label define hhmaizeq23 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq23 "mm/Is there a minimum quantity (kgs) that you mill?"
drop hhmaizeq23x 

destring hhmaizeq23a, replace force float
replace hhmaizeq23a=99 if hhmaizeq23a==.
label variable hhmaizeq23a "mm/What minimum quantity do you mill"

rename hhmaizeq24 hhmaizeq24x
encode hhmaizeq24x, gen(hhmaizeq24)
order hhmaizeq24, after(hhmaizeq24x)
recode hhmaizeq24 1=0 2=1
label define hhmaizeq24 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq24 "mm/Do customers get discounts if they come to process large quantities?"
drop hhmaizeq24x 

destring hhmaizeq24b, replace force float
replace hhmaizeq24b=999 if hhmaizeq24b==.
label variable hhmaizeq24b "mm/How much is a large quantity?"

rename hhmaizeq25a hhmaizeq25ax
encode hhmaizeq25ax, gen(hhmaizeq25a)
order hhmaizeq25a, after(hhmaizeq25ax)
recode hhmaizeq25a 1=0 2=1
label define hhmaizeq25a 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25a "mm/service1 provided to clients- advance paymt/credit"
note hhmaizeq25a: service/assistance provided to clients
drop hhmaizeq25ax 

rename hhmaizeq25b hhmaizeq25bx
encode hhmaizeq25bx, gen(hhmaizeq25b)
order hhmaizeq25b, after(hhmaizeq25bx)
recode hhmaizeq25b 1=0 2=1
label define hhmaizeq25b 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25b "mm/service2 provided to clients- market information"
note hhmaizeq25b: service/assistance provided to clients
drop hhmaizeq25bx 

rename hhmaizeq25c hhmaizeq25cx
encode hhmaizeq25cx, gen(hhmaizeq25c)
order hhmaizeq25c, after(hhmaizeq25cx)
recode hhmaizeq25c 1=0 2=1
label define hhmaizeq25c 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25c "mm/service3 provided to clients- inputs (seed, fertilizer)"
note hhmaizeq25c: service/assistance provided to clients
drop hhmaizeq25cx 

rename hhmaizeq25d hhmaizeq25dx
encode hhmaizeq25dx, gen(hhmaizeq25d)
order hhmaizeq25d, after(hhmaizeq25dx)
recode hhmaizeq25d 1=0 2=1
label define hhmaizeq25d 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25d "mm/service4 provided to clients- tarpaulins for drying of maize"
note hhmaizeq25d: service/assistance provided to clients
drop hhmaizeq25dx 

rename hhmaizeq25e hhmaizeq25ex
encode hhmaizeq25ex, gen(hhmaizeq25e)
order hhmaizeq25e, after(hhmaizeq25ex)
recode hhmaizeq25e 1=0 2=1
label define hhmaizeq25e 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25e "mm/service1 provided to clients- PICS bags"
note hhmaizeq25e: service/assistance provided to clients
drop hhmaizeq25ex 

rename hhmaizeq25f hhmaizeq25fx
encode hhmaizeq25fx, gen(hhmaizeq25f)
order hhmaizeq25f, after(hhmaizeq25fx)
recode hhmaizeq25f 1=0 2=1
label define hhmaizeq25f 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25f "mm/service5 provided to clients- gunny bags"
note hhmaizeq25f: service/assistance provided to clients
drop hhmaizeq25fx 


rename hhmaizeq25g hhmaizeq25gx
encode hhmaizeq25gx, gen(hhmaizeq25g)
order hhmaizeq25g, after(hhmaizeq25gx)
recode hhmaizeq25g 1=0 2=1
label define hhmaizeq25g 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25g "mm/service6 provided to clients- storage services"
note hhmaizeq25g: service/assistance provided to clients
drop hhmaizeq25gx 

rename hhmaizeq25h hhmaizeq25hx
encode hhmaizeq25hx, gen(hhmaizeq25h)
order hhmaizeq25h, after(hhmaizeq25hx)
recode hhmaizeq25h 1=0 2=1
label define hhmaizeq25h 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25h "mm/service7 provided to clients- drying services"
note hhmaizeq25h: service/assistance provided to clients
drop hhmaizeq25hx 

rename hhmaizeq25i hhmaizeq25ix
encode hhmaizeq25ix, gen(hhmaizeq25i)
order hhmaizeq25i, after(hhmaizeq25ix)
recode hhmaizeq25i 1=0 2=1
label define hhmaizeq25i 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25i "mm/service8 provided to clients- cleaning services"
note hhmaizeq25i: service/assistance provided to clients
drop hhmaizeq25ix 

rename hhmaizeq25j hhmaizeq25jx
encode hhmaizeq25jx, gen(hhmaizeq25j)
order hhmaizeq25j, after(hhmaizeq25jx)
recode hhmaizeq25j 1=0 2=1
label define hhmaizeq25j 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25j "mm/service9 provided to clients- technical assistance/training"
note hhmaizeq25j: service/assistance provided to clients
drop hhmaizeq25jx 

rename hhmaizeq25k hhmaizeq25kx
encode hhmaizeq25kx, gen(hhmaizeq25k)
order hhmaizeq25k, after(hhmaizeq25kx)
recode hhmaizeq25k 1=0 2=1
label define hhmaizeq25k 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq25k "mm/service10 provided to clients- don't give any services"
note hhmaizeq25k: service/assistance provided to clients
drop hhmaizeq25kx 

rename hhmaizeq2596 hhmaizeq2596x
encode hhmaizeq2596x, gen(hhmaizeq2596)
order hhmaizeq2596, after(hhmaizeq2596x)
recode hhmaizeq2596 1=0 2=1
label define hhmaizeq2596 0 "False" 1 "True" 96 "Other" 99 "NA", replace
label variable hhmaizeq2596 "mm/service11 provided to clients- give other service"
note hhmaizeq2596: service/assistance provided to clients
drop hhmaizeq2596x 

label variable hhmaizeother_q25 "mm/other service provide specify"

label variable hhmaizeq26 "mm/How many milling machines do you have?"
label variable hhmaizeq27 "mm/How many debranning machines do you have?"

rename hhmaizeq28 hhmaizeq28x
encode hhmaizeq28x, gen(hhmaizeq28)
order hhmaizeq28, after(hhmaizeq28x)
label define hhmaizeq28 1 "Electricity 3-phase" 2 "Electricity single phase" 3 "Diesel" 4 "Animal" 5 "Human" 96 "Other" 99 "NA", replace
label variable hhmaizeq28 "mm/How is your milling machine powered?"
note hhmaizeq28: machine power
drop hhmaizeq28x 
label variable hhmaizeother_q28 "mm/other milling machine power specify"

rename hhmaizeq29 hhmaizeq29x
encode hhmaizeq29x, gen(hhmaizeq29)
order hhmaizeq29, after(hhmaizeq29x)
recode hhmaizeq29 1=0 2=1
label define hhmaizeq29 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq29 "mm/Do you have any tables to sieve before milling?"
drop hhmaizeq29x 

rename hhmaizeq30 hhmaizeq30x
encode hhmaizeq30x, gen(hhmaizeq30)
order hhmaizeq30, after(hhmaizeq30x)
recode hhmaizeq30 1=0 2=1
label define hhmaizeq30 0 "No" 1 "Yes" 96 "Other" 99 "NA", replace
label variable hhmaizeq30 "mm/Is the mill in a separate structure?"
drop hhmaizeq30x 

rename hhmaizeq31 hhmaizeq31x
encode hhmaizeq31x, gen(hhmaizeq31)
order hhmaizeq31, after(hhmaizeq31x)
label define hhmaizeq31 2 "Iron sheets" 1 "Grass" 3 "Tiles" 96 "Other" 99 "NA", replace
label variable hhmaizeq31 "mm/Material of roof"
drop hhmaizeq31x 

rename hhmaizeq32 hhmaizeq32x
encode hhmaizeq32x, gen(hhmaizeq32)
order hhmaizeq32, after(hhmaizeq32x)
recode hhmaizeq32 1=96 2=1 3=2 4=3 5=4
label define hhmaizeq32 2 "Dirt/mud" 1 "Concrete" 3 "Wood" 4 "Iron sheets" 96 "Other" 99 "NA", replace 
label variable hhmaizeq32 "mm/Material of walls"
drop hhmaizeq32x 
label variable hhmaizeother_q32 "mm/Other material of walls"

rename hhmaizeq33 hhmaizeq33x
encode hhmaizeq33x, gen(hhmaizeq33)
order hhmaizeq33, after(hhmaizeq33x)
recode hhmaizeq33 1=96 2=1 3=2 4=3 5=4
label define hhmaizeq33 1 "Concrete" 2 "Dirt/mud" 3 "Wood" 4 "Iron sheets" 96 "Other" 99 "NA", replace 
label variable hhmaizeq33 "mm/Material of floor"
drop hhmaizeq33x 
label variable hhmaizeother_q33 "mm/Other material of floor"

label variable hhmaizeq36 "mm/self-rating on location of mill"
notes hhmaizeq36: considering ease of access, convinience to visit
notes hhmaizeq36: 1 extremely inaccessible – 5 very good location and access

label variable hhmaizeq37 "mm/self-rating on price charge"
notes hhmaizeq37: considering properties of your mill & quality of product
notes hhmaizeq37: 1 way too expensive, 5 extremely cheap

label variable hhmaizeq38 "mm/self-rating on quality of of end product"
notes hhmaizeq38: considering properties of your mill & the price charged
notes hhmaizeq38: 1 very poor quality, 5 excellent quality

label variable hhmaizeq39 "mm/self-rating on quality of service/assistance"
notes hhmaizeq39: according to your own opinion, are you and your employees friendly, are waiting times at your mill acceptable, is the place reasonably clean, …
notes hhmaizeq39: 1 very poor overall quality of service, 5 excellent overall quality of service

label variable hhmaizeq40 "mm/self-rating on reputation"
notes hhmaizeq40: considering what other people think about you as a miller and your business
notes hhmaizeq40: 1 they think you are lousy, 5 very honest business person

compress
sort idmiller












