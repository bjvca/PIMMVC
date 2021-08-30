clear

pwd
cd
cd "G:\My Drive\Classroom\Documents from Drive\Pre Doctoral KUL\Paper with Bjorn\PIMMVC\papers\perceptions\data_seed_systems"

*import delimited "G:\My Drive\Classroom\Documents from Drive\Pre Doctoral KUL\Paper with Bjorn\PIMMVC\papers\perceptions\data_seed_systems\data\farmer\rating_dyads.csv"
*save rating_dyads

import delimited "G:\My Drive\Classroom\Documents from Drive\Pre Doctoral KUL\Paper with Bjorn\PIMMVC\papers\perceptions\data_seed_systems\data\input_dealer\baseline_dealer.csv"
*save baseline_dealer

*merging 
merge m:m shop_id using rating_dyads

*cleaning data
mvdecode _all, mv(98)

*changing to numeric variables 
encode farmer_id, gen(farm_id)
encode shop_id, gen(deal_id)

encode quality_rating, gen(qual_rat)
encode seed_quality_general_rating, gen(seed_quality_general_rat)
encode seed_yield_rating, gen(seed_yield_rat)
encode seed_drought_rating, gen(seed_drought_rat)
encode seed_disease_rating, gen(seed_disease_rat)
encode seed_maturing_rating, gen(seed_maturing_rat)
encode seed_germinate_rating, gen(seed_germinate_rat)

*generating overall score
egen score = rowmean(qual_rat seed_quality_general_rat seed_yield_rat seed_drought_rat seed_disease_rat seed_maturing_rat seed_germinate_rat)

drop if score == .

*dealer gender dummy 
gen male = 0
replace male = 1 if maizeowneragreegender == "Male"

*dealer education dummy
gen prim = 0 
replace prim = 1 if maizeowneragreeeduc == "c" | maizeowneragreeeduc == "d" | maizeowneragreeeduc == "e" | maizeowneragreeeduc == "f"

mvdecode maizeowneragreeq3, mv(999)

*number of years for the shop's operation
gen yr = substr(maizeowneragreeq8,1,4)
gen yr_real=real(yr)
gen y=2020
gen years_shop = y-yr_real

gen goodfloor = 0
replace goodfloor = 1 if maizeowneragreetempq77 == "Cement" | maizeowneragreetempq77 == "Tiles"
gen badlighting = 0
replace badlighting = 1 if maizeowneragreetempq78 == 1
gen badstored = 0
replace badstored = 1 if maizeowneragreetempq79 == 1 | maizeowneragreetempq79 == 2 | maizeowneragreetempq79 == 96

encode maizeowneragreeq5, gen(q5)
encode maizeowneragreetempq69, gen(q69)
encode maizeowneragreetempq71, gen(q71)
encode maizeowneragreetempq72, gen(q72)
encode maizeowneragreetempq73, gen(q73)
encode maizeowneragreetempq74, gen(q74)
encode maizeowneragreetempq75, gen(q75)
encode maizeowneragreetempq76, gen(q76)
encode maizeowneragreetempq80, gen(q80)
encode maizeowneragreetempq81, gen(q81)
encode maizeowneragreeq96, gen(q96)

keep male score farm_id deal_id maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70 qual_rat seed_quality_general_rat ///
			seed_yield_rat seed_drought_rat seed_disease_rat seed_maturing_rat seed_germinate_rat

set matsize 800

*regressions 
*xi: regress score male i.farm_id
tset farm_id deal_id


xtreg score male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg qual_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_quality_general_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_yield_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_drought_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_disease_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_maturing_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
xtreg seed_germinate_rat male maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 q5 years_shop ///
            q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting badstored q80 q81 ///
            maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			
			

********************************************************************************************************************
clear

*import delimited "G:\My Drive\Classroom\Documents from Drive\Pre Doctoral KUL\Paper with Bjorn\PIMMVC\papers\perceptions\data_seed_systems\data\farmer\baseline_f_merged.csv", case(preserve)


import delimited "G:\My Drive\Classroom\Documents from Drive\Pre Doctoral KUL\Paper with Bjorn\PIMMVC\fedata.csv", case(preserve)


encode farmer_ID, gen(farm_id)
encode shop_ID, gen(deal_id)

tset farm_id deal_id

*table deal_id farm_id
*xttab deal_id

sum maizeowneragreeage prim maizeowneragreeq3 maizeowneragreeq4 maizeowneragreeq5 years_shop  ///
 maizeowneragreetempq69 maizeowneragreetempq71 maizeowneragreetempq72 maizeowneragreetempq73 ///
 maizeowneragreetempq74 maizeowneragreetempq75 maizeowneragreetempq76 badlighting ///
 badstored maizeowneragreetempq80 maizeowneragreetempq81 maizeowneragreetempq82  maizeowneragreeq96 maizeowneragreeq70
		

gen score_n = real(score)
gen qual_rat = real(quality_rating)
gen seed_quality_general_rat = real(seed_quality_general_rating)
gen seed_yield_rat = real(seed_yield_rating)
gen seed_drought_rat = real(seed_drought_rating)
gen seed_disease_rat = real(seed_disease_rating)
gen seed_maturing_rat = real(seed_maturing_rating)
gen seed_germinate_rat = real(seed_germinate_rating)
	


*encode prim, gen(educ)

gen q3 = real(maizeowneragreeq3)

*encode maizeowneragreeq3, gen(q3)
encode maizeowneragreeq5, gen(q5)
encode maizeowneragreetempq69, gen(q69)
encode maizeowneragreetempq71, gen(q71)
encode maizeowneragreetempq72, gen(q72)
encode maizeowneragreetempq73, gen(q73)
encode maizeowneragreetempq74, gen(q74)
encode maizeowneragreetempq75, gen(q75)
encode maizeowneragreetempq76, gen(q76)

*encode goodfloor, gen(goodfloor_n)
*encode badlighting, gen(badlighting_n)
*encode badstored, gen(badstored_n)

encode maizeowneragreetempq80, gen(q80)
encode maizeowneragreetempq81, gen(q81)
encode maizeowneragreeq96, gen(q96)

reg score_n genderdummy

xtreg score_n genderdummy, fe

xtreg score_n genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
	
xtreg qual_rat genderdummy, fe

xtreg qual_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg seed_quality_general_rat genderdummy, fe

xtreg seed_quality_general_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe

xtreg seed_yield_rat genderdummy, fe

xtreg seed_yield_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg seed_drought_rat genderdummy, fe

xtreg seed_drought_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg seed_disease_rat genderdummy, fe

xtreg seed_disease_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg seed_maturing_rat genderdummy, fe

xtreg seed_maturing_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg seed_germinate_rat genderdummy, fe

xtreg seed_germinate_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe

			  
**** NON SEED RATINGS *****


gen overall = real(overall_rating)
gen gen_nonseed = real(general_rating_nonseed)
gen loc = real(location)
gen price_n = real(price)
gen stock_n = real(stock)
gen rep = real(reputation)

xtreg overall genderdummy, fe

xtreg overall genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
	
xtreg gen_nonseed genderdummy, fe

xtreg gen_nonseed genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg loc genderdummy, fe

xtreg loc genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe

xtreg price_n genderdummy, fe

xtreg price_n genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg qual_rat genderdummy, fe

xtreg qual_rat genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg stock_n genderdummy, fe

xtreg stock_n genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
xtreg rep genderdummy, fe

xtreg rep genderdummy maizeowneragreeage prim q3 maizeowneragreeq4 q5 years_shop q69 q71 q72 q73 q74 q75 q76 goodfloor badlighting ///
			  badstored q80 q81 maizeowneragreetempq82 q96 maizeowneragreeq70, fe
			  
