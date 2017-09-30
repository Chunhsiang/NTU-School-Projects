import excel "C:\Users\User\Desktop\stupid stata\final_ABT.xlsx", sheet("¤u§@ªí1") firstrow

gen ln_rainfall = log(rainfall)

gen avg_wind3 = avg_wind^3
gen avg_wind2 = avg_wind^2

gen ln_month2_rain = log(month2_rain)

reg q_change avg_wind ln_rainfall
cd "C:\Users\User\Desktop\econometrics_final\stata_tables"
outreg2 using final_table1.doc, replace ctitle(Model 1)

reg q_change avg_wind avg_wind2 avg_wind3 ln_rainfall
outreg2 using final_table1.doc, append ctitle(Model 2)

reg q_change avg_wind ln_rainfall days June July August September October
outreg2 using final_table1.doc, append ctitle(Model 3)

reg q_change avg_wind ln_rainfall month2_rain
outreg2 using final_table2.doc, replace ctitle(Model 1)

reg q_change avg_wind ln_rainfall month2_rain worms
outreg2 using final_table2.doc, append ctitle(Model 2)

//reg q_change avg_wind ln_rainfall cabbage_q month2_rain worms
//outreg2 using final_table2.doc, append ctitle(Model 3)

//outreg2 using final_table.doc
