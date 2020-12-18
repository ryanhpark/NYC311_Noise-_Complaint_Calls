lab var year "Year"
lab var ntacode "NTA Code"
lab var ntaname "NTA Name"
lab var population "Population"
lab var malep "Male (%)"
lab var hispanicp "Hispanic (%)"
lab var whitep "White (%)"
lab var blackp "Black (%)"
lab var asianp "Asian (%)"
lab var lessthanhsp "Less than HS (%)"
lab var morethanbap "BA degree or higher (%)"
lab var foreignbornp "Foreign-born (%)"
lab var younger18p "HH with people <18 (%)"
lab var older65p "HH with people >65 (%)"
lab var medianincome "Median HH income ($)"
lab var renteroccup "Renter-occupied housing units (%)"
lab var owneroccup "Owner-occupied housing units (%)"
lab var rentvacrate "Rental vacancy rate"
lab var medianrent "Median gross rent ($)"
lab var samehousep "Same house >1 yr (%)"
lab var gini "Gini index (NTA-level)"
lab var noisecallcount "# of Noise comp. calls"
lab var heatcallcount " # of Non-noise comp. calls (heat)"
lab var noisecallrate "Noise complaint calls per capita"
lab var heatcallrate "Non-noise (heating) complaint calls per capita"
lab var lognoiserate "Logged noise complaint calls per capita"
lab var logheatrate "Logged non-noise (heating) complaint calls per capita"

gen logmedinc = log(medianincome)
lab var logmedinc "Logged median HH income"

lab var ginitertile "25%,50%,25% Gini Tertiles"
lab var ginitertile2 "33%,33%,33% Gini Tertiles"
lab define ginitert 0 "Low Gini" 1 "Medium Gini" 2 "High Gini"
lab val ginitertile ginitert
lab val ginitertile2 ginitert


hist lr_ncall, normal
hist gini, normal
sktest lrateofcalls
avplots //heteroskedasticity and endogeneity
hettest
rvfplot
vif //multicollinearity test
ovtest //omitted variable bias
predict levs1, leverage //leverage points
lvr2plot, mlabel(ntacode)

**************************************
**************************************

//baseline model
reg noisecallcount gini
outreg2 using myreg1.doc, replace ctitle(Bivariate) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)


//fixed effects ols
xtset year

xtreg noisecallcount gini heatcallcount population malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p  medianrent logmedinc, fe 
outreg2 using myreg1.doc, append ctitle(FE) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

//fixed effect poisson
xtset year

xtpoisson noisecallcount gini heatcallcount malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p  medianrent logmedinc, fe exposure(population)
outreg2 using myreg1.doc, append ctitle(FE Poisson) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

//fixed effects negative binomial
xtset year

xtnbreg noisecallcount gini heatcallcount malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, fe exposure(population)
outreg2 using myreg1.doc, append ctitle(FE Neg.Bin.) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

**************************************
**************************************
//tertile comparison

xtnbreg noisecallcount gini heatcallcount malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, fe exposure(population)
outreg2 using myreg2.doc, replace ctitle(FE Neg.Bin.) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

// 33%,33%,33%
xtset year

xtnbreg noisecallcount ib1.ginitertile2 overall_gini heatcallcount malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, fe exposure(population)
outreg2 using myreg2.doc, append ctitle(33% 33% 33%) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

// 25%,50%,25%
xtset year

xtnbreg noisecallcount ib1.ginitertile overall_gini heatcallcount malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, fe exposure(population)
outreg2 using myreg2.doc, append ctitle(25% 50% 25%) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)


***  Regress noisecallcount on gini
***  Save expected values
reg noisecallcount gini 
predict noisegini_ols if e(sample)
lab var noisegini_ols "Linear"

***  Create gini categories to see nonlinearies
gen ginigroup=0.36362 if gini<=0.37464
replace ginigroup=0.38566 if gini>0.37464&gini<=0.39668
replace ginigroup=0.4077 if gini>0.39668&gini<=0.41872
replace ginigroup=0.42974 if gini>0.41872&gini<=0.44076
replace ginigroup=0.45178 if gini>0.44076&gini<=0.4628
replace ginigroup=0.47382 if gini>0.4628&gini<=0.48484
replace ginigroup=0.49586 if gini>0.48484&gini<=0.50688
replace ginigroup=0.5179 if gini>0.50688&gini<=0.52892
replace ginigroup=0.53994 if gini>0.52892&gini<=0.55096
replace ginigroup=0.56198 if gini>0.55096&gini<=0.573

***  Regress noisecallcount on ginigroup to get means
***  Save expected values
reg noisecallcount i.ginigroup
predict noisegini_mean if e(sample)
lab var noisegini_mean "Means"

***  Narrow bandwidth to 0.10
lowess noisecallcount gini, bw(0.8) gen(s80noisegini)
lowess noisecallcount gini, bw(0.5) gen(s50noisegini)

lab var s80noisegini `""Smoothed""(bw=0.80)""'
lab var s50noisegini `""Smoothed""(bw=0.50)""'
lab var s10noisegini `""Smoothed""(bw=0.10)""'

#delimit ;
tw (scatter noisecallcount gini , jitter(2) mc(sea%8))
	(line noisegini_ols gini, sort lp(l) lc(sea*.5))
	(line s80noisegini s50noisegini gini, sort
	lp(-_) lw(medthick)),
	legend(size(small) pos(3) symxsiz(*1.5) label(1 "NTA (N=188)") cols(1))
	xlab(0.35(0.02)0.58) xscale(range(0.35 0.57) extend)
	ylab(0(500)5000,angle(0))
	graphregion(color(white)) bgcolor(white)
	title("")
	xtitle("Gini Index (Average of 3 years)")
	ytitle("Number of Noise Complaint Calls")
	aspect(.6) xsize(8)
;

// leads and Lags

// noise calls from 2017 with 2015 socio-economic diversity (Lag)
nbreg noisecallcount17 gini heatcallcount17 malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, exposure(population)
outreg2 using myreg3.doc, replace ctitle(Lag) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)

// noise calls from 2013 with 2015 socio-economic diversity (Lead)
nbreg noisecallcount13 gini heatcallcount13 malep foreignbornp hispanicp whitep blackp asianp lessthanhsp morethanbap samehousep rentvacrate owneroccup younger18p older65p medianrent logmedinc, exposure(population)
outreg2 using myreg3.doc, replace ctitle(Lead) label dec(3) pdec(3) alpha(0.001, 0.01, 0.05)



