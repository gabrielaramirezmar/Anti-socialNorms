/****************************************************************************
*NSSA COVID Analysis
*Created: 4/06/2022
*Last updated: 26/07/2022 by Paula Torres Higuera
*PURPOSE: analyse data for NSSA COVID project
*DATASETS Used

	Modified: 
		- $modified/Covid_modified

*DATASETS Created:
		- $modified/Strategy_method_covid

****************************************************************************/

do "C:\Users\gaby_\OneDrive\Documentos\RA - Jose Guerra\Anti_social norms covid\Codes\TPGGamelikeCovid2022\GR_00_master.do"

use "$modified/Covid_modified", clear
cap drop if DF==1 


							****************************** 
							*         Controls           *
							******************************

			
*| Add a Control from major: students that have an economic degree or business degree

   *Modify values to make them comparable (i.e: "Gobierno" & "gobierno" as "GOBIERNO")
	gen degree=survey4
	replace degree=upper(degree)
	label var degree "Participant's Major"
		* Economic degree
		gen econ_degree=regexm(degree, "ECON") //We can also use strpo
		label var econ_degree "Economics Major"
		* Business degree
		gen busi_degree=regexm(degree, "ADMIN")
		label var busi_degree "Business Major"
		* One unique variable from major
		gen related_degree=(econ_degree==1 | busi_degree==1)
		label var related_degree "Dummy - Participant has a related degree"
		label define related 0"No related" 1"Has a related major"
		label values related_degree related
		
	global controls gender semester rational stratum related_degree
	
	save "$modified/Covid_modified", replace
	
	
	

/****************************************************************************
************************* Descriptive Statistics *******************************
****************************************************************************/	

recode covvac (1 = 2) (2 = 1)
   label define covvac2 0 "No" 1 "Parcialmente" 2 "Sí"
   label values covvac covvac2
   
 replace survey3 = 0 if survey3 == -2  
   
/*
label define justicia_2 0"Justo" 1"Injusto"
label values fair1 fair2 justicia_2
*/
   
   

global descriptive "survey1 gender survey3 related_degree semester survey7 survey8 rational survey10 situ_financiera stratum educ_m educ_p control1 control2 control3 control4 covvac covvacaft covinfc covperfam covbef trustbin trustknown trustnatgov trustlocgov trustint polcd polcr polcon pollib polver polpol polcol polduq poluri polvar polchar polpas polmar polgav polsan pollop polloz polpet polbol polcep polcompas polimport infor poli1 poli2 poli3 poli4 demo1 demo2 demo3 demo4 demo5 demo6 goodcau fav1 reci1 reci2 survey11 survey12 survey13 survey14"

mat characteristics =J(68,4,.)

tokenize ${descriptive}

forvalues j=1/68{
	
	qui sum ``j''
	matrix characteristics [`j',1]= r(mean)
	matrix characteristics [`j',2]= r(min)
	matrix characteristics [`j',3]= r(max)
	matrix characteristics [`j',4]= r(sd)		
	matlist characteristics
	
}

frmttable using "$tables2/Descriptive_survey.doc", replace sdec(1) statmat(characteristics) ctitles("", "Media", "Min", "Max", "SD") rtitles("Age" \"Woman" \ "Number of siblings" \"Degree related to Economics/Business" \ "Current Semester" \ "Previously participated in experiments" \ "Donated money/volunteer" \ "Guess 2/3 of the Average" \ "Weekly Expenses" \ "Financial situation" \ "House of Residence Stratum" \ "Higher educational level (Mother)" \ "Higher educational level (Father)" \ "Control 1" \ "Control 2" \ "Control 3" \ "Control 4 " \ "Vaccinated from Covid" \ "Willingness to get vaccinated" \ "Probability of contagion during the pandemic" \ "% of friends and family who got infected by COVID-19"\ "How did your home live economically before the pandemic" \ "Trust in others" \ "Trust in people you know" \ "Trust in Colombias National Government" \ "Trust in Bogotas District Government" \ "Trust in Multilateral Organizations" \ "Centro Democrático" \ "Cambio Radical" \ "Partido Conservador" \ "Partido Liberal" \ "Partido Verde" \ "Polo Democratico" \ "Colombia Humana" \ "Iván Duque"\ "Alvaro Uribe" \ "Germán Vargas Lleras" \ "Alejandro Char" \ "Andres Pastrana" \ "Marta Lucia Ramirez" \ "Cesar Gaviria" \ "Juan Manuel Santos" \ "Claudia Lopez" \ "Angélica Lozano" \ "Gustavo Petro" \ "Gustavo Bolivar" \ "Ivan Cepeda" \ "Political compass" \ "Importance of politics in your life" \ "Media from which you inform yourself" \ "Political Systems 1" \ "Political Systems 2" \ "Political Systems 3" \ "Political Systems 4" \ "Democracy characteristics 1" \ "Democracy characteristics 2" \ "Democracy characteristics 3" \ "Democracy characteristics 4" \ "Democracy characteristics 5" \ "Democracy characteristics 6" \ "Give with nothing in return" \ "Willingness to return a favor" \ "Help others who help us" \ "Punish someone who does something bad to me" \ "Compliance with rules by classmates" \ "Compliance with rules by politicians" \ "Compliance with rules by citizens" \ "Compliance with rules by family members")
	
	
/****************************************************************************
************************* Preliminar Analysis *******************************
****************************************************************************/

use "$modified/Covid_modified", clear
global controls gender semester rational stratum related_degree
	          *--------------------------------------*
		      *   Participants by role and treatment *
			  *--------------------------------------*

	*All sessions*	
	
	tabout treatment id_role if DF!=1 using "$tables/distribution_covid.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-5) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)



		
	/********************************************************
	*                        GRAPHS                         *
	*********************************************************/
		
/*Sent amount*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/
	
	cap drop if DF==1

	/*Neutral
	- We want to test the effect of being under the neutral, Duque or López treatment over the amount of money sent by a Sender. This variable is 0 for third parties as they are no involved in this interaction*/
	 
	reg group_sent_amount if treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*Duque*/
	reg group_sent_amount if treatment == 1 & id_role==1
	estimates store Duq
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*López*/
	reg group_sent_amount if treatment == 2 & id_role==1
	estimates store Lop
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu Duq Lop, robust /*robust errors at the participant level.*/ //Qué es ese lnvar?
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[Duq_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*non significant difference*/
	
	test [Neu_mean]_cons=[Lop_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [Duq_mean]_cons=[Lop_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_sent_amount if id_role==1, over(treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Sent amount/HT_covid.gph", replace
	graph export "$graphs/Sent amount/HT_covid.png", replace
	graph export "$graphs/Sent amount/HT_covid.pdf", replace	



	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_sent_amount if treatment==0 & id_role==1
	estimates store Neutral
	
	/*Duque*/
	reg group_sent_amount if treatment == 1 & id_role==1
	estimates store Duque
	
	/*López*/
	reg group_sent_amount if treatment == 2 & id_role==1
	estimates store López
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (Duque, bcolor(gs9)) (López, bcolor(gs5)), bylabel(treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Sent amount/Coefplot_SA_Covid.gph", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid.png", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid.pdf", replace		
		

		
		
		
/*Expected Sent amount*/
	/********************************************************
	*                 Hypothesis testing                    *
	*********************************************************/

	/*Neutral*/
	reg group_expectation_sent if treatment==0 & id_role==2
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if treatment == 0 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*Duque*/
	reg group_expectation_sent if treatment == 1 & id_role==2
	estimates store Duq
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if treatment == 1 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*López*/
	reg group_expectation_sent if treatment == 2 & id_role==2
	estimates store Lop
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if treatment == 2 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu Duq Lop, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[Duq_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[Lop_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [Duq_mean]_cons=[Lop_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_expectation_sent if id_role==2, over(treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Expected Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Expected/EXP_HT_covid.gph", replace
	graph export "$graphs/Expected/Exp_HT_covid.png", replace
	graph export "$graphs/Expected/Exp_HT_covid.pdf", replace
	

	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_expectation_sent if treatment==0 & id_role==2
	estimates store Neutral
	
	/*Duque*/
	reg group_expectation_sent if treatment == 1 & id_role==2
	estimates store Duque
	
	/*López*/
	reg group_expectation_sent if treatment == 2 & id_role==2
	estimates store López
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (Duque, bcolor(gs9)) (López, bcolor(gs5)), bylabel(treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Expected Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Expected/Coefplot_Exp_Covid.gph", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid.png", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid.pdf", replace
	
/*Equality*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg equality if treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum equality if treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*Duque*/
	reg equality if treatment == 1 & id_role==1
	estimates store Duq
	/*Confidence Interval is very similar to the regression result*/
	sum equality if treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*López*/
	reg equality if treatment == 2 & id_role==1
	estimates store Lop
	/*Confidence Interval is very similar to the regression result*/
	sum equality if treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu Duq Lop, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[Duq_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[Lop_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [Duq_mean]_cons=[Lop_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) equality, over(treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Equality) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.37 33 "___________________") text(0.37 33 "`pDGTPG'", size(medsmall)) text(0.39 49 "_____________________________________") text(0.39 49 "`pDGTPGM'", size(medsmall)) text(0.37 67 "_________________") text(0.37 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.4) 
	graph save "$graphs/Equality/EHT_covid.gph", replace
	graph export "$graphs/Equality/EHT_covid.png", replace
	graph export "$graphs/Equality/EHT_covid.pdf", replace		
	
	
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg equality if treatment==0 & id_role==1
	estimates store Neutral
	
	/*Duque*/
	reg equality if treatment == 1 & id_role==1
	estimates store Duque
	
	/*López*/
	reg equality if treatment == 2 & id_role==1
	estimates store López
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (Duque, bcolor(gs9)) (López, bcolor(gs5)), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Equality") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.5, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Equality/Coefplot_E_Covid.gph", replace
		graph export "$graphs/Equality/Coefplot_E_Covid.png", replace
		graph export "$graphs/Equality/Coefplot_E_Covid.pdf", replace			
	
	
/*Message*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg group_message if treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*Duque*/
	reg group_message if treatment == 1 & id_role==1
	estimates store Duq
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*López*/
	reg group_message if treatment == 2 & id_role==1
	estimates store Lop
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu Duq Lop, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[Duq_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[Lop_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [Duq_mean]_cons=[Lop_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_message if id_role==1, over(treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(% of sent messages) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.37 33 "___________________") text(0.37 33 "`pDGTPG'", size(medsmall)) text(0.39 49 "_____________________________________") text(0.39 49 "`pDGTPGM'", size(medsmall)) text(0.37 67 "_________________") text(0.37 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.4) 
	graph save "$graphs/Message/MessHT_covid.gph", replace
	graph export "$graphs/Message/MessHT_covid.png", replace
	graph export "$graphs/Message/MessHT_covid.pdf", replace		
		
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_message if treatment==0 & id_role==1
	estimates store Neutral
	
	/*Duque*/
	reg group_message if treatment == 1 & id_role==1
	estimates store Duque
	
	/*López*/
	reg group_message if treatment == 2 & id_role==1
	estimates store López
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (Duque, bcolor(gs9)) (López, bcolor(gs5)), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("% of sent messages") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.5, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Message/Coefplot_Mess_Covid.gph", replace
		graph export "$graphs/Message/Coefplot_Mess_Covid.png", replace
		graph export "$graphs/Message/Coefplot_Mess_Covid.pdf", replace
	

/*Expected Message*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg group_expectation_message if treatment==0 & id_role==2
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if treatment == 0 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*Duque*/
	reg group_expectation_message if treatment == 1 & id_role==2
	estimates store Duq
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if treatment == 1 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*López*/
	reg group_expectation_message if treatment == 2 & id_role==2
	estimates store Lop
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if treatment == 2 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu Duq Lop, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[Duq_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[Lop_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [Duq_mean]_cons=[Lop_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_expectation_message if id_role==2, over(treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(% of Receivers that expect message) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.57 33 "___________________") text(0.57 33 "`pDGTPG'", size(medsmall)) text(0.59 49 "_____________________________________") text(0.59 49 "`pDGTPGM'", size(medsmall)) text(0.57 67 "_________________") text(0.57 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.6) 
	graph save "$graphs/Message/ExpMessHT_covid.gph", replace
	graph export "$graphs/Message/ExpMessHT_covid.png", replace
	graph export "$graphs/Message/ExpMessHT_covid.pdf", replace		
		
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_expectation_message if treatment==0 & id_role==2
	estimates store Neutral
	
	/*Duque*/
	reg group_expectation_message if treatment == 1 & id_role==2
	estimates store Duque
	
	/*López*/
	reg group_expectation_message if treatment == 2 & id_role==2
	estimates store López
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (Duque, bcolor(gs9)) (López, bcolor(gs5)), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("% of Receivers that expect message") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.6, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Message/Coefplot_ExpMess_Covid.gph", replace
		graph export "$graphs/Message/Coefplot_ExpMess_Covid.png", replace
		graph export "$graphs/Message/Coefplot_ExpMess_Covid.pdf", replace

		
/*Social norms*/	

/*Dummy variables for social norms*/ /*Generamos una dummy para cada variable de social norms con base en los valores de apropiado o no socialmente */
foreach var in Q_1 Q_2 Q_3 Q_4 Q_5 Q_6 {
    
	cap gen `var'd=1 if `var'==2 | `var'==3 
	replace `var'd=0 if `var'==0 | `var'==1
	
//En este caso, la variable toma el valor de 1 si la persona tiene una noción de apropiado socialmente, y 0 de lo contrario
}
save "$modified/Covid_modified", replace

	/*******************GRAPH (by id_role)*******************/	
	/*Receivers*/
	//Analizamos si el hecho de ser third parties o reveivers tiene un efecto sobre la percepcion que se tiene de apropiado o no socialmente para cada afirmación
	 reg Q_1d if id_role==2, r 
	 estimates store R
	 
	 reg Q_2d if id_role==2, r 
	 estimates store R2
	
	 reg Q_3d if id_role==2, r 
	 estimates store R3
	 
	 /*Third Parties*/
	 reg Q_4d if id_role==3, r 
	 estimates store TP
	 
	 reg Q_5d if id_role==3, r 
	 estimates store TP2
	 
	 reg Q_6d if id_role==3, r 
	 estimates store TP3
	 
	 		coefplot (R, bcolor(gs13) label(Receiver)) (TP, bcolor(gs9) label(Third Party)) (R2, bcolor(gs13) nokey) (TP2, bcolor(gs9) nokey) (R3, bcolor(gs13) nokey) (TP3, bcolor(gs9) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.08) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_Covid.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_Covid.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_Covid.pdf", replace	
		
		
	/*******************GRAPH RECEIVERS (by treatment)*******************/	/* Acá se realiza el mismo ejercicio anterior, pero diferenciando por tratamiento */
	 reg Q_1d if id_role==2 & treatment==0, r 
	 estimates store RN
	 
	 reg Q_1d if id_role==2 & treatment==1, r 
	 estimates store RD
	 
	 reg Q_1d if id_role==2  & treatment==2, r 
	 estimates store RL
	 
	 reg Q_2d if id_role==2 & treatment==0, r 
	 estimates store RN2
	 
	 reg Q_2d if id_role==2  & treatment==1, r 
	 estimates store RD2
	 	 
	 reg Q_2d if id_role==2  & treatment==2, r 
	 estimates store RL2
	 
	 reg Q_3d if id_role==2 & treatment==0, r 
	 estimates store RN3
	 
	 reg Q_3d if id_role==2  & treatment==1, r 
	 estimates store RD3
	 	 
	 reg Q_3d if id_role==2  & treatment==2, r 
	 estimates store RL3
	 
	 	coefplot (RN, bcolor(gs13) label(Neutral)) (RD, bcolor(gs9) label(Duque)) (RL, bcolor(gs5) label(López)) (RN2, bcolor(gs13) nokey) (RD2, bcolor(gs9) nokey) (RL2, bcolor(gs5) nokey) (RN3, bcolor(gs13) nokey) (RD3, bcolor(gs9) nokey) (RL3, bcolor(gs5) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.06) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3" , labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_Receivers_Covid.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_Receivers_Covid.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_Receivers_Covid.pdf", replace	 
	 	
		
	/*******************GRAPH THIRD PARTIES (by treatment)*******************/	
	 reg Q_4d if id_role==3 & treatment==0, r 
	 estimates store TPN
	 
	 reg Q_4d if id_role==3 & treatment==1, r 
	 estimates store TPD
	 
	 reg Q_4d if id_role==3  & treatment==2, r 
	 estimates store TPL
	 
	 reg Q_5d if id_role==3 & treatment==0, r 
	 estimates store TPN2
	 
	 reg Q_5d if id_role==3  & treatment==1, r 
	 estimates store TPD2
	 	 
	 reg Q_5d if id_role==3  & treatment==2, r 
	 estimates store TPL2
	 
	 reg Q_6d if id_role==3 & treatment==0, r 
	 estimates store TPN3
	 
	 reg Q_6d if id_role==3  & treatment==1, r 
	 estimates store TPD3
	 	 
	 reg Q_6d if id_role==3  & treatment==2, r 
	 estimates store TPL3
	 
	 	coefplot (TPN, bcolor(gs13) label(Neutral)) (TPD, bcolor(gs9) label(Duque)) (TPL, bcolor(gs5) label(López)) (TPN2, bcolor(gs13) nokey) (TPD2, bcolor(gs9) nokey) (TPL2, bcolor(gs5) nokey) (TPN3, bcolor(gs13) nokey) (TPD3, bcolor(gs9) nokey) (TPL3, bcolor(gs5) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.06) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3" , labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_TP_Covid.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_TP_Covid.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_TP_Covid.pdf", replace	
		
/**************************************************************************************************** Regression Analysis *********************
***********************************************************************/

global controls gender semester rational stratum related_degree

tab treatment, gen(t)
rename t1 Neutral 
label var Neutral "1 if treatment==Neutral"
rename t2 Duque
label var Duque "1 if treatment==Duque"
rename t3 López
label var López "1 if treatment==López"

/******************* Sent amount, Message, Equality ******************/	

    *For Senders 
    reg group_sent_amount Duque López if id_role==1, r
    reg group_message Duque López if id_role==1, r
    reg equality Duque López if id_role==1, r		


    *Define table content 
    cd "$tables/Sent amount"

    *Number of table
local tab `i'
di "`tab'"


estimates clear
local c = 1
foreach var of varlist group_sent_amount group_sent_amount group_message group_message equality equality {
	
	// Si el local toma el valor de 1, 3 o 5 (Es decir, en la iteración 1, 3 y 5) la regresión 1 será la de la variable dependiente tomando los tratamientos de Duque y López para el rol de Senders 	
	
	if `c'==1 | `c'==3 | `c'==5 {

	eststo regs`c': reg `var' Duque López if id_role==1, r
			
			//Hallar el efecto relativo (constante) para cada tratamiento
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

//Hacer la prueba de igualdad para los coeficientes asociados a cada tratamiento en la regresión

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.

qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}

//Ahora se realiza el mismo ejercicio anterior pero incluyendo controles 
		else {
			eststo regs`c': reg `var' Duque López $controls if id_role==1, r
		
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1 //Vamos haciendo que el local c cambie
}

			

*Keep coefficients
local coefs _cons Duque López $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the amount sent, the probability of sending the message and the probability of an equal split"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Sent amount} & \multicolumn{2}{c}{Message sent} & \multicolumn{2}{c}{Equality} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "Sender_variables_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Duque López _cons) ///
coeflabels(Duque "Duque" López "López") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Sender_variables_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Sender_variables_covid.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect Duque vs Neutral(\%)" "Effect López vs Neutral(\%)" "p-value H0: Duque = López")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Senders} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})		
		
		
/*************** Expected sent amount, Expected message **************/	
reg group_expectation_sent Duque López if id_role==2, r
reg group_expectation_message Duque López if id_role==2, r		


*Define table content 
cd "$tables/Expected"

*Number of table
local tab `i'
di "`tab'"


//Now for Receivers
estimates clear
local c = 1
foreach var of varlist group_expectation_sent group_expectation_sent group_expectation_message group_expectation_message {
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' Duque López if id_role==2, r
			
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}

//Hacemos el mismo ejercicio pero incluyendo controles
		else {
			eststo regs`c': reg `var' Duque López $controls if id_role==2, r
		
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Duque López $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on Receivers expectations"
local titles "& \multicolumn{4}{c}{Dependent Variables} \\ \cmidrule{2-5} & \multicolumn{2}{c}{Expected sent amount} & \multicolumn{2}{c}{Expected message} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "Receiver_variables_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Duque López _cons) ///
coeflabels(Duque "Duque" López "López") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Receiver_variables_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Receiver_variables_covid.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect Duque vs Neutral(\%)" "Effect López vs Neutral(\%)" "p-value H0: Duque = López")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Receivers} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})			
		
		
/******************* Appropriateness (Receivers) *******************/	

reg Q_1d Duque López if id_role==2, r
reg Q_2d Duque López if id_role==2, r		
reg Q_3d Duque López if id_role==2, r	

*Define table content 
cd "$tables/Appropriateness"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist Q_1d Q_1d Q_2d Q_2d Q_3d Q_3d {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Duque López if id_role==2, r
			
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Duque López $controls if id_role==2, r
		
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Duque López $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the probability that Receivers will consider Senders' actions socially appropriate"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Q1. Unfairness} & \multicolumn{2}{c}{Q2. Sanctioning unfairness} & \multicolumn{2}{c}{Q3. Enforcing DBS} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "Receiver_app_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Duque López _cons) ///
coeflabels(Duque "Duque" López "López") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Receiver_app_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Receiver_app_covid.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect Duque vs Neutral(\%)" "Effect López vs Neutral(\%)" "p-value H0: Duque = López")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Receivers} sample. Neutral treatment is the baseline of analysis. Dependent variable is 1 if the player considers that a given action taken by the Sender is socially appropriate. Questions are: Q1 \textit{Is it socially appropriate if a Sender decides to transfer less than 50 points to a Receiver?}; Q2 \textit{Is it socially appropriate if a Third Party decides to charge deduction points to the Sender if this Sender sends less than 50 points to the Receiver?}; Q3 \textit{Is it socially appropriate if a Sender decides to send the message: \textit{"No te metas en lo que no te importa"} after being charged with deduction points}. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})
		
		
/*************** Appropriateness (Third Parties) ***************/	

reg Q_4d Duque López if id_role==3, r
reg Q_5d Duque López if id_role==3, r		
reg Q_6d Duque López if id_role==3, r	

*Define table content 
cd "$tables/Appropriateness"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist Q_4d Q_4d Q_5d Q_5d Q_6d Q_6d {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Duque López if id_role==3, r
			
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Duque López $controls if id_role==3, r
		
estadd scalar effectD = (_b[Duque]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[López]/_b[_cons])*100: regs`c'

test Duque==López
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Duque López $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the probability that Third Parties will consider Senders' actions socially appropriate"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Q1. Unfairness} & \multicolumn{2}{c}{Q2. Sanctioning unfairness} & \multicolumn{2}{c}{Q3. Enforcing DBS} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "TP_app_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Duque López _cons) ///
coeflabels(Duque "Duque" López "López") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "TP_app_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "TP_app_covid.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect Duque vs Neutral(\%)" "Effect López vs Neutral(\%)" "p-value H0: Duque = López")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Parties} sample. Neutral treatment is the baseline of analysis. Dependent variable is 1 if the player considers that a given action taken by the Sender is socially appropriate. Questions are: Q1 \textit{Is it socially appropriate if a Sender decides to transfer less than 50 points to a Receiver?}; Q2 \textit{Is it socially appropriate if a Third Party decides to charge deduction points to the Sender if this Sender sends less than 50 points to the Receiver?}; Q3 \textit{Is it socially appropriate if a Sender decides to send the message: \textit{"No te metas en lo que no te importa"} after being charged with deduction points}. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})		
		
		
		
		
		
		
		
		
/***********************************************************************
***************************** ANALYSIS *********************************
***********************************************************************/

//This is the analysis set in the Overleaf

use "$modified/Covid_modified", clear
global controls gender semester rational stratum related_degree		

/*Generate treatment variable (if political ideology matches or not)*/

tab polchoic treatment

/*
      Duque vs |            Treatment
         López |   Neutral      Duque      López |     Total
---------------+---------------------------------+----------
    Iván Duque |        20         17         11 |        48 
 Claudia López |       105        107        114 |       326 
---------------+---------------------------------+----------
         Total |       125        124        125 |       374 
*/

gen ac_treatment=treatment if treatment==0 /*Neutral treatment*/
tab ac_treatment treatment

replace ac_treatment=1 if (treatment==1 & polchoic==0) | (treatment==2 & polchoic==1) /*Ideology matches*/

replace ac_treatment=2 if ac_treatment==. /*Ideology does not match*/

label var ac_treatment "Ideology treatment"
label define ac_treatment 0"Neutral" 1"= ideology" 2"≠ ideology"
label values ac_treatment ac_treatment 

br treatment ac_treatment polchoic

tab ac_treatment 
/*

   Ideology |
  treatment |      Freq.     Percent        Cum.
------------+-----------------------------------
    Neutral |        125       33.42       33.42
 = ideology |        131       35.03       68.45
 ≠ ideology |        118       31.55      100.00
------------+-----------------------------------
      Total |        374      100.00

*/

save "$modified/Covid_modified", replace

	          *--------------------------------------*
		      *   Participants by role and treatment *
			  *--------------------------------------*

	*All sessions*	
	
	tabout ac_treatment id_role if DF!=1 using "$tables/distribution_covid_ac.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-5) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)



		
	/********************************************************
	*                        GRAPHS                         *
	*********************************************************/
		
/*Sent amount*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/
	
	cap drop if DF==1

	/*Neutral*/
	reg group_sent_amount if ac_treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ac_treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology */
	reg group_sent_amount if ac_treatment == 1 & id_role==1
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ac_treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_sent_amount if ac_treatment == 2 & id_role==1
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ac_treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_sent_amount if id_role==1, over(ac_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Sent amount/HT_covid_ac.gph", replace
	graph export "$graphs/Sent amount/HT_covid_ac.png", replace
	graph export "$graphs/Sent amount/HT_covid_ac.pdf", replace

	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_sent_amount if ac_treatment==0 & id_role==1
	estimates store Neutral
	
	/*= ideology*/
	reg group_sent_amount if ac_treatment == 1 & id_role==1
	estimates store match
	
	/*Different ideology*/
	reg group_sent_amount if ac_treatment == 2 & id_role==1
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Sent amount/Coefplot_SA_Covid_ac.gph", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid_ac.png", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid_ac.pdf", replace		


/*Expected Sent amount*/
	/********************************************************
	*                 Hypothesis testing                   *
	*********************************************************/

	/*Neutral*/
	reg group_expectation_sent if ac_treatment==0 & id_role==2
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ac_treatment == 0 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology*/
	reg group_expectation_sent if ac_treatment == 1 & id_role==2
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ac_treatment == 1 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_expectation_sent if ac_treatment == 2 & id_role==2
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ac_treatment == 2 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_expectation_sent if id_role==2, over(ac_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Expected Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Expected/EXP_HT_covid_ac.gph", replace
	graph export "$graphs/Expected/Exp_HT_covid_ac.png", replace
	graph export "$graphs/Expected/Exp_HT_covid_ac.pdf", replace
	

	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_expectation_sent if ac_treatment==0 & id_role==2
	estimates store Neutral
	
	/*= ideology*/
	reg group_expectation_sent if ac_treatment == 1 & id_role==2
	estimates store match
	
	/*Different ideology*/
	reg group_expectation_sent if ac_treatment == 2 & id_role==2
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Expected Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Expected/Coefplot_Exp_Covid_ac.gph", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid_ac.png", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid_ac.pdf", replace

	
/*Equality*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg equality if ac_treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum equality if ac_treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology*/
	reg equality if ac_treatment == 1 & id_role==1
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum equality if ac_treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg equality if ac_treatment == 2 & id_role==1
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum equality if ac_treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) equality, over(ac_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Equality) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.37 33 "___________________") text(0.37 33 "`pDGTPG'", size(medsmall)) text(0.39 49 "_____________________________________") text(0.39 49 "`pDGTPGM'", size(medsmall)) text(0.37 67 "_________________") text(0.37 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.4) 
	graph save "$graphs/Equality/EHT_covid_ac.gph", replace
	graph export "$graphs/Equality/EHT_covid_ac.png", replace
	graph export "$graphs/Equality/EHT_covid_ac.pdf", replace		
	
	
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg equality if ac_treatment==0 & id_role==1
	estimates store Neutral
	
	/*Duque*/
	reg equality if ac_treatment == 1 & id_role==1
	estimates store match
	
	/*López*/
	reg equality if ac_treatment == 2 & id_role==1
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Equality") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.5, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Equality/Coefplot_E_Covid_ac.gph", replace
		graph export "$graphs/Equality/Coefplot_E_Covid_ac.png", replace
		graph export "$graphs/Equality/Coefplot_E_Covid_ac.pdf", replace
	

/*Message*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg group_message if ac_treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if ac_treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology*/
	reg group_message if ac_treatment == 1 & id_role==1
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if ac_treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_message if ac_treatment == 2 & id_role==1
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_message if ac_treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_message if id_role==1, over(ac_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(% of sent messages) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.37 33 "___________________") text(0.37 33 "`pDGTPG'", size(medsmall)) text(0.39 49 "_____________________________________") text(0.39 49 "`pDGTPGM'", size(medsmall)) text(0.37 67 "_________________") text(0.37 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.4) 
	graph save "$graphs/Message/MessHT_covid_ac.gph", replace
	graph export "$graphs/Message/MessHT_covid_ac.png", replace
	graph export "$graphs/Message/MessHT_covid_ac.pdf", replace		
		
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_message if ac_treatment==0 & id_role==1
	estimates store Neutral
	
	/*= ideology*/
	reg group_message if ac_treatment == 1 & id_role==1
	estimates store match
	
	/*Different ideology*/
	reg group_message if ac_treatment == 2 & id_role==1
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("% of sent messages") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.5, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Message/Coefplot_Mess_Covid_ac.gph", replace
		graph export "$graphs/Message/Coefplot_Mess_Covid_ac.png", replace
		graph export "$graphs/Message/Coefplot_Mess_Covid_ac.pdf", replace
		
		
/*Expected Message*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/

	/*Neutral*/
	reg group_expectation_message if ac_treatment==0 & id_role==2
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if ac_treatment == 0 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology*/
	reg group_expectation_message if ac_treatment == 1 & id_role==2
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if ac_treatment == 1 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_expectation_message if ac_treatment == 2 & id_role==2
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_message if ac_treatment == 2 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_expectation_message if id_role==2, over(ac_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(% of Receivers that expect message) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(0.57 33 "___________________") text(0.57 33 "`pDGTPG'", size(medsmall)) text(0.59 49 "_____________________________________") text(0.59 49 "`pDGTPGM'", size(medsmall)) text(0.57 67 "_________________") text(0.57 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(0.1)0.6) 
	graph save "$graphs/Message/ExpMessHT_covid_ac.gph", replace
	graph export "$graphs/Message/ExpMessHT_covid_ac.png", replace
	graph export "$graphs/Message/ExpMessHT_covid_ac.pdf", replace		
		
	
	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_expectation_message if ac_treatment==0 & id_role==2
	estimates store Neutral
	
	/*= ideology*/
	reg group_expectation_message if ac_treatment == 1 & id_role==2
	estimates store match
	
	/*Different ideology*/
	reg group_expectation_message if ac_treatment == 2 & id_role==2
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("% of Receivers that expect message") xtitle("Treatment", size(medsmall)) ylabel(0(0.1)0.6, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Message/Coefplot_ExpMess_Covid_ac.gph", replace
		graph export "$graphs/Message/Coefplot_ExpMess_Covid_ac.png", replace
		graph export "$graphs/Message/Coefplot_ExpMess_Covid_ac.pdf", replace

		
/*Social norms*/	

	/*******************GRAPH (by id_role)*******************/	
	/*Receivers*/
	 reg Q_1d if id_role==2, r 
	 estimates store R
	 
	 reg Q_2d if id_role==2, r 
	 estimates store R2
	
	 reg Q_3d if id_role==2, r 
	 estimates store R3
	 
	 /*Third Parties*/
	 reg Q_4d if id_role==3, r 
	 estimates store TP
	 
	 reg Q_5d if id_role==3, r 
	 estimates store TP2
	 
	 reg Q_6d if id_role==3, r 
	 estimates store TP3
	 
	 		coefplot (R, bcolor(gs13) label(Receiver)) (TP, bcolor(gs9) label(Third Party)) (R2, bcolor(gs13) nokey) (TP2, bcolor(gs9) nokey) (R3, bcolor(gs13) nokey) (TP3, bcolor(gs9) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.08) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_Covid.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_Covid.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_Covid.pdf", replace	
		
		
	/*******************GRAPH RECEIVERS (by treatment)*******************/	
	 reg Q_1d if id_role==2 & ac_treatment==0, r 
	 estimates store RN
	 
	 reg Q_1d if id_role==2 & ac_treatment==1, r 
	 estimates store RD
	 
	 reg Q_1d if id_role==2  & ac_treatment==2, r 
	 estimates store RL
	 
	 reg Q_2d if id_role==2 & ac_treatment==0, r 
	 estimates store RN2
	 
	 reg Q_2d if id_role==2  & ac_treatment==1, r 
	 estimates store RD2
	 	 
	 reg Q_2d if id_role==2  & ac_treatment==2, r 
	 estimates store RL2
	 
	 reg Q_3d if id_role==2 & ac_treatment==0, r 
	 estimates store RN3
	 
	 reg Q_3d if id_role==2  & ac_treatment==1, r 
	 estimates store RD3
	 	 
	 reg Q_3d if id_role==2  & ac_treatment==2, r 
	 estimates store RL3
	 
	 	coefplot (RN, bcolor(gs13) label(Neutral)) (RD, bcolor(gs9) label(= ideology)) (RL, bcolor(gs5) label(≠ ideology)) (RN2, bcolor(gs13) nokey) (RD2, bcolor(gs9) nokey) (RL2, bcolor(gs5) nokey) (RN3, bcolor(gs13) nokey) (RD3, bcolor(gs9) nokey) (RL3, bcolor(gs5) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.06) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3" , labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_Receivers_Covid_ac.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_Receivers_Covid_ac.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_Receivers_Covid_ac.pdf", replace	 
	 	
		
	/*******************GRAPH RECEIVERS (by treatment)*******************/	
	 reg Q_4d if id_role==3 & ac_treatment==0, r 
	 estimates store TPN
	 
	 reg Q_4d if id_role==3 & ac_treatment==1, r 
	 estimates store TPD
	 
	 reg Q_4d if id_role==3  & ac_treatment==2, r 
	 estimates store TPL
	 
	 reg Q_5d if id_role==3 & ac_treatment==0, r 
	 estimates store TPN2
	 
	 reg Q_5d if id_role==3  & ac_treatment==1, r 
	 estimates store TPD2
	 	 
	 reg Q_5d if id_role==3  & ac_treatment==2, r 
	 estimates store TPL2
	 
	 reg Q_6d if id_role==3 & ac_treatment==0, r 
	 estimates store TPN3
	 
	 reg Q_6d if id_role==3  & ac_treatment==1, r 
	 estimates store TPD3
	 	 
	 reg Q_6d if id_role==3  & ac_treatment==2, r 
	 estimates store TPL3
	 
	 	coefplot (TPN, bcolor(gs13) label(Neutral)) (TPD, bcolor(gs9) label(= ideology)) (TPL, bcolor(gs5) label(≠ ideology)) (TPN2, bcolor(gs13) nokey) (TPD2, bcolor(gs9) nokey) (TPL2, bcolor(gs5) nokey) (TPN3, bcolor(gs13) nokey) (TPD3, bcolor(gs9) nokey) (TPL3, bcolor(gs5) nokey), /*bylabel(tratamiento)*/ title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.06) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Appropriateness") xtitle("", size(medsmall)) ylabel(0(0.2)1, glcolor(gs15) format(%9.0g)) xlabel(0.7"Q1" 1"Q2" 1.3"Q3" , labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Appropriateness/Appropriateness_TP_Covid_ac.gph", replace
		graph export "$graphs/Appropriateness/Appropriateness_TP_Covid_ac.png", replace
		graph export "$graphs/Appropriateness/Appropriateness_TP_Covid_ac.pdf", replace
		
		

		
		
		
		
/**************************************************************************************************** Regression Analysis *********************
***********************************************************************/
global controls gender semester rational stratum related_degree

cap drop t1 t2 t3 Neutral
tab ac_treatment, gen(t)
rename t1 Neutral 
label var Neutral "1 if treatment==Neutral"
rename t2 Match
label var Match "1 if treatment== = ideology"
rename t3 NM
label var NM "1 if treatment== ≠ ideology"

save "$modified/Covid_modified", replace


use "$modified/Covid_modified", clear
global controls gender semester rational stratum related_degree
/******************* Sent amount, Message, Equality ******************/	
reg group_sent_amount Match NM if id_role==1, r
reg group_message Match NM if id_role==1, r
reg equality Match NM if id_role==1, r		


*Define table content 
cd "$tables/Sent amount"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_sent_amount group_sent_amount group_message group_message equality equality {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Match NM if id_role==1, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match NM $controls if id_role==1, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match NM $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the amount sent, the probability of sending the message and the probability of an equal split"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Sent amount} & \multicolumn{2}{c}{Message sent} & \multicolumn{2}{c}{Equality} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "Sender_variables_covid_ac.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match NM _cons) ///
coeflabels(Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Sender_variables_covid_ac.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Sender_variables_covid_ac.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Senders} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})
		 
		 
/*************** Expected sent amount, Expected message **************/	
reg group_expectation_sent Match NM if id_role==2, r
reg group_expectation_message Match NM if id_role==2, r		


*Define table content 
cd "$tables/Expected"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_expectation_sent group_expectation_sent group_expectation_message group_expectation_message {
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' Match NM if id_role==2, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match NM $controls if id_role==2, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match NM $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on Receivers expectations"
local titles "& \multicolumn{4}{c}{Dependent Variables} \\ \cmidrule{2-5} & \multicolumn{2}{c}{Expected sent amount} & \multicolumn{2}{c}{Expected message} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "Receiver_variables_covid_ac.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match NM _cons) ///
coeflabels(Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Receiver_variables_covid_ac.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Receiver_variables_covid_ac.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Receivers} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})			
		
		
/******************* Appropriateness (Receivers) *******************/	

reg Q_1d Match NM if id_role==2, r
reg Q_2d Match NM if id_role==2, r		
reg Q_3d Match NM if id_role==2, r	

*Define table content 
cd "$tables/Appropriateness"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist Q_1d Q_1d Q_2d Q_2d Q_3d Q_3d {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Match NM if id_role==2, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match NM $controls if id_role==2, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match NM $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the probability that Receivers will consider Senders' actions socially appropriate"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Q1. Unfairness} & \multicolumn{2}{c}{Q2. Sanctioning unfairness} & \multicolumn{2}{c}{Q3. Enforcing DBS} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "Receiver_app_covid_ac.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match NM _cons) ///
coeflabels(Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Receiver_app_covid_ac.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Receiver_app_covid_ac.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Receivers} sample. Neutral treatment is the baseline of analysis. Dependent variable is 1 if the player considers that a given action taken by the Sender is socially appropriate. Questions are: Q1 \textit{Is it socially appropriate if a Sender decides to transfer less than 50 points to a Receiver?}; Q2 \textit{Is it socially appropriate if a Third Party decides to charge deduction points to the Sender if this Sender sends less than 50 points to the Receiver?}; Q3 \textit{Is it socially appropriate if a Sender decides to send the message: \textit{"No te metas en lo que no te importa"} after being charged with deduction points}. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})
		
		
/*************** Appropriateness (Third Parties) ***************/	

reg Q_4d Match NM if id_role==3, r
reg Q_5d Match NM if id_role==3, r		
reg Q_6d Match NM if id_role==3, r	

*Define table content 
cd "$tables/Appropriateness"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist Q_4d Q_4d Q_5d Q_5d Q_6d Q_6d {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Match NM if id_role==3, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match NM $controls if id_role==3, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match NM $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the probability that Third Parties will consider Senders' actions socially appropriate"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Q1. Unfairness} & \multicolumn{2}{c}{Q2. Sanctioning unfairness} & \multicolumn{2}{c}{Q3. Enforcing DBS} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "TP_app_covid_ac.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match NM _cons) ///
coeflabels(Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "TP_app_covid_ac.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "TP_app_covid_ac.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Parties} sample. Neutral treatment is the baseline of analysis. Dependent variable is 1 if the player considers that a given action taken by the Sender is socially appropriate. Questions are: Q1 \textit{Is it socially appropriate if a Sender decides to transfer less than 50 points to a Receiver?}; Q2 \textit{Is it socially appropriate if a Third Party decides to charge deduction points to the Sender if this Sender sends less than 50 points to the Receiver?}; Q3 \textit{Is it socially appropriate if a Sender decides to send the message: \textit{"No te metas en lo que no te importa"} after being charged with deduction points}. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})		
		 
/*************************************************************************************************** Strategy Method ***************************
***********************************************************************/

use "$modified/Covid_modified", clear

//Acá lo que se hace es convertir la base de datos en formato long para que cada individuo (Id_code) de thirdparties y de receivers, posea 6 observaciones asociadas a las variables de expectations y deducciones. Eso daría = 6*374 = 2244. Entonces, como cada valor de deducciones es 0, 10, 20, 30, 40, 50 (6), estos se asignaron a la variable sent_cond. 


/*
--------+---------------------------------+
  id    | Sent condition  G_D_P   G_E_D_P |  
--------+---------------------------------+
    1   |       0          X         X    |       
    1   |      10          X         X    | 
	1   |      20          X         X    | 
    1   |      30          X         X    | 
	1   |      40          X         X    |
    1   |      50          X         X    |
--------+---------------------------------+   
//Y así para cada id       
*/

//El valor de group deduction que se le asigna a un receiver corresponde al group deduction que tiene el third party involucrado en su partida, y lo mismo entre third party y receiver para el expected deduction points 


br
reshape long group_deduction_ group_expectation_deduction_ , i(id_code) j(sent_cond)
br id_code group_deduction_ group_expectation_deduction_ sent_cond

label data "Covid data for strategy method"	
save "$modified/Strategy_method_covid", replace


use "$modified/Strategy_method_covid", clear
global controls gender semester rational stratum related_degree	
	
/*Generate dummies for each posible amount*/
tab sent_cond, gen(sent)

/*Rename to match sent amount*/
   forvalues i=1/6{
   	local cuenta = `i'- 1
   	rename sent`i' sent`cuenta' 
	label var sent`cuenta' "Sent = `cuenta'0"
   }
   
   label var sent0 "Sent = 0"
	
/*Generate interaction between match, no match and sent dummies */
	forvalues i=1/5{
		cap gen M_sent`i' = sent`i'*Match
		label var M_sent`i' "= ideology * Sent = `i'0"
		cap gen NM_sent`i' = sent`i'*NM
		label var NM_sent`i' "$\neq$ ideology * Sent = `i'0"
	}
	
/*Generate interaction between match, no match and sent  */
	gen M_sent = sent_cond*Match
	label var M_sent "= ideology * Sent"
	gen NM_sent = sent_cond*NM
	label var NM_sent "$\neq$ ideology * Sent"

save "$modified/Strategy_method_covid", replace
	

/***************************************************************************************************** Dummies ***********************************
************************************************************************/

use "$modified/Strategy_method_covid", clear

*Define table content 
cd "$tables/Strategy method"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_deduction_ group_deduction_ group_expectation_deduction_ group_expectation_deduction_ {
	
	if regexm("`var'","exp") == 1 {
		local i = 2
	} 
	else {
		local i = 3
	}
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* if id_role==`i', vce(cluster id_code) 
			
test (M_sent1=0) (M_sent2=0) (M_sent3=0) (M_sent4=0) (M_sent5=0)
estadd scalar pvalue=`r(p)': regs`c'

test (Match + M_sent1=0) (Match + M_sent2=0) (Match + M_sent3=0) (Match + M_sent4=0) (Match + M_sent5=0)
estadd scalar pvalue2=`r(p)': regs`c'

test (NM_sent1=0) (NM_sent2=0) (NM_sent3=0) (NM_sent4=0) (NM_sent5=0)
estadd scalar pvalue3=`r(p)': regs`c'

test (NM + NM_sent1=0) (NM + NM_sent2=0) (NM + NM_sent3=0) (NM + NM_sent4=0) (NM + NM_sent5=0)
estadd scalar pvalue4=`r(p)': regs`c'
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* $controls if id_role==`i', vce(cluster id_code) 

test (M_sent1=0) (M_sent2=0) (M_sent3=0) (M_sent4=0) (M_sent5=0)
estadd scalar pvalue=`r(p)': regs`c'

test (Match + M_sent1=0) (Match + M_sent2=0) (Match + M_sent3=0) (Match + M_sent4=0) (Match + M_sent5=0)
estadd scalar pvalue2=`r(p)': regs`c'

test (NM_sent1=0) (NM_sent2=0) (NM_sent3=0) (NM_sent4=0) (NM_sent5=0)
estadd scalar pvalue3=`r(p)': regs`c'

test (NM + NM_sent1=0) (NM + NM_sent2=0) (NM + NM_sent3=0) (NM + NM_sent4=0) (NM + NM_sent5=0)
estadd scalar pvalue4=`r(p)': regs`c'
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}
			
*Keep coefficients
local coefs _cons sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of the deduction points and expected deduction points at any given sent amount using Strategy Method"
local titles "& \multicolumn{2}{c}{Deduction Points} & \multicolumn{2}{c}{Expected Deduction Points}   \\"
local numbers " \cmidrule(l){2-3} \cmidrule(l){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "reg_strategy_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* _cons) ///
coeflabels(Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(table2_strategy)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "reg_strategy_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N part, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations" "Participants")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1' using "reg_strategy_covid.tex", 			///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(pvalue pvalue2 pvalue3 pvalue4,  fmt(3 3) ///
labels("p-value H0: = ideology * Sent $ ^{*} $=0" "p-value H0: = ideology + = ideology * Sent $ ^{*} $=0" "p-value H0: $\neq$ ideology * Sent $ ^{*} $=0" "p-value H0: $\neq$ ideology + $\neq$ ideology * Sent $ ^{*} $=0")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
	     \item Notes: Robust standard errors clustered at the individual level in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Party} sample in columns (1) and (2), and for the \textbf{Receivers} sample in columns (3) and (4). \textbf{Neutral} treatment is the baseline of the analysis. Dependent variable is the amount of points a Third Party would deduct in columns (1) and (2), and the amount of points a Receivers expects a Third Party would deduct in columns (3) and (4). Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})	  
			
	
/*************************************************************************************************** Continuous **********************************
************************************************************************/

*Define table content 
cd "$tables/Strategy method"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_deduction_ group_deduction_ group_expectation_deduction_ group_expectation_deduction_ {
	
	if regexm("`var'","exp") == 1 {
		local i = 2
	} 
	else {
		local i = 3
	}
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' sent_cond Match M_sent NM NM_sent if id_role==`i', vce(cluster id_code) 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' sent_cond Match M_sent NM NM_sent $controls if id_role==`i', vce(cluster id_code) 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}
			
*Keep coefficients
local coefs _cons sent_cond Match M_sent NM NM_sent $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of the deduction points and expected deduction points at any given sent amount using Strategy Method"
local titles "& \multicolumn{2}{c}{Deduction Points} & \multicolumn{2}{c}{Expected Deduction Points}   \\"
local numbers " \cmidrule(l){2-3} \cmidrule(l){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "reg_strategy_c_covid.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(sent_cond Match M_sent NM NM_sent _cons) ///
coeflabels(sent_cond "Sent" Match "= ideology" NM "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(table2_strategy)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "reg_strategy_c_covid.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N part, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations" "Participants")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
	     \item Notes: Robust standard errors clustered at the individual level in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Party} sample in columns (1) and (2), and for the \textbf{Receivers} sample in columns (3) and (4). \textbf{Neutral} treatment is the baseline of the analysis. Dependent variable is the amount of points a Third Party would deduct in columns (1) and (2), and the amount of points a Receivers expects a Third Party would deduct in columns (3) and (4). Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})		
	
	
********************************************************************************************************* GRAPH **********************************
*************************************************************************

/*Deduction points*/
forvalues i=0/5{
	
	reg group_deduction_ if sent`i' & ac_treatment==0 & id_role==3, vce(cluster id_code)
	estimates store N`i'
	
	reg group_deduction_ if sent`i' & ac_treatment==1 & id_role==3, vce(cluster id_code)
	estimates store M`i'
	
	reg group_deduction_ if sent`i' & ac_treatment==2 & id_role==3, vce(cluster id_code)
	estimates store NM`i'	
	
}
	
		coefplot (N0, bcolor(gs9) label(Neutral)) (M0, bcolor(gs5) label(= ideology)) (NM0, bcolor(gs3) label(≠ ideology)) (N1, bcolor(gs9) nokey) (M1, bcolor(gs5) nokey) (NM1, bcolor(gs3) nokey) (N2, bcolor(gs9) nokey) (M2, bcolor(gs5) nokey) (NM2, bcolor(gs3) nokey) (N3, bcolor(gs9) nokey) (M3, bcolor(gs5) nokey) (NM3, bcolor(gs3) nokey) (N4, bcolor(gs9) nokey) (M4, bcolor(gs5) nokey) (NM4, bcolor(gs3) nokey) (N5, bcolor(gs9) nokey) (M5, bcolor(gs5) nokey) (NM5, bcolor(gs3) nokey), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.045) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Potential Deduction") xtitle("Sent Amount", size(medsmall)) ylabel(0(4)26, glcolor(gs15) format(%9.0g)) xlabel(0.61"0" 0.765"10" 0.92"20" 1.08"30" 1.24"40" 1.395"50", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Strategy method/SM_covid.gph", replace
		graph export "$graphs/Strategy method/SM_covid.png", replace
		graph export "$graphs/Strategy method/SM_covid.pdf", replace	

		
/*Expected deduction points*/
forvalues i=0/5{
	
	reg group_expectation_deduction_ if sent`i' & ac_treatment==0 & id_role==2, vce(cluster id_code)
	estimates store EN`i'
	
	reg group_expectation_deduction_ if sent`i' & ac_treatment==1 & id_role==2, vce(cluster id_code)
	estimates store EM`i'
	
	reg group_expectation_deduction_ if sent`i' & ac_treatment==2 & id_role==2, vce(cluster id_code)
	estimates store ENM`i'	
	
}
	
		coefplot (EN0, bcolor(gs9) label(Neutral)) (EM0, bcolor(gs5) label(= ideology)) (ENM0, bcolor(gs3) label(≠ ideology)) (EN1, bcolor(gs9) nokey) (EM1, bcolor(gs5) nokey) (ENM1, bcolor(gs3) nokey) (EN2, bcolor(gs9) nokey) (EM2, bcolor(gs5) nokey) (ENM2, bcolor(gs3) nokey) (EN3, bcolor(gs9) nokey) (EM3, bcolor(gs5) nokey) (ENM3, bcolor(gs3) nokey) (EN4, bcolor(gs9) nokey) (EM4, bcolor(gs5) nokey) (ENM4, bcolor(gs3) nokey) (EN5, bcolor(gs9) nokey) (EM5, bcolor(gs5) nokey) (ENM5, bcolor(gs3) nokey), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.045) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Expected Deduction") xtitle("Sent Amount", size(medsmall)) ylabel(0(4)26, glcolor(gs15) format(%9.0g)) xlabel(0.61"0" 0.765"10" 0.92"20" 1.08"30" 1.24"40" 1.395"50", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Strategy method/SM_exp_covid.gph", replace
		graph export "$graphs/Strategy method/SM_exp_covid.png", replace
		graph export "$graphs/Strategy method/SM_exp_covid.pdf", replace	
	

/***********************************************************************
***************** Analysis with different specification ****************
***********************************************************************/

use "$modified/Covid_modified", clear
global controls gender semester rational stratum related_degree		
		

	tabout polchoic treatment if DF!=1 using "$tables/distribution_ac_covid.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-5) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)
	

tab polcompas polchoic	
/*

  Espectro |
  político |
         1 |
(izquierda |
       ) 5 |    Duque vs López
 (derecha) | Iván Duqu  Claudia L |     Total
-----------+----------------------+----------
         1 |         1         16 |        17 
         2 |         2        101 |       103 
         3 |        17        166 |       183 
         4 |        22         40 |        62 
         5 |         6          3 |         9 
-----------+----------------------+----------
     Total |        48        326 |       374 


*/

/*Generate Duque vs López variable according to political spectrum (polcompas)*/
cap drop polcompas_vs	
gen polcompas_vs=1 if polcompas==1 | polcompas==2 | (polcompas==3 & polchoic==1)
replace polcompas_vs=0 if polcompas_vs==.	
label variable polcompas_vs "Duque vs López according to political spectrum"
label define vs 0"Iván Duque" 1"Claudia López"
label values polcompas_vs vs

tab polcompas polcompas_vs
/*
  Espectro |
  político |
         1 |    Duque vs López
(izquierda |     according to
       ) 5 |  political spectrum
 (derecha) | Iván Duqu  Claudia L |     Total
-----------+----------------------+----------
         1 |         0         17 |        17 
         2 |         0        103 |       103 
         3 |        17        166 |       183 
         4 |        62          0 |        62 
         5 |         9          0 |         9 
-----------+----------------------+----------
     Total |        88        286 |       374 
*/

	preserve
	label var polcompas "Political Spectrum"
	tabout polcompas polchoic if DF!=1 using "$tables/distribution_diffesp_covid.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-4) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)
	restore
	

tab polcompas_vs	

/*Generate treatment based on political spectrum*/

tab treatment
tab treatment, nolabel

gen ps_treatment=treatment if treatment==0 /*Neutral treatment*/
tab ps_treatment

replace ps_treatment=1 if (treatment==1 & polcompas_vs==0) | (treatment==2 & polcompas_vs==1) /*Ideology match*/
	
replace ps_treatment=2 if ps_treatment==. /*Ideology does not match*/

tab ps_treatment id_role

label var ps_treatment "Ideology Treatment according to political spectrum"
label values ps_treatment ac_treatment 

	preserve
	label var ps_treatment "Ideology Treatment"
	tabout ps_treatment id_role if DF!=1 using "$tables/distribution_diffesp_covid2.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-5) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)
	restore
	
tab polcompas_vs ps_treatment
	
	preserve
	label var polcompas_vs "Duque vs López"
	label var ps_treatment "Ideology Treatment"
	tabout polcompas_vs ps_treatment if DF!=1 using "$tables/distribution_diffesp_covid3.tex", ///
	cells(freq)  format(0 1) clab(_ _ _) ///
	replace ///
	style(tex) bt font(bold) cl1(2-5) ///
	topf("$tables\top.tex") botf("$tables\bot.tex") topstr(14cm)
	restore
	
	

	/********************************************************
	*                        GRAPHS                         *
	*********************************************************/
		
/*Sent amount*/
	/********************************************************
	*                 Hypothesis testings                   *
	*********************************************************/
	
	cap drop if DF==1

	/*Neutral*/
	reg group_sent_amount if ps_treatment==0 & id_role==1
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ps_treatment == 0 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology */
	reg group_sent_amount if ps_treatment == 1 & id_role==1
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ps_treatment == 1 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_sent_amount if ps_treatment == 2 & id_role==1
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_sent_amount if ps_treatment == 2 & id_role==1
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_sent_amount if id_role==1, over(ps_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Sent amount/HT_covid_ps.gph", replace
	graph export "$graphs/Sent amount/HT_covid_ps.png", replace
	graph export "$graphs/Sent amount/HT_covid_ps.pdf", replace

	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_sent_amount if ps_treatment==0 & id_role==1
	estimates store Neutral
	
	/*= ideology*/
	reg group_sent_amount if ps_treatment == 1 & id_role==1
	estimates store match
	
	/*Different ideology*/
	reg group_sent_amount if ps_treatment == 2 & id_role==1
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ac_treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Sent amount/Coefplot_SA_Covid_ps.gph", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid_ps.png", replace
		graph export "$graphs/Sent amount/Coefplot_SA_Covid_ps.pdf", replace		


		
/*Expected Sent amount*/
	/********************************************************
	*                 Hypothesis testing                   *
	*********************************************************/

	/*Neutral*/
	reg group_expectation_sent if ps_treatment==0 & id_role==2
	estimates store Neu
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ps_treatment == 0 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/	
	
	/*= ideology*/
	reg group_expectation_sent if ps_treatment == 1 & id_role==2
	estimates store match
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ps_treatment == 1 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Different ideology*/
	reg group_expectation_sent if ps_treatment == 2 & id_role==2
	estimates store nm
	/*Confidence Interval is very similar to the regression result*/
	sum group_expectation_sent if ps_treatment == 2 & id_role==2
	display r(mean) - 1.96*(r(sd)/sqrt(r(N))) /**/
	display r(mean) + 1.96*(r(sd)/sqrt(r(N))) /**/
	
	/*Saving the estimations for hypothesis testing */
	suest Neu match nm, robust /*robust errors at the participant level.*/
	
	
	/*Hypothesis testing*/
	test [Neu_mean]_cons=[match_mean]_cons
	local pDGTPG= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPG'" /*significant difference at the 10%*/
	
	test [Neu_mean]_cons=[nm_mean]_cons
	local pDGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*"))) 
	di "`pDGTPGM'" /*Non significant difference*/
	
	test [match_mean]_cons=[nm_mean]_cons
	local pTPGTPGM= cond(r(p)<0.01, "***", cond(r(p)>0.1, "n.s.",cond(r(p)<0.05,"**","*")))
	di "`pTPGTPGM'" /*Significant difference at the 10%*/
	
		* Graph bar across rounds*
	
	graph bar (mean) group_expectation_sent if id_role==2, over(ps_treatment) bargap(30) title("", size(medlarge) color(black)) graphregion (fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) plotregion(fcolor(white) lcolor(none) ifcolor(white) ilcolor(none)) ytitle(Expected Sent Amount) title(, size(medlarge) color(black)) ylabel(, glcolor(gs15)) asyvars blabel(bar, size(small) format(%9.3g)) bar(1,color(gs13)) bar(2,color(gs9)) bar(3,color(gs5)) text(35 33 "___________________") text(35 33 "`pDGTPG'", size(medsmall)) text(37 49 "_____________________________________") text(37 49 "`pDGTPGM'", size(medsmall)) text(35 67 "_________________") text(35 67 "`pTPGTPGM'", size(medsmall)) legend(cols(3) margin(tiny) region(lcolor(none))) yscale(range(0 0.18)) yla(0(5)35) 
	graph save "$graphs/Expected/EXP_HT_covid_ps.gph", replace
	graph export "$graphs/Expected/Exp_HT_covid_ps.png", replace
	graph export "$graphs/Expected/Exp_HT_covid_ps.pdf", replace
	

	/********************************************************
	*                       Coefplot                        *
	*********************************************************/
	
	/*Neutral*/
	reg group_expectation_sent if ps_treatment==0 & id_role==2
	estimates store Neutral
	
	/*= ideology*/
	reg group_expectation_sent if ps_treatment == 1 & id_role==2
	estimates store match
	
	/*Different ideology*/
	reg group_expectation_sent if ps_treatment == 2 & id_role==2
	estimates store nm
		
	
	* Coefplot 
	coefplot (Neutral, bcolor(gs13)) (match, bcolor(gs9) label(= ideology)) (nm, bcolor(gs5) label(≠ ideology)), bylabel(ps_treatment) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.1) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Expected Sent Amount") xtitle("Treatment", size(medsmall)) ylabel(0(5)35, glcolor(gs15) format(%9.0g)) xlabel("", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Expected/Coefplot_Exp_Covid_ps.gph", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid_ps.png", replace
		graph export "$graphs/Expected/Coefplot_Exp_Covid_ps.pdf", replace

		
/**************************************************************************************************** Regression Analysis *********************
***********************************************************************/
global controls gender semester rational stratum related_degree

cap drop Neutral
tab ps_treatment, gen(t)
rename t1 Neutral 
label var Neutral "1 if treatment==Neutral"
rename t2 Match2
label var Match2 "1 if treatment== = ideology"
rename t3 NM2
label var NM2 "1 if treatment== ≠ ideology"

save "$modified/Covid_modified", replace


use "$modified/Covid_modified", clear
global controls gender semester rational stratum related_degree
/******************* Sent amount, Message, Equality ******************/	
reg group_sent_amount Match2 NM2 if id_role==1, r
reg group_message Match2 NM2 if id_role==1, r
reg equality Match2 NM2 if id_role==1, r		


*Define table content 
cd "$tables/Sent amount"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_sent_amount group_sent_amount group_message group_message equality equality {
		if `c'==1 | `c'==3 | `c'==5 {

			eststo regs`c': reg `var' Match2 NM2 if id_role==1, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match2==NM2
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match2 NM2 $controls if id_role==1, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match2==NM2
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match2 NM2 $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on the amount sent, the probability of sending the message and the probability of an equal split"
local titles "& \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7} & \multicolumn{2}{c}{Sent amount} & \multicolumn{2}{c}{Message sent} & \multicolumn{2}{c}{Equality} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} & (1) & (2) & (3) & (4) & (5) & (6)  \\ \midrule"
esttab `estimates1'  using "Sender_variables_covid_ps.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match2 NM2 _cons) ///
coeflabels(Match2 "= ideology" NM2 "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Sender_variables_covid_ps.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Sender_variables_covid_ps.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Senders} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})

/*************** Expected sent amount, Expected message **************/	
reg group_expectation_sent Match2 NM2 if id_role==2, r
reg group_expectation_message Match2 NM2 if id_role==2, r		


*Define table content 
cd "$tables/Expected"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_expectation_sent group_expectation_sent group_expectation_message group_expectation_message {
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' Match2 NM2 if id_role==2, r
			
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' Match2 NM2 $controls if id_role==2, r
		
estadd scalar effectD = (_b[Match]/_b[_cons])*100: regs`c'
estadd scalar effectL = (_b[NM]/_b[_cons])*100: regs`c'

test Match==NM
estadd scalar pvalue=`r(p)': regs`c' 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}

			

*Keep coefficients
local coefs _cons Match2 NM2 $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of treatments on Receivers expectations"
local titles "& \multicolumn{4}{c}{Dependent Variables} \\ \cmidrule{2-5} & \multicolumn{2}{c}{Expected sent amount} & \multicolumn{2}{c}{Expected message} \\"
local numbers " \cmidrule(lr){2-3} \cmidrule(lr){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "Receiver_variables_covid_ps.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(Match2 NM2 _cons) ///
coeflabels(Match2 "= ideology" NM2 "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(Sender_variables)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "Receiver_variables_covid_ps.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1'  using "Receiver_variables_covid_ps.tex",  ///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(effectD effectL pvalue,  fmt(1 1 3 0) ///
labels("Effect = ideology vs Neutral(\%)" "Effect $\neq$ ideology vs Neutral(\%)" "p-value H0: = ideology = $\neq$ ideology")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
         \item Notes: * p$<$0.1, ** p$<$0.05, *** p$<$0.01. Robust standard errores in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Receivers} sample. Neutral treatment is the baseline of analysis. Dependent variable is specified in columns. Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})	
		 
/*************************************************************************************************** Strategy Method ***************************
***********************************************************************/

use "$modified/Covid_modified", clear

br
reshape long group_deduction_ group_expectation_deduction_ , i(id_code) j(sent_cond)
br id_code group_deduction_ group_expectation_deduction_ sent_cond

label data "Covid data for strategy method"	
save "$modified/Strategy_method_covid", replace


use "$modified/Strategy_method_covid", clear
global controls gender semester rational stratum related_degree	
	
/*Generate dummies for each posible amount*/
tab sent_cond, gen(sent)

/*Rename to match sent amount*/
   forvalues i=1/6{
   	local cuenta = `i'- 1
   	rename sent`i' sent`cuenta' 
	label var sent`cuenta' "Sent = `cuenta'0"
   }
   
   label var sent0 "Sent = 0"
	
/*Generate interaction between match, no match and sent dummies */
	forvalues i=1/5{
		cap gen M_sent`i' = sent`i'*Match
		label var M_sent`i' "= ideology * Sent = `i'0"
		cap gen NM_sent`i' = sent`i'*NM
		label var NM_sent`i' "$\neq$ ideology * Sent = `i'0"
	}
	
	forvalues i=1/5{
		cap gen M2_sent`i' = sent`i'*Match2
		label var M2_sent`i' "= ideology * Sent = `i'0"
		cap gen NM2_sent`i' = sent`i'*NM2
		label var NM2_sent`i' "$\neq$ ideology * Sent = `i'0"
	}
	
/*Generate interaction between match, no match and sent  */
	gen M_sent = sent_cond*Match
	label var M_sent "= ideology * Sent"
	gen NM_sent = sent_cond*NM
	label var NM_sent "$\neq$ ideology * Sent"

	gen M2_sent = sent_cond*Match2
	label var M2_sent "= ideology * Sent"
	gen NM2_sent = sent_cond*NM2
	label var NM2_sent "$\neq$ ideology * Sent"

save "$modified/Strategy_method_covid", replace
	

/***************************************************************************************************** Dummies ***********************************
************************************************************************/

use "$modified/Strategy_method_covid", clear

*Define table content 
cd "$tables/Strategy method"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_deduction_ group_deduction_ group_expectation_deduction_ group_expectation_deduction_ {
	
	if regexm("`var'","exp") == 1 {
		local i = 2
	} 
	else {
		local i = 3
	}
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' sent1 sent2 sent3 sent4 sent5 Match2 M2_sent* NM2 NM2_sent* if id_role==`i', vce(cluster id_code) 
			
test (M2_sent1=0) (M2_sent2=0) (M2_sent3=0) (M2_sent4=0) (M2_sent5=0)
estadd scalar pvalue=`r(p)': regs`c'

test (Match2 + M2_sent1=0) (Match2 + M2_sent2=0) (Match2 + M2_sent3=0) (Match2 + M2_sent4=0) (Match2 + M2_sent5=0)
estadd scalar pvalue2=`r(p)': regs`c'

test (NM2_sent1=0) (NM2_sent2=0) (NM2_sent3=0) (NM2_sent4=0) (NM2_sent5=0)
estadd scalar pvalue3=`r(p)': regs`c'

test (NM2 + NM2_sent1=0) (NM2 + NM2_sent2=0) (NM2 + NM2_sent3=0) (NM2 + NM2_sent4=0) (NM2 + NM2_sent5=0)
estadd scalar pvalue4=`r(p)': regs`c'
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' sent1 sent2 sent3 sent4 sent5 Match2 M2_sent* NM2 NM2_sent* $controls if id_role==`i', vce(cluster id_code) 
			
test (M2_sent1=0) (M2_sent2=0) (M2_sent3=0) (M2_sent4=0) (M2_sent5=0)
estadd scalar pvalue=`r(p)': regs`c'

test (Match2 + M2_sent1=0) (Match2 + M2_sent2=0) (Match2 + M2_sent3=0) (Match2 + M2_sent4=0) (Match2 + M2_sent5=0)
estadd scalar pvalue2=`r(p)': regs`c'

test (NM2_sent1=0) (NM2_sent2=0) (NM2_sent3=0) (NM2_sent4=0) (NM2_sent5=0)
estadd scalar pvalue3=`r(p)': regs`c'

test (NM2 + NM2_sent1=0) (NM2 + NM2_sent2=0) (NM2 + NM2_sent3=0) (NM2 + NM2_sent4=0) (NM2 + NM2_sent5=0)
estadd scalar pvalue4=`r(p)': regs`c'
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}
			
*Keep coefficients
local coefs _cons sent1 sent2 sent3 sent4 sent5 Match2 M2_sent* NM2 NM2_sent* $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of the deduction points and expected deduction points at any given sent amount using Strategy Method"
local titles "& \multicolumn{2}{c}{Deduction Points} & \multicolumn{2}{c}{Expected Deduction Points}   \\"
local numbers " \cmidrule(l){2-3} \cmidrule(l){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "reg_strategy_covid_ps.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(sent1 sent2 sent3 sent4 sent5 Match2 M2_sent* NM2 NM2_sent* _cons) ///
coeflabels(Match2 "= ideology" NM2 "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(table2_strategy)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "reg_strategy_covid_ps.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N part, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations" "Participants")) ///
collabels(none) nomtitles posthead(\midrule) ///

esttab `estimates1' using "reg_strategy_covid_ps.tex", 			///
append b(3) plain se bookt nodepvars nonumbers						 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(pvalue pvalue2 pvalue3 pvalue4,  fmt(3 3) ///
labels("p-value H0: = ideology * Sent $ ^{*} $=0" "p-value H0: = ideology + = ideology * Sent $ ^{*} $=0" "p-value H0: $\neq$ ideology * Sent $ ^{*} $=0" "p-value H0: $\neq$ ideology + $\neq$ ideology * Sent $ ^{*} $=0")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
	     \item Notes: Robust standard errors clustered at the individual level in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Party} sample in columns (1) and (2), and for the \textbf{Receivers} sample in columns (3) and (4). \textbf{Neutral} treatment is the baseline of the analysis. Dependent variable is the amount of points a Third Party would deduct in columns (1) and (2), and the amount of points a Receivers expects a Third Party would deduct in columns (3) and (4). Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})

/*************************************************************************************************** Continuous **********************************
************************************************************************/

*Define table content 
cd "$tables/Strategy method"

*Number of table
local tab `i'
di "`tab'"

estimates clear
local c = 1
foreach var of varlist group_deduction_ group_deduction_ group_expectation_deduction_ group_expectation_deduction_ {
	
	if regexm("`var'","exp") == 1 {
		local i = 2
	} 
	else {
		local i = 3
	}
		if `c'==1 | `c'==3 {

			eststo regs`c': reg `var' sent_cond Match2 M2_sent NM2 NM2_sent if id_role==`i', vce(cluster id_code) 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "": regs`c'
local estimates`c' `estimates1' regs`c'
}
		else {
			eststo regs`c': reg `var' sent_cond Match2 M2_sent NM2 NM2_sent $controls if id_role==`i', vce(cluster id_code) 
			
qui sum `e(depvar)' if e(sample) 
local medi `r(mean)'

estadd scalar med=`medi' : regs`c' // Recuperamos la media de la variable dependiente.
qui unique id_code if e(sample) 
estadd scalar part = `r(unique)': regs`c' 
estadd local cont "$\checkmark$": regs`c'
local estimates`c' `estimates1' regs`c'
			
		}
	local c = `c'+1
}
			
*Keep coefficients
local coefs _cons sent_cond Match2 M2_sent NM2 NM2_sent $controls /*Run this code with the table */

*-------------------------------------------------------------------------------
*Tex table
*Tile of table
local title_tab "Linear estimation of the deduction points and expected deduction points at any given sent amount using Strategy Method"
local titles "& \multicolumn{2}{c}{Deduction Points} & \multicolumn{2}{c}{Expected Deduction Points}   \\"
local numbers " \cmidrule(l){2-3} \cmidrule(l){4-5} & (1) & (2) & (3) & (4)  \\ \midrule"
esttab `estimates1'  using "reg_strategy_c_covid_ps.tex", 					     ///
replace b(3) lines se bookt nodepvars 							 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) keep(sent_cond Match2 M2_sent NM2 NM2_sent _cons) ///
coeflabels(sent_cond "Sent" Match2 "= ideology" NM2 "$\neq$ ideology") ///
stats(cont , fmt(0)                ///
labels("Controls")) ///
collabels(none) nomtitles title(\label(table2_strategy)) substitute(\_ _) ///
nonumbers posthead( " `titles'  `numbers'  ")                       ///
prehead(\begin{table}[H]										 ///
		\centering 												 ///
		\scalebox{0.8}{ 											 ///
		\begin{threeparttable} 									 ///
	 \caption{`title_tab'} ///
		\begin{tabular}{lccccccc} 								 ///
		\toprule[0.5pt] \toprule[0.5pt])
		

esttab `estimates1'  using "reg_strategy_c_covid_ps.tex", 					     ///
append b(3) plain se bookt nodepvars nonumbers					 ///
star(* 0.10 ** 0.05 *** 0.01) fragment label                     ///
eqlabels(none) drop(`coefs')                          ///
stats(med r2 N part, fmt(3 3 0 0) ///
labels("Mean Dep. Variable" "R Squared" "Observations" "Participants")) ///
collabels(none) nomtitles posthead(\midrule) ///
postfoot(\bottomrule[0.5pt]  									 ///
         \label{tab:table2} 								 ///
         \end{tabular} 											 ///
		 \vspace{-13pt} 										 ///
         \begin{tablenotes}[flushleft]{\setlength{\itemindent}{-3pt}} ///
         \small 												 ///
	     \item Notes: Robust standard errors clustered at the individual level in parentheses. Coefficients come from an ordinary least squares regression for the \textbf{Third Party} sample in columns (1) and (2), and for the \textbf{Receivers} sample in columns (3) and (4). \textbf{Neutral} treatment is the baseline of the analysis. Dependent variable is the amount of points a Third Party would deduct in columns (1) and (2), and the amount of points a Receivers expects a Third Party would deduct in columns (3) and (4). Controls include whether subject is female, Semester, socio-economic strata (from 1 to 6), whether studying an economics related major, and a rationality measure. ///
         \end{tablenotes} 										 ///
         \end{threeparttable} 									 /// 
         } 														 ///
         \end{table})
		 
		 
	
********************************************************************************************************* GRAPH **********************************
*************************************************************************

/*Deduction points*/
forvalues i=0/5{
	
	reg group_deduction_ if sent`i' & ps_treatment==0 & id_role==3, vce(cluster id_code)
	estimates store N`i'
	
	reg group_deduction_ if sent`i' & ps_treatment==1 & id_role==3, vce(cluster id_code)
	estimates store M`i'
	
	reg group_deduction_ if sent`i' & ps_treatment==2 & id_role==3, vce(cluster id_code)
	estimates store NM`i'	
	
}
	
		coefplot (N0, bcolor(gs9) label(Neutral)) (M0, bcolor(gs5) label(= ideology)) (NM0, bcolor(gs3) label(≠ ideology)) (N1, bcolor(gs9) nokey) (M1, bcolor(gs5) nokey) (NM1, bcolor(gs3) nokey) (N2, bcolor(gs9) nokey) (M2, bcolor(gs5) nokey) (NM2, bcolor(gs3) nokey) (N3, bcolor(gs9) nokey) (M3, bcolor(gs5) nokey) (NM3, bcolor(gs3) nokey) (N4, bcolor(gs9) nokey) (M4, bcolor(gs5) nokey) (NM4, bcolor(gs3) nokey) (N5, bcolor(gs9) nokey) (M5, bcolor(gs5) nokey) (NM5, bcolor(gs3) nokey), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.045) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Potential Deduction") xtitle("Sent Amount", size(medsmall)) ylabel(0(4)26, glcolor(gs15) format(%9.0g)) xlabel(0.61"0" 0.765"10" 0.92"20" 1.08"30" 1.24"40" 1.395"50", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Strategy method/SM_covid_ps.gph", replace
		graph export "$graphs/Strategy method/SM_covid_ps.png", replace
		graph export "$graphs/Strategy method/SM_covid_ps.pdf", replace	

		
/*Expected deduction points*/
forvalues i=0/5{
	
	reg group_expectation_deduction_ if sent`i' & ps_treatment==0 & id_role==2, vce(cluster id_code)
	estimates store EN`i'
	
	reg group_expectation_deduction_ if sent`i' & ps_treatment==1 & id_role==2, vce(cluster id_code)
	estimates store EM`i'
	
	reg group_expectation_deduction_ if sent`i' & ps_treatment==2 & id_role==2, vce(cluster id_code)
	estimates store ENM`i'	
	
}
	
		coefplot (EN0, bcolor(gs9) label(Neutral)) (EM0, bcolor(gs5) label(= ideology)) (ENM0, bcolor(gs3) label(≠ ideology)) (EN1, bcolor(gs9) nokey) (EM1, bcolor(gs5) nokey) (ENM1, bcolor(gs3) nokey) (EN2, bcolor(gs9) nokey) (EM2, bcolor(gs5) nokey) (ENM2, bcolor(gs3) nokey) (EN3, bcolor(gs9) nokey) (EM3, bcolor(gs5) nokey) (ENM3, bcolor(gs3) nokey) (EN4, bcolor(gs9) nokey) (EM4, bcolor(gs5) nokey) (ENM4, bcolor(gs3) nokey) (EN5, bcolor(gs9) nokey) (EM5, bcolor(gs5) nokey) (ENM5, bcolor(gs3) nokey), bylabel(tratamiento) title ("", color(black) size(medsmall)) vertical recast(bar) barwidth(0.045) bargap(30) /*fcolor(*.8)*/ lwidth(0) ciopts(recast(rcap) lwidth(*0.5) lcolor(black)) citop format(%9.2f) /*addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black))*/ /*msize(vsmall))*/ ytitle("Expected Deduction") xtitle("Sent Amount", size(medsmall)) ylabel(0(4)26, glcolor(gs15) format(%9.0g)) xlabel(0.61"0" 0.765"10" 0.92"20" 1.08"30" 1.24"40" 1.395"50", labsize(small) nogextend labc(black)) msymbol(O) msize(small) levels(95)  plotregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) graphregion(lcolor(none) fcolor(white) ifcolor(white) ilcolor(none)) yscale(range(0 0.18) lc(black)) xscale() legend(cols(3) margin(0) region(lcolor(none)))
		graph save "$graphs/Strategy method/SM_exp_covid_ps.gph", replace
		graph export "$graphs/Strategy method/SM_exp_covid_ps.png", replace
		graph export "$graphs/Strategy method/SM_exp_covid_ps.pdf", replace	
		
/*************************************************************************************************** Stratum Dummy **************************** 
***********************************************************************/
use "$modified/Covid_modified", clear

/*0 es estrato 1 y así. 6 es "No sabe/No responde"*/
tab stratum
/*
 Estrato de |
la vivienda |
      donde |
     reside |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |          6        1.60        1.60
          1 |         46       12.30       13.90
          2 |         96       25.67       39.57
          3 |        116       31.02       70.59
          4 |         66       17.65       88.24
          5 |         28        7.49       95.72
          6 |         16        4.28      100.00
------------+-----------------------------------
      Total |        374      100.00
*/
/*4.28% dijeron que no sabían*/

/*Stratum dummy */

cap gen stratum_d=1 if stratum==6
replace stratum_d=0 if stratum_d==.
tab stratum_d

save "$modified/Covid_modified", replace

/*
  stratum_d |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        358       95.72       95.72
          1 |         16        4.28      100.00
------------+-----------------------------------
      Total |        374      100.00
*/

		

global controls gender semester rational stratum stratum_d related_degree


/**************************************************************************************************** Regression Analysis *********************
***********************************************************************/
/*The results from the following regressions (including the stratum dummy for those that report not knowing their stratum) remain very similar in magnitude, sign, and significance to original regressions without the dummy*/

/******************* Sent amount, Message, Equality ******************/	
reg group_sent_amount Match NM $controls if id_role==1, r
reg group_message Match NM $controls if id_role==1, r
reg equality Match NM $controls if id_role==1, r			
		
/*************** Expected sent amount, Expected message **************/	
reg group_expectation_sent Match NM $controls if id_role==2, r
reg group_expectation_message Match NM $controls if id_role==2, r			
/******************* Appropriateness (Receivers) *******************/	
reg Q_1d Match NM $controls if id_role==2, r
reg Q_2d Match NM $controls if id_role==2, r		
reg Q_3d Match NM $controls if id_role==2, r	

/*************** Appropriateness (Third Parties) ***************/	
reg Q_4d Match NM $controls if id_role==3, r
reg Q_5d Match NM $controls if id_role==3, r		
reg Q_6d Match NM $controls if id_role==3, r	
		
/************************ Strategy Method ************************/	

/***************************************************************************************************** Dummies ***********************************
************************************************************************/

use "$modified/Strategy_method_covid", clear

cap gen stratum_d=1 if stratum==6
replace stratum_d=0 if stratum_d==.
tab stratum_d

save "$modified/Strategy_method_covid", replace

use "$modified/Strategy_method_covid", clear
global controls gender semester rational stratum stratum_d related_degree

reg group_deduction_ sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* $controls if id_role==3, vce(cluster id_code)
reg group_expectation_deduction_ sent1 sent2 sent3 sent4 sent5 Match M_sent* NM NM_sent* $controls if id_role==2, vce(cluster id_code)

	
/*************************************************************************************************** Continuous **********************************
************************************************************************/
reg group_deduction_ sent_cond Match M_sent NM NM_sent $controls if id_role==3, vce(cluster id_code)
reg group_expectation_deduction_ sent_cond Match M_sent NM NM_sent $controls if id_role==2, vce(cluster id_code)

		
		
		
		
		