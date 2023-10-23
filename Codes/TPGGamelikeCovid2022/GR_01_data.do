/****************************************************************************
*
*Created: 13/05/2022
*Last updated: 13/05/2022 by Paula Torres Higuera
*PURPOSE: Append raw data and save as .dta
*DATASETS Used
	Raw: 
		- $raw/20211021_0800
		- $raw/20211021_1000
		- $raw/20211021_1200
		- $raw/20211025_0800
		- $raw/20211025_1400
		- $raw/20211026_1130
		- $raw/20211026_1400
		- $raw/20211027_1000
		- $raw/20211028_1000
		- $raw/20211102_1000
		- $raw/20211103_1000
		- $raw/20211103_1400
		- $raw/20211104_1000
		- $raw/20211104_1200
		- $raw/20211104_1400
	

		
	
*DATASETS Created:
		- $sessions/session`k'.dta with k E (1, 15)
		- $modified/NSS_covid
		- $modified/Covid_modified
	
	


****************************************************************************/
do "C:\Users\gaby_\OneDrive\Documentos\RA - Jose Guerra\Anti_social norms covid\Codes\TPGGamelikeCovid2022\GR_00_master.do"



/*Save excel files in .dta*/

local k = 1

local h = 0

//Run a loop to iterate over each base

foreach i in "$raw/20211021_0800" "$raw/20211021_1000" "$raw/20211021_1200" "$raw/20211025_0800" "$raw/20211025_1400" "$raw/20211026_1130" "$raw/20211026_1400" "$raw/20211027_1000" "$raw/20211028_1000" "$raw/20211102_1000" "$raw/20211103_1000" "$raw/20211103_1400" "$raw/20211104_1000" "$raw/20211104_1200" "$raw/20211104_1400" {
    import delimited using "`i'", clear
	
	cap replace third_party_tree1playeridcard1 = subinstr(third_party_tree1playeridcard1, "CC", "", .)
	
	cap replace third_party_tree1playeridcard1 = subinstr(third_party_tree1playeridcard1, "cc", "", .)
	
	destring third_party_tree1playeridcard1, replace
	
	drop if participantid_in_session==. /*Deletes observations from session 13 that for some reason only have missing values*/
	
	gen session =`k' //Assign the number of sessión for each id (the local changes for each base)
	
	gen id_code = _n //Generates the id code for each participant within the session
	
	replace id_code = id_code + `h' // Generates de id code of each participant in the aggregate databse (Creates an id code from 1 to N (number of participants in all sessions))
	
	sum id_code
	
	local h = r(max) //For example, if session 1 has 24 participants, the first id code for session 2 will be (h=24)+1 ...
	
	save "$sessions/session`k'.dta", replace
	
	
	
	
	local k = `k'+1

}


     *Append databases from sessions 2 - 15 to session 1

use "$sessions/session1.dta", clear

local b = 2 
  

forvalues i=`b'(1)15{
	append using "$sessions/session`i'", force
} 

//Drop vaariables with all missing values
drop collective_*
drop v207 v209 v211 v305 v306 v307 v371

//dropmiss, force // Drop variables that have all missing values

label data "NSS Covid raw data (all sessions)"
save "$modified/NSS_covid", replace


use "$modified/NSS_covid", clear /*381 observations. Según el documento de word son 378*/

/*Generate dummy for the 7 participants that didn't finish the experiment*/

gen DF=.
label var DF "1 if participant didn't finish the experiment"
replace DF=1 if participantlabel=="mbautistas" | participantlabel=="abarrera12" | participantlabel=="squirogar" | participantlabel=="mmendozap" | participantlabel=="svguayara" | participantlabel=="vpinzons" | participantlabel=="mbordah" 
replace DF=0 if DF==. 

/*Anonymize data set.*/
drop participantcode participantlabel third_party_tree1playername third_party_tree1playersignature third_party_tree1playernamep third_party_tree1playeremail v97 third_party_tree1playername_2 participant_index_in_pages participant_max_page_index participant_current_app_name participant_current_page_name participanttime_started_utc participantvisited participantpayoff sessionconfigname sessionconfigreal_world_currency third_party_tree1playerday third_party_tree1playermonth third_party_tree1playeryear third_party_tree1playerhour third_party_tree1playerminutes third_party_tree1playerampm third_party_tree1playeridcard third_party_tree1playercitycard third_party_tree1playerrandom_ro third_party_tree1playerdir* third_party_tree1playerround_pay third_party_tree1playerglobal_pa third_party_tree1playeridcard1 third_party_tree1playerexpday third_party_tree1playerexpmonth third_party_tree1playerexpyear third_party_tree1playerdepto third_party_tree1playercityp third_party_tree1playercountry1 third_party_tree1playertelephone third_party_tree1playerdayp third_party_tree1playeryearp third_party_tree1playeridcard2 third_party_tree1playercedula third_party_tree1playertypepayme third_party_tree1playerplatform third_party_tree1playerbank third_party_tree1playerid_bank third_party_tree1playeraccount_t third_party_tree1playeridcard3 third_party_tree1playerpayoff third_party_tree1playercity third_party_tree1playermonthp v102 third_party_tree1groupid_in_subs third_party_tree1subsessionround

/*Rename and label variables*/

   /*Treatment*/
   rename sessionconfigtratamiento treatment
   label var treatment "Treatment"
   recode treatment (3=0)
   label define treatment 0"Neutral" 1"Duque" 2"López" 
   label values treatment treatment

   /*Role*/
   rename third_party_tree1playerid_in_gro id_role
   label var id_role "Role"
   label define role 1"Sender" 2"Receiver" 3"Third Party"
   label values id_role role


   /*Control questions*/
   rename third_party_tree1playersurvey_10 control1
   label variable control1 "Pregunta de control 1"

   rename third_party_tree1playersurvey_11 control2
   label variable control2 "Pregunta de control 2"

   rename third_party_tree1playersurvey_12 control3
   label variable control3 "Pregunta de control 3"

   rename third_party_tree1playersurvey_14 control4
   label variable control4 "Pregunta de control 4"

   /*Demographics*/
   rename third_party_tree1playerq1 survey1
   label var survey1 "Edad"

   rename third_party_tree1playerq2 gender
   label var gender "Género"
   label define genero 0"Masculino" 1"Femenino"
   label values gender genero

   rename third_party_tree1playerq3 survey3
   label var survey3 "Número de hermanos"

   rename third_party_tree1playerq4 survey4
   label var survey4 "Carrera"

   rename third_party_tree1playerq5 survey5
   label var survey5 "Pregrado/Posgrado"
   label define pregrado 0"Pregrado" 1"Posgrado"
   label values survey5 pregrado

   rename third_party_tree1playerq6 semester
   label var semester "Semestre actual"

   rename third_party_tree1playerq7 survey7
   label var survey7 "Participado en experimentos antes"

   rename third_party_tree1playerq8 survey8
   label var survey8 "Ha donado dinero o ha sido voluntario"
   label define afirmacion 0"Sí" 1"No"
   label values survey7 survey8 afirmacion 

   rename third_party_tree1playerq9 rational
   label var rational "Guess 2/3 of the Average"

   rename third_party_tree1playerq10 survey10
   label var survey10 "Gastos semanales"

   rename third_party_tree1playerq11 survey11
   label var survey11 "Que tanto cumplen las reglas mis compañeros de clase 0 (Nada) y 10 (Mucho)"

   rename third_party_tree1playerq12 survey12
   label var survey12 "Que tanto cumplen las reglas los políticos 0 (Nada) y 10 (Mucho)"

   rename third_party_tree1playerq13 survey13
   label var survey13 "Que tanto cumplen las reglas los ciudadanos en general 0 (Nada) y 10 (Mucho)"

   rename third_party_tree1playerq14 survey14
   label var survey14 "Que tanto cumplen las reglas mis familiares 0 (Nada) y 10 (Mucho)"

   rename third_party_tree1playerq15 stratum
   label var stratum "Estrato de la vivienda donde reside" /*6 es no sabe/no responde*/

   rename third_party_tree1playeredu_m educ_m
   label var educ_m "Máximo nivel de educación alcanzado por la madre"

   rename third_party_tree1playeredu_f educ_p
   label var educ_p "Máximo nivel de educación alcanzado por el padre"
   label define educacion 0"Ninguno" 1"Primaria" 2"Secundaria/Bachillerato" 3"Educación universitaria/tecnológica" 4"Posgrado" 5"No sabe" 
   label values educ_m educ_p educacion

   rename third_party_tree1playersitu_f situ_financiera
   label var situ_financiera "Cómo definiría la situación financiera de su hogar (0 muy buena, 5 muy mala)"

    /*COVID*/
   rename third_party_tree1playercovvac covvac
   label var covvac "Se ha vacunado contra el coronavirus"
   label define covvac 1"Sí" 2"Parcialmente" 0"No"
   label values covvac covvac

   rename third_party_tree1playercovvacaft covvacaft
   label var covvacaft "Se vacunaría de estar disponible la vacuna para usted"
   label define covvacaft 1"Sí" 0"No"
   label values covvacaft covvacaft 

   rename third_party_tree1playercovinfc covinfc
   label var covinfc "Probabilidad contagio durante la pandemia"
   label define covinfc 1"Estoy seguro que no" 2"Es poco probable" 3"Es probable" 4"Estoy seguro"
   label values covinfc covinfc

   rename third_party_tree1playercovperfam covperfam
   label var covperfam "Porcentaje de amigos y familiares que se han contagiado de COVID-19"

   rename third_party_tree1playercovbef covbef
   label var covbef "Cómo vivía su hogar económicamente amtes del inicio de la pandemia"
   label define covbef 0"Mucho peor" 1"Algo peor" 2"Igual" 3"Algo mejor" 4"Mucho mejor"
   label values covbef covbef

   rename third_party_tree1playercovidfocu covidfocus
   label var covidfocus "Cómo se identifica con las estrategias sugeridas en el video"

/*Trust*/
   rename third_party_tree1playertrustbin trustbin
   label var trustbin "Confiar en la mayoría de las personas o nunca es lo suficiente cuidadoso en el trato con los demás"
   label define trustbin 0"Uno nunca es lo suficiente cuidadoso" 1"Se puede confiar en la mayoría"
   label values trustbin trustbin

   rename third_party_tree1playertrustknow trustknown
   label var trustknown "Cuánta confianza tiene en las personas que conoce"

   rename third_party_tree1playertrustnatg trustnatgov
   label var trustnatgov "Cuánta confianza tiene en el Gobierno Nacional"

   rename third_party_tree1playertrustlocg trustlocgov
   label var trustlocgov "Cuánta confianza tiene en el gobierno distrital de Bogotá"

   rename third_party_tree1playertrustint trustint
   label var trustint "Cuánta confianza tiene en organismos multilaterales"

   label define trust 1"Ninguna" 2"Poca" 3"Alta" 4"Mucha"
label values trustknown trustnatgov trustlocgov trustint trust


/*Trust in political parties*/
rename third_party_tree1playerpolcd polcd
label var polcd "Cuánta confianza tiene en el Centro Democrático"

rename third_party_tree1playerpolcr polcr
label var polcr "Cuánta confianza tiene en Cambio Radical"

rename third_party_tree1playerpolcon polcon
label var polcon "Cuánta confianza tiene en el Partido Conservador"

rename third_party_tree1playerpollib pollib
label var pollib "Cuánta confianza tiene en el Partido Liberal"

rename third_party_tree1playerpolver polver
label var polver "Cuánta confianza tiene en el Partido Verde"

rename third_party_tree1playerpolpol polpol
label var polpol "Cuánta confianza tiene en el Polo Democrático"

rename third_party_tree1playerpolcol polcol
label var polcol "Cuánta confianza tiene en Colombia Humana"

/*Trust in politicians*/
rename third_party_tree1playerpolduq polduq
label var polduq "Cuánta confianza tiene en Iván Duque"

rename third_party_tree1playerpoluri poluri
label var poluri "Cuánta confianza tiene en Álvaro Uribe"

rename third_party_tree1playerpolvar polvar
label var polvar "Cuánta confianza tiene en Germán Vargas Lleras"

rename third_party_tree1playerpolchar polchar
label var polchar "Cuánta confianza tiene en Alejandro Char"

rename third_party_tree1playerpolpas polpas
label var polpas "Cuánta confianza tiene en Andrés Pastrana"

rename third_party_tree1playerpolmar polmar
label var polmar "Cuánta confianza tiene en Marta Lucía Ramírez"

rename third_party_tree1playerpolgav polgav
label var polgav "Cuánta confianza tiene en César Gaviria"

rename third_party_tree1playerpolsan polsan
label var polsan "Cuánta confianza tiene en Juan Manuel Santos"

rename third_party_tree1playerpollop pollop
label var pollop "Cuánta confianza tiene en Claudia López"

rename third_party_tree1playerpolloz polloz
label var polloz "Cuánta confianza tiene en Angélica Lozano"

rename third_party_tree1playerpolpet polpet
label var polpet "Cuánta confianza tiene en Gustavo Petro"

rename third_party_tree1playerpolbol polbol
label var polbol "Cuánta confianza tiene en Gustavo Bolívar"

rename third_party_tree1playerpolcep polcep
label var polcep "Cuánta confianza tiene en Iván Cepeda"


label define trust2 1"Nada" 2"Poco" 3"Algo" 4"Mucho"
label values polcd polcr polcon pollib polver polpol polcol polduq poluri polvar polchar polpas polmar polgav polsan pollop polloz polpet polbol polcep trust2

/*Politics*/
rename third_party_tree1playerpolchoic polchoic
label var polchoic "Duque vs López"
label define choice 0"Iván Duque" 1"Claudia López"
label values polchoic choice

rename third_party_tree1playerpolcompas polcompas
label var polcompas "Espectro político 1 (izquierda) 5 (derecha)"

rename third_party_tree1playerpolimport polimport
label var polimport "Importancia de la política en su vida 1 (poco) 5 (mucho)"

rename third_party_tree1playerpolid polid
label var polid "Partido político con el que se identifica"
label define polid 1"Centro Democrático" 2"Cambio Radical" 3"Partido Conservador" 4"Partido Liberal" 5"Partido Verde" 6"Polo Democrático" 7"Colombia Humana" 8"Otro"
label values polid polid

rename third_party_tree1playerpolidoth polidoth
label var polidoth "Si es otro partido político, ¿Cuál?"

rename third_party_tree1playerinfor infor
label var infor "Medio por el cual se informa principalmente sobre actualidad nacional"

rename third_party_tree1playerinfoth infoth
label var infoth "Si respondió otra (infoth), ¿cuál?"

rename third_party_tree1playerpoli1 poli1
label var poli1 "Sistemas políticos: tener un/a líder fuerte que no tenga que preocuparse por el Congreso y las elecciones"

rename third_party_tree1playerpoli2 poli2
label var poli2 "Sistemas políticos: tener expertos/as, no solo gobiernos, que tomen decisiones de acuerdo con lo que consideren mejor para el país."

rename third_party_tree1playerpoli3 poli3
label var poli3 "Sistemas políticos: tener al ejército como gobierno"

rename third_party_tree1playerpoli4 poli4
label var poli4 "Sistemas políticos: tener un sistema político democrático"

rename third_party_tree1playerdemo1 demo1
label var demo1 "Características democracia: impuestos a ricos y subsidios a pobres"

rename third_party_tree1playerdemo2 demo2
label var demo2 "Características democracia: gente elige líderes en elecciones libres"

rename third_party_tree1playerdemo3 demo3
label var demo3 "Características democracia: ayuda estatal en caso de desempleo"

rename third_party_tree1playerdemo4 demo4
label var demo4 "Características democracia: ejército a cargo cuando gobierno es incompetente"

rename third_party_tree1playerdemo5 demo5
label var demo5 "Características democracia: derechos protegen personas de opresión estatal"

rename third_party_tree1playerdemo6 demo6
label var demo6 "Características democracia: gente obedece a sus gobernantes"


/*Justice and reciprocity*/
rename third_party_tree1playergoodcau goodcau
label var goodcau "Qué tan dispuesto está a dar sin esperar nada a cambio"

rename third_party_tree1playerfair1 fair1
label var fair1 "Justicia: amigo encuentra 100,000 y se los queda"

rename third_party_tree1playerfair2 fair2
label var fair2 "Justicia: amigo encuentra 100,000, se queda 51 y le da 49"

label define justicia 0"Injusto" 1"Injusto"
label values fair1 fair2 justicia 

rename third_party_tree1playerfav1 fav1
label var fav1 "Dispuesto a devolver un favor (0 'muy poco' 10 'muy dispuesto' )"

rename third_party_tree1playerreci1 reci1
label var reci1 "Siempre hay que ayudar a aquellas personas que nos ayudan"

rename third_party_tree1playerreci2 reci2
label var reci2 "El que me la hace me la paga"

label define reciprocidad 1"Totalmente en desacuerdo" 2"En desacuerdo" 3"De acuerdo" 4"Totalmente de acuerdo"
label values reci1 reci2 reciprocidad


/*Experiment*/

/*Deduction*/
rename third_party_tree1groupdeduction_ group_deduction_0 
label var group_deduction_0 "Deducción cuando se envían 0 puntos"

rename v178 group_deduction_10
label var group_deduction_10 "Deducción cuando se envían 10 puntos"

rename v179 group_deduction_20 
label var group_deduction_20 "Deducción cuando se envían 20 puntos"

rename v180 group_deduction_30 
label var group_deduction_30 "Deducción cuando se envían 30 puntos"

rename v181 group_deduction_40 
label var group_deduction_40 "Deducción cuando se envían 40 puntos"

rename v182 group_deduction_50
label var group_deduction_50 "Deducción cuando se envían 50 puntos"

rename third_party_tree1playerded_point ded_points 
label var ded_points "Puntos que realmente se dedujeron (los que se le muestran al Sender)"

/*Expectations*/
rename third_party_tree1groupexpectati group_expectation_sent
label var group_expectation_sent "Cuánto cree que mandarán la mayoría de Senders"

rename v184 group_expectation_deduction_0
label var group_expectation_deduction_0 "Cuánto cree que TP deducen si Sender envía 0"

rename v185 group_expectation_deduction_10
label var group_expectation_deduction_10 "Cuánto cree que TP deducen si Sender envía 10"

rename v186 group_expectation_deduction_20
label var group_expectation_deduction_20 "Cuánto cree que TP deducen si Sender envía 20"

rename v187 group_expectation_deduction_30
label var group_expectation_deduction_30 "Cuánto cree que TP deducen si Sender envía 30"

rename v188 group_expectation_deduction_40
label var group_expectation_deduction_40 "Cuánto cree que TP deducen si Sender envía 40"

rename v189 group_expectation_deduction_50
label var group_expectation_deduction_50 "Cuánto cree que TP deducen si Sender envía 50"

/*Message*/  
rename third_party_tree1groupmessage group_message
label var group_message "1 si envía mensaje"
	* oTree were not able to use missings values so we must store 2 as the 0 option of "no message"
	recode group_message (0=.)
	recode group_message (2=0)

rename v191 group_expectation_message
label var group_expectation_message "1 si Receiver espera que Sender envíe mensaje"
recode group_expectation_message (2=0)

/*Sent amount*/
rename third_party_tree1groupsent_amoun group_sent_amount
label var group_sent_amount "Cantidad que el Sender le envía al Receiver"

/*Social norms*/
rename v192 Q_1
label var Q_1 "(Opinión del Receiver) Sender envía menos de 50 puntos"

rename v193 Q_2 
label var Q_2 "(Opinión del Receiver) Third Party castiga si se envían menos de 50 puntos"

rename v194 Q_3
label var Q_3 "(Opinión del Receiver) Sender envía mensaje 'No te metas' si le deducen puntos"

rename v195 Q_4
label var Q_4 "(Opinión del Third Party) Sender envía menos de 50 puntos"

rename v196 Q_5
label var Q_5 "(Opinión del Third Party) Third Party castiga si se envían menos de 50 puntos"

rename v197 Q_6
label var Q_6 "(Opinión del Third Party) Sender envía mensaje 'No te metas' si le deducen puntos"

label define norms 0"Muy socialmente inapropiado" 1"Algo socialmente inapropiado" 2"Algo socialmente apropiado" 3"Muy socialmente apropiado"
label values Q_1 Q_2 Q_3 Q_4 Q_5 Q_6 norms

/*Equality*/

gen equality = 1 if group_sent_amount == 50
replace equality = 0 if group_sent_amount != 50
replace equality =.  if id_role != 1

label data "NSS Covid data for analysis"
save "$modified/Covid_modified", replace



