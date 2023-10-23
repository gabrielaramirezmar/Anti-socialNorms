/****************************************************************************
*Master do file
*Created: 13/05/2022 by Paula Torres Higuera
*Last updated: 18/10/2013 by Gabriela Ramírez Martínez
*PURPOSE: set relevant paths to call data, save data and store results for NSS Covid project
*IMPORTANT: Run every time you work on the project


****************************************************************************/


clear all
set more off
cap log close
cls


*** User path: (change accordingly) [Here goes to the dropbox folder]
global user "C:\Users\gaby_\OneDrive\Documentos\RA - Jose Guerra\Anti_social norms covid"

*** Relevant paths:

*Codes:
global codes "$user/Codes/TPGGamelikeCovid2022"

*Data
global data "$user/Data"
global modified "$data/Modified/TPGGamelikeCovid"
global raw "$data/Raw/TPGGameLikeCovid"
global sessions "$modified/Sessions"



 
* Results - tables
global results "$user/Results/TPGGamelikeCovid"
global tables "$results/Tables"
global graphs "$results/Graphs"

global results2 "$user/Results/CovidAn_2023"
global tables2 "$results2/Tables"




