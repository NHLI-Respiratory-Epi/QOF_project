*****************************************************************
***Compare 7 definitions of COPD codelists + process codelists***
*****************************************************************
*Authors: HW & LD 

global data "X:\QOF_project\Data\COPD\cohort\"
local working "X:\QOF_project\Data\COPD\working_data"

***********************************************


**********************************************
*1. COPD_REG- 1=select, 0=next rule - COMBO 1
**********************************************
use ${data}1_cohort_COPD_with_DAT_cols, clear

gen PPED=date("31/03/2024", "DMY")
format PPED %td 

*rule 1
gen COPD_REG_RN1=1 if EUNRESCOPD_DAT<QSSD & EUNRESCOPD_DAT!=. 

*rule 2
gen COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCDIAG_DAT!=.
replace COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=.

*rule 3
gen COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCREG_DAT!=.
replace COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186)  & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=.

*rule 4 
gen COPD_REG_RN4=1 if EUNRESCOPD_DAT>=QSSD  & EUNRESCOPD_DAT!=. 

forvalues i=1/4{
	recode COPD_REG_RN`i' .=0 
}

gen COPD_REG=1 if COPD_REG_RN1==1 | COPD_REG_RN2==1 | COPD_REG_RN3==1 | COPD_REG_RN4==1 


*numbers/proportion of patients in register
tab COPD_REG

keep if COPD_REG == 1
keep patid

gen COMBO1 = 1

save `working'/COMBO1_ptlist, replace

*****************************************************
*1. COPD_REG- 1=select, 0=next rule - COMBO 2 THRU 7
*****************************************************
forvalues c=2/7 {
	use ${data}1_cohort_COPD_with_DAT_cols_COMBO`c', clear

	gen PPED=date("31/03/2024", "DMY")
	format PPED %td 

	*rule 1
	gen COPD_REG_RN1=1 if EUNRESCOPD_DAT<QSSD & EUNRESCOPD_DAT!=. 

	*rule 2
	gen COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCDIAG_DAT!=.
	replace COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=.

	*rule 3
	gen COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCREG_DAT!=.
	replace COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186)  & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=.

	*rule 4 
	gen COPD_REG_RN4=1 if EUNRESCOPD_DAT>=QSSD  & EUNRESCOPD_DAT!=. 

	forvalues i=1/4{
		recode COPD_REG_RN`i' .=0 
	}

	gen COPD_REG=1 if COPD_REG_RN1==1 | COPD_REG_RN2==1 | COPD_REG_RN3==1 | COPD_REG_RN4==1 


	*numbers/proportion of patients in register
	tab COPD_REG

	keep if COPD_REG == 1
	keep patid

	gen COMBO`c' = 1

	save `working'/COMBO`c'_ptlist, replace
}

//See whicih patients are in original QOF, new combination, or both
use `working'/COMBO1_ptlist, clear
merge 1:1 patid using `working'/COMBO2_ptlist
drop _m
merge 1:1 patid using `working'/COMBO3_ptlist
drop _m
merge 1:1 patid using `working'/COMBO4_ptlist
drop _m
merge 1:1 patid using `working'/COMBO5_ptlist
drop _m
merge 1:1 patid using `working'/COMBO6_ptlist
drop _m
merge 1:1 patid using `working'/COMBO7_ptlist
drop _m

forvalues i=1/7{
	recode COMBO`i' .=0 
}

tab COMBO1 COMBO2 if COMBO1 == 1 | COMBO2 == 1
tab COMBO1 COMBO3 if COMBO1 == 1 | COMBO3 == 1
tab COMBO1 COMBO4 if COMBO1 == 1 | COMBO4 == 1
tab COMBO1 COMBO5 if COMBO1 == 1 | COMBO5 == 1
tab COMBO1 COMBO6 if COMBO1 == 1 | COMBO6 == 1
tab COMBO1 COMBO7 if COMBO1 == 1 | COMBO7 == 1

save `working'/all_COMBOS_ptlist, replace