*****************************************************************
***Make flags for each definition registry, nume, and denom   ***
*****************************************************************
*Authors: HW & LD 

global data "X:\QOF_project\Data\COPD\cohort\"
local working "X:\QOF_project\Data\COPD\working_data"

***********************************************

**************************************************************
*1. COPD_REG- 1=select, 0=next rule - ORIGINAL QOF DEFINTION
**************************************************************
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

recode COPD_REG .=0



*****************************************
*2. COPD010, 1=select/reject, 0=next 
****************************************

*denom rule 1 (1=select)
gen COPD010_RN1= COPDRVW_DAT>(PPED-365.25) & COPDEXACB_DAT>(PPED-365.25) & MRC_DAT>(PPED-365.25) & COPDRVW_DAT!=. & COPDEXACB_DAT!=. & MRC_DAT!=. 

*denom rule 2 (1=reject)
gen COPD010_RN2=1 if COPDPCAPU_DAT>(PPED-365.25) & COPDPCAPU_DAT!=.

*denom rule 3 (1=reject)
gen COPD010_RN3=1 if COPDPCADEC_DAT>(PPED-365.25) & COPDPCADEC_DAT!=.

*denom rule 4 (4=reject)
gen COPD010_RN4=1 if COPDINVITE1_DAT!=. & COPDINVITE2_DAT!=. 

*denom rule 5 (1=reject)
gen COPD010_RN5=1 if COPD_DAT>(PPED-(365.25/4)) & COPD_DAT!=.

*denom rule 6 (1=reject)
gen COPD010_RN6=1 if REG_DAT>(PPED-(365.25/4)) & REG_DAT!=.

forvalues i=1/6{
	recode COPD010_RN`i' .=0 
} 

*create indicator denominator 
gen COPD010_denom=1 if COPD_REG==1 & COPD010_RN1==1 
replace COPD010_denom=1 if COPD010_RN1==0 & COPD010_RN2==0 & COPD010_RN3==0 & COPD010_RN4==0 & COPD010_RN5==0 & COPD010_RN6==0 & COPD_REG==1


*create indicator numerator 
gen COPD010_num=1 if COPD010_denom==1 & COPDRVW_DAT>(PPED-365.25) & COPDEXACB_DAT>(PPED-365.25) & MRC_DAT>(PPED-365.25) & COPDRVW_DAT!=. & COPDEXACB_DAT!=. & MRC_DAT!=. 

recode COPD010_denom .=0
recode COPD010_num .=0

*************************************************
*3. COPD014, 1=select/reject/next, 0=next/reject
*************************************************

*denom rule 1 (1=next rule)
gen COPD014_RN1=1 if MRC1_DAT>(PPED-365.25) & MRC1_DAT!=.

*denom rule 2 (1=reject) 
gen COPD014_RN2=1 if PULRHBATT_DAT!=. 

*denom rule 3 (1=select)
gen COPD014_RN3=1 if PULRHBREF_DAT>(PPED-365.25) & PULRHBREF_DAT!=.

*denom rule 4 (2=reject)
gen COPD014_RN4=1 if PULRHBSU_DAT>(PPED-365.25) & PULRHBSU_DAT!=. 
replace COPD014_RN4=1 if COPDPCASU_DAT>(PPED-365.25) & COPDPCASU_DAT!=. 

*denom rule 5 (1=reject)
gen COPD014_RN5=1 if PULRHBPU_DAT>(PPED-365.25) & PULRHBPU_DAT!=.

*denom rule 6 (1=reject)
gen COPD014_RN6=1 if COPDPCAPU_DAT>(PPED-365.25) & COPDPCAPU_DAT!=.

*denom rule 7 (1=reject)
gen COPD014_RN7=1 if PULRHBDEC_DAT>(PPED-365.25) & PULRHBDEC_DAT!=.

*denom rule 8 (1=reject)
gen COPD014_RN8=1 if COPDPCADEC_DAT>(PPED-365.25) & COPDPCADEC_DAT!=.

*denom rule 9 (1=reject)
gen COPD014_RN9=1 if COPDINVITE1_DAT!=. & COPDINVITE2_DAT!=. 

*denom rule 10 (1=reject)
gen COPD014_RN10=1 if COPD_DAT>(PPED-(365.25/4)) & COPD_DAT!=.

*denom rule 11 (1=reject)
gen COPD014_RN11=1 if REG_DAT>(PPED-(365.25/4)) & REG_DAT!=.

forvalues i=1/11{
	recode COPD014_RN`i' .=0 
	} 
			
*create indicator denominator 
gen COPD014_denom=1 if COPD_REG==1 & COPD014_RN1==1 & COPD014_RN2==0 & COPD014_RN3==1 
replace COPD014_denom=1 if COPD_REG==1 & COPD014_RN1==1 & COPD014_RN2==0 & COPD014_RN3==0 &  COPD014_RN4==0 & COPD014_RN5==0 & COPD014_RN6==0 & COPD014_RN7==0 & COPD014_RN8==0 & COPD014_RN9==0 & COPD014_RN10==0 & COPD014_RN11==0


*create indicator numerator 
gen COPD014_num=1 if PULRHBREF_DAT>(PPED-365.25) & PULRHBREF_DAT!=. & COPD_REG==1 & COPD014_denom==1

recode COPD014_denom .=0
recode COPD014_num .=0


keep patid COPD_REG_RN1 - COPD014_num

export delimited `working'/1_cohort_COPD_with_indicators_Sankey, replace
































