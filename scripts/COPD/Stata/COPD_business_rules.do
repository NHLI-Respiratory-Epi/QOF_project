*****************************
***COPD QOF Business Rules***
*****************************
*Authors: HW & LD 
*COPD register: COPD_REG
*Clinical indicators: COPD010, COPD014, COPD015 (don't need to do?), COPD009MI2, COPD009MI3

global data "X:\QOF_project\Data\COPD\cohort\"

***********************************************
*use one or the other for now: 
*use ${data}1_cohort_COPD_with_DAT_cols_COMBO2, clear
*use ${data}1_cohort_COPD_with_DAT_cols_COMBO3, clear 
*use ${data}1_cohort_COPD_with_DAT_cols_COMBO4, clear 
*use ${data}1_cohort_COPD_with_DAT_cols_COMBO5, clear 
*use ${data}1_cohort_COPD_with_DAT_cols_COMBO6, clear 
use ${data}1_cohort_COPD_with_DAT_cols_COMBO7, clear 

*use ${data}2_NEW_cohort_COPD_with_DAT_cols, clear 

*use ${data}3_NEW_cohort_COPD_with_DAT_cols_just_diagnosis_codes, clear 

******************************************
*1. COPD_REG- 1=select, 0=next rule 
******************************************
gen PPED=date("31/03/2024", "DMY")
format PPED %td 

*rule 1
gen COPD_REG_RN1=1 if EUNRESCOPD_DAT<QSSD & EUNRESCOPD_DAT!=. 

*rule 2
gen COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCDIAG_DAT!=.
replace COPD_REG_RN2=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=.

*rule 2 with alternative FEV1FVC variable 
gen COPD_REG_RN2b=1 if EUNRESCOPD_DAT>=QSSD & CONV_FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & CONV_FEV1FVCDIAG_DAT!=.
replace COPD_REG_RN2b=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=.

*rule 3
gen COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCREG_DAT!=.
replace COPD_REG_RN3=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186)  & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=.

*rule 3 with alternative FEV1FVC variable
gen COPD_REG_RN3b=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & CONV_FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & CONV_FEV1FVCREG_DAT!=.
replace COPD_REG_RN3b=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186)  & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=.


*rule 4 
gen COPD_REG_RN4=1 if EUNRESCOPD_DAT>=QSSD  & EUNRESCOPD_DAT!=. 

forvalues i=1/4{
	recode COPD_REG_RN`i' .=0 
}

gen COPD_REG=1 if COPD_REG_RN1==1 | COPD_REG_RN2==1 | COPD_REG_RN3==1 | COPD_REG_RN4==1 

gen COPD_REGb=1 if COPD_REG_RN1==1 | COPD_REG_RN2b==1 | COPD_REG_RN3b==1 | COPD_REG_RN4==1 


*numbers/proportion of patients in register
tab COPD_REG //  46,575= old, 48,255=new, 45,585= new diag only
tab COPD_REGb //  Gives the same COPD register number

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

codebook patid if COPD010_denom==1 // 38,440=old, 40,103=new, 37,939= new diag only

*create indicator numerator 
gen COPD010_num=1 if COPD010_denom==1 & COPDRVW_DAT>(PPED-365.25) & COPDEXACB_DAT>(PPED-365.25) & MRC_DAT>(PPED-365.25) & COPDRVW_DAT!=. & COPDEXACB_DAT!=. & MRC_DAT!=. 

codebook patid if COPD010_num==1 // 34,676=old, 35,044=new, 34,047= new diag only


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

codebook patid if COPD014_denom==1 // 3,985=old, 4,091=new, 3,938= new diag only

*create indicator numerator 
gen COPD014_num=1 if PULRHBREF_DAT>(PPED-365.25) & PULRHBREF_DAT!=. & COPD_REG==1 & COPD014_denom==1

codebook patid if COPD014_num==1 // 3,769=old, 3,803=new, 3707= new diag only



*************************************************
*4. COPD009MI2, 1=select, 0=next/reject
*************************************************

*rule 1 (1=select)
gen COPD009MI2_RN1=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCDIAG_DAT!=. 
replace COPD009MI2_RN1=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=. 

*rule 1 (1=select) with alernative FEV1FVC variable
gen COPD009MI2_RN1b=1 if EUNRESCOPD_DAT>=QSSD & CONV_FEV1FVCDIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & CONV_FEV1FVCDIAG_DAT!=. 
replace COPD009MI2_RN1b=1 if EUNRESCOPD_DAT>=QSSD & FEV1FVCL70DIAG_DAT<=(EUNRESCOPD_DAT+186) & EUNRESCOPD_DAT!=. & FEV1FVCL70DIAG_DAT!=. 


*rule 2 (1=select)
gen COPD009MI2_RN2=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCREG_DAT!=. 
replace COPD009MI2_RN2=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=. 

*rule 2 (1=select) with alternative FEV1FVC variable
gen COPD009MI2_RN2b=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & CONV_FEV1FVCREG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & CONV_FEV1FVCREG_DAT!=. 
replace COPD009MI2_RN2b=1 if EUNRESCOPD_DAT>=QSSD & REG_DAT>(PPED-365.25) & FEV1FVCL70REG_DAT<=(REG_DAT+186) & EUNRESCOPD_DAT!=. & REG_DAT!=. & FEV1FVCL70REG_DAT!=. 
recode COPD009MI2_RN2b .=0


forvalues i=1/2{
	recode COPD009MI2_RN`i' .=0
}

forvalues i=1/2{
	recode COPD009MI2_RN`i'b .=0
}


*create indicator count 
gen COPD009MI2=1 if  COPD009MI2_RN1==1 & COPD_REG==1
replace COPD009MI2=1 if COPD_REG==1 & COPD009MI2_RN1==0 & COPD009MI2_RN2==1 

gen COPD009MI2b=1 if  COPD009MI2_RN1b==1 & COPD_REG==1
replace COPD009MI2b=1 if COPD_REG==1 & COPD009MI2_RN1b==0 & COPD009MI2_RN2b==1 

recode COPD009MI2 .=0 
recode COPD009MI2b .=0

codebook patid if COPD009MI2==1 // 500=new, 503=old, 491= new diag only

codebook patid if COPD009MI2b==1 // 703=new, 709=old, 694= new diag only 


*************************************************
*5. COPD009MI3, 1=reject/select, 0=next/reject
*************************************************

*note- rules 1 & 2 the same as COPD009MI2 

*rule 3 (1=select)
gen COPD009MI3_RN3=1 if EUNRESCOPD_DAT>=QSSD & EUNRESCOPD_DAT!=. 
recode COPD009MI3_RN3 .=0

*create indicator count 
gen COPD009MI3=1 if COPD009MI2_RN1!=1 & COPD_REG==1 & COPD009MI2_RN2==0 & COPD009MI3_RN3==1 
recode COPD009MI3 .=0

gen COPD009MI3b=1 if COPD009MI2_RN1b!=1 & COPD_REG==1 & COPD009MI2_RN2b==0 & COPD009MI3_RN3==1 
recode COPD009MI3b .=0

codebook patid if COPD009MI3==1 // 4,711=new, 3,980=old, 3,865= new diag only

codebook patid if COPD009MI3b==1 // 4,508=new, 3,774=old, 3,662= new diag only





























