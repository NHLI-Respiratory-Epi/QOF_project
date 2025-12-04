## Datasets used for creating codelists

# refsets
GPData_Cluster_Refset_1000230_20250904 - based on 30 July 2025 SNOMED CT UK Clinical release.
Note that there was an update released a week after this, which uses the same clinical release
and is the most recent update. 


an update was relased a week later (GPData_Cluster_Refset_1000230_20250912) - these were the only changes:
   Output_ID   Cluster_ID                                                                            Cluster_Description
      <char>       <char>                                                                                         <char>
1:    AST012 SKINTEST_COD                                                      Skin prick test for house dust mite codes
2:    NDA001   NDADM1_COD National diabetes audit (NDA) diabetes mellitus codes to determine diabetes type for the audit
3:    NDA001    DMREM_COD                                                       Codes for diabetes mellitus in remission
        SNOMED_code                                                 SNOMED_code_description      PCD Refset ID insep12 insep4
             <char>                                                                  <char>              <i64>   <num>  <num>
1: 2580481000000102 Positive skin prick test reaction to house dust mite allergen (finding) 999036551000230102       1     NA
2:        315051004                                             Diabetes resolved (finding) 999002971000230106       1     NA
3:        315051004                                             Diabetes resolved (finding) 999025931000230108      NA      1





# SNOMEDCT codes
uk_sct2mo_40.5.0_20250827000001Z - monolith. (next update: uk_sct2mo_41.0.0_20250924000001Z)
uk_sctqths_40.5.0_20250827000001 - query and history substitution table
uk_sct2pc_58.0.0_20250912000000Z - refsets 

# BNF
BNF Snomed Mapping data 20251017 - BNF to SNOMED mapping data
nhsbsa_dmd_8.2.0_20250818000001 - dmd data used in the downloaded snomed monolith
                                         

