library(tidyverse)
library(haven)
library(dplyr)
#save as OHIE_wrangle.R

oregonhie_descritpive_vars <- read_dta("oregonhie_descriptive_vars.dta")
oregonhie_ed_vars <- read_dta(file = "oregonhie_ed_vars.dta")
oregonhie_inperson_vars <- read_dta(file = "oregonhie_inperson_vars.dta")
oregonhie_patterns_vars <- read_dta(file = "oregonhie_patterns_vars.dta")
oregonhie_stateprograms_vars <- read_dta(file = "oregonhie_stateprograms_vars.dta")
oregonhie_survey0m_vars <- read_dta(file = "oregonhie_survey0m_vars.dta")
oregonhie_survey12m_vars <- read_dta(file = "oregonhie_survey12m_vars.dta")
oregonhie_survey6m_vars <- read_dta(file = "oregonhie_survey6m_vars.dta")

OHIE_data_for_analysis <- read_dta("~/Documents/Business Analytics/Data/OHIE_data_for_analysis.dta")

#filter all datasets out that we don't want
OHIE_data_filter <- OHIE_data_for_analysis[-unique(c(which(colnames(OHIE_data_for_analysis)%in%colnames(oregonhie_survey12m_vars)),
                                              which(colnames(OHIE_data_for_analysis)%in%colnames(oregonhie_survey6m_vars)),
                                              which(colnames(OHIE_data_for_analysis)%in%colnames(oregonhie_survey0m_vars))))]
OHIE_data_filter<- bind_cols(OHIE_data_for_analysis[1], OHIE_data_filter)

# Individual df column names to delete and aggregated at the end
descriptive_var_del <- c("draw_treat", "draw_lottery", "dt_notify_lottery", "dt_app_decision",
                         "applied_app", "approved_app", "postn_death", "have_phone_list", "first_day_list",
                         "last_day_list", "pobox_list", "self_list", "week_list") #13
ed_vars_del <- c("sample_ed","any_hosp_pre_ed", "any_hosp_ed","num_hosp_pre_cens_ed","num_hosp_cens_ed",
                 "any_out_pre_ed","any_out_ed","num_out_pre_cens_ed","num_out_cens_ed","any_on_pre_ed",
                 "any_on_ed","num_on_pre_cens_ed","num_on_cens_ed","any_off_pre_ed",
                 "any_off_ed","num_off_pre_cens_ed","num_off_cens_ed","num_edcnnp_pre_ed","num_edcnnp_ed",
                 "num_edcnpa_pre_ed","num_edcnpa_ed","num_epct_pre_ed","num_epct_ed","num_ne_pre_ed",
                 "num_ne_ed","num_unclas_pre_ed","num_unclas_ed","any_acsc_pre_ed","any_acsc_ed",
                 "num_acsc_pre_cens_ed","num_acsc_cens_ed","any_mail_match_ed","num_mail_match_ed","any_inp_match_ed",
                 "num_inp_match_ed","any_hiun_pre_ed", "any_hiun_ed", "num_hiun_pre_cens_ed", "num_hiun_cens_ed",
                 "any_loun_pre_ed", "any_loun_ed", "num_loun_pre_cens_ed", "num_loun_cens_ed") #43
inperson_vars_del <- c("in_data_inp", "sample_resp_inp", "dt_release_inp", "dt_completed_inp", "interview_location_inp",
                       "interviewer_inp","scale_id_inp","stadio_id_inp", "omron_id_inp","interpreter_inp",
                        "usual_clinic_inp","cvd_risk_point_inp", "doc_num_incl_probe_inp", "ed_num_incl_probe_inp",
                        "surg_num_incl_probe_inp","hosp_any_incl_probe_inp","a1c_inp", "hdl_inp","chl_inp","bp_sar_inp",
                        "bp_dar_inp","has_bp_inp","has_waist_inp","has_hght_wght_inp","has_dbs_inp","has_all_dbs_inp",
                        "hbp_diure_med_inp","antihyperlip_med_inp","diabetes_med_inp","antidep_med_inp","meds_miss_inp") #31
patterns_vars_del <- c("any_visit_180p_180", "any_visit_180p_360","any_visit_180p_540","any_visit_180p_720",
                       "medicaid_all_180p_period_180","medicaid_all_180p_period_360","medicaid_all_180p_period_540",
                       "medicaid_all_180p_period_720","draw_lottery","visit_dt_max","visit_dt_min","weight_720days",
                       "weight_540days","weight_360days","weight_180days","numhh_list","treatment","dt_notify_treat",
                       "have_phone_list","pobox_list","first_day_list","self_list","ohp_all_ever_inperson","dt_completed_inp",
                       "sample_inp_resp","doc_any_incl_probe_inp","ed_any_incl_probe_inp","any_inp_match_ed") #28
stateprogram_vars_del <- c("ohp_all_ever_matchn_30sep2009","ohp_all_ever_firstn_survey0m","ohp_all_ever_firstn_survey6m",
                           "ohp_all_ever_inperson","ohp_all_ever_firstn_30sep2009","ohp_all_end_30sep2009",
                           "ohp_all_end_survey0m","ohp_all_end_survey6m","ohp_all_end_inperson","ohp_all_at_12m",
                           "ohp_all_mo_matchn_30sep2009","ohp_all_mo_matchn_30sep2009","ohp_all_mo_firstn_survey0m",
                           "ohp_all_mo_firstn_survey6m","ohp_all_mo_inperson","ohp_all_mo_inperson",
                           "ohp_all_mo_firstn_30sep2009","ohp_all_mo_12m","ohp_std_ever_matchn_30sep2009",
                           "ohp_std_ever_inperson","ohp_std_ever_firstn_30sep2009") #21

delete <- unique(c(descriptive_var_del, ed_vars_del, inperson_vars_del, patterns_vars_del, stateprogram_vars_del))
index <- which(colnames(OHIE_data_filter)%in%delete) #these are fitered out.

#Clean out direct searches
OHIE_data_filter <- OHIE_data_filter[-index]
#manual selection of which to keep.
#keep <- c("person_id","household_id","birthyear_list","english_list","female_list","zip_msa","prenany_snap_bin","pren_survey12m_snap_bin","postn_snap_bin","postn_survey12m_snap_bin","prenany_snap_hh_amt","pren_survey12m_snap_hh_amt","postn_snap_hh_amt","postn_survey12m_snap_hh_amt","prenany_tanf_bin","pren_survey12m_tanf_bin","postn_tanf_bin","postn_survey12m_tanf_bin","prenany_tanf_hh_amt","pren_survey12m_tanf_hh_amt","postn_tanf_hh_amt","postn_survey12m_tanf_hh_amt","")

#Join together OG datasets
joineddf <- full_join(select(oregonhie_descritpive_vars, !matches(descriptive_var_del)),select(oregonhie_ed_vars, !matches(ed_vars_del)), by = "person_id")
joineddf <- full_join(joineddf, select(oregonhie_inperson_vars, !matches(inperson_vars_del)), by = "person_id")
joineddf <- full_join(joineddf, select(oregonhie_patterns_vars, !matches(patterns_vars_del)), by = "person_id")

joineddf <- full_join(joineddf, select(oregonhie_stateprograms_vars, !matches(stateprogram_vars_del)), by = "person_id")

joineddf <- full_join(joineddf, select(oregonhie_stateprograms_vars, !matches(stateprogram_vars_del)), by = "person_id")

  #xxx <- mutate(joineddf, number_ed_visits = num_visit_pre_cens_ed + num_visit_cens_ed)
joined <- select(joineddf, matches("any_visit_pre_ed","any_visit_ed", "num_visit_pre_cens_ed", "num_visit_cens_ed",""))
