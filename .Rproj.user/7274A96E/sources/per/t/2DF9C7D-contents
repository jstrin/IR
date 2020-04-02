#' DataMart helper files
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export read_acad_organization
#' @export read_admissions
#' @export read_cohort
#' @export read_bot_course
#' @export read_eot_course
#' @export read_course_info
#' @export read_degree
#' @export read_enrollment
#' @export read_financials
#' @export read_instructors
#' @export read_itemtype
#' @export read_students
#'

read_acad_organization <- function(acadorg_file = "Acad_Organization.csv"){
  readr::read_csv(acadorg_file,
                  col_types =  cols(
                        academic_plan_code = col_character(),
                        academic_plan = col_character(),
                        academic_plan_type = col_character(),
                        academic_program = col_character(),
                        degree = col_character(),
                        diploma_descr = col_character(),
                        transcript_descr = col_character(),
                        degree_plan_cip_code = col_double(),
                        acad_career = col_character(),
                        eff_status = col_character(),
                        effdt = col_date(format = ""),
                        acad_org = col_character(),
                        percent_owned = col_double()
                      ))
}


read_admissions <- function(adm_file = "Admissions.csv"){
    readr::read_csv(adm_file,
                    col_types = cols(
                          report_date = col_datetime(format = ""),
                          acad_career = col_character(),
                          student_id_number = col_double(),
                          admit_term = col_double(),
                          first_enrollment_term = col_double(),
                          admit_type_code = col_character(),
                          admit_type_descr = col_character(),
                          prog_action = col_character(),
                          prog_status = col_character(),
                          last_school_attended = col_double(),
                          last_school_name = col_character(),
                          last_school_type = col_character(),
                          last_school_atp_code = col_double(),
                          hs_attended = col_double(),
                          hs_name = col_character(),
                          hs_type = col_character(),
                          hs_atp_code = col_double(),
                          hs_gpa_converted = col_double(),
                          another_gpa = col_double(),
                          recalc_gpa = col_double(),
                          hs_grad_date = col_datetime(format = ""),
                          sat_new_read = col_double(),
                          sat_new_math = col_double(),
                          sat_score = col_double(),
                          sat_superscore = col_double(),
                          act_score = col_double(),
                          act_converted = col_double(),
                          act_converted_new = col_double(),
                          class_size = col_double(),
                          class_rank = col_double(),
                          percentile = col_double(),
                          appl_program = col_character(),
                          appl_plan = col_character(),
                          admit_date = col_datetime(format = ""),
                          admit_prog = col_character(),
                          admit_plan = col_character(),
                          enrl_prog = col_character(),
                          enrl_plan = col_character(),
                          paid_prog = col_character(),
                          paid_plan = col_character(),
                          residency = col_character(),
                          citizenship = col_character(),
                          citizenship_status = col_double(),
                          citizenship_descr = col_character(),
                          first_gen_status = col_character(),
                          athlete = col_character(),
                          td_recruit = col_character(),
                          jaa_student = col_character(),
                          college_admit_to = col_character(),
                          major_admit_to = col_character(),
                          admit_date_1 = col_datetime(format = ""),
                          college_deposit_to = col_character(),
                          major_deposit_to = col_character(),
                          deposit_date = col_datetime(format = "")
                    ))
}

read_cohort <- function(cohort_file, cohort_type = NULL){
  dfCohort <- readr::read_csv(cohort_file,
                  col_types = cols(
                       emplid = col_double(),
                       term = col_double()
                     ))

  if(!is.null(cohort_type)){
    dfCohort <- dfCohort %>%
      mutate(cohort_type = cohort_type)
  }
}



read_course_enroll <- function(course_enroll_file){
  readr::read_csv(course_enroll_file,
                  col_types = cols(
                       stage_of_term_file = col_character(),
                       as_of_date = col_date(format = ""),
                       student_id = col_double(),
                       acad_career = col_character(),
                       enrollment_term = col_double(),
                       class_nbr = col_double(),
                       course_id = col_double(),
                       enrl_status = col_character(),
                       enrl_status_descr = col_character(),
                       enrl_status_reason = col_character(),
                       enrl_status_reason_descr = col_character(),
                       acad_group = col_character(),
                       subject = col_character(),
                       catalog_nbr = col_character(),
                       subject_catalog = col_character(),
                       class_section = col_character(),
                       class_descr = col_character(),
                       class_status = col_character(),
                       class_status_descr = col_character(),
                       class_type = col_character(),
                       class_type_descr = col_character(),
                       units_taken = col_double(),
                       units_earned = col_character(),
                       course_grade = col_character(),
                       course_grade_pts = col_character(),
                       multiple_attempts = col_character(),
                       attempt_count = col_double()
                     ))
}

read_bot_course <- function(course_enroll_file){
  read_course_enroll(course_enroll_file) %>%
    mutate(SnapshotType = "BoT")
}

read_eot_course <- function(course_enroll_file){
  read_course_enroll(course_enroll_file) %>%
    mutate(SnapshotType = "EoT")
}


read_course_info <- function(course_info_file){
  readr::read_csv(course_info_file,
                  col_types= cols(
                       course_term = col_double(),
                       course_term_descr = col_character(),
                       acad_career = col_character(),
                       acad_org = col_character(),
                       acad_group = col_character(),
                       subject = col_character(),
                       catalog_nbr = col_character(),
                       subject_catalog = col_character(),
                       class_section = col_character(),
                       class_component = col_character(),
                       class_nbr = col_double(),
                       class_descr = col_character(),
                       course_id = col_double(),
                       enrl_status = col_character(),
                       class_status = col_character(),
                       class_type = col_character(),
                       combined_section = col_character(),
                       crosslisted_total = col_double(),
                       crosslist_course = col_character(),
                       campus = col_character(),
                       location = col_character(),
                       meeting_days = col_character(),
                       facility_id = col_character(),
                       room_cap_request = col_double(),
                       enrl_cap = col_double(),
                       enrl_term_begin = col_double(),
                       enrl_end_add_pd = col_double(),
                       enrl_end_drop_pd = col_double(),
                       enrl_census_dt = col_double(),
                       enrl_term_end = col_double(),
                       instruction_mode = col_character(),
                       instr_mode_descr = col_character(),
                       units_min = col_double(),
                       units_max = col_double(),
                       grading_basis = col_character(),
                       instr_id_1 = col_double(),
                       instr_name_1 = col_character(),
                       instr_role_1 = col_character(),
                       instr_id_2 = col_double(),
                       instr_name_2 = col_character(),
                       instr_role_2 = col_character(),
                       instr_id_3 = col_double(),
                       instr_name_3 = col_character(),
                       instr_role_3 = col_character(),
                       instr_id_4 = col_double(),
                       instr_name_4 = col_character(),
                       instr_role_4 = col_character(),
                       instr_id_5 = col_double(),
                       instr_name_5 = col_character(),
                       instr_role_5 = col_character()
                     ))
}


read_degree <- function(degree_file){
  readr::read_csv( degree_file,
                   col_types =    cols(
                        student_id_number = col_double(),
                        completion_term = col_double(),
                        degree_number = col_double(),
                        degree = col_character(),
                        degree_descr = col_character(),
                        degree_career = col_character(),
                        conferral_date = col_date(format = ""),
                        degree_status_dt = col_date(format = ""),
                        plans = col_character(),
                        major_plan_1 = col_character(),
                        major_plan_cip_1 = col_double(),
                        major_plan_2 = col_character(),
                        major_plan_cip_2 = col_double(),
                        major_plan_3 = col_character(),
                        major_plan_cip_3 = col_double(),
                        major_plan_4 = col_character(),
                        major_plan_cip_4 = col_double(),
                        minor_plan_1 = col_character(),
                        minor_plan_2 = col_character(),
                        minor_plan_3 = col_character(),
                        minor_plan_4 = col_character(),
                        honors_code = col_character(),
                        honors_code_descr = col_character(),
                        total_cred_attempted = col_double(),
                        total_cred_passed = col_double(),
                        total_cred_transfer = col_double(),
                        total_cred_test = col_double(),
                        total_cred_other = col_double(),
                        total_cred_cum = col_double(),
                        transferred_from = col_character()
                      ))
}


read_enrollment <- function(enrollment_file, snapshot_type){
 dfEnrollment <-  readr::read_csv( enrollment_file,
                   col_types =    cols(
                        file_type = col_character(),
                        census_date = col_date(format = ""),
                        student_id_number = col_double(),
                        acad_career = col_character(),
                        enrollment_term = col_double(),
                        acad_prog_1 = col_character(),
                        acad_plan_1 = col_character(),
                        acad_plan_descr_1 = col_character(),
                        acad_plan_1_cip = col_double(),
                        acad_plan_2 = col_character(),
                        acad_plan_descr_2 = col_character(),
                        acad_plan_cip_2 = col_double(),
                        acad_plan_3 = col_character(),
                        acad_plan_descr_3 = col_character(),
                        acad_plan_cip_3 = col_double(),
                        acad_plan_4 = col_character(),
                        acad_plan_descr_4 = col_character(),
                        acad_plan_cip_4 = col_double(),
                        acad_plan_minor_1 = col_character(),
                        acad_plan_minor_descr_1 = col_character(),
                        acad_plan_minor_2 = col_character(),
                        acad_plan_minor_descr_2 = col_character(),
                        acad_plan_minor_3 = col_character(),
                        acad_plan_minor_descr_3 = col_character(),
                        acad_plan_minor_4 = col_character(),
                        acad_plan_minor_descr_4 = col_character(),
                        enroll_status = col_character(),
                        entry_code = col_character(),
                        residency = col_character(),
                        acad_load = col_character(),
                        acad_load_descr = col_character(),
                        acad_level_bot = col_character(),
                        acad_level_eot = col_character(),
                        academic_level = col_character(),
                        term_cred_attempted = col_double(),
                        term_cred_passed = col_double(),
                        term_cred_transfer = col_double(),
                        term_cred_test = col_double(),
                        term_cred_other = col_double(),
                        total_cred_attempted = col_double(),
                        total_cred_passed = col_double(),
                        total_cred_transfer = col_double(),
                        total_cred_test = col_double(),
                        total_cred_other = col_double(),
                        total_cred_cum = col_double(),
                        term_gpa = col_double(),
                        cum_gpa = col_double(),
                        student_group_codes = col_character(),
                        student_group_1 = col_character(),
                        student_group_2 = col_character(),
                        student_group_3 = col_character(),
                        student_group_4 = col_character(),
                        student_group_5 = col_character(),
                        student_group_6 = col_character(),
                        study_agreement = col_character(),
                        grad_assistant_status = col_character(),
                        withdraw_code = col_character(),
                        withdraw_date = col_date(format = ""),
                        withdraw_reason = col_character(),
                        admit_athlete = col_character(),
                        admit_td_recruit = col_character(),
                        td_student_ind = col_character(),
                        job_recd_ind = col_character(),
                        job_dept = col_character(),
                        job_description = col_character(),
                        enrl_census_ind = col_character()
                      ))

  if(!is.null(snapshot_type)){
    dfEnrollment <- dfEnrollment %>%
      mutate(snapshot_type = snapshot_type)
  }
 dfEnrollment
}



read_financials <- function(financials_file){
  readr::read_csv( financials_file,
                   col_types =    cols(
                        academic_year = col_double(),
                        term_range = col_character(),
                        student_id = col_double(),
                        name = col_character(),
                        tot_tuition_amt = col_double(),
                        total_fees_amt = col_double(),
                        grand_total_fees = col_double(),
                        total_actual_charges = col_double(),
                        total_fed_need_amt = col_double(),
                        total_merit_amt = col_double(),
                        pell_award_offered = col_double(),
                        pell_award_disbursed = col_double(),
                        athletic_aid_amt = col_double(),
                        athletic_aid_offered = col_character(),
                        athletic_aid_disbursed = col_character(),
                        work_study_aid_offered = col_double(),
                        work_study_aid_disbursed = col_double(),
                        tuition_waiver_amt = col_double(),
                        tuition_waiver_amt_offered = col_double(),
                        tuition_waiver_amt_disbursed = col_double(),
                        tuition_reductions = col_double(),
                        tot_sub_loan_amt_offered = col_double(),
                        tot_sub_loan_amt_disbursed = col_double(),
                        tot_unsub_loan_amt_offered = col_double(),
                        tot_unsub_loan_amt_disbursed = col_double(),
                        tot_other_loan_amt_offered = col_double(),
                        tot_other_loan_amt_disbursed = col_double(),
                        tot_inst_aid_amt_offered = col_double(),
                        tot_inst_aid_amt_disbursed = col_double(),
                        tot_state_aid_amt_offered = col_double(),
                        tot_state_aid_amt_disbursed = col_double(),
                        tot_fed_aid_amt_offered = col_double(),
                        tot_fed_aid_amt_disbursed = col_double(),
                        efc_amount = col_double(),
                        agi_parent_amt = col_double(),
                        agi_student_amt = col_double(),
                        agi_total_amt = col_double(),
                        ram_grant_offered = col_double(),
                        ram_grant_disbursed = col_double(),
                        diversity_schol_offered = col_character(),
                        diversity_schol_disbursed = col_character(),
                        univ_grant_offered = col_double(),
                        univ_grant_disbursed = col_double(),
                        founders_grant_offered = col_double(),
                        founders_grant_disbursed = col_double(),
                        hardge_scholarship_offered = col_double(),
                        hardge_scholarship_disbursed = col_double(),
                        dimaio_scholarship_offered = col_character(),
                        dimaio_scholarship_disbursed = col_character(),
                        uri_college_crusade_offered = col_character(),
                        uri_college_crusade_disbursed = col_character(),
                        other_need = col_double(),
                        sup_ed_op_grant_offered = col_character(),
                        sup_ed_op_grant_disbursed = col_character(),
                        fed_teach_grant_offered = col_character(),
                        fed_teach_grant_disbursed = col_character(),
                        ri_state_schol_offered = col_double(),
                        ri_state_schol_disbursed = col_double(),
                        fed_plus_loans_offered = col_double(),
                        fed_plus_loans_disbursed = col_double(),
                        nursing_loan_offered = col_character(),
                        nursing_loan_disbursed = col_character(),
                        health_prof_loan_offered = col_double(),
                        health_prof_loan_disbursed = col_double(),
                        mass_state_funds_offered = col_character(),
                        mass_state_funds_disbursed = col_character(),
                        merit_award_offered = col_double(),
                        merit_award_disbursed = col_double(),
                        centennial_scholarship_offer = col_character(),
                        centennial_scholarship_disb = col_character(),
                        total_other_inst_offered = col_double(),
                        total_other_inst_disbursed = col_double(),
                        financial_aid_ind = col_character()
                      ))
}


read_instructors <- function(instructor_file){
  readr::read_csv(instructor_file,
                  col_types =  cols(
                       term = col_double(),
                       term_descr = col_character(),
                       instructor_id = col_double(),
                       instructor_name = col_character(),
                       acad_org = col_character(),
                       acad_org_descr = col_character(),
                       instructor_type = col_character(),
                       instr_type_descr = col_character(),
                       advisor_ind = col_character(),
                       rank = col_character(),
                       tenure_status = col_character()
                     ))
}


read_itemtype <- function(itemtype_file){
  readr::read_csv(itemtype_file,
                 col_types = cols(
                   item_type = col_double(),
                   descr = col_character()
                 )) %>%
    rename(item_descr = descr)
}


read_students <- function(students_file){
  readr::read_csv(students_file,
                  col_types =
                    cols(
                          student_id_number = col_double(),
                          student_name = col_character(),
                          gender = col_character(),
                          ethnic_group = col_character(),
                          ethnicity = col_character(),
                          date_of_birth = col_date(format = ""),
                          date_of_death = col_date(format = ""),
                          home_city = col_character(),
                          home_state = col_character(),
                          postal_code = col_character(),
                          citizenship_status = col_character(),
                          citizenship_status_descr = col_character(),
                          citizenship_country = col_character(),
                          residency = col_character(),
                          degree_level = col_character(),
                          dual_concurrent_existed = col_character(),
                          first_gen_ind = col_character(),
                          ferpa = col_character()
                        ))
}

