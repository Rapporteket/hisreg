#' Provide global dataframe for NoRGast
#'
#' Provides Hisreg data from staging
#'
#' @return RegData data frame
#' @export

hisregHentRegData <- function() {

  registryName <- "hisreg"
  dbType <- "mysql"

  query <- paste0("SELECT AlleVarNum.m_mceid,
                  AlleVarNum.p_age_abscess,
                  AlleVarNum.p_education,
                  AlleVarNum.p_surgery,
                  AlleVarNum.p_antibiotics,
                  AlleVarNum.pre_smoking,
                  AlleVarNum.pre_bmi,
                  AlleVarNum.pre_work,
                  AlleVarNum.pre_dlqisum,
                  AlleVarNum.pre_hsscoresum,
                  AlleVarNum.pre_vasscore,
                  AlleVarNum.pre_hurley_score,
                  AlleVarNum.i_type,
                  AlleVarNum.i_surgery_type,
                  AlleVarNum.i_biological_treatment,
                  AlleVarNum.i_antibiotic_therapy,
                  AlleVarNum.i_antiinflammatory_treatment,
                  AlleVarNum.i_analgesics,
                  AlleVarNum.i_localized_med_treatment,
                  AlleVarNum.i_aksille,
                  AlleVarNum.i_lyske,
                  AlleVarNum.i_pubis,
                  AlleVarNum.i_genitalt,
                  AlleVarNum.i_peritalt,
                  AlleVarNum.i_glutealt,
                  AlleVarNum.i_mammae,
                  AlleVarNum.i_other_location,
                  AlleVar.i_type AS i_type_label,
                  ForlopsOversikt_v1.ForlopsID,
                  ForlopsOversikt_v1.AvdRESH,
                  ForlopsOversikt_v1.HovedDato,
                  ForlopsOversikt_v1.PasientAlder,
                  ForlopsOversikt_v1.BasisRegStatus,
                  ForlopsOversikt_v1.OppflgRegStatus,
                  ForlopsOversikt_v1.ForlopsType1,
                  ForlopsOversikt_v1.ForlopsType1Num,
                  ForlopsOversikt_v1.ForlopsType2,
                  ForlopsOversikt_v1.ForlopsType2Num,
                  ForlopsOversikt_v1.ErMann,
                  ForlopsOversikt_v1.SykehusNavn,
                  ForlopsOversikt_v1.PasientID,
                  FollowupsNum.c_do_month,
                  FollowupsNum.c_infection,
                  FollowupsNum.c_delayed_wound_healing,
                  FollowupsNum.c_stricturer,
                  FollowupsNum.c_nervedamage,
                  FollowupsNum.c_bloodpoisoning,
                  FollowupsNum.c_bleeding,
                  FollowupsNum.c_other_complications,
                  FollowupsNum.c_dlqisum,
                  FollowupsNum.c_hsscoresum,
                  FollowupsNum.c_vasscore,
                  FollowupsNum.c_hurley_score
                  FROM AlleVarNum INNER JOIN ForlopsOversikt_v1
                  ON AlleVarNum.m_mceid = ForlopsOversikt_v1.ForlopsID
                  INNER JOIN AlleVar ON AlleVarNum.m_mceid = AlleVar.m_mceid
                  LEFT JOIN FollowupsNum ON AlleVarNum.m_mceid = FollowupsNum.c_mceid")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}

#' Fetch chosen table for Hisreg
#'
#' Provides Hisreg data from staging
#'
#' @param tabnavn Name of requested table
#'
#' @return RegData data frame
#' @export
hisregHentTabell <- function(tabnavn, registryName="hisreg") {

  # registryName <- "hisreg2"
  dbType <- "mysql"

  query <- paste0("SELECT * FROM ", tabnavn)

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}




