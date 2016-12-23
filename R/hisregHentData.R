#' Provide global dataframe for NoRGast
#'
#' Provides Hisreg data from staging
#'
#' @inheritParams hisregFigAndeler
#'
#' @return RegData data frame
#' @export

hisregHentRegData <- function() {

  registryName <- "hisreg"
  dbType <- "mysql"

  query <- paste0("SELECT *
                  FROM AlleVarNum INNER JOIN ForlopsOversikt
                  ON AlleVarNum.m_mceid = ForlopsOversikt.ForlopsID
                  INNER JOIN AlleVar ON AlleVarNum.m_mceid = AlleVar.m_mceid
                  LEFT JOIN FollowupsNum ON AlleVarNum.m_mceid = FollowupsNum.c_mceid")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
