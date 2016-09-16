#' Provide global dataframe for NoRGast
#'
#' Provides NoRGast data from staging
#'
#' @inheritParams hisregFigAndeler
#'
#' @return RegData data frame
#' @export

hisregHentRegData <- function() {

  registryName <- "hisreg"
  dbType <- "mysql"

  query <- paste0("SELECT

                    FROM AlleVarNum INNER JOIN ForlopsOversikt
                    ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
                    INNER JOIN AlleVar ON AlleVarNum.ForlopsID = AlleVar.ForlopsID")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
