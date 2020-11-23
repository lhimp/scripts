library(devtools)
library(here)
library(readxl)
library(httr)

# 4 cas d'utilisation pour faire lire un fichier de projet
#
# CAS 1: Fichier dans son projet local
# Usage:
#   chemin(
#       fileName = "webexpo.seg.dataprep.R",
#       relPath = c("DATA PREPARATION", "SEG ANALYSIS")
#   )
#
# CAS 2: Fichier dans un projet Github
# Usage:
#   chemin(
#       fileName = "webexpo.seg.dataprep.R",
#       relPath = c("DATA PREPARATION", "SEG ANALYSIS"),
#       githubCredentials = list(userName = "webexpo", repoName = "webexpo_r_lib")
#   )
#
# CAS 3: Fichier dans un projet situé quelque part sur le disque dur
# Usage:
#   chemin(
#       fileName = "webexpo.seg.dataprep.R",
#       relPath = c("DATA PREPARATION", "SEG ANALYSIS"),
#       rootPath = c("Users", "p0070611", "Documents", "R", "projects", "webexpo_r_lib")
#   )
#
# CAS 4: Fichier dans un projet situé quelque part dans son répertoire Dropbox
# Usage:
#   chemin(
#       fileName = "webexpo.seg.dataprep.R",
#       relPath = c("DATA PREPARATION", "SEG ANALYSIS"),
#       rootPath = c("SHARE Daniel Margulius", "webexpo_r_lib"),
#       usingDropboxRootPath = T
#   )

chmn <- ""

chemin <- function(
  fileName,
  relPath = c(),
  rootPath = c(),
  githubCredentials = list(),
  usingDropboxRootPath = F,
  read = T,
  printSourcingInfo = F,
  options = list(
      xlsSheetNo = 1,
      csvSeparator = ','
    )
  )
  {
    regexMatch <- function(patt, x) { return(length(grep(patt, x, ignore.case = T)) == 1) }
    chmn <- ""
    
    if ( length(rootPath) == 0 && !usingDropboxRootPath && length(githubCredentials) == 0 ) {
      chmn <- do.call("here", as.list(c(relPath, fileName)))
    } else
    if ( length(githubCredentials) == 2 ) {
      githubPathComponents <- c("https:/", "github.com", githubCredentials$userName, githubCredentials$repoName, "raw", "master", relPath, fileName)
      chmn <- URLencode(paste(githubPathComponents, collapse = '/'))
    } else {
      if ( usingDropboxRootPath ) {
        pathComponents <- c(strsplit(Sys.getenv('USERPROFILE'), '\\\\|/')[[1]], 'Dropbox')
      } else {
        pathComponents <- c("C:")
      }
      pathComponents <- c(pathComponents, rootPath, relPath, fileName)
      chmn <- paste(pathComponents, collapse = '/')
    }
    
    sourceFunc <- NULL
    sourceArgs <- NULL
    sourceData <- NULL
    conn <- NULL
    xlsConn <- NULL
    rdsFilePattern <- "\\.RDS$"
    xlsFilePattern <- "\\.xlsx?$"
    csvFilePattern <- "\\.csv$"
    rFilePattern <- "\\.R$"
    readingR <- F
    isUrl <- regexMatch("://", chmn)
    
    if ( regexMatch(rdsFilePattern, chmn) ) {
      conn <- file(chmn)
      sourceFunc <- "readRDS"
      sourceArgs <- list(file = conn)
    } else
      if ( regexMatch(csvFilePattern, chmn) )   {
        sourceFunc <- "read.csv"
        sourceArgs <- list(file = chmn, sep = options$csvSeparator)
    } else
    if ( regexMatch(xlsFilePattern, chmn) )   {
      if ( isUrl ) {
        tmpfilepath <- tempfile(fileext = ".xlsx")
        req <- GET(chmn, write_disk(path = tmpfilepath))
        sourceArgs <- list(path = tmpfilepath, sheet = options$xlsSheetNo)
      } else {
        sourceArgs <- list(path = chmn, sheet = options$xlsSheetNo)
      }
      sourceFunc <- "read_excel"
      
    } else
    if ( isUrl ) {
      sourceFunc <- "source_url"
      sourceArgs <- list(url = chmn)
    }
    else {
      readingR <- T
      conn <- file(chmn)
      sourceFunc <- "source"
      sourceArgs <- list(file = conn)
    }
    
    if ( printSourcingInfo ) {
      cat(sprintf("FONCTION À APPELER POUR LECTURE: %s\n", sourceFunc))
    }
    
    if ( read ) {
      # Hide SHA1 hash message
      sourceData <- suppressMessages(do.call(sourceFunc, sourceArgs))
      if ( !is.null(conn) ) {
        close(conn)
      }
      if ( !readingR ) {
        return(sourceData)
      }
    } else {
      return(chmn)
    }
}