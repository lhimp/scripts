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
  readData = T,
  options = list(
    xlsSheetNo = 1,
    csvSeparator = ','
  )
  )
  {
    regexMatch <- function(patt, x) { return(length(grep(patt, x, ignore.case = T)) == 1) }
    
    if ( length(rootPath) == 0 && !usingDropboxRootPath && length(githubCredentials) == 0 ) {
      l <- as.list(c(relPath, fileName))
      chmn <<- do.call(here, l)
    } else
    if ( length(githubCredentials) == 2 ) {
      githubPathComponents <- c("https:/", "github.com", githubCredentials$userName, githubCredentials$repoName, "raw", "master", relPath, fileName)
      chmn <<- URLencode(paste(githubPathComponents, collapse = '/'))
    } else {
      if ( usingDropboxRootPath ) {
        pathComponents <- c(strsplit(Sys.getenv('USERPROFILE'), '\\\\|/')[[1]], 'Dropbox')
      } else {
        pathComponents <- c("C:")
      }
      pathComponents <- c(pathComponents, rootPath, relPath, fileName)
      chmn <<- paste(pathComponents, collapse = '/')
    }
  
    if ( readData ) {
      conn <- file(chmn)
      rdsFilePattern <- "\\.RDS$"
      xlsFilePattern <- "\\.xlsx?$"
      csvFilePattern <- "\\.csv$"
      rFilePattern <- "\\.R$"
      isUrl <- regexMatch("://", chmn)
      data <- T
      
      if ( regexMatch(rdsFilePattern, chmn) ) {
        data <- readRDS(conn)
      } else
        if ( regexMatch(csvFilePattern, chmn) )   {
          data <- read.csv(conn, sep = options$csvSeparator)
      } else
      if ( regexMatch(xlsFilePattern, chmn) )   {
        if ( isUrl ) {
          conn <- tempfile(fileext = ".xlsx")
          req <- GET(chmn, write_disk(path = conn))
        } else {
          conn <- chmn
        }
        data <- read_excel(conn, options$xlsSheetNo)
      } else
      if ( isUrl ) {
        source_url(chmn)
      }
      else {
        source(conn)
      }
      
      return(data)
      
    } else {
      print(chmn)
      return(chmn)
    }
}

fileExts <- c("R", "RDS", "csv", "xlsx")
runTests <- function() {
  for ( j in 1:length(fileExts) ) {
    fileExt <- fileExts[[j]]
    
    fileName <- sprintf("test.%s", fileExt)
    relPath <- c("Revalidation", "test")
    rootPath <- c("Users", "p0070611", "Documents", "Visual Studio 2017", "Projects", "WebexpoValidation")
    repoName <- "webexpo_cs_validation"
    dropboxRootPath <- c(repoName)
    githubCred <- list(userName = "webexpo", repoName = repoName)
    
    caseNo <- 1
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    print(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName,
           chemin(fileName = fileName, relPath = relPath),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 2
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    print(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName,
           chemin(fileName = fileName, relPath = relPath, githubCredentials = githubCred),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 3
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    print(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName, 
           chemin(fileName = fileName, relPath = relPath, rootPath = rootPath),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 4
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    print(sprintf("Case 4: Ext \"%s\"", fileExt))
    assign(varName, 
           chemin(fileName = fileName, relPath = relPath, rootPath = dropboxRootPath, usingDropboxRootPath = T),
           envir = .GlobalEnv)
    print(get(varName))
  }
}