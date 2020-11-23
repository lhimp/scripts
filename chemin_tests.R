fileExts <- c("R", "RDS", "xlsx", "csv")
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
    cat(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName,
           chemin(fileName = fileName, relPath = relPath, read = T),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 2
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    cat(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName,
           chemin(fileName = fileName, relPath = relPath, githubCredentials = githubCred, read = T),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 3
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    cat(sprintf("Case %d: Ext \"%s\"", caseNo, fileExt))
    assign(varName, 
           chemin(fileName = fileName, relPath = relPath, rootPath = rootPath, read = T),
           envir = .GlobalEnv)
    print(get(varName))
    
    caseNo <- 4
    varName <- sprintf("data.case%d.%s", caseNo, fileExt)
    cat(sprintf("Case 4: Ext \"%s\"", fileExt))
    assign(varName, 
           chemin(fileName = fileName, relPath = relPath, rootPath = dropboxRootPath, usingDropboxRootPath = T, read = T),
           envir = .GlobalEnv)
    print(get(varName))
  }
}
runTests()