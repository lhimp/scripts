#####################################################################################################
#
#
#  Script de création des résultats de comparaison entre R_webexpo / R_JAGS et CSHARP
#
#
#
###################################################################################################

library(ggplot2)

########################## Résultats pour un échantillon

fun.seg.resultone <- function( mylist , label , sampleindex ) {
  
                #mylist <- res.SEG.informedvar.noME
                
                #label <- "Informedvar"
                
                #sampleindex <- 12
                

                ####### ACROSS PLATFORM R1:  mean difference (ref WEBEXPO) relative to within variation in WEBEXPO 
                
                #######résultats pour MU
                
                
                res.mu <- data.frame(
                  
                  quantile.label=rep( mylist[[sampleindex]]$rjags$quantile[1:9] , 2) ,
                  
                  r1 = c( abs(100*(mylist[[sampleindex]]$rjags$mean[1:9] - mylist[[sampleindex]]$web$mean[1:9])/ mylist[[sampleindex]]$web$sd[1:9]) ,
                          abs(100*(mylist[[sampleindex]]$sharp$mean[1:9] - mylist[[sampleindex]]$web$mean[1:9])/ mylist[[sampleindex]]$web$sd[1:9]) ),
                  
                  model = rep( c("jags","sharp") , c(9,9)) ,
                  
                  stringsAsFactors = FALSE
                  
                ) 
                
                ## finalisation du data.frame

                res.mu$quantile.label <- ordered(res.mu$quantile.label, 
                                                 levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
                
              ##creation de résultats numérique pour exportation
                
                res.num <- list(2)
                
                res.num$mu <- list(2)
                
                res.num$mu$r1 <- res.mu
                
                
                ## graphique
                p1 <-ggplot(data = res.mu , aes ( y = r1 , x = quantile.label , color = model  ))  
                
                p1 <- p1 + geom_point()
                
                p1 <- p1 + ylab("R1 (%)") + ggtitle("MU")
                p1 <- p1 + geom_hline(yintercept = 100)
                
                
                
                
                #######SIGMA
                
                # initialisation
                res.sd <- data.frame(
                  
                  quantile.label=rep( mylist[[sampleindex]]$rjags$quantile[10:18] , 2) ,
                  
                  r1 = c( abs(100*(mylist[[sampleindex]]$rjags$mean[10:18] - mylist[[sampleindex]]$web$mean[10:18])/ mylist[[sampleindex]]$web$sd[10:18]) ,
                          abs(100*(mylist[[sampleindex]]$sharp$mean[10:18] - mylist[[sampleindex]]$web$mean[10:18])/ mylist[[sampleindex]]$web$sd[10:18]) ),
                  
                  model = rep( c("jags","sharp") , c(9,9)) ,
                  
                  stringsAsFactors = FALSE
                  
                ) 
                
                ## finalisation du data.frame

                res.sd$quantile.label <- ordered(res.sd$quantile.label, 
                                                 levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
                
                ##creation de résultats numérique pour exportation
                
                res.num$sigma <- list(2)
                
                res.num$sigma$r1 <- res.sd
                
                
                ## graphique
                p2 <-ggplot(data = res.sd , aes ( y = r1 , x = quantile.label , color = model )) 
                
                p2 <- p2 + geom_point()
                
                p2 <- p2 + ylab("R1 (%)") + ggtitle( "SIGMA" )
                p2 <- p2 + geom_hline(yintercept = 100)
                
                
                
                
                
                ############ WITHIN PLATFORM : ratio of within platform SD to the REF SD across 50 iterations                  
                
                
                #######MU
                
                # initialisation
                res.mu <- data.frame(
                  
                  quantile.label=rep( mylist[[sampleindex]]$rjags$quantile[1:9] , 2) ,
                  
                  r2 = c( mylist[[sampleindex]]$rjags$sd[1:9] /  mylist[[sampleindex]]$web$sd[1:9]  ,
                          mylist[[sampleindex]]$sharp$sd[1:9] /  mylist[[sampleindex]]$web$sd[1:9] ),
                  
                  model = rep( c("jags","sharp") , c(9,9)) ,

                  stringsAsFactors = FALSE
                  
                ) 
                
                ## finalisation du data.frame

                res.mu$quantile.label <- ordered(res.mu$quantile.label, 
                                                 levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
                
                
                ##creation de résultats numérique pour exportation
                
                res.num$mu$r2 <- res.mu
                
                
                ## graphique
                p3 <-ggplot(data = res.mu , aes ( y = r2 , x = quantile.label , color = model  )) 
                
                p3 <- p3 + geom_point() + ylim(0,3)
                
                p3 <- p3 + ylab("R2") + ggtitle( "MU" )
                p3 <- p3 + geom_hline(yintercept = 1)
                
                
                
                
                #######SIGMA
                
                # initialisation
                res.sd <- data.frame(
                  
                  quantile.label=rep( mylist[[sampleindex]]$rjags$quantile[10:18] , 2) ,
                  
                  r2 = c( mylist[[sampleindex]]$rjags$sd[10:18] /  mylist[[sampleindex]]$web$sd[10:18]  ,
                          mylist[[sampleindex]]$sharp$sd[10:18] /   mylist[[sampleindex]]$web$sd[10:18] ),
                  
                  model = rep( c("jags","sharp") , c(9,9)) ,
                  
                  stringsAsFactors = FALSE
                  
                ) 
                
                ## finalisation du data.frame

                res.sd$quantile.label <- ordered(res.sd$quantile.label, 
                                                 levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
                
                ##creation de résultats numérique pour exportation
                
                res.num$sigma$r2 <- res.sd
                
                ## graphique
                p4 <-ggplot(data = res.sd , aes ( y = r2 , x = quantile.label , color = model )) 
                
                p4 <- p4 + geom_point() + ylim(0,3)
                
                p4 <- p4 + ylab("R2") + ggtitle( "SIGMA")
                p4 <- p4 + geom_hline(yintercept = 1)
                
                
                
                
                return(list( r1= list(mu=p1,sigma=p2),
                             r2= list(mu=p3,sigma=p4),
                             num = res.num ))
                
              }
              
              
              


######################## Résultats pour tous les échantillons

fun.seg.resultall <- function( mylist , label ) {
  
  #mylist <- res.SEG.informedvar.noME
  
  #label <- "Informedvar"
  
  ### deactivation of mymin my max for the report
  
  mymin <- 1
  mymax <- 24
  
  
  ####### ACROSS PLATFORM R1:  mean difference (ref WEBEXPO) relative to within variation in WEBEXPO 
  
  #######résultats pour MU
  
  # initialisation
  res.mu <- data.frame(
    
    quantile.label=rep( mylist[[mymin]]$rjags$quantile[1:9] , 2) ,
    
    r1 = c( abs(100*(mylist[[mymin]]$rjags$mean[1:9] - mylist[[mymin]]$web$mean[1:9])/ mylist[[mymin]]$web$sd[1:9]) ,
            abs(100*(mylist[[mymin]]$sharp$mean[1:9] - mylist[[mymin]]$web$mean[1:9])/ mylist[[mymin]]$web$sd[1:9]) ),
    
    model = rep( c("jags","sharp") , c(9,9)) ,
    
    sample = rep (paste("sample", mymin , sep="") , 18) ,
    
    stringsAsFactors = FALSE
    
  ) 
  
  # échantillons 2 à 24
  for (i in (mymin+1):mymax) {
    
    
    res.mu <- rbind( res.mu ,
                     
                     data.frame(
                       
                       quantile.label=rep( mylist[[i]]$rjags$quantile[1:9] , 2) ,
                       
                       r1 = c( abs(100*(mylist[[i]]$rjags$mean[1:9] - mylist[[i]]$web$mean[1:9])/ mylist[[i]]$web$sd[1:9]) ,
                               abs(100*(mylist[[i]]$sharp$mean[1:9] - mylist[[i]]$web$mean[1:9])/ mylist[[i]]$web$sd[1:9]) ),
                       
                       model = rep( c("jags","sharp") , c(9,9)) ,
                       
                       sample = rep (paste("sample", i , sep="") , 18) ,
                       
                       stringsAsFactors = FALSE
                       
                     ) )
    
  }
  
  
  ## finalisation du data.frame
  res.mu$dist <- c( rep("LogN" , 108) , rep("N" , 108) )
  
  res.mu$quantile.label <- ordered(res.mu$quantile.label, 
                                   levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
  
  
  
  ##creation de résultats numérique pour exportation
  
  res.num <- list(2)
  
  res.num$mu <- list(2)
  
  res.num$mu$r1 <- res.mu
  
  
  ## graphique
  p1 <-ggplot(data = res.mu , aes ( y = r1 , x = quantile.label , color = model  , shape = dist )) + scale_shape_manual(values=c(1, 3)) +  geom_jitter(width = 0.1, height = 0)
  
  p1 <- p1 + geom_point()
  
  p1 <- p1 + ylab("R1 (%)") + ggtitle("MU")
  p1 <- p1 + geom_hline(yintercept = 100)
  
  
  
  
  #######SIGMA
  
  # initialisation
  res.sd <- data.frame(
    
    quantile.label=rep( mylist[[mymin]]$rjags$quantile[10:18] , 2) ,
    
    r1 = c( abs(100*(mylist[[mymin]]$rjags$mean[10:18] - mylist[[mymin]]$web$mean[10:18])/ mylist[[mymin]]$web$sd[10:18]) ,
            abs(100*(mylist[[mymin]]$sharp$mean[10:18] - mylist[[mymin]]$web$mean[10:18])/ mylist[[mymin]]$web$sd[10:18]) ),
    
    model = rep( c("jags","sharp") , c(9,9)) ,
    
    sample = rep (paste("sample", mymin , sep="") , 18) ,
    
    stringsAsFactors = FALSE
    
  ) 
  
  # échantillons 2 à 24
  for (i in (mymin+1):mymax) {
    
    
    res.sd <- rbind( res.sd ,
                     
                     data.frame(
                       
                       quantile.label=rep( mylist[[i]]$rjags$quantile[10:18] , 2) ,
                       
                       r1 = c( abs(100*(mylist[[i]]$rjags$mean[10:18] - mylist[[i]]$web$mean[10:18])/ mylist[[i]]$web$sd[10:18]) ,
                               abs(100*(mylist[[i]]$sharp$mean[10:18] - mylist[[i]]$web$mean[10:18])/ mylist[[i]]$web$sd[10:18]) ),
                       
                       model = rep( c("jags","sharp") , c(9,9)) ,
                       
                       sample = rep (paste("sample", i , sep="") , 18) ,
                       
                       stringsAsFactors = FALSE
                       
                     ) )
    
  }
  
  ## finalisation du data.frame
  res.sd$dist <- c( rep("LogN" , 108) , rep("N" , 108) )
  
  res.sd$quantile.label <- ordered(res.sd$quantile.label, 
                                   levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
  
  
  
  ##creation de résultats numérique pour exportation
  
  res.num$sigma <- list(2)
  
  res.num$sigma$r1 <- res.sd
  
  
  ## graphique
  p2 <-ggplot(data = res.sd , aes ( y = r1 , x = quantile.label , color = model , shape = dist )) + scale_shape_manual(values=c(1, 3)) +  geom_jitter(width = 0.1, height = 0)
  
  p2 <- p2 + geom_point()
  
  p2 <- p2 + ylab("R1 (%)") + ggtitle( "SIGMA" )
  p2 <- p2 + geom_hline(yintercept = 100)
  
  
  
  
  
  ############ WITHIN PLATFORM R2: ratio of within platform SD to the REF SD across 50 iterations                  
  
  
  #######MU
  
  # initialisation
  res.mu <- data.frame(
    
    quantile.label=rep( mylist[[mymin]]$rjags$quantile[1:9] , 2) ,
    
    r2 = c( mylist[[mymin]]$rjags$sd[1:9] /  mylist[[mymin]]$web$sd[1:9]  ,
            mylist[[mymin]]$sharp$sd[1:9] /  mylist[[mymin]]$web$sd[1:9] ),
    
    model = rep( c("jags","sharp") , c(9,9)) ,
    
    sample = rep (paste("sample", mymin , sep="") , 18) ,
    
    stringsAsFactors = FALSE
    
  ) 
  
  # échantillons 2 à 24
  for (i in (mymin+1):mymax) {
    
    
    res.mu <- rbind( res.mu ,
                     
                     data.frame(
                       
                       quantile.label=rep( mylist[[i]]$rjags$quantile[1:9] , 2) ,
                       
                       r2 = c( mylist[[i]]$rjags$sd[1:9] /  mylist[[i]]$web$sd[1:9]  ,
                               mylist[[i]]$sharp$sd[1:9] /   mylist[[i]]$web$sd[1:9] ),
                       
                       model = rep( c("jags","sharp") , c(9,9)) ,
                       
                       sample = rep (paste("sample", i , sep="") , 18) ,
                       
                       stringsAsFactors = FALSE
                       
                     ) )
    
  }
  
  
  ## finalisation du data.frame
  res.mu$dist <- c( rep("LogN" , 108) , rep("N" , 108) )
  
  res.mu$quantile.label <- ordered(res.mu$quantile.label, 
                                   levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
  
  
  ##creation de résultats numérique pour exportation
  
  res.num$mu$r2 <- res.mu
  
  
  ## graphique
  p3 <-ggplot(data = res.mu , aes ( y = r2 , x = quantile.label , color = model  , shape = dist )) + scale_shape_manual(values=c(1, 3)) +  geom_jitter(width = 0.1, height = 0)
  
  p3 <- p3 + geom_point() + ylim(0,3)
  
  p3 <- p3 + ylab("R2") + ggtitle( "MU" )
  p3 <- p3 + geom_hline(yintercept = 1)
  
  
  
  
  #######SIGMA
  
  # initialisation
  res.sd <- data.frame(
    
    quantile.label=rep( mylist[[mymin]]$rjags$quantile[10:18] , 2) ,
    
    r2 = c( mylist[[mymin]]$rjags$sd[10:18] /  mylist[[mymin]]$web$sd[10:18]  ,
            mylist[[mymin]]$sharp$sd[10:18] /   mylist[[mymin]]$web$sd[10:18] ),
    
    model = rep( c("jags","sharp") , c(9,9)) ,
    
    sample = rep (paste("sample", mymin , sep="") , 18) ,
    
    stringsAsFactors = FALSE
    
  ) 
  
  # échantillons 2 à 24
  for (i in (mymin+1):mymax) {
    
    
    res.sd <- rbind( res.sd ,
                     
                     data.frame(
                       
                       quantile.label=rep( mylist[[i]]$rjags$quantile[10:18] , 2) ,
                       
                       r2 = c( mylist[[i]]$rjags$sd[10:18] /  mylist[[i]]$web$sd[10:18]  ,
                               mylist[[i]]$sharp$sd[10:18] /   mylist[[i]]$web$sd[10:18] ),
                       
                       model = rep( c("jags","sharp") , c(9,9)) ,
                       
                       sample = rep (paste("sample", i , sep="") , 18) ,
                       
                       stringsAsFactors = FALSE
                       
                     ) )
    
  }
  
  ## finalisation du data.frame
  res.sd$dist <- c( rep("LogN" , 108) , rep("N" , 108) )
  
  res.sd$quantile.label <- ordered(res.sd$quantile.label, 
                                   levels = c("Q2.5%","Q5%","Q10%","Q25%","Q50%","Q75%","Q90%","Q95%","Q97.5%"))
  
  ##creation de résultats numérique pour exportation
  
  res.num$sigma$r2 <- res.sd
  
  ## graphique
  p4 <-ggplot(data = res.sd , aes ( y = r2 , x = quantile.label , color = model , shape = dist )) + scale_shape_manual(values=c(1, 3)) +  geom_jitter(width = 0.1, height = 0)
  
  p4 <- p4 + geom_point() + ylim(0,3)
  
  p4 <- p4 + ylab("R2") + ggtitle( "SIGMA")
  p4 <- p4 + geom_hline(yintercept = 1)
  
  
  
  
  return(list( r1= list(mu=p1,sigma=p2),
               r2= list(mu=p3,sigma=p4),
               num = res.num ))
  
}
    
seg.resultall.fun <- function(mylist, label, incljava = T) {
  foo <- fun.seg.resultall(mylist, label)
  return(foo$r1$mu)
}

cmpAbsDiff <- function(mylist) {
  sampInfo <- readRDS(here('ref.SEGsamples.RDS'))
  all.absdiff <- data.frame()
  for ( i in 1:length(sampInfo) ) {
    dist <- sampInfo[[i]]$distribution
    res <- mylist[[i]]
    lemay <- res$lemay
    csharp <- res$cs_quant
    absdiff <- cbind(i, csharp$param, csharp$type, dist, abs(lemay[-c(1:2),3:11] - csharp[,3:11]))
    all.absdiff <- rbind(all.absdiff, absdiff)
  }
  rownames(all.absdiff) <- 1:nrow(all.absdiff)
  colnames(all.absdiff) <- c("samp", "param", "type", "dist", paste0('q', 1:9))
  return(all.absdiff)
}