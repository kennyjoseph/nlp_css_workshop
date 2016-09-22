library(data.table)
library(ggplot2)
library(mgcv)
library(xtable)

# library(GGally)
# library(ggrepel)
# library(stringr)
# library(fields)
# library(MASS)
# library(dplyr)
# library(stringi)
# 
# library(dHSIC)
# library(tidyr)
# library(visreg)

paper_loc <- "~/Dropbox/Kenny/papers/submitted/nlp_css/"
theme_set(theme_bw(20))

source("util.R")

# get the simlex basic identity data
simlex_basic <- get_simlex_basic()

# get survey data
survey_one_data <- get_survey_one_data()
min_survey_one_data <- get_minimal_survey_one_data(survey_one_data)
logistic_data <- get_logistic_data(min_survey_one_data,simlex_basic)

##########################
# CK we got answers to all questions
m <- unique(min_survey_one_data[,c("query","simlexpair"),with=F])
m$questiontype <- "SeenWith"
c <- unique(min_survey_one_data[,c("query","simlexpair"),with=F])
c$questiontype <- "IsA"
m <- rbind(m,c)
m <- merge(m, logistic_data[,c("query","simlexpair","questiontype","total"),with=F],by=c("query","simlexpair","questiontype"),all.x=T)
table(m$total)
####################
####################

sim_levels <- c("High Sim., High Assoc.","High Sim., Low Assoc.","Low Sim., High Assoc.","Low Sim., Low Assoc.")

min_survey_one_data$t <- with(min_survey_one_data, ifelse(SimLex999 > 0,
                                                          ifelse(association_sqrt > 0,sim_levels[1], sim_levels[2]),
                                                          ifelse(association_sqrt > 0, sim_levels[3],sim_levels[4])))
min_survey_one_data$t <- factor(min_survey_one_data$t,  levels=rev(sim_levels))
p1 <- ggplot(min_survey_one_data[,list(V1=log((sum(result)+1)/(nrow(.SD)-sum(result)+1)),V2=t[1]),
                                 by=c("query","simlexpair","questiontype")], aes(V2,V1)) 
p1 <- p1 + stat_summary(fun.data="mean_cl_boot") + scale_x_discrete("Metrics Above Mean")
p1 <- p1 + scale_y_continuous("LogOdds Pair Selection") + facet_wrap(~questiontype) + coord_flip()
p1 <- p1 + geom_hline(yintercept=0,color='red') 
p1 <- p1 + geom_hline(yintercept=log(1/5),color='blue',linetype='dashed')
ggsave(paste0(paper_loc,"high_level_res_no_affect.jpeg"),w=7,h=3)


### EDA
scatterp <- melt(logistic_data[,c("query","simlexpair","questiontype","association_sqrt","logodds","SimLex999"),with=F],id=c("query","simlexpair","questiontype","logodds"))
scatterp[variable == "association_sqrt"]$variable <- "Sqrt(Association)"
scatterp[variable == "SimLex999"]$variable <- "Similarity"
p <- ggplot(scatterp, aes(value,logodds)) + geom_point() + facet_grid(variable~questiontype) + ylab("Log Odds of Selection") + xlab("(Scaled and Centered) Similarity or Sqrt(Association)")
ggsave(paste0(paper_loc,"eda3.png"),height=7,width=8,p)

#################################
# Lets do some LR
run_gam <- function(qt){
  library(visreg)
  lg <- logistic_data[questiontype==qt]
  g <- gam((y+1)/(y+n+2)~ti(SimLex999)+ ti(association_sqrt)+ti(SimLex999,association_sqrt),family='binomial',weights=y+n+2,data=lg)
  print(summary(g))
  #pdf(paste0(paper_loc,qt,"_gam_sl.pdf"),h=5,w=5)
  visreg(g,"SimLex999",ylab="Partial Residual",xlab="Semantic Similarity",rug=T,partial=F)
  visreg(g,"association_sqrt",ylab="Partial Residual",xlab="Sqrt(Semantic Association)",rug=T,partial=F)
  visreg2d(g,"SimLex999","association_sqrt",xlab="Semantic Similarity",ylab="Sqrt(Semantic Association)", main="")
  lg$gv <- predict(g)
  lg$resid <- residuals(g,"response")
  #dev.off()
  return(lg)
}

gam_dat_isa <- run_gam("IsA")
gam_dat_seenwith <- run_gam("SeenWith")
fields_to_show <- c("query", "simlexpair", "gv","logodds")
print(xtable(arrange(gam_dat_seenwith[,c(fields_to_show,"association_sqrt"),with=F], gv -logodds)[1:10],include.rownames=F))


## Similarity is not symmetric 
l <- data.table(logistic_data)
l$query <- as.character(l$query)
l$simlexpair <- as.character(l$simlexpair)
l$w1 <- ifelse(l$query > l$simlexpair, l$query, l$simlexpair)
l$w2 <- ifelse(l$query < l$simlexpair, l$query, l$simlexpair)
l <- l[,as.list(logodds),by=c("w1","w2","questiontype")]

xtable(arrange(l[questiontype=='IsA',c("w1","w2","V1","V2"),with=F],-abs(V1-V2))[1:10])


logistic_data[,prob:=interp.surface(kde2d(SimLex999, association_sqrt),
                                    data.frame(x=SimLex999,y= association_sqrt))]
p <- ggplot(logistic_data,aes(SimLex999,
                              association_sqrt,size=logodds,
                              color=logodds,label=ifelse(prob < .04,paste(query,simlexpair),NA)))
p <- p + geom_point(alpha=.5) + facet_wrap(~questiontype)
p <- p + scale_color_gradientn("Log Odds", colors=c("red","grey","blue"),guide=guide_legend(nrow=1,keywidth=unit(1,"cm"))) + scale_size("Log Odds",range=c(1,10))
p <- p + geom_text_repel(size=4.5,color='black')
p <- p + ylab("Sqrt(Semantic Association)") + xlab("Semantic Similarity")
p <- p + theme(legend.position='bottom')
ggsave(paste0(paper_loc,"eda2.png"),height=7,width=11,p)

## Check robustness against not using sqrt
library(visreg)
par(mfrow=c(2,2))
g1 <- gam((y+1)/(y+n+2)~ti(SimLex999)+ ti(association)+ti(SimLex999,association),
          family='binomial',weights=y+n+2,
          data=logistic_data[questiontype=="SeenWith"])

visreg(g1,"SimLex999",ylab="Partial Residual",xlab="Semantic Similarity",rug=T,partial=F)
visreg(g1,"association",ylab="Partial Residual",xlab="Sqrt(Semantic Association)",rug=T,partial=F)

g2 <- gam((y+1)/(y+n+2)~ti(SimLex999)+ ti(association)+ti(SimLex999,association),
          family='binomial',weights=y+n+2,
          data=logistic_data[questiontype=="IsA"])

visreg(g2,"SimLex999",ylab="Partial Residual",xlab="Semantic Similarity",rug=T,partial=F)
visreg(g2,"association",ylab="Partial Residual",xlab="Sqrt(Semantic Association)",rug=T,partial=F)



## Check robustness against removing zero-scored association
library(visreg)
par(mfrow=c(2,2))
g1 <- gam((y+1)/(y+n+2)~ti(SimLex999)+ ti(association_sqrt)+ti(SimLex999,association_sqrt),
          family='binomial',weights=y+n+2,
          data=logistic_data[questiontype=="SeenWith" & association_raw > 0])

visreg(g1,"SimLex999",ylab="Partial Residual",xlab="Semantic Similarity",rug=T,partial=F)
visreg(g1,"association_sqrt",ylab="Partial Residual",xlab="Sqrt(Semantic Association)",rug=T,partial=F)

g2 <- gam((y+1)/(y+n+2)~ti(SimLex999)+ ti(association_sqrt)+ti(SimLex999,association_sqrt),
          family='binomial',weights=y+n+2,
          data=logistic_data[questiontype=="IsA" & association_raw > 0])

visreg(g2,"SimLex999",ylab="Partial Residual",xlab="Semantic Similarity",rug=T,partial=F)
visreg(g2,"association_sqrt",ylab="Partial Residual",xlab="Sqrt(Semantic Association)",rug=T,partial=F)


