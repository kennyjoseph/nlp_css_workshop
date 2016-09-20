get_simlex_basic <- function(scale_data=T){
  library(data.table)
  # simlex identity data
  simlex <- fread("nlp_data/all_identities_simlex.txt")
  setnames(simlex,"conc(w1)","concr_w1")
  setnames(simlex,"conc(w2)","concr_w2")
  
  ##### Get free association data #########
  free_assoc <- fread("nlp_data/usf_norms_data.tsv")
  
  
  assoc_data <- rbindlist(apply(simlex,1, function(f){
                                      u1 <- toupper(f['word1'])
                                      u2 <- toupper(f['word2'])
                                      x1 <- free_assoc[V1 ==u1 & V2 == u2]
                                      x2 <- free_assoc[V1 ==u2 & V2 == u1]
                                      a <- ifelse(nrow(x1) == 0, 0, x1$V4/x1$V5)
                                      b <- ifelse(nrow(x2) == 0, 0, x2$V4/x2$V5)
                                      return(data.frame(word1=f['word1'],word2=f['word2'],assoc12=a,assoc21=b))
                                      })
                                    )
  
  simlex_basic <- merge(simlex,assoc_data,by=c("word1","word2"))
  
  # scale simlex on its own before combining both "sides" of each pair
  if(scale_data){
    simlex_basic$SimLex999 <- scale(simlex_basic$SimLex999)
  }
  simlex_basic$type <- 'ONE'
  simlex_basic2 <- data.table(simlex_basic)
  simlex_basic2$type <- 'TWO'
  simlex_basic$association <- simlex_basic$assoc12
  simlex_basic$assoc12 <- NULL
  simlex_basic$assoc21 <- NULL
  
  simlex_basic2$association <- simlex_basic2$assoc21
  setnames(simlex_basic2, c("word1","word2","concr_w1","concr_w2"), 
                          c("word2","word1","concr_w2","concr_w1"))
  simlex_basic2$assoc12 <- NULL
  simlex_basic2$assoc21 <- NULL
  
  simlex_basic <- rbind(simlex_basic,simlex_basic2)
  if(scale_data){
    simlex_basic$association_raw <- simlex_basic$association
    simlex_basic$association_sqrt <- scale(sqrt(simlex_basic$association))
    simlex_basic$association <- scale(simlex_basic$association)
    simlex_basic$concr_w1 <- scale(simlex_basic$concr_w1)
    simlex_basic$concr_w2 <- scale(simlex_basic$concr_w1)
  }
  return(simlex_basic)
}


inner_get_survey_one_data <- function(filename_ending="_dat",post_processed_dir){
  k <- fread(file.path(post_processed_dir,paste0("labeling_dat",filename_ending,".csv")))
  k$V1 <- NULL
  q <- fread(file.path(post_processed_dir,paste0("question_dat",filename_ending,".csv")))
  q$V1 <- NULL
  d <- fread(file.path(post_processed_dir,paste0("demographics_dat",filename_ending,".csv")))
  d$V1 <- NULL
  f <- merge(q,k,by="QuestionID")
  f <- merge(f, d,by="ResponseID")
  f$EmbeddedData <- as.character(f$EmbeddedData)
  me_rock <- c("33.79248451297", "17.2594844224536")
  f <- f[! EmbeddedData %in% me_rock]
  return(f)
}

get_survey_one_data <- function(post_processed_dir="survey_data/"){
  s1_data <- inner_get_survey_one_data("_first_run", post_processed_dir)
  s1_data$survey <- 1
  s2_data <- inner_get_survey_one_data("_second_run", post_processed_dir)
  s2_data$survey <- 2
  s3_data <- inner_get_survey_one_data("_third_run", post_processed_dir)
  s3_data$survey <- 3
  survey_data <- rbind(s1_data,s2_data,s3_data)
  setnames(survey_data,tolower(names(survey_data)))
  return(survey_data)
}

get_minimal_survey_one_data <- function(survey_data){
  data <- survey_data[,c("query","simlexpair","answer","questiontype","responseid","questionid"),with=F]
  data <- merge(data,simlex_basic, by.x=c("query","simlexpair"),by.y=c("word1","word2"))
  data$result <- data$answer==data$simlexpair
  data$questiontype <- factor(data$questiontype)
  return(data)
}

get_logistic_data <- function(data,simlex_basic){
  logistic_data <- data[, list(y=sum(simlexpair==answer),n=sum(simlexpair!=answer)),by=c("query","simlexpair","questiontype")]
  logistic_data <- merge(logistic_data,simlex_basic, by.x=c("query","simlexpair"),by.y=c("word1","word2"))
  logistic_data <- logistic_data[,total:=y+n]
  logistic_data[,logodds:=log((y+1)/(n+1))]
  logistic_data$SimLex999 <- as.vector(logistic_data$SimLex999)
  logistic_data$association <- as.vector(logistic_data$association)
  logistic_data$association_sqrt <- as.vector(logistic_data$association_sqrt)
  return(logistic_data)
}


