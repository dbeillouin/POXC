#POXC metaanalysis based on 2.2024 POXC file
#Authors: Cécile Chéron-Bessou, Damien Beillouin, Alexis Thoumazeau, Lydie Chapuis-Lardy, Tiphaine Chevallier, Julien Demenois, Paul N Nelson

############################
###########  Analysis
############################


# Load data
TAB <- POXC_meta

# load packages
library(broom) # Pour utiliser la fonction tidy
library(broom.mixed) # pour utiliser broom et metafor
library(magrittr)  # pour les pipes

workers = parallel::detectCores()-1
future::plan(future::multisession(workers= parallel::detectCores()-1))

## Choose grouping variables
# par exemple si on veux des analyses par profondeur de sol et par type d'intervention
Grouping_var<- c("Cat_Intervention","Depth")

### suite du code à ajuster/adapter. J'ai pas encore testé....

DATABASE <-TAB                                            %>%
  tidyr::nest(data= -all_of(Grouping_var))%>%
  dplyr::mutate(n_ES   = furrr::future_map_dbl(data, nrow),
                n_art   = furrr::future_map_dbl(data, ~{length(unique(.x$biblio_title))}))

DATABASE_plu<-DATABASE %>% dplyr::filter(n_ES>1)
DATABASE_un<-DATABASE %>% dplyr::filter(n_ES<=1)

######  For combination with >2 ES, we report the value ####
#
# DATABASE_un <- DATABASE_un %>%
#   dplyr::mutate(
#     estimate        = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`)),
#     conf.low         = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`- sqrt(.x$vi)*1.96)),
#     conf.high        = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`+sqrt(.x$vi)*1.96)))
#

###### Two level meta-analytical model. ####
DATABASE_two <- DATABASE_plu %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fit_2_lvl = furrr::future_map(data, ~
                                    tryCatch({
                                      mod  <-metafor::rma.mv(`Effect size`,
                                                             vi,data=.x,
                                                             random=~1|NUM,
                                                             method="REML",
                                                             control=list(optimizer="optim",
                                                                          optmethod="BFGS"))
                                    }, error=function(e){
                                      mod  <- metafor::rma.mv(`Effect size`,
                                                              vi,
                                                              data=.x,
                                                              random=~1|NUM,
                                                              method="REML")
                                    }),.progress = TRUE),
    tidied = purrr::map(fit_2_lvl, broom.mixed::tidy,conf.int = TRUE),
    Weights = purrr::map(fit_2_lvl, ~ weights(.x)),
    AIC = purrr::map(fit_2_lvl, ~ AIC(.x))) %>%
  tidyr::unnest(tidied) %>%
  dplyr::select(-c("term","type" )) %>%
  dplyr::rename_with(~ paste0(., "_m1.2lvl"), -c(all_of(Grouping_var), "n_ES", "n_art","data"))



###### Classical three level meta-analytical model. ####
DATABASE_Three <- DATABASE_plu %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fit_3_lvl = furrr::future_map(data, ~
                                    tryCatch({
                                      mod  <-metafor::rma.mv(`Effect size`,
                                                             vi,data=.x,
                                                             random=~1|ID/NUM,
                                                             method="REML",
                                                             control=list(optimizer="optim",
                                                                          optmethod="BFGS"))
                                    }, error=function(e){
                                      mod  <- metafor::rma.mv(`Effect size`,
                                                              vi,
                                                              data=.x,
                                                              random=~1|ID/NUM,
                                                              method="REML")}),.progress = TRUE),
    tidied = purrr::map(fit_3_lvl, tidy,conf.int = TRUE),
    Weights = purrr::map(fit_3_lvl, ~ weights(.x)),
    AIC = purrr::map(fit_3_lvl, ~ AIC(.x))) %>%
  tidyr::unnest(tidied) %>%
  dplyr::select(-c("term","type" )) %>%
  dplyr::rename_with(~ paste0(., "_m2.3lvl"), -c(Grouping_var, "n_ES", "n_art","data"))

#
# ##### Quality two random effect meta-analytical model. ####
#
# DATABASE_Two_Q <- DATABASE_plu %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     fit_2_lvlQ = furrr::future_map(data, ~
#                                      tryCatch({
#                                        mod  <-metafor::rma.mv(`Effect size`,
#                                                               QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
#                                                               data=.x,
#                                                               random=~1|NUM,
#                                                               method="REML",
#                                                               control=list(optimizer="optim",
#                                                                            optmethod="BFGS"))
#                                      }, error=function(e){
#                                        mod  <- metafor::rma.mv(`Effect size`,
#                                                                vi,
#                                                                data=.x,
#                                                                random=~1|NUM,
#                                                                method="REML")}),.progress = TRUE),
#     tidied = purrr::map(fit_2_lvlQ, tidy,conf.int = TRUE),
#     Weights = purrr::map(fit_2_lvlQ, ~ weights(.x)),
#     AIC = purrr::map(fit_2_lvlQ, ~ AIC(.x))) %>%
#   tidyr::unnest(tidied) %>%
#   dplyr::select(-c("term","type" )) %>%
#   dplyr::rename_with(~ paste0(., "_m3.2lvl2"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
#

#   GG<-DATABASE_plu[[4]][[2]]
# mod<-metafor::rma.mv(`Effect size`,
#                      vi,
#                      data=GG,
#                      random=~1|ID,
#                      method="REML",
#                      control=list(optimizer="optim",
#                                   optmethod="BFGS"))
#
# AIC(mod)
#
# metafor::rma.mv(`Effect size`,
#                 V=
#                   FUN_MAT_RED(
#                     Matrix_name        = "Global_Redundancy_matrix.xlsx",
#                     Selected_rows_ID   = GG$ID ,
#                     Variance           =  QUALITY_DOI(SCORE =GG$SCORE, vi= GG$vi)),
#                 data=GG,
#                 random=~1|ID/NUM,
#                 method="REML",
#                 control=list(optimizer="optim",
#                              optmethod="BFGS"),verbose=TRUE)
#


#
# DATABASE_Quality <- DATABASE_plu %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     fit_Q = furrr::future_map(data, ~
#                                 tryCatch({
#                                   mod  <-metafor::rma.mv(`Effect size`,
#                                                          QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
#                                                          data=.x,
#                                                          random=~1|ID/NUM,
#                                                          method="REML",
#                                                          control=list(optimizer="optim",
#                                                                       optmethod="BFGS"))
#                                 }, error=function(e){
#                                   mod  <-metafor::rma.mv(`Effect size`,
#                                                          QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
#                                                          data=.x,
#                                                          random=~1|ID/NUM,
#                                                          method="REML")
#                                 }) ,.progress = TRUE),
#     tidied_Q = purrr::map(fit_Q, tidy,conf.int = TRUE),
#     Weights = purrr::map(fit_Q, ~ weights(.x)),
#     AIC = purrr::map(fit_Q, ~ AIC(.x)))%>%
#   tidyr::unnest(tidied_Q) %>%
#   dplyr::select(-c("term","type" )) %>%
#   dplyr::rename_with(~ paste0(., "_m4.3lvlQ"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
#
#
# ######   Quality and redundancy meta-analytical model ####
# # We first create the global Redundancy matrix
#
# Global_Redundancy_matrix(File_name   = File_name,
#                          sheet_name  = "Primary_studies",
#                          ID          = "ID",
#                          DOI         = "DOI",
#                          FACTOR      = 1)
#
# DATABASE_Quality_Red <- DATABASE_plu %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     fit_QR = furrr::future_map(data, ~
#                                  tryCatch({
#                                    mod  <-metafor::rma.mv(`Effect size`,
#                                                           V=
#                                                             FUN_MAT_RED(
#                                                               Matrix_name        = "Global_Redundancy_matrix.xlsx",
#                                                               Selected_rows_ID   = .$ID ,
#                                                               Variance           =  QUALITY_DOI(SCORE =.$SCORE, vi= .$vi)),
#                                                           data=.x,
#                                                           random=~1|ID/NUM,
#                                                           method="REML",
#                                                           control=list(optimizer="optim",
#                                                                        optmethod="BFGS"))
#                                  }, error=function(e){
#                                    mod  <-metafor::rma.mv(`Effect size`,
#                                                           vi,
#                                                           data=.x,
#                                                           random=~1|ID/NUM,
#                                                           method="REML")
#                                  }),.progress = TRUE),
#     tidied_QR = purrr::map(fit_QR, tidy,conf.int = TRUE),
#     Weights = purrr::map(fit_QR, ~ weights(.x)),
#     AIC = purrr::map(fit_QR, ~ AIC(.x))) %>%
#   tidyr::unnest(tidied_QR) %>%
#   dplyr::select(-c("term","type" )) %>%
#   dplyr::rename_with(~ paste0(., "_m5.3lvl.QR"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
#
# ####   Quality  two random effect model and redundancy meta-analytical model ####
# DATABASE_Quality_2lvl_Red <- DATABASE_plu %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     fit_2lvl_QR = furrr::future_map(data, ~
#                                       tryCatch({
#                                         mod  <-metafor::rma.mv(`Effect size`,
#                                                                V=
#                                                                  FUN_MAT_RED(
#                                                                    Matrix_name        = "Global_Redundancy_matrix.xlsx",
#                                                                    Selected_rows_ID   = .$ID ,
#                                                                    Variance           =  QUALITY_DOI(SCORE =.$SCORE, vi= .$vi)),
#                                                                data=.x,
#                                                                random=~1|NUM,
#                                                                method="REML",
#                                                                control=list(optimizer="optim",
#                                                                             optmethod="BFGS"))
#                                       }, error=function(e){
#                                         mod  <-metafor::rma.mv(`Effect size`,
#                                                                vi,
#                                                                data=.x,
#                                                                random=~1|NUM,
#                                                                method="REML")
#                                       }),.progress = TRUE),
#     tidied_QR = purrr::map(fit_2lvl_QR, tidy,conf.int = TRUE),
#     Weights = purrr::map(fit_2lvl_QR, ~ weights(.x)),
#     AIC = purrr::map(fit_2lvl_QR, ~ AIC(.x))) %>%
#   tidyr::unnest(tidied_QR) %>%
#   dplyr::select(-c("term","type" )) %>%
#   dplyr::rename_with(~ paste0(., "_m6.2lvl.QR"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


######   linear meta-analytical model ####
# We first create the global Redundancy matrix

DATABASE_Linear <- DATABASE_plu %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fit_L = furrr::future_map(data, ~
                                tryCatch({
                                  mod  <-metafor::rma.uni(`Effect size`,
                                                          vi,
                                                          data=.x,
                                                          method="REML",
                                                          control=list(optimizer="optim",
                                                                       optmethod="BFGS"))
                                }, error=function(e){
                                  mod  <- metafor::rma.uni(`Effect size`,
                                                           vi,
                                                           data=.x,
                                                           method="REML")
                                }) ,.progress = TRUE),
    tidied = purrr::map(fit_L, tidy,conf.int = TRUE),
    Weights = purrr::map(fit_L, ~ weights(.x)),
    AIC = purrr::map(fit_L, ~ AIC(.x))) %>%
  tidyr::unnest(tidied) %>%
  dplyr::select(-c("term","type" )) %>%
  dplyr::rename_with(~ paste0(., "_m7.Lin"), -c(Grouping_var, "n_ES", "n_art","data"))


###### We add the rosenfail safe number and Trim and fill model #####
for (i in 1 : dim(DATABASE_Linear)[1]){
  DATABASE_Linear$Rosen[i]<-metafor::fsn(`Effect size`, vi, data=DATABASE_plu[[length(Grouping_var)+1]][[i]],type="Rosenberg")$fsnum
  DATABASE_Linear$TF[i]<-tryCatch({metafor::trimfill(DATABASE_Linear[[9]][[i]], control=list(stepadj=0.5))$b[,1][[1]]},
                                  error=function(e){'NA'})
  DATABASE_Linear$Egger[i]<-tryCatch({  (metafor::regtest(DATABASE_Linear[[9]][[i]], model="lm")$pval)},
                                     error=function(e){'NA'})
}

DATABASE_Linear$diffTF_lin<-(1-exp(DATABASE_Linear$estimate_m7.Lin))-(1-exp(as.numeric(DATABASE_Linear$TF)))
DATABASE_Linear$diffTF_lin<- DATABASE_Linear$diffTF_lin*100

EGGER<-DATABASE_Linear %>% dplyr::filter(as.numeric(Egger) < 0.05)

FINAL_freq<-plyr::join_all(list(DATABASE_two,DATABASE_Three,DATABASE_Linear),
                           by=c(Grouping_var, "n_art", "n_ES"), type='left')



## Identify model with best AIC
AIC<-FINAL_freq %>%
  dplyr::select(dplyr::contains("AIC")) %>%
  dplyr::select(-"AIC_m7.Lin")    %>%
  tidyr::unnest()

AIC$answer <- substr(names(AIC)[apply(AIC, MARGIN = 1, FUN = which.min)],5,19)

## Select estimates of the best model
AA<-NULL
for (i in 1 : dim(DATABASE_two)[1]){
  AA[[i]]<- FINAL_freq[i,] %>%
    dplyr::select(-c(data,n_ES)) %>%
    dplyr::select(contains(AIC$answer[i]))
  names(AA[[i]]) <- gsub("_.*","",names(AA[[i]]))
}

## Combine in a dataframe
BEST_fit<-cbind(DATABASE_two %>% dplyr::select(c(Grouping_var,"n_ES")),
                do.call(rbind, AA), AIC$answer)


# On assemble les effect sizes calculés (quand n_ES>2) et ceux quand n_ES=1:
FINAL_freq<-dplyr::bind_rows(FINAL_freq,DATABASE_un)
BEST_fit<-dplyr::bind_rows(BEST_fit,DATABASE_un)



rm(DATABASE_two,DATABASE_Three,DATABASE_Quality,DATABASE_Quality_Red,DATABASE_Linear,DATABASE_plu)
