###POXC metaanalysis based on 2.2024 POXC file
#Authors: Cécile Chéron-Bessou, Damien Beillouin, Alexis Thoumazeau, Lydie Chapuis-Lardy, Tiphaine Chevallier, Julien Demenois, Paul N Nelson

############################
########### 1. PLot de vérification des données
############################



### Plot 1 ####
AA<-POXC_meta
AA$percent<-(exp(AA$yi)-1)*100
names(AA)

summary(AA$percent)

histogram<-ggplot2::ggplot(AA,
                           ggplot2::aes(x = !!dplyr::sym('percent'))) +
  ggplot2::geom_histogram(ggplot2::aes(y=..density..),fill= "white", color="black",binwidth=0.1) +
  ggplot2::geom_density(binwidth=1)+
  ggplot2::scale_fill_viridis_c(name = "Effect_size", option = "C") +
  ggplot2::theme(axis.title.y = ggplot2::element_blank())+
  ggpubr::theme_pubr()+
  ggplot2::geom_vline(ggplot2::aes(xintercept=0), linetype=2)
  #ggplot2::facet_wrap(~Land_use, scales= "free")

histogram

#### -> Des valeurs très élevées: a vérifier

##### Plot 2 ####

Variance_All_metric<-ggplot(aes(yi,vi),data=AA)+
  ggplot2::geom_point()+
  ggpubr::theme_pubr()
 # ggplot2::facet_wrap(~metric,scales="free")
# Variance_All_metric<-plotly::ggplotly(Variance_All_metric)

#### -> Des valeurs très élevées: a vérifier

##### Plot 3 ####
AA<- POXC_meta %>%
  dplyr::ungroup() %>%
  dplyr::group_by(!!dplyr::sym('Cat_Intervention')) %>%
 # dplyr::arrange(!!dplyr::sym(Intervention), !!dplyr::sym(Name_Col_Outcome), !!dplyr::sym(Effect_size)) %>%
  dplyr::mutate(positionInCategory = 1:dplyr::n(),
                order = dplyr::row_number())


# calcul paramétriques des CI
z_value <- qnorm((1 + 0.95) / 2)
margin_of_error <- z_value * sqrt(AA$vi)
AA$lower_CI <- AA$yi - margin_of_error
AA$upper_CI <- AA$yi + margin_of_error


forestplot<-ggplot2::ggplot(AA)+
  ggplot2::geom_point(ggplot2::aes(reorder(order, !!dplyr::sym('yi')), !!dplyr::sym('yi'), color = !!dplyr::sym('Cat_Intervention')))+
  ggplot2::geom_hline(ggplot2::aes(yintercept= 0), linetype=2, color="gray")+
  ggplot2::geom_errorbar(ggplot2::aes(reorder(order, !!dplyr::sym('yi')), !!dplyr::sym('yi'),
                                      ymin = lower_CI, ymax = upper_CI), width = 0.2, position = ggplot2::position_dodge(.9))+
  ggpubr::theme_pubr() +
  ggplot2::theme(legend.position='none')
forestplot
# dynamic plot
# forestplot<-plotly::ggplotly(forestplot)


#### -> Toujours qq valeurs bizarres à vérifier


##### Plot 4 ####
varianceplot<-ggplot2::ggplot(AA)+
  ggplot2::geom_point(ggplot2::aes(!!dplyr::sym('yi'), log(vi), color = !!dplyr::sym('Cat_Intervention'), size = as.numeric(Treat_Rep),fill=POXC_ID))+
  ggplot2::geom_hline(ggplot2::aes(yintercept= 0), linetype=2, color="gray")+
  ggpubr::theme_pubr()+
  ggplot2::theme(legend.position='none')+
  ggplot2::scale_size(range = c(0, 3))
varianceplot
# dynamic plot
varianceplot<-plotly::ggplotly(varianceplot)
