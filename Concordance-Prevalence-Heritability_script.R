##################################################################################
#install "ggplot2" package with: install.packages("ggplot2")
library (ggplot2)

#################################################################################
#formula from Smith 1970 for the relation between concordance, prevalence, relatives' resemblance, and heritability.
#log10("concordance rate")/log10("prevalence")=(tan((pi/4)*(1-"resemblance"*"heritability")*(1+("resemblance"*"heritability")^5))
#
#to calculate concordance in monozygotic twins("resemblance"=1) the formula can be transformed as:
#10^(tan((pi/4)*(1-"heritability")*(1+"heritability"^5))*log10("prevalence"))="concordance rate"
#
###########################################################################################
#drawing the plot

#the range of the values for heritability (%) from 0 to 97 to draw the expected concordance functions for the range of prevalences
concordance_heritability<- ggplot(data.frame(x = c(0,0.97))) +

#the functions of the expected concordance based on the formula from 
#Smith (1970) for prevalences (%):0.01,0.1,1,5,10,20,50,80.
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.0001))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.001))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.01))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.05))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.1))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.2))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.5))}) +
  stat_function(fun = function(x){10^(tan((pi/4)*(1-x)*(1+x^5))*log10(0.8))}) +

#annotations for the specified functions  
  annotate(geom="text", label = "Prevalence (%)", x = 0.2, y = 0.70,size =3.5) + 
  annotate(geom="text", label = "0.01", x = 0.71, y = 0.115, size =3.5) +
  annotate(geom="text", label = "0.1", x = 0.71, y = 0.19, size =3.5) + 
  annotate(geom="text", label = "1.0", x = 0.71, y = 0.325, size =3.5) + 
  annotate(geom="text", label = "5", x = 0.71, y = 0.46, size =3.5) + 
  annotate(geom="text", label = "10", x = 0.71, y = 0.57, size =3.5) +
  annotate(geom="text", label = "20", x = 0.71, y = 0.68, size =3.5) + 
  annotate(geom="text", label = "50", x = 0.71, y = 0.85, size =3.5) +
  annotate(geom="text", label = "80", x = 0.71, y = 0.96, size =3.5)+

#adding points, lines, and labels to the plot
# Type 2 diabetes
# each point of the plot has coordinates (x:y)
# x equal reported in the supplementary table heritability = 60%
# to get y (concordance rate) we used the Smith (1970) formula:
# 10^(tan((pi/4)*(1-"heritability")*(1+"heritability"^5))*log10("prevalence"))="concordance rate"
# to calculate y, we add the reported prevalence equal to 10.5% and heritability of 60% to Smith's (1970) formula 
  geom_point(aes(x=0.6,y=(10^(tan((pi/4)*(1-0.60)*(1+0.60^5))*log10(0.105)))),colour="#f47721")+
  geom_segment(x=0.6,y=(10^(tan((pi/4)*(1-0.60)*(1+0.60^5))*log10(0.105))), 
               xend=0.60,yend=0,colour="#f47721",linetype="dashed")+
  geom_segment(x=0.6,y=(10^(tan((pi/4)*(1-0.60)*(1+0.60^5))*log10(0.105))),
               xend=0,yend=(10^(tan((pi/4)*(1-0.60)*(1+0.60^5))*log10(0.105))),
               colour="#f47721",linetype="dashed")+
  geom_label(
    label="Type 2 diabetes (45%)", 
    x = 0.43, y = (10^(tan((pi/4)*(1-0.60)*(1+0.60^5))*log10(0.105))+.03),colour="#f47721",
    show.legend = FALSE,fontface = "bold", label.size=0, size=3)+
  
#Rheumatoid arthritis
# x equal reported in the supplementary table heritability = 57%
# to calculate y, we add the reported prevalence equal to 0.46% and heritability of 57% to Smith's (1970) formula
  geom_point(aes(x=0.6,y=(10^(tan((pi/4)*(1-0.57)*(1+0.57^5))*log10(0.0046)))),colour="#006c7b")+
  geom_segment(x=0.6,y=(10^(tan((pi/4)*(1-0.57)*(1+0.57^5))*log10(0.0046))), 
               xend=.6,yend=0,colour="#006c7b",linetype="dashed")+
  geom_segment(x=0.6,y=(10^(tan((pi/4)*(1-0.57)*(1+0.57^5))*log10(0.0046))),
               xend=0,yend=(10^(tan((pi/4)*(1-0.57)*(1+0.57^5))*log10(0.0046))),
               colour="#006c7b",linetype="dashed")+
  geom_label(
    label="Rheumatoid arthritis (13%)", 
    x = 0.32, y = (10^(tan((pi/4)*(1-0.57)*(1+0.57^5))*log10(0.0046))+0.03),
    colour="#006c7b",show.legend = FALSE,fontface = "bold", label.size=0, size=3)+
  
#Alzheimer’s disease
# x equal reported in the supplementary table heritability = 79%
# to calculate y, we add the reported prevalence equal to 7% and heritability of 79% to Smith's (1970) formula
  geom_point(aes(x=0.80,y=(10^(tan((pi/4)*(1-0.79)*(1+0.79^5))*log10(0.07)))),colour="#147bb6")+
  geom_segment(x=0.80,y=(10^(tan((pi/4)*(1-0.79)*(1+0.79^5))*log10(0.07))), 
               xend=0.80,yend=0,colour="#147bb6",linetype="dashed")+
  geom_segment(x=0.80,y=(10^(tan((pi/4)*(1-0.79)*(1+0.79^5))*log10(0.07))),
               xend=0,yend=(10^(tan((pi/4)*(1-0.79)*(1+0.79^5))*log10(0.07))),
               colour="#147bb6",linetype="dashed")+
  geom_label(
    label="Alzheimer’s disease (55%)", 
    x = 0.38, y = (10^(tan((pi/4)*(1-0.79)*(1+0.79^5))*log10(0.07))+0.025),
    colour="#147bb6",show.legend = FALSE,fontface = "bold", label.size=0, size=3)+
  
#Schizophrenia
# x equal reported in the supplementary table heritability for same sex twins = 80%
# to calculate y, we add the reported mean prevalence equal to 0.54% and heritability of 80% to Smith's (1970) formula
  geom_point(aes(x=0.79,y=(10^(tan((pi/4)*(1-0.80)*(1+0.80^5))*log10(0.0054)))),colour="#002960")+
  geom_segment(x=0.79,y=(10^(tan((pi/4)*(1-0.80)*(1+0.80^5))*log10(0.0054))), 
               xend=0.79,yend=0,colour="#002960",linetype="dashed")+
  geom_segment(x=0.79,y=(10^(tan((pi/4)*(1-0.80)*(1+0.80^5))*log10(0.0054))),
               xend=0,yend=(10^(tan((pi/4)*(1-0.80)*(1+0.80^5))*log10(0.0054))),
               colour="#002960",linetype="dashed")+
  geom_label(
    label="Schizophrenia (33%)", 
    x = 0.60, y = (10^(tan((pi/4)*(1-0.80)*(1+0.80^5))*log10(0.0054))+0.03),
    colour="#002960",show.legend = FALSE,fontface = "bold", label.size=0, size=3)+
  
#Breast cancer
# x equal reported in the supplementary table heritability = 33%
# to calculate y, we add the reported prevalence equal to 0.48% and heritability of 33% to Smith's (1970) formula
  geom_point(aes(x=0.33,y=(10^(tan((pi/4)*(1-0.33)*(1+0.33^5))*log10(0.0048)))),colour="#813006")+
  geom_segment(x=0.33,y=(10^(tan((pi/4)*(1-0.33)*(1+0.33^5))*log10(0.0048))), 
               xend=0.33,yend=0,colour="#813006",linetype="dashed")+
  geom_segment(x=0.33,y=(10^(tan((pi/4)*(1-0.33)*(1+0.33^5))*log10(0.0048))),
               xend=0,yend=(10^(tan((pi/4)*(1-0.33)*(1+0.33^5))*log10(0.0048))),
               colour="#813006",linetype="dashed")+
  geom_label(
    label="Breast cancer (4%)", 
    x = 0.20, y = (10^(tan((pi/4)*(1-0.33)*(1+0.33^5))*log10(0.0048))+0.03),
    colour="#813006",show.legend = FALSE,fontface = "bold", label.size=0, size=3)+

#specify the outline for axis y and x
  ylab('Concordance rate (%)') + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0),breaks = c(0.20,0.40,0.60,0.80,1),
                     labels = scales::percent_format(accuracy = 1)) + 
  xlab('Heritability of liability (%)') + 
  scale_x_continuous(limits = c(0, 0.97), expand = c(0, 0), breaks = c(0.20,0.40,0.60,0.80,0.97),
                     position = 'bottom',labels = scales::percent_format(accuracy = 1))+ 
  geom_hline(yintercept=0,size = 1)+
  geom_vline(xintercept = 0,size = 1)+
  # geom_hline(yintercept=0.2,size = 0.5,linetype="dotted", color = "black")+
  # geom_hline(yintercept=0.4,size = 0.5,linetype="dotted", color = "black")+
  # geom_hline(yintercept=0.6,size = 0.5,linetype="dotted", color = "black")+
  # geom_hline(yintercept=0.8,size = 0.5,linetype="dotted", color = "black")+
  # geom_hline(yintercept=1,size = 0.5,linetype="dotted", color = "black")+
  theme(axis.text = element_text(color = "black") )+
  theme(axis.title = element_text(color = "black",size=10) )+
  
#specify the outline for the plot
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.margin = margin(0.25,0.35,0.25,0.25, "cm"))
#represent the plot
  concordance_heritability


  #save the plot
  ggsave("concordance_heritability_88_110_mm.tiff",dpi=300)



