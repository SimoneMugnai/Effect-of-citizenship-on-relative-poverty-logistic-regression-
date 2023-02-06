library(Hmisc)
library(corrplot)
library(sjPlot)
library(psych)
library(dplyr)
library(ggplot2)
library(lmtest)
library(estimatr)
library(lmtest)
library(generics)
library(mfx)
setwd("C:/Users/simon/OneDrive/Desktop/tesi/Data")
getwd()

h1 <- read.csv("H_Spending.csv", sep = ";")
na.omit(h1)

#selection variables and modification of cittad_1
h2 <-  h1 %>% 
  mutate(cittad_1 = ifelse(h1$cittad_1 == 1,1,0),
         pnasc_padre_1 = ifelse(h1$pnasc_padre_1 == 1,1,0),
         pnasc_madre_1 = ifelse(h1$pnasc_madre_1 == 1,1,0),
         pnasc_1 = ifelse(h1$pnasc_1 == 1,1,0)) %>%
  dplyr::select(poveri,c_Ncmp_fatto,pnasc_1, sesso_1,c_c_etacalc_1,cittad_1,
                staciv_1,pnasc_padre_1,pnasc_madre_1,c_titstu_1,c_ateco_1
                ,c_pospro_1,c_cond_1,contratto_1,Titoccup,X2_33_bf,X2_29_bf,
                Possauto,rip,sp_tot_str_aggr_1,povassc,	c_profess1dig_1)

#to use for correlation matrix
h3 <- h2 %>%
            dplyr::select(c_Ncmp_fatto,pnasc_1, sesso_1,c_c_etacalc_1,cittad_1,
                           pnasc_padre_1,pnasc_madre_1,c_titstu_1,c_ateco_1,rip)
h4 <- h3 %>%
           dplyr::select(pnasc_padre_1,pnasc_madre_1,pnasc_1,cittad_1)

h5 <- h3%>%
           dplyr::select(c_Ncmp_fatto,sesso_1,c_c_etacalc_1,cittad_1,c_titstu_1,c_ateco_1,rip)


M1<-length(h1$max_percettore[h1$max_percettore != 1])#control number of ind1 max earner 
print(M1)

#model 1 variable 

logitmodel1 <- glm(poveri~cittad_1, data = h2, family = binomial(logit))
summary(logitmodel1)
probitmfx(logitmodel1,h2, atmean = FALSE)



tab_model(
 logitmodel1, 
 transform = NULL, 
 show.se = TRUE,
 show.stat = TRUE,
 dv.labels = "Relative Poverty",
  p.style = "stars"
)
#tabella riassuntiva 1 regressione 


odds <- exp(1.33) 
odds/(1+odds)#odds to probability 



ggplot(h2, aes(x= sp_tot_str_aggr_1, y=poveri)) + 
  geom_point(alpha=.5)



ggplot(h2, aes(x=cittad_1, y=poveri)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

#model 2 

# povrelogit2 <- glm(poveri~log(c_Ncmp_fatto)+pnasc_1+ sesso_1+ c_c_etacalc_1+I(c_c_etacalc_1^2)+cittad_1
#                   +staciv_1+cittad_1*pnasc_1+pnasc_padre_1+pnasc_madre_1+pnasc_padre_1*pnasc_madre_1+c_titstu_1+c_ateco_1
#                   +c_pospro_1+c_ateco_1*c_pospro_1+c_cond_1+contratto_1+ Titoccup+log(X2_33_bf)+log(X2_29_bf)+
#                     +Possauto+rip,
#                   data = h1, family = binomial(logit))


povrelogit2 <- glm(poveri~cittad_1+pnasc_madre_1+pnasc_padre_1+pnasc_1+pnasc_padre_1*pnasc_madre_1*pnasc_1+
                         +log(c_Ncmp_fatto)+ sesso_1+ I(c_c_etacalc_1)+I(c_c_etacalc_1^2)+c_titstu_1+
                         +c_ateco_1+contratto_1+rip+c_pospro_1+c_ateco_1*c_pospro_1,
                  data = h2, 
                  family = binomial(logit))
tab_model(
  povrelogit2, 
  transform = NULL, 
  show.se = TRUE,
  show.stat = TRUE,
  dv.labels = "Relative Poverty",
  p.style = "stars"
)

povrelogit3 <- glm(poveri~cittad_1+
                                  +log(c_Ncmp_fatto)+ sesso_1+ c_c_etacalc_1+I(c_c_etacalc_1^2)+c_titstu_1+
                                  +c_ateco_1+rip,
                                
                                  data = h2, 
                                  family = binomial(logit))
tab_model(
  povrelogit3, 
   
  show.se = TRUE,
  show.stat = TRUE,
  dv.labels = "Relative Poverty and citizenship",
  p.style = "stars"
)
waldtest(povrelogit3, c("sesso_1","log(c_Ncmp_fatto)","c_titstu_1","c_c_etacalc_1","I(c_c_etacalc_1^2)","rip"))
probitmfx(povrelogit3,h2, atmean = FALSE)


#correlation matrix
#na.omit(h3)
#inf.omit(h3)
res2 <- rcorr(as.matrix(h3))
res3 <- rcorr(as.matrix(h4))
t <- as.matrix(res3$r)
res <- as.matrix(res2$r)
res4 <- rcorr(as.matrix(h5))
s <- as.matrix(res4$r)

res6 <- rcorr(as.matrix(h6))

res6$r
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


corrplot(s,type = "upper", order = "hclust", 
                    tl.col = "black", tl.srt = 45)


tab_model(
  logitmodel, povrelogit3,
  transform = NULL, 
  show.se = TRUE,
  dv.labels = "Relative Poverty",
  p.style = "stars",
  show.ci = NULL
)


