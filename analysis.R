student_data=read.csv("/Users/F4RBOD/Documents/medical education thesis/final/medical_ed_data.csv")
prof_survey=read.csv("/Users/F4RBOD/Documents/medical education thesis/final/prof_survey.csv")
student_survey=read.csv("/Users/F4RBOD/Documents/medical education thesis/final/student_survey.csv")

impact_score_calculator=function(data){
  require(tidyverse)
  
  frequency_finder=function(data){
    table(data)/length(data)
  }
  frequencies=apply(data[,2:6],MARGIN = 2,FUN = frequency_finder)
  importance_scores=frequencies*as.numeric(rownames(frequencies))
  colSums(importance_scores)
}

impact_scores=impact_score_calculator(prof_survey)
write.csv(impact_scores,"impact_scores.csv")

#EFA
#https://stats.oarc.ucla.edu/spss/seminars/introduction-to-factor-analysis/a-practical-introduction-to-factor-analysis/
#https://support.sas.com/resources/papers/proceedings/proceedings/sugi31/200-31.pdf
#https://web.cortland.edu/andersmd/psy341/efa.pdf
#https://www.promptcloud.com/blog/exploratory-factor-analysis-in-r/

install.packages("psych",Ncpus=2)
library(psych)
library(corrplot)
library("GPArotation")

student_data=sapply(student_data,function(x) x=ifelse(x==0,yes=NA,x))
datamatrix=cor(student_data[,6:10],use="complete.obs")
corrplot(datamatrix,method="number")

KMO(r=datamatrix)

cortest.bartlett(student_data[,6:10])

det(datamatrix)

fafitfree=fa(r=datamatrix,nfactors = 5, rotate="none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

parallel <- fa.parallel(student_data[,6:10], fm="minres", fa="fa")

fa.none <- fa(r=datamatrix, 
              nfactors = 3, 
              fm="minres", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="oblimin") # none rotation
print(fa.none)
fa.diagram(fa.none)
print(fa.none$loadings,cutoff = 0.3)





factanal.none <- factanal(student_data[,6:10], factors=3, scores = c("regression"), rotation = "varimax")
print(factanal.none)

head(fa.var$scores)


#CFA
#https://lavaan.ugent.be/tutorial/syntax1.html
#https://benwhalley.github.io/just-enough-r/model-fit.html
#https://stats.oarc.ucla.edu/r/seminars/rcfa/
install.packages("lavaan",Ncpus=2)
install.packages("semPlot",Ncpus=2)
install.packages("pander",Ncpus=2)
library(lavaan)
library(pander)

student_model="
cognitive_method=~q1+q2
cognitive_critical=~q3
attitudinal=~q4
social=~q5
"

student.fit <- cfa(student_model, data=student_data)
summary(student.fit, standardized=TRUE)
semPlot::semPaths(student.fit, "std")
fitmeasures(student.fit, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
modificationindices(student.fit)


student_model="
cognitive=~q1+q2+q3
attitudinal=~q4
social=~q5
"

student.fit <- cfa(student_model, data=student_data)
summary(student.fit, standardized=TRUE)
semPlot::semPaths(student.fit, "std")
modificationindices(student.fit)


#cronbachs alpha
#https://www.statology.org/cronbachs-alpha-in-r/

library(ltm)
cronbach.alpha(data=student_data[,6:10],na.rm = T,CI = T,B=500)


#interrater reliability
#https://www.datanovia.com/en/lessons/inter-rater-reliability-analyses-quick-r-codes/#intraclass-correlation-coefficients-continuous-scales
#http://www2.hawaii.edu/~georgeha/Handouts/meas/Exercises/_book/interrater.html

install.packages("irr")
library(irr)

b=student_data%>%
  group_by(name,time,prof)%>%
  mutate(total_above_1=sum(q1>1,q2>1,q3>1,q4>1,q5>1,na.rm = T)
         ,mean=mean(c(q1,q2,q3,q4,q5),na.rm = T))

student_data_g=subset(b,prof=="G")
student_data_r=subset(b,prof=="R")

colnames(student_data_g)[6:12]=sapply(colnames(student_data_g)[6:12],paste,"g",sep="_")
colnames(student_data_r)[6:12]=sapply(colnames(student_data_r)[6:12],paste,"r",sep="_")

a=inner_join(student_data_g[,-c(3,5)],student_data_r[,-c(3,5)],by=c("name","time","scenario"))

colnames(a)
[1] "name"     "time"     "scenario" "q1_g"     "q2_g"     "q3_g"    
[7] "q4_g"     "q5_g"     "q1_r"     "q2_r"     "q3_r"     "q4_r"    
[13] "q5_r"  

icc(a[c("q1_g"
        ,"q1_r")],type = "agreement", unit="average")
icc(a[c("q2_g"
        ,"q2_r")])
icc(a[c("q3_g"
        ,"q3_r")])
icc(a[c("q4_g"
        ,"q4_r")])
icc(a[c("q5_g"
        ,"q5_r")])
icc(a[c("mean_g"
        ,"mean_r")])
icc(a[c("total_above_1_g"
        ,"total_above_1_r")],type = "agreement", unit="average",model="twoway")

student_data_g_1=subset(student_data_g,time==1)
student_data_g_2=subset(student_data_g,time==2)
student_data_r_1=subset(student_data_r,time==1)
student_data_r_2=subset(student_data_r,time==2)
student_data_1=subset(b,time==1)
student_data_2=subset(b,time==2)

student_data_g_t=inner_join(student_data_g_1,student_data_g_2,by="name")
student_data_r_t=inner_join(student_data_r_1,student_data_r_2,by="name")
student_data_t=inner_join(student_data_1,student_data_2,by=c("name","prof","scenario"))

colnames(student_data_t)
> colnames(student_data_t)
[1] "name"            "time.x"          "group.x"         "scenario"       
[5] "prof"            "q1.x"            "q2.x"            "q3.x"           
[9] "q4.x"            "q5.x"            "total_above_1.x" "mean.x"         
[13] "time.y"          "group.y"         "q1.y"            "q2.y"           
[17] "q3.y"            "q4.y"            "q5.y"            "total_above_1.y"
[21] "mean.y"  

icc(student_data_t[c("total_above_1.x","total_above_1.y")]
    ,type = "consistency", unit="single",model="twoway")

colnames(student_data_r_t)
> colnames(student_data_r_t)
[1] "name"              "time.x"            "group.x"          
[4] "scenario.x"        "prof.x"            "q1_r.x"           
[7] "q2_r.x"            "q3_r.x"            "q4_r.x"           
[10] "q5_r.x"            "total_above_1_r.x" "mean_r.x"         
[13] "time.y"            "group.y"           "scenario.y"       
[16] "prof.y"            "q1_r.y"            "q2_r.y"           
[19] "q3_r.y"            "q4_r.y"            "q5_r.y"           
[22] "total_above_1_r.y" "mean_r.y"

icc(student_data_r_t[c("total_above_1_r.x","total_above_1_r.y")]
    ,type = "consistency", unit="single",model="twoway")

icc(student_data_g_t[c("total_above_1_g.x","total_above_1_g.y")]
    ,type = "consistency", unit="single",model="twoway")






















