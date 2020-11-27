library(tidyverse)
library(splines)

SA_heart_data=read.table("SAheart.txt",sep=',',header=TRUE)

SA_heart_data=SA_heart_data[,2:ncol(SA_heart_data)] %>%
  dplyr::select(-adiposity,-typea)

SA_heart_data$famhist=factor(SA_heart_data$famhist)

# Pairs Plot
pairs(SA_heart_data[1:7],col=as.factor(SA_heart_data$chd))

#Initial Logistic Regression, we specify family='binomial' since our response is binary.

heartModel=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family='binomial',data=SA_heart_data)
#summary(gg)

backstep = step(heartModel) # Backwards selection is the default
#summary(backstep)

# Fitting Splines
format = "chd ~ ns(sbp,df=4) + ns(tobacco,df=4) + ns(ldl,df=4) + famhist + ns(obesity,df=4) + ns(alcohol,df=4) + ns(age,df=4)"
format = formula(format)

splineModel = glm( format, data=SA_heart_data, family=binomial )
print( summary(splineModel), digits=3 )

drop1(splineModel, scope=format, test="Chisq" )

backstep2 = step(modelSpline) # Backwards selection is the default
summary(backstep2)
drop1( backstep2, test="Chisq" )

