# Compare GLM (normal error assumed) 
# to GzLM (normal error specified)
# Limpet respiration data from Rohlf in Sokal and Rohlf
# Import SRBX11_2.xls  A1:C49
#General Linear model, hence gaussian error
RespModLM<-lm(VO2~Sp*Sal, data=SRBX11_2)
summary.aov(RespModLM)
#Generalized linear model, error specified
RespModGLM<-glm(VO2~Sp*Sal, family=gaussian, data=SRBX11_2)
summary.glm(RespModGLM)
#Compare outputs
anova(RespModLM)
anova(RespModGLM)
#Specify Type II SS for standard ANOVA table from GLM routine
#Requires car package
Anova(RespModLM, type=2)  
Anova(RespModGLM, type=2)
# Note difference in p-value for Salinity
# Calculate LR
SpLR<-exp(1.4629/2)
SalLR<-exp(7.4526/2)
SpLR
SalLR
# SpLR < 20
# 20 < SalLR < 100

#Declare a crossed factor (Sp) as random
RespModLMran1<-lm(VO2~(1|Sp)*Sal, data=SRBX11_2)
anova(RespModLMran1)
Anova(RespModLMran1, type=2)
summary(RespModLMran1)
#Random and mixed terms not reported, all exiled to residual
#Rewrite the model
RespModLMran2<-lm(VO2~(1|Sp)+Sal+(1|Sp)*Sal, data=SRBX11_2)
#Specifying Type II SS not necessary with terms written out
anova(RespModLMran2)
# Rewrite model
# Tests not correctly specified, despite (1|SP) specification
# Test over the residual is incorrect, 
# Test over the mixed term is correct
# Try nesting the interaction term
RespModLMran2<-lm(VO2~(1|Sp)+Sal/(1|Sp)*Sal, data=SRBX11_2)
anova(RespModLMran2)
# Test remains incorrectly specified,

