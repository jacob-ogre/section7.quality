# Working R Code

# To begin, load in the code for the following programs:
library(readxl)
library(nnet)
library(MASS)
library(lm.beta)
library(AICcmodavg)
library(glmulti)
library(rJava)
library(lme4)
# setwd("/Users/megevansen/Desktop")
# read_excel("working_copy.xlsx", na = "NA")

# dat <- read_excel("working_copy.xlsx")
dat <- read_excel("consultation_data.xlsx")
dim(dat)
dat <- dat[c(1:366), ]

# This is to convert variables to numeric
to_num <- c(5,13,18:37,41:47,50,52,54,57:60)
for (i in to_num) {
  dat[[i]] <-as.numeric(dat[[i]])
}

# This is to convert variables to factor
to_fac <- c(4,6,15:17,38:40,48,49,51,53,56)
for (i in to_fac) {
  dat[[i]] <-as.factor(dat[[i]])
}

# To save the file
save(dat, file="working_data.RData")

# To load the data back in
#load("working_data.RData")

#########################################################################
# Analyses

#Need to develop "if RPMs? Then run this code" loop
#It will look like this:
new_RPMs <-dat[dat$RPMs==1,]

#Then any models reference "new_RPMs" instead of dat
#If there are any NA in the data (varY)
#new2<-dat[!is.na(dat$varY),]
#Where varY is any column that has both numbers and 'NA'

hist(resid(mod1))
summary(mod1)
aov(mod1)

#########################################################################
# Analyses
boxplot(RPMs_Q ~ Service, data=dat)
boxplot(RPMs_Q ~ Trtl_species, data=dat)
plot(RPMs_Q ~ Year, data=dat)
abline(lm(RPMs_Q ~ Year, data=dat), col="red")

#########################################################################
# Preliminary Analysis

# Trtl_species left out, as preliminary statistics showed it does not affect overall quality
# These are more for preliminary statistics, as the models should be the
# arcsin squareroot transformation of the linear model

mod2 <- lm(CM_Q ~ Service + Year + Action_type + Trtl_species, data=dat)
hist(resid(mod2))
summary(mod2)
aov(mod2)

# Models specifically looking at Overall Quality

mod3 <- lm(Overall_Q ~ Service + Year + Action_type + Office, data=dat)
hist(resid(mod3))
summary(mod3)
aov(mod3)

mod4 <- lm(Overall_Q ~ Service + Year + Action_type, data=dat)
hist(resid(mod4))
summary(mod4)
aov(mod4)

mod5 <- lm(Overall_Q ~ Service + Year, data=dat)
hist(resid(mod5))
summary(mod5)
aov(mod5)

# This code for mod6 gets an error message: using type = "numeric" with a factor response will be ignored
# Need to change "concl_logical" from numeric to factor? But why?
mod6 <- lm(concl_logical ~ Service + Year + Action_type + Trtl_species, data=dat)
hist(resid(mod6))
summary(mod6)
aov(mod6)

#mod7 under informal tab - to test loading in informal data

mod8 <- lm(status_Q ~ Service + Year + Action_type + Trtl_species, data=dat)
hist(resid(mod8))
summary(mod8)
aov(mod8)

mod9 <- lm(Cons_rec_Q ~ Service + Year + Action_type + Trtl_species, data=dat)
hist(resid(mod9))
summary(mod9)
aov(mod9)

# Similar models, but removing Action_type 
mod10 <- lm(Cons_rec_Q ~ Service + Year + Office, data=dat)
hist(resid(mod10))
summary(mod10)
aov(mod10)

#########################################################################
#To decide what to include in the analysis, explore boxplots for the variables that affect quality
#boxplot(wt_dredge ~ Year)
#Collapse data down to one row per consultation!

#To Collapse data
dat$dups <- duplicated(dat$PDF_ID)
col_dat <- dat[dat$dups==FALSE, ]

#To decide what to include in the analysis
wt_dredge <- dat[grep("dredg", dat$Action_type, fixed=TRUE), ]
wt_nourish <- dat[grep("nourish", dat$Action_type, fixed=TRUE), ]
wt_pier <- dat[grep("pier", dat$Action_type, fixed=TRUE), ]
wt_seawall <- dat[grep("seawall", dat$Action_type, fixed=TRUE), ]
wt_prog <- dat[grep("prog", dat$Action_type, fixed=TRUE), ]


boxplot(Overall_Q ~ Action_type, data = col_dat)

boxplot(Overall_Q ~ Service, data = col_dat)
#########################################################################
# Testing out Multinomial Distributions
# Going to need to use the "nnet" package, which deals with multinomial GLM
# GLM = generalized linear models
# Jacob's code has a releveling step, though I don't think we should have
# to do that, since our code should all be from 0 to 5 at the most (no neg numbs)
# We may be using Ordinal logistical regression for this instead

#########################################################################
# Ordinal logistic regression - can be used for any of the rankings on the
# Excel file that are ordered (for example, quality rankings from 0 to 5)
# The info for this type of code came from the UCLA website http://www.ats.ucla.edu/stat/r/dae/ologit.htm

# Note: data must be in factor form, hence the "as.factor" code
RPM_t1 <- polr(as.factor(dat$RPMs_Q) ~ dat$Year + dat$Action_type, Hess = TRUE)
summary(RPM_t1)
AICc(RPM_t1)

# Store table
ctable <- coef(summary(RPM_t1))

# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
(ctable <- cbind(ctable, "p value" = p))

# Confidence intervals assuming normality
confint.default(RPM_t1)

# odds ratio
exp(coef(RPM_t1))

RPM_t2 <- polr(as.factor(dat$RPMs_Q) ~ dat$Year + dat$Action_type + dat$Service, Hess = TRUE)
summary(RPM_t2)

RPM_t3 <- polr(as.factor(dat$RPMs_Q) ~ dat$Service + dat$Programmatic + dat$Action_type, Hess = TRUE)
AICc(RPM_t3)

RPM_var <- list(RPM_t1, RPM_t2, RPM_t3)
boot.wt(RPM_var)

CM_t1 <- polr(as.factor(dat$CM_Q) ~ dat$Service + dat$Year + dat$Action_type, Hess = TRUE)
AICc(CM_t1)

CM_t2 <- polr(as.factor(dat$CM_Q) ~ dat$Service + dat$Programmatic + dat$Action_type, Hess = TRUE)
AICc(CM_t2)

dat$LAA <- ifelse(dat$Conclus == "NJ",
                  1,
                  0)
boxplot(dat$Overall_Q ~ dat$LAA)
boxplot(dat$Sum_Q ~ dat$LAA)

mod_t <- polr(as.factor(dat$CM_Q) ~ dat$Year +dat$Service + dat$Action_type, Hess = TRUE)
AICc(mod_t)

#########################################################################
# Global Model 
global_mod <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod))
summary(global_mod)
aov(global_mod)

# This code puts all the data on the same scale (since quality rankings, years, etc.
# use different scales) - make sure to load the lm.beta library
lm.beta(global_mod)
summary.lm.beta(global_mod)

# The Multiple R-squared statistic will show you how much variation can be 
# explained by the variables chosen in the model. We can compare to the Intercept
# to see whether the variables being explored significantly alter the results. 
# For example, if the Intercept for overall quality is .57 and the Service variable
# NMFS is .12, we can see it's almost 1/5 higher quality (.12/.57). Results thus
# far seem to indicate that the LAA is slightly significant to better quality 
# results, but the Service and year also make a huge difference (NMFS better)

# Model simply looking at the effect of Programmatics and LAA conclusions
# This will eliminate any NAs from the Overall Quality analysis
cur_dat <- dat[!is.na(dat$Overall_Q), ]

a <- lm(Overall_Q ~ Programmatic + LAA, data = cur_dat)
hist(resid(a))
summary(a)
aov(a)
boxplot (Overall_Q ~ Programmatic + LAA, data = cur_dat)

b <- lm(resid(a) ~ cur_dat$Service)
hist(resid(b))
summary(b)

c <- lm(Overall_Q ~ Service, data = cur_dat)
hist(resid(c))
summary(c)

d <- lm(Overall_Q ~ Service + Programmatic + LAA, data = cur_dat)
hist(resid(d))
summary(d)

d2 <- lm(asin(sqrt(Overall_Q)) ~ Service + Programmatic + LAA, data = cur_dat)
hist(resid(d2))
summary(d2)

e <- lm(asin(sqrt(Overall_Q)) ~ Service + Year + Office + Action_type, data = cur_dat)
hist(resid(e))
summary(e)

# More models stemming from the Global Model:

f <- lm(asin(sqrt(Overall_Q)) ~ Service + Year, data = cur_dat)
hist(resid(f))
summary(f)
aov(f)

g <- lm(asin(sqrt(Overall_Q)) ~ Year + Office, data = cur_dat)
hist(resid(g))
summary(g)

h <- lm(asin(sqrt(Overall_Q)) ~ Year + Service + Office, data = cur_dat)
hist(resid(h))
summary(h)

# Focus on transforming the data with the arcsin square root transformation
# This transformation allows the results to be seen in a normal distribution
# Arcsin sqrt transformations only be used on results from 0 to 1

#########################################################################
# To only look at the NJ conclusions in the data
# Remember ! means "not"

#Question for Jacob - how do you get it to look at only NJ AND remove programmatics at the same time?
#Does that happen if I just do the sub code first and then sub2?

sub <- dat[dat$Conclus == 'NJ', ]
dim(sub)
table(sub$Service)

sub2 <- big_dat[big_dat$Programmatic != 1, ]

# To get images side by side

par(mfrow=c(1,2))

boxplot(Overall_Q ~ Service, data = sub2)
boxplot(Points_Possible ~ Service, data = sub2)

##########################################################################
#Poisson distribution - To be used for the number of references 
# Poisson distribution must be a GLM Poisson distribution with link = "log"

fit <- glm(n_ref ~ Year + Service, data = dat, family = poisson())
summary(fit)

# Adjust this code as needed to add more variables, such as Action_type, Office, etc. 

#########################################################################
# Code to combine the Formal and Informal Consultation data
inf <- read_excel("working_copy.xlsx", sheet = 2)

to_num <- c(3,5,13,15,16,18:23)
for (i in to_num) {
  inf[[i]] <-as.numeric(inf[[i]])
}

inf$LAA <- rep(0, length(inf[[1]]))
inf$Programmatic <- rep(0, length(inf[[1]]))

formal <- read_excel("working_copy.xlsx", sheet = 1)

to_num <- c(5,13,18:37,41:47,50,52,54,57:60)
for (i in to_num) {
  formal[[i]] <-as.numeric(formal[[i]])
}

to_fac <- c(4,6,15:17,38:40,48,49,51,53,56)
for (i in to_fac) {
  formal[[i]] <-as.factor(formal[[i]])
}

form_sub <- formal[,c(3,4,5,6,7,8,61,60,59,58,57)]
inf_sub <- inf[,c(3,4,5,6,7,8,26,27,25,24,23)]
names(inf_sub) <- c("Formal", "Service", "Year", "Office", "Trtl_species", "Action_type", "LAA", "Programmatic", "Overall_Q", "Points_Possible", "Sum_Q")
big_dat <- rbind(inf_sub,form_sub)
dim(big_dat)
head(big_dat)

big_dat <- big_dat[c(1:366), ]

# Subset the data to combine the sheets
# The numbers in the parenthesis should be the column numbers of the variables
# Remember - the variables should have same column number for each sheet!

#########################################################################
# Running the Models (and Global Model) from big_dat (created above)
big_dat$LAA <- as.factor(big_dat$LAA)
big_dat$Formal <- as.factor(big_dat$Formal)

global_mod <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod))
summary(global_mod)
AICc(global_mod)

simple_global <- lm(asin(sqrt(Overall_Q)) ~ Year + Service + Formal + Programmatic, data = big_dat)
A <- ggplot(big_dat, aes(x = Service, y = Overall_Q)) + geom_boxplot() + ggtitle("Overall Quality by Service")
AICc(simple_global)


#Testing global_mod by dropping nested variables one at a time

global_mod_nooffice <- lm(asin(sqrt(Overall_Q)) ~ Year + Service + Programmatic + Formal + Action_type + LAA, data = big_dat)
hist(resid(global_mod_nooffice))
summary(global_mod_nooffice)
AICc(global_mod_nooffice)

global_mod_noservice <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod_noservice))
summary(global_mod_noservice)

global_mod_noformal <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Office + Programmatic + LAA, data = big_dat)
hist(resid(global_mod_noformal))
summary(global_mod_noformal)

global_mod_noLAA <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Office + Programmatic + Formal, data = big_dat)
hist(resid(global_mod_noLAA))
summary(global_mod_noLAA)

global_mod_noLAAoff <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Programmatic + Formal, data = big_dat)

#########################################################################

global_mod_nest <- lmer(Overall_Q ~ Year + Action_type + (Service) + (1|Service:Office) + (1|Service:Programmatic) + (Formal) + (1|Formal:LAA), data = big_dat)
hist(resid(global_mod))
summary(global_mod)
AICc(global_mod)

global_mod <- lm(asin(sqrt(Overall_Q)) ~ Year + Action_type + Service + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod))
summary(global_mod)
AICc(global_mod)

model2 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Action_type + Programmatic + Year, data = big_dat)
hist(resid(moda))
summary(moda)
AICc(model2)

model3 <- lm(asin(sqrt(Overall_Q)) ~ Service + Formal + Year + Action_type, data = big_dat)
hist(resid(modb))
AICc(model3)

model4 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Programmatic + Action_type, data = big_dat)
hist(resid(modc))
AICc(model4)

model5 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Programmatic + LAA, data = big_dat)
hist(resid(modd))
AICc(model5)

model6 <- lm (asin(sqrt(Overall_Q)) ~ Service + Office + Programmatic, data = big_dat)
hist(resid(mode))
AICc(model6)

model7 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Formal, data = big_dat)
hist(resid(modf))
AICc(model7)

model8 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Action_type, data = big_dat)
hist(resid(modg))
AICc(model8)

model9 <- lm(asin(sqrt(Overall_Q)) ~ Service + Office, data = big_dat)
hist(resid(modh))
AICc(model9)

model10 <- lm(asin(sqrt(Overall_Q)) ~ Programmatic + LAA, data = big_dat)
hist(resid(modi))
AICc(model10)

# Weightables for comparing model to itself (using only parameters listed in model)
test2 <- glmulti(global_mod, level = 1, crit="aicc")
weightable(test2)
coefs <- coef.glmulti(test2)
write.table(coefs, file="~/Desktop/glmulti_params_avgd.tab",
            sep="\t", quote=FALSE)

#########################################################################
# Testing Model fit
new_var <- list(moda, modb, modc, modd, mode, modf, modg, modh, modi, modj)
boot.wt(new_var)

#########################################################################
# To load worksheet 2, the Informal Consultation data, into R

# To load the data
dat2 <- read_excel("working_copy.xlsx", sheet = 2)
dim(dat2)
dat2 <- dat2[c(1:340), ]

# This is to convert variables to numeric (DOUBLE CHECK OK FOR INFORMAL)
to_num <- c(3,5,13,15,16,18:23)
for (i in to_num) {
  dat2[[i]] <-as.numeric(dat2[[i]])
}

mod7 <- lm(Overall_Q ~ Service + Year + Action_type + Office, data=dat2)
hist(resid(mod7))
summary(mod7)
aov(mod7)

#########################################################################
# Graphing
library(ggplot2)
library(ggthemes)

# Now for the plots that work:
# Must create 'new_overall' that removes NAs from data

form2 <- formal[!is.na(formal$Service), ]

# Quality by Service (formal - normal + programmatic)
plt <- ggplot(form2, aes(Service, Overall_Q)) + geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3, size = 3) +
  labs(x = "Service",
       y = "Overall Quality") +
  ggtitle(expression(atop("Formal consultation quality", 
                          atop(italic("normal + programmatic"), "")))) +
  theme_hc()
plt


# Quality by Service (formal - normal only)

non_program <- form2[form2$Programmatic == "0", ]


plt <- ggplot(non_program, aes(Service, Overall_Q)) + geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3, size = 3) +
  labs(x = "Service",
       y = "Overall Quality") +
  ggtitle(expression(atop("Formal consultation quality", 
                          atop(italic("normal consultations only"), "")))) +
  theme_hc()
plt

# Quality of baseline

plt2 <- ggplot(form2, aes(Service, baseline_Q)) + geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3, size = 3) +
  labs(x = "Service",
       y = "Overall Quality") +
  ggtitle(expression(atop("Formal consultation quality", 
                          atop(italic("normal + programmatic", 
                                      atop("baseline"), ""))))) +
  theme_hc()
plt2


# Models to graph

par(mfrow=c(2,1))

FWS <- big_dat[big_dat$Service=="FWS", ]
NMFS <- big_dat[big_dat$Service !="FWS", ]

boxplot(Overall_Q ~ Year, xlab = "Year", ylab = "Overall Quality", data = FWS)
title(main = "Overall Quality of Consultations by Year - FWS")

boxplot(Overall_Q ~ Year, xlab = "Year", ylab = "Overall Quality", data = NMFS)
title(main = "Overall Quality of Consultations by Year - NMFS")



p2 <- ggplot(big_dat, aes(x = Year, y = Overall_Q, colour = factor(Action_type))) +
  geom_point() +
  ggtitle("What")

p2 + theme_stata() + scale_colour_stata()

p4 <- ggplot(big_dat, aes(factor(Service), Overall_Q))
p4 + theme_tufte(ticks = FALSE) + geom_tufteboxplot()

p4 + theme_tufte(ticks=FALSE) +
  geom_tufteboxplot(median.type = "line", whisker.type = 'point', hoffset = 0)

# Hmm...not the best way to display this, but interesting nonetheless
ggplot(dat, aes(x = Year, fill = RPMs_Q)) +
  geom_bar() +
  scale_fill_excel() +
  theme_excel()

ggplot(big_dat, aes(x = Year, y = Overall_Q, group = Office, color = Office)) +
  geom_line() + 
  geom_point(size = 1.1) + 
  ggtitle("Overall Quality through the Years") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

ggplot(big_dat, aes(x = Year, y = Overall_Q, group = Office, color = Office)) +
  geom_line() +
  geom_point(size = 1.5) + 
  ggtitle("Here's a Thing") +
  theme_hc() +
  scale_colour_hc()

trial <- boxplot(Overall_Q ~ Service, data = big_dat)

qplot(factor(cyl), wt, data = mtcars, geom = c("boxplot", "jitter"))

###############################################################################
# To get the statistics:

#Formal quality with programmatics
results1 <- lm(asin(sqrt(form2$Overall_Q)) ~ form2$Service)
summary(results1)

#Formal quality without programmatics
results2 <- lm(asin(sqrt(non_program$Overall_Q)) ~ non_program$Service)
summary(results2)

#CE quality with programmatics
results3 <- lm(asin(sqrt(form2$baseline_Q)) ~ form2$Service)
summary(results3)

#CE quality without programmatics
results4 <- lm(asin(sqrt(non_program$CE_Q)) ~ non_program$Service)
summary(results4)
