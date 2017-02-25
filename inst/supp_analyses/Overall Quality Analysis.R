# Model of best fit - arcsin sqrt transformations of lm for Overall Quality

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

form_sub <- formal[,c(3,4,5,6,7,8,61,60,59)]
inf_sub <- inf[,c(3,4,5,6,7,8,26,27,25)]
names(inf_sub) <- c("Formal", "Service", "Year", "Office", "Trtl_species", "Action_type", "LAA", "Programmatic", "Overall_Q")
big_dat <- rbind(inf_sub,form_sub)
dim(big_dat)
head(big_dat)

# Subset the data to combine the sheets
# The numbers in the parenthesis should be the column numbers of the variables
# Remember - the variables should have same column number for each sheet!

#########################################################################
# Global Model 
global_mod <- lm(Overall_Q ~ Year + Action_type + Service + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod))
summary(global_mod)
aov(global_mod)

# This code puts all the data on the same scale (since quality rankings, years, etc.
# use different scales) - make sure to load the lm.beta library
lm.beta(global_mod)
summary.lm.beta(global_mod)

# This will eliminate any NAs from the Overall Quality analysis
cur_dat <- dat[!is.na(dat$Overall_Q), ]

# Running the Models (and Global Model) from big_dat (created above)
big_dat$LAA <- as.factor(big_dat$LAA)
big_dat$Formal <- as.factor(big_dat$Formal)

global_mod <- lm(Overall_Q ~ Year + Action_type + Service + Office + Programmatic + LAA + Formal, data = big_dat)
hist(resid(global_mod))
summary(global_mod)
AICc(global_mod)

global_mod <- lmer(Overall_Q ~ Year + Action_type + (Service) + (1|Service:Office) + (1|Service:Programmatic) + (Formal) + (1|Formal:LAA), data = big_dat)
hist(resid(global_mod))
summary(global_mod)
AICc(global_mod)

moda <- lm(asin(sqrt(Overall_Q)) ~ Programmatic + LAA, data = big_dat)
hist(resid(moda))
summary(moda)
AICc(moda)

# Weightables for comparing model to itself (using only parameters listed in model)
test2 <- glmulti(global_mod, level = 1, crit="aicc")
weightable(test2)
coefs <- coef.glmulti(test2)
write.table(coefs, file="~/Desktop/glmulti_params_avgd.tab",
            sep="\t", quote=FALSE)

modb <- lm(asin(sqrt(Overall_Q)) ~ Service + Year + Action_type, data = big_dat)
hist(resid(modb))
AICc(modb)

modc <- lm(asin(sqrt(Overall_Q)) ~ Service + Office, data = big_dat)
hist(resid(modc))
AICc(modc)

modd <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Programmatic, data = big_dat)
hist(resid(modd))
AICc(modd)

mode <- lm (asin(sqrt(Overall_Q)) ~ Service + Office + Programmatic + LAA, data = big_dat)
hist(resid(mode))
AICc(mode)

modf <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Action_type + Programmatic, data = big_dat)
hist(resid(modf))
AICc(modf)

modg <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Formal, data = big_dat)
hist(resid(modg))
AICc(modg)

modh <- lm(asin(sqrt(Overall_Q)) ~ Service + Office + Action_type + Programmatic + Year, data = big_dat)
hist(resid(modh))
AICc(modh)

modi <- lm(asin(sqrt(Overall_Q)) ~ Service + Formal + Action_type + Year, data = big_dat)
hist(resid(modi))
AICc(modi)

modj <- lm(asin(sqrt(Overall_Q)) ~ Service + Formal + Office + Programmatic + Year + Action_type + LAA, data = big_dat)
hist(resid(modj))
AICc(modj)

#########################################################################
# Testing Model fit
new_var <- list(moda, modb, modc, modd, mode, modf, modg, modh, modi, modj)
boot.wt(new_var)

#########################################################################
# Analysis of sum of total quality points
sum_global_mod <- lm(Points_Possible ~ Year + Service + Programmatic + Formal, data = big_dat)
hist(resid(sum_global_mod))
summary(sum_global_mod)
AICc(sum_global_mod)

sum_global_mod2 <- lm(Points_Possible ~ Year + Service + Office + Programmatic + Formal + LAA + Action_type, data = big_dat)
hist(resid(sum_global_mod2))
summary(sum_global_mod2)
AICc(sum_global_mod2)

sum_a <- lm(Points_Possible ~ Programmatic + LAA, data = big_dat)
hist(resid(sum_a))
summary(sum_a)
boxplot(sum_a)
AICc(sum_a)

sum_b <- lm(Points_Possible ~ Service + Year + Action_type, data = big_dat)
hist(resid(sum_b))
summary(sum_b)
AICc(sum_b)

sum_c <- lm(Points_Possible ~ Service + Office, data = big_dat)
hist(resid(sum_c))
AICc(sum_c)

sum_d <- lm(Points_Possible ~ Service + Office + Programmatic, data = big_dat)
hist(resid(sum_d))
AICc(sum_d)

huh <- glm.nb(sum_d)
hist(resid(huh))
huh2 <- 

sum_e <- lm(Points_Possible ~ Service + Office + Programmatic + LAA, data = big_dat)
hist(resid(sum_e))
AICc(sum_e)

sum_f <- lm(Points_Possible ~ Service + Office + Action_type + Programmatic, data = big_dat)
hist(resid(sum_f))
AICc(sum_f)

sum_g <- lm(Points_Possible ~ Service + Office + Formal, data = big_dat)
hist(resid(sum_g))
AICc(sum_g)

sum_h <- lm(Points_Possible ~ Service + Office + Action_type + Programmatic + Year, data = big_dat)
hist(resid(sum_h))
AICc(sum_h)

sum_i <- glm(Points_Possible ~ Service + Formal + Action_type + Year, family = "quasipoisson", data = big_dat)
hist(resid(sum_i))
summary(sum_i)
AICc(sum_i)

sum_j <- lm(Points_Possible ~ Service + Formal + Office + Programmatic + Year + Action_type + LAA, data = big_dat)
hist(resid(sum_j))
AICc(sum_j)
