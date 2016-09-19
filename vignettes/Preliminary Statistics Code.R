#########################################################################
# Preliminary Statistics
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
# To only look at the NJ conclusions in the data
# Remember ! means "not"

sub <- dat[dat$Conclus == 'NJ', ]
dim(sub)
table(sub$Service)

sub2 <- dat[sub$Programmatic != 1, ]

# To get images side by side

par(mfrow=c(1,2))

boxplot(Overall_Q ~ Service, data = sub2)
boxplot(Points_Possible ~ Service, data = sub2)

 
