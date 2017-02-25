#########################################################################
# Ordinal logistic regression - can be used for any of the rankings on the
# Excel file that are ordered (for example, quality rankings from 0 to 5)
# The info for this type of code came from the UCLA website http://www.ats.ucla.edu/stat/r/dae/ologit.htm

# Note: data must be in factor form, hence the "as.factor" code
RPM_t1 <- polr(as.factor(dat$RPMs_Q) ~ dat$Year + dat$Service + dat$Programmatic + dat$Formal, Hess = TRUE)
summary(RPM_t1)
AICc(RPM_t1)

test_dat <- dat[as.character(dat$Programmatic) !=1, ]
RPM_t1 <- polr(as.factor(test_dat$RPMs_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(RPM_t1)

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

exp(cbind(OR = coef(RPM_t1), ci))

CM_t1 <- polr(as.factor(test_dat$CM_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(CM_t1)

CE_t1 <- polr(as.factor(test_dat$CE_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(CE_t1)

status_t1 <- polr(as.factor(test_dat$status_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(status_t1)

baseline_t1 <- polr(as.factor(test_dat$baseline_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(baseline_t1)

eff_t1 <- polr(as.factor(test_dat$eff_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(eff_t1)

Cons_rec_t1 <- polr(as.factor(test_dat$Cons_rec_Q) ~ test_dat$Year + test_dat$Service, Hess = TRUE)
summary(Cons_rec_t1)

# To only look at NJ conclusions:
dat$LAA <- ifelse(dat$Conclus == "NJ",
                  1,
                  0)
boxplot(dat$Overall_Q ~ dat$LAA)
boxplot(dat$Sum_Q ~ dat$LAA)

mod_t <- polr(as.factor(dat$CM_Q) ~ dat$Year +dat$Service + dat$Action_type, Hess = TRUE)
AICc(mod_t)