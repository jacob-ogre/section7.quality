# BSD_2_clause

library(AICcmodavg)
library(ggplot2)
library(ggthemes)
library(glmulti)
library(lm.beta)
library(lmtest)
library(MASS)
library(nnet)
library(readxl)
library(section7.quality)

# Load the data
data(combo, package = "section7.quality")
data(formal, package = "section7.quality")
data(informal, package = "section7.quality")
data(no_prog, package = "section7.quality")
data(all_no_prog, package = "section7.quality")

#########################################################################
# Analyses
#
# Going to start broad and slowly drill down on the results.
# 1. overall quality for formal + informal
# 2. overall specificity of conservation actions for formals
# 3. components of specificity and quality for formals
# 4. components of informal quality

##############
# Overall quality
# all consults
all_overQ <- run_glm_combo(combo)
aictab(all_overQ$mods)  # mod 9 (best) & 2 differ only by year...base inference on 9, but note year effect estimates
all_mod2_OR_CI9 <- get_ORs_CIs(all_overQ$mods[[9]])
all_mod2_OR_CI2 <- get_ORs_CIs(all_overQ$mods[[2]])
all_mod2_std <- lm.beta(all_overQ$mods[[9]])
all_mod2_std_OR <- get_ORs_CIs(all_mod2_std)

write.table(aictab(all_overQ$mods),
            file = "inst/overallQ_all_AICtable.tsv",
            sep = "\t",
            quote = FALSE)

write.table(all_overQ$summaries[[9]]$coefficients,
            file = "inst/overallQ_all_coef9.tsv",
            sep = "\t",
            quote = FALSE)

write.table(all_mod2_OR_CI9,
            file = "inst/overallQ_all_consults_OR_CI_mod9.tsv",
            sep = "\t",
            quote = FALSE)
write.table(all_mod2_OR_CI2,
            file = "inst/overallQ_all_consults_OR_CI_mod2.tsv",
            sep = "\t",
            quote = FALSE)

write.table(all_mod2_std_OR,
            file = "inst/overallQ_all_consults_std_OR9.tsv",
            sep = "\t",
            quote = FALSE)


####################
# quality components: formal

# species status quality
statusQ_mods <- run_polrs(formal, formal$status_Q)
aictab(statusQ_mods$mods)
statusQ_mods$summaries[[2]]  # looking @2 b/c all params are present and very similar
statusQ_mods$coef[[2]]

write.table(get_ORs_CIs_alt(statusQ_mods$mods[[2]]),
            file = "inst/statusQ_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)
write.table(statusQ_mods$summaries[[2]]$coefficients,
            file = "inst/statusQ_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

# baseline cond quality
baselineQ_mods <- run_polrs(formal, formal$baseline_Q)
aictab(baselineQ_mods$mods)
baselineQ_mods$summaries[[1]]
baselineQ_mods$coef[[1]]

write.table(get_ORs_CIs_alt(baselineQ_mods$mods[[1]]),
            file = "inst/baselineQ_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)
write.table(baselineQ_mods$summaries[[1]]$coefficients,
            file = "inst/baselineQ_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

# effects analysis quality
effQ_mods <- run_polrs(formal, formal$eff_Q)
aictab(effQ_mods$mods)
effQ_mods$summaries[[1]]
effQ_mods$coef[[1]]

write.table(get_ORs_CIs_alt(effQ_mods$mods[[1]]),
            file = "inst/effQ_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)
write.table(effQ_mods$summaries[[1]]$coefficients,
            file = "inst/effQ_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

# cumulative effects
CEQ_mods <- run_polrs(formal, formal$CE_Q)
aictab(CEQ_mods$mods)
CR_mods$summaries[[1]]
CM_mods$coef[[1]]

write.table(get_ORs_CIs_alt(CEQ_mods$mods[[1]]),
            file = "inst/CEQ_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)
write.table(CEQ_mods$summaries[[1]]$coefficients,
            file = "inst/CEQ_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)


# quality components: informal
action_mods <- inf_binom(informal, informal$mention_action)
aictab(action_mods$mods)
action_mods$summaries[[1]] # nothing significant (or even close)

#######
act_analysis_mods <- inf_binom(informal, informal$act_analysis)
aictab(act_analysis_mods$mods)
act_analysis_mods$summaries[[1]] # only duration significant, and tiny effect (1.07 OR)

ggplot(data = informal, aes(x = total_duration, y = act_analysis)) +
geom_jitter(height = 0.1, size = 3, alpha = 0.2) +
theme_hc()

#######
sp_analysis_mods <- inf_binom(informal, informal$sp_analysis)
aictab(sp_analysis_mods$mods)
sp_analysis_mods$summaries[[2]] # only duration significant, and tiny effect (1.07 OR)
plot(sp_analysis ~ total_duration, data = informal)

ggplot(data = informal, aes(x = total_duration, y = sp_analysis)) +
geom_jitter(height = 0.1, size = 3, alpha = 0.2) +
theme_hc()

#######
mention_reason_inf_mods <- inf_binom(informal, informal$mention_reason_inf)
aictab(mention_reason_inf_mods$mods)
mention_reason_inf_mods$summaries[[1]]
plot(mention_reason_inf ~ total_duration, data = informal)

qplot(data = informal, x = Service, y = mention_reason_inf, geom="violin") +
    geom_jitter(height = 0.1, width = 0.4, alpha = 0.2, size = 3) +
    theme_hc()

#######
map_mods <- inf_binom(informal, informal$map)
aictab(map_mods$mods)
map_mods$summaries[[1]] # Service has OR = 5.08
map_mods$summaries[[3]]
boxplot(map ~ Service, data = informal)

qplot(data = informal, x = Service, y = map, geom="violin") +
    geom_jitter(height = 0.1, width = 0.4, alpha = 0.2, size = 3) +
    theme_hc()

#############################################################################
#############################################################################
# Plots: qualities
#############################################################################
#############################################################################

par(mfrow = c(1, 3))
boxplot(combo$Overall_Q ~ combo$Programmatic,
        main = "Programmatics",
        ylab = "% overall quality")
boxplot(combo$Overall_Q ~ combo$Service,
        main = "With programmatics",
        ylab = "")
boxplot(combo[combo$Programmatic == "0", ]$Overall_Q ~ combo[combo$Programmatic == "0", ]$Service,
        main = "Without programmatics",
        ylab = "")
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))
boxplot(formal$Overall_Q ~ formal$Programmatic,
        main = "Programmatics",
        ylab = "% overall quality")
boxplot(formal$Overall_Q ~ formal$Service,
        main = "With programmatics",
        ylab = "")
boxplot(formal[formal$Programmatic == "0", ]$Overall_Q ~ formal[formal$Programmatic == "0", ]$Service,
        main = "Without programmatics",
        ylab = "")
par(mfrow = c(1, 1))

par(mfrow = c(3, 1))
boxplot(combo$Overall_Q ~ factor(combo$Year),
        main = "All through time",
        ylab = "% overall quality")
boxplot(formal$Overall_Q ~ factor(formal$Year),
        main = "Formal through time",
        ylab = "% overall quality")
boxplot(informal$Overall_Q ~ factor(informal$Year),
        main = "Informals through time",
        ylab = "% overall quality")
par(mfrow = c(1, 1))

boxplot(informal$Overall_Q ~ informal$Service,
        main = "Informals By Service",
        ylab = "% overall quality")

plot(combo$Overall_Q ~ combo$total_duration,
     xlab = "Duration (days)",
     ylab = "% overall quality",
     pch = c(17, 19)[as.numeric(factor(combo$Formal))],
     col = c("red", "blue")[as.numeric(factor(combo$Service))])
abline(lm(Overall_Q ~ total_duration, data = combo[combo$Service == "NMFS",]),
       col="red")
abline(lm(Overall_Q ~ total_duration, data = combo[combo$Service == "FWS",]),
       col="blue")

# Some quick plots of quality components so I could visualize the data while
# troubleshooting the models
plt <- ggplot(data = formal, aes(status_Q)) +
       geom_bar() +
       facet_grid(. ~ Service + Programmatic) +
       theme_hc()
plt

plt <- ggplot(data = formal, aes(baseline_Q)) +
       geom_bar() +
       facet_grid(. ~ Service + Programmatic) +
       theme_hc()
plt

plt <- ggplot(data = formal, aes(eff_Q)) +
       geom_bar() +
       facet_grid(. ~ Service + Programmatic) +
       theme_hc()
plt

plt <- ggplot(data = formal, aes(CE_Q)) +
       geom_bar() +
       facet_grid(. ~ Service + Programmatic) +
       theme_hc()
plt

#############################################################################
#############################################################################
# Some obsolete code...
#############################################################################
#############################################################################

# run_glm_form <- function(x) {
#     resp <- cbind(as.integer(x$Sum_Q), x$Points_Possible)
#     mod_1 <- glm(resp ~ Service + Year + LAA + Action_type + Programmatic + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_2 <- glm(resp ~ Service + Year + LAA + Programmatic + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_3 <- glm(resp ~ Service + Year + LAA + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_4 <- glm(resp ~ Service + Year + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_5 <- glm(resp ~ Service * Year,
#                 data = x,
#                 family = binomial)
#
#     mod_6 <- glm(resp ~ Service,
#                 data = x,
#                 family = binomial)
#     mod_7 <- glm(resp ~ Year,
#                 data = x,
#                 family = binomial)
#     mod_8 <- glm(resp ~ total_duration,
#                 data = x,
#                 family = binomial)
#     mod_9 <- glm(resp ~ Service + Year + Programmatic + total_duration,
#                 data = x,
#                 family = binomial)
#
#     mods <- list(m1 <- mod_1,
#                  m2 <- mod_2,
#                  m3 <- mod_3,
#                  m4 <- mod_4,
#                  m5 <- mod_5,
#                  m6 <- mod_6,
#                  m7 <- mod_7,
#                  m8 <- mod_8,
#                  m9 <- mod_9)
#     AICs <- list(m1 <- AICc(mod_1),
#                  m2 <- AICc(mod_2),
#                  m3 <- AICc(mod_3),
#                  m4 <- AICc(mod_4),
#                  m5 <- AICc(mod_5),
#                  m6 <- AICc(mod_6),
#                  m7 <- AICc(mod_7),
#                  m8 <- AICc(mod_8),
#                  m9 <- AICc(mod_9))
#
#     sums <- list(m1 <- summary(mod_1),
#                  m2 <- summary(mod_2),
#                  m3 <- summary(mod_3),
#                  m4 <- summary(mod_4),
#                  m5 <- summary(mod_5),
#                  m6 <- summary(mod_6),
#                  m7 <- summary(mod_7),
#                  m8 <- summary(mod_8),
#                  m9 <- summary(mod_9))
#     aovs <- list(m1 <- aov(mod_1),
#                  m2 <- aov(mod_2),
#                  m3 <- aov(mod_3),
#                  m4 <- aov(mod_4),
#                  m5 <- aov(mod_5),
#                  m6 <- aov(mod_6),
#                  m7 <- aov(mod_7),
#                  m8 <- aov(mod_8),
#                  m9 <- aov(mod_9))
#     return(list(mods = mods, AICs = AICs, summaries = sums, aovs = aovs))
# }
#
# run_glm_inf <- function(x) {
#     resp <- cbind(as.integer(x$Sum_Q), x$Points_Possible)
#     mod_1 <- glm(resp ~ Service + Year + Action_type + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_2 <- glm(resp ~ Service + Year + total_duration,
#                 data = x,
#                 family = binomial)
#     mod_3 <- glm(resp ~ Service + Year,
#                 data = x,
#                 family = binomial)
#     mod_4 <- glm(resp ~ Service,
#                 data = x,
#                 family = binomial)
#     mod_5 <- glm(resp ~ Service * Year,
#                 data = x,
#                 family = binomial)
#     mod_6 <- glm(resp ~ total_duration,
#                 data = x,
#                 family = binomial)
#
#     mods <- list(m1 <- mod_1,
#                  m2 <- mod_2,
#                  m3 <- mod_3,
#                  m4 <- mod_4,
#                  m5 <- mod_5,
#                  m6 <- mod_6)
#     AICs <- list(m1 <- AICc(mod_1),
#                  m2 <- AICc(mod_2),
#                  m3 <- AICc(mod_3),
#                  m4 <- AICc(mod_4),
#                  m5 <- AICc(mod_5),
#                  m6 <- AICc(mod_6))
#
#     sums <- list(m1 <- summary(mod_1),
#                  m2 <- summary(mod_2),
#                  m3 <- summary(mod_3),
#                  m4 <- summary(mod_4),
#                  m5 <- summary(mod_5),
#                  m6 <- summary(mod_6))
#     aovs <- list(m1 <- aov(mod_1),
#                  m2 <- aov(mod_2),
#                  m3 <- aov(mod_3),
#                  m4 <- aov(mod_4),
#                  m5 <- aov(mod_5),
#                  m6 <- aov(mod_6))
#     return(list(mods = mods, AICs = AICs, summaries = sums, aovs = aovs))
# }

# # formal consults: not run, but here for posterity
# formal_overQ <- run_glm_form(formal)
# formal_mod2_OR_CI <- get_ORs_CIs(formal_overQ$mods[[2]])
# formal_mod2_std <- lm.beta(formal_overQ$mods[[2]])
# formal_mod2_std_OR <- get_ORs_CIs(formal_mod2_std)
#
# write.table(formal_mod2_OR_CI,
#             file = "inst/overallQ_formal_consults_OR_CI.tsv",
#             sep = "\t",
#             quote = FALSE)
# write.table(formal_mod2_std_OR,
#             file = "inst/overallQ_formal_consults_std_OR.tsv",
#             sep = "\t",
#             quote = FALSE)
#
# # informal consults
# informal_overQ <- run_glm_inf(informal)
# informal_mod2_OR_CI <- get_ORs_CIs(informal_overQ$mods[[2]])
# informal_mod2_std <- lm.beta(informal_overQ$mods[[2]])
# informal_mod2_std_OR <- get_ORs_CIs(informal_mod2_std)
#
# write.table(formal_mod2_OR_CI,
#             file = "inst/overallQ_formal_consults_OR_CI.tsv",
#             sep = "\t",
#             quote = FALSE)
# write.table(formal_mod2_std_OR,
#             file = "inst/overallQ_formal_consults_std_OR.tsv",
#             sep = "\t",
#             quote = FALSE)
