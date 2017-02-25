###############################
# Formal consults overall specificities
formal_overS <- run_glm_Ospec(formal)
aictab(formal_overS$mods)
overS_mod2_OR_CI <- get_ORs_CIs(formal_overS$mods[[1]])
overS_mod2_std <- lm.beta(formal_overS$mods[[1]])
overS_mod2_std_OR <- get_ORs_CIs(overS_mod2_std)

write.table(aictab(formal_overS$mods),
            file = "inst/overallSpec_formal_AICtable.tsv",
            sep = "\t",
            quote = FALSE)

write.table(formal_overS$summaries[[1]]$coefficients,
            file = "inst/overallS_formal_coef1.tsv",
            sep = "\t",
            quote = FALSE)

write.table(overS_mod2_OR_CI,
            file = "inst/overallS_formal_consults_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)
write.table(overS_mod2_std_OR,
            file = "inst/overallS_formal_consults_std_OR.tsv",
            sep = "\t",
            quote = FALSE)

#############################################################################
# Analysis of the specificity components

# RPMs
RPM_mods <- run_polrs(formal, formal$RPMs_Q)
aictab(RPM_mods$mods) # mod 1 ~ 2, use mod 2 for inference but mention year fx
RPM_mods$summaries[[2]]
RPM_mods$coef[[2]]

write.table(get_ORs_CIs_alt(RPM_mods$mods[[2]]),
            file = "inst/RPM_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)

# write the coeff table, but note it's _log_ odds ratio (LOD)
write.table(RPM_mods$summaries[[2]]$coefficients,
            file = "inst/RPM_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

# CMs
CM_mods <- run_polrs(formal, formal$CM_Q)
aictab(CM_mods$mods)
CM_mods$summaries[[1]]
CM_mods$coef[[1]]

write.table(get_ORs_CIs_alt(CM_mods$mods[[1]]),
            file = "inst/CM_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)

# write the coeff table, but note it's _log_ odds ratio (LOD)
write.table(CM_mods$summaries[[1]]$coefficients,
            file = "inst/CM_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

# CRs
CR_mods <- run_polrs(formal, formal$Cons_rec_Q)
aictab(CR_mods$mods)
CR_mods$summaries[[2]]  # looking @2 b/c all params are present and very similar
CM_mods$coef[[2]]

write.table(get_ORs_CIs_alt(CR_mods$mods[[2]]),
            file = "inst/CR_polr_OR_CI.tsv",
            sep = "\t",
            quote = FALSE)

# write the coeff table, but note it's _log_ odds ratio (LOD)
write.table(CR_mods$summaries[[2]]$coefficients,
            file = "inst/CR_polr_coeffs_LOD.tsv",
            sep = "\t",
            quote = FALSE)

check_noprog <- run_polrs(no_prog, no_prog$RPMs_Q)
