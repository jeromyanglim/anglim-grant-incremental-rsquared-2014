# Score tests
# import and configure meta data
meta <- list()
meta$ipipfacets <- measures.ipipfacets
meta$ipipscales <- measures.ipipscales
meta$swl <- measures.swl
meta$pwb <- measures.pwb
meta$panas <- measures.panas


# Score IPIP Facets
meta$ipipfacets <- merge(meta$ipipfacets, 
                         meta$ipipscales[,c('scale', 'subscale', 
           'scale_abbreviation', 'subscale_name')], all.x=TRUE)
meta$ipipfacets <- meta$ipipfacets[order(meta$ipipfacets$itemNumber), ]

# Score Tests
create_scoring_key <- function(items, scales, reverse) {
    unique_scales <- unique(scales)
    key <- sapply(seq(unique_scales), 
                  function(X) ifelse(scales == unique_scales[X], reverse, 0))
    key <- data.frame(key)
    names(key) <- unique_scales
    row.names(key) <- items
    key
}

score_test <- function(meta_data, case_data, subscale_name='subscale_name', id='id', reverse='reverse') {
    scoring_key <- create_scoring_key(meta_data[, id], 
                                  meta_data[ ,subscale_name],  
                                  meta_data[ ,reverse])
    scored <- score.items(scoring_key, case_data[,rownames(scoring_key)])
    scored$key <- scoring_key
    scored
}

scored <- list()
# IPIP
scored$ipipfacets <- score_test(meta$ipipfacets, ccases)
ccases[,colnames(scored$ipipfacets$scores)] <- scored$ipipfacets$scores

# IPIP Scales
scored$ipipscales <- score_test(meta$ipipscales, ccases, subscale_name="scale_name", 
                              id="subscale_name")
ccases[,colnames(scored$ipipscales$scores)] <- scored$ipipscales$scores


# swl
scored$swl <- score_test(meta$swl, ccases, subscale_name='subscale')
ccases[,colnames(scored$swl$scores)] <- scored$swl$scores

# panas
scored$panas <- score_test(meta$panas, ccases) 
ccases[,colnames(scored$panas$scores)] <-  scored$panas$scores

# pwb
scored$pwb <- score_test(meta$pwb, ccases) 
ccases[,colnames(scored$pwb$scores)] <-  scored$pwb$scores


# age
ccases$age <- car::recode(ccases$demog2, "'16 and under'='16'; '30-39'='34.5'; '40-49'='44.5'; '50+'='55'")
ccases$age <- as.numeric(ccases$age)

# gender as numeric
ccases$male <- car::recode(ccases$demog1, "'Male'=1; 'Female'=0; else=NA")



# variable lists
v <- list()
v$ipip_facets <- c("ipip_n_anxiety", 
                   "ipip_n_anger", "ipip_n_depression", "ipip_n_self_consciousness", 
                   "ipip_n_immoderation", "ipip_n_vulnerability", "ipip_e_friendliness", 
                   "ipip_e_gregariousness", "ipip_e_assertiveness", "ipip_e_activity_level", 
                   "ipip_e_excitement_seeking", "ipip_e_cheerfulness", "ipip_o_imagination", 
                   "ipip_o_artistic_interests", "ipip_o_emotionality", "ipip_o_adventurousness", 
                   "ipip_o_intellect", "ipip_o_liberalism", "ipip_a_trust", "ipip_a_morality", 
                   "ipip_a_altruism", "ipip_a_cooperation", "ipip_a_modesty", "ipip_a_sympathy", 
                   "ipip_c_self_efficacy", "ipip_c_orderliness", "ipip_c_dutifulness", 
                   "ipip_c_achievement_striving", "ipip_c_self_discipline", "ipip_c_cautiousness")
v$ipip_factors <- c("ipip_neuroticism", "ipip_extraversion", "ipip_openness", 
                    "ipip_agreeableness",  "ipip_conscientiousness")
v$swb <-  c("swl", "panas_pa", "panas_na")
v$pwb <- c("pwb_prelwo",  "pwb_autonomy", "pwb_emastery", "pwb_pgrowth", 
           "pwb_plife", "pwb_selfaccept")
v$wellbeing <- c(v$swb, v$pwb)
v$allscales <- c(v$ipip_facets, v$ipip_factors, v$wellbeing)


    

            