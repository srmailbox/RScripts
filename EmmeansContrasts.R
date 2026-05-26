#### Emmeans tips and tricks

# 0 Data ####
include(emmeans)
dat = expand.grid(SID = 1:10, Obs = 1:10, F1=1:3, F2=1:5) %>% 
  mutate( DV = F1+F2+F1*F2+rnorm(n())
    , across(-DV, factor)
    )

dat %>% 
  group_by(F1, F2) %>% 
  summarize(mDV = mean(DV), sDV = sd(DV)) %>% 
  ungroup() %>% 
  ggplot(aes(x=F2, col=F1, y=mDV, group=F1))+geom_line()+geom_point()+
  geom_errorbar(aes(ymin = mDV-sDV, ymax=mDV+sDV))

## 0.1 Model ####
dat.lm = lm(DV~F1*F2, dat, contrasts = c(F1=contr.sum, F2=contr.sum))

joint_tests(dat.lm)


# 1.0 Simple emmeans ####
# All combinations
dat.emm = emmeans(dat.lm, c("F1", "F2"))
dat.emm = emmeans(dat.lm, ~F1+F2)

# By level of F1
dat.emm2 = emmeans(dat.lm, specs="F2", by="F1")
dat.emm2 = emmeans(dat.lm, ~F2|F1)
### For these, any of the other functions will be applied to each level of F1
# separately - e.g., you can no longer test interactions

# 2.0 simple contrasts ####
# Get all pairwise contrasts for levels of F2, at each level of F1
contrast(dat.emm, "pairwise", by="F1") # Uses the "unsplit" version
contrast(dat.emm2, "pairwise") # Already split by F1, so no need to specify

# Get all sequential contrasts for levels of F2, at each level of F1
contrast(dat.emm, "consec", by="F1")
contrast(dat.emm2, "consec")
#### Note, this is simply a subset of the previous contrasts, excluding
# contrasts between "non-adjacent" values of F2. This changes the number
# of comparisons, so can affect the p-value adjustment for multiple comparisons.

# 3.0 Interactions ####
# compares differences in F1 across differences in F2 (or vice versa)
contrast(dat.emm, interaction="pairwise") 

#FAILS, because dat.emm2 is split by F1, so you can't compare levels of F1 to 
# each other anymore.
contrast(dat.emm2, interaction="pairwise") 

# 4.0 "Ordered" factors ####
# In this case, both F1 and F2 are "ordered", so we could ask emmeans to take
# that into account and fit "curves" rather than doing comparisons.

contrast(dat.emm, "poly", by="F1")
# returns linear, quadratic, cubic and quartic terms

## 4.1 restrict levels NOT WORKING ####
contrast(dat.emm, interaction=list(F1="pairwise", F2="poly"))
# returns linear, quadratic, cubic and quartic terms

