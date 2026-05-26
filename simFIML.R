include(mvtnorm)
include(lavaan)
include(semTools)
sgma = data.frame(Y=c(1,.8,.3,.1), X1=c(.8,1,.2,.8), X2=c(.3*.2,.2,1,.4), A1 =c(.64,.8,.4,1)) %>% as.matrix
sgma[lower.tri(sgma)]=t(sgma)[lower.tri(sgma)]

dats = vector("list", 1000)
for(i in 1:length(dats)) 
  dats[[i]] = rmvnorm(600, sigma=as.matrix(sgma)) %>% data.frame


simFIML = function(x.full) {
  semMod = 'L1=~X1
        L2=~X2
        L3=~X3
        L1~L2+L3' 
  
  x.mcar10 = x.full %>% 
    mutate(across(-X4, ~ ifelse(runif(n())<=.1, NA, .)))
  x.mcar50 = x.full %>% 
    mutate(across(-X4, ~ ifelse(runif(n())<=.5, NA, .)))
  x.mar10 = x.full %>% 
    mutate(across(-c(X4, X1), ~ifelse(runif(n())*(X4+50)< 5.5, NA, .)))
  x.mar50 = x.full %>% 
    mutate(across(-c(X4, X1), ~ifelse(runif(n())*(X4+50)< 23.5, NA, .)))
  
  sem.full = cfa(semMod, data = x.full)

  sem.mcar10.lw = cfa(semMod, data = x.mcar10)
  sem.mcar10.ml = cfa(semMod, data = x.mcar10, missing="fiml")
  sem.mar10 = cfa.auxiliary(semMod, missing="fiml", data = x.mar10, aux="X4")
  sem.mnar10 = cfa(semMod, data=x.mar10, missing="fiml")
  sem.mcar50.lw = cfa(semMod, data = x.mcar50)
  sem.mcar50.ml = cfa(semMod, data = x.mcar50, missing="fiml")
  sem.mar50 = cfa.auxiliary(semMod, missing="fiml", data = x.mar50, aux="X4")
  sem.mnar50 = cfa(semMod, data=x.mar50, missing="fiml")
  
  
  
  list(full=sem.full, lw.10=sem.mcar10.lw, fiml.10=sem.mcar10.ml
       , aux.10=sem.mar10, mnar.10=sem.mnar10
       , lw.50=sem.mcar50.lw, fiml.50=sem.mcar50.ml, aux.50=sem.mar50
       , mnar.50=sem.mnar50) %>% 
    lapply(function(x) parameterTable(x) %>% filter(op=="~")) %>% 
    bind_rows(.id="id") %>% arrange(rhs) %>% select(id, rhs, est, se)
  
}


simFIML(x.full)

simFIMLs = lapply(dats, simFIML)

results = simFIMLs %>% bind_rows(.id="dataset")

results %>% filter(id %in% c("full", "mnar.10", "mnar.50")) %>% 
  pivot_wider(id_cols=c("dataset", "rhs"), names_from="id"
              , values_from=c("est", "se")) %>% 
  ggplot(aes(x=est_full)) +
  geom_point(aes(y=est_mnar.10))+
  geom_abline(slope=1)+
  facet_wrap(rhs~., scales="free")

results %>% #filter(id %in% c("full", "mnar.10", "mnar.50")) %>% 
  pivot_wider(id_cols=c("dataset", "rhs"), names_from="id"
              , values_from=c("est", "se")) %>% 
  ggplot(aes(x=est_full, y=est_lw.50)) +
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline(slope=1)+
  facet_wrap(rhs~., scales="free")
