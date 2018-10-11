library(nlmixr)
library(ggplot2)
library(xpose.nlmixr)
#-----------------------------------------------------------
#Exploratory plots

df = Theoph

ggplot(df, aes(x=Time, y=conc, color=Subject)) +
  geom_point() +
  geom_line()

ggplot(df, aes(x=Time, y=conc)) +
  geom_point() +
  stat_smooth() +
  scale_y_log10()

#------------------------------------------------------------
# Theophylline model using linCmt
m1 <- function() {
  ini({
    tka <- .5
    tcl <- -3.2
    tv <- -1
    eta.ka ~ 1
    eta.cl ~ 2
    eta.v ~ 1
    add.err <- 0.1
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.err)
  })
}

fit1 <- nlmixr(m1, theo_sd, est="saem", table=tableControl(cwres=TRUE, npde=TRUE))

print(fit1)

#fit1 <- fit1 %>% addCwres() # In case this was not specified under model fit prior to estimation one could add this here to the results

#GoF by xpose
xpdb <- xpose_data_nlmixr(fit1)

dv_vs_pred(xpdb) +
  ylab("Observed Theophylline Concentrations (ng/mL)") +
  xlab("Population Predicted Theophylline Concentrations (ng/mL)")
dv_vs_ipred(xpdb) +
  ylab("Observed Theophylline Concentrations (ug/mL)") +
  xlab("Individual Predicted Theophylline Concentrations (ng/mL)")
res_vs_pred(xpdb) +
  ylab("Conditional Weighted Residuals") +
  xlab("Population Predicted Theophylline Concentrations (ng/mL)")
res_vs_idv(xpdb) +
  ylab("Conditional Weighted Residuals") +
  xlab("Time (h)")
prm_vs_iteration(xpdb)
absval_res_vs_idv(xpdb, res = 'IWRES') +
  ylab("Individual Weighted Residuals") +
  xlab("Time (h)")
absval_res_vs_pred(xpdb, res = 'IWRES')  +
  ylab("Individual Weighted Residuals") +
  xlab("Population Predicted Theophylline Concentrations (ng/mL)")
ind_plots(xpdb, nrow=3, ncol=4) +
  ylab("Predicted and Observed Theophylline concentrations (ng/mL)") +
  xlab("Time (h)")
res_distrib(xpdb) +
  ylab("Density") +
    xlab("Conditional Weighted Residuals")
nlmixr::vpc(fit1,nsim=500, show=list(obs_dv=T),
            ylab = "Theophylline Concentrations (ng/mL)", xlab = "Time (h)")


