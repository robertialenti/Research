# Preliminaries --------------
rm(list = ls())
options(scipen = 999)
setwd("C:/Users/Robert/OneDrive/Desktop/Bobby/School/MA Economics/Research")

library(haven)
library(vars)
library(tidyverse)
library(dplyr)
library(MASS)
library(caret)
library(astsa)
library(urca)
library(bruceR)
library(stargazer)
library(devtools)
library(ggpubr)
library(reshape2)

# Functions
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

# Investment Rate -----------------
for (i in c(1:6)) {
  # Data Set
  dataset = read_dta("USVAR.dta")
  dataset = as.data.frame(dataset)
  dataset$investment = dataset$investment * 100
  
  # Selecting Relevant Variables
  data_investment = dataset %>% dplyr::select(c(canrsmv, usrsmv, vix, canepu, usepu, srv, sp500_dlog, tobinsq, salesgrowth, cashflow, usfedfundrate, investment, fqtr, uselectionyear, date, leverage))
  
  # Remove Missing Values
  nonmissing = colSums(!is.na(data_investment))
  data_investment = data_investment[(nrow(data_investment) - nonmissing[i]):nrow(data_investment), ]
  
  # Specifying Endogenous and Exogenous Variables
  endog_variables = data_investment[ ,c(i,9,10,12,11,7,8)] 
  exog_variables = data_investment[ ,c(13,14)]
  uncertainty = toString(colnames(data_investment[i]))
  
  # Optimal Lag Selection
  lagsselect = VARselect(endog_variables[complete.cases(endog_variables),], lag.max = 8, type = "cons", exog = exog_variables[complete.cases(endog_variables),], season = NULL)
  
  # Model Estimation
  VAR_investment  = VAR(y = endog_variables[complete.cases(endog_variables),], p = lagsselect$selection[3], type = "cons", exogen = exog_variables[complete.cases(endog_variables),], season = NULL)
  #stargazer(VAR_investment[["varresult"]], type = "text")
  
  # Diagnostics
  print(arch.test(VAR_investment, lags.multi = lagsselect$selection[3]))
  #print(normality.test(VAR_investment, multivariate.only = TRUE))
  #plot(stability(VAR_investment, type = "OLS-CUSUM"))
  
  # Analysis
  irf_investment = irf(VAR_investment, impulse = colnames(data_investment[i]), response = "investment", boot = T, n.ahead = 20, runs = 250, ci = 0.90, ortho = TRUE)
  fevd_investment = fevd(VAR_investment, n.ahead = 20)
  granger_causality(VAR_investment, var.y = "investment", var.x = colnames(data_investment[i]))
  
  # IRF Plots
  variablename_table = paste0("irf_investment_table_", i)
  variablename_table_df = assign(variablename_table,extract_varirf(irf_investment))
  colnames(variablename_table_df) = c("period","irf_actual_investment","irf_lower_investment","irf_upper_investment")
  
  variablename_ggplot = paste0("irf_investment_ggplot_", i)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(x = period, y = irf_actual_investment, ymin = irf_lower_investment, ymax = irf_upper_investment)) +
           geom_hline(yintercept = 0, color = "black") + 
           geom_ribbon(fill = "grey", alpha = 0.3) + 
           geom_line(size = 1) + 
           coord_cartesian(ylim = c(-0.3, 0.3)) +
           theme_classic() + 
           ggtitle(toupper(uncertainty)) + 
           xlab("Number of Periods") + 
           ylab("Investment Rate (%)") +
           theme(plot.title = element_text(size = 10, hjust = 0.5),
                 axis.title.y = element_text(size = 9),
                 axis.title.x = element_text(size = 9)))
  
  # Variance Decomposition Plots
  variablename_table = paste0("fevd_investment_table_", i)
  variablename_table_df = assign(variablename_table,fevd_investment)
  variablename_table_df = as.data.frame(variablename_table_df$investment)
  variablename_table_df$row = rownames(variablename_table_df)
  variablename_table_df1 = reshape2::melt(variablename_table_df, id.vars = "row", measure.vars = c(uncertainty,"salesgrowth","cashflow","investment","usfedfundrate","sp500_dlog","tobinsq"))
  colnames(variablename_table_df1) = c("period","variable","value")
  
  variablename_table_df1 = variablename_table_df1[order(as.numeric(as.character(variablename_table_df1$period))), ]
  variablename_table_df = variablename_table_df1
  
  variablename_ggplot = paste0("fevd_investment_ggplot_", i)
  condition = rep(c("Uncertainty","Sales Growth","Cash Flow","Investment Rate","Monetary Policy Rate","Stock Market","Tobin's Q"), 20)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(fill = condition, x = period, y = value)) +
           geom_bar(position = "fill", stat = "identity") +
           scale_x_discrete(limits = c("1","5","10","15","20")) +
           scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
           theme_classic(base_size = 10) + 
           theme(legend.title=element_blank()) +
           scale_fill_manual(values = c("#FFFFB3", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")) +
           ggtitle(toupper(uncertainty)) + 
           xlab("Number of Periods") + 
           ylab("Proportion of Variance Explained") +
           theme(plot.title = element_text(size = 10, hjust = 0.5),
           axis.title.y = element_text(size = 9),
           axis.title.x = element_text(size = 9)))
}


investment_irf_plots = ggarrange(irf_investment_ggplot_1,irf_investment_ggplot_2,irf_investment_ggplot_3,irf_investment_ggplot_4,irf_investment_ggplot_5,irf_investment_ggplot_6,
          ncol = 3, nrow = 2)
investment_irf_plots

investment_fevd_plots = ggarrange(fevd_investment_ggplot_1,fevd_investment_ggplot_2,fevd_investment_ggplot_3,fevd_investment_ggplot_4,fevd_investment_ggplot_5,fevd_investment_ggplot_6,
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
investment_fevd_plots

# Productivity Growth -----------------
for (i in c(1:6)) {
  # Data Set
  dataset = read_dta("USVAR.dta")
  dataset = as.data.frame(dataset)
  dataset$productivitygrowth = dataset$productivitygrowth * 100
  
  # Keeping Relevant Variables
  data_productivity = dataset %>% dplyr::select(c(canrsmv, usrsmv, vix, canepu, usepu, srv, sp500_dlog, tobinsq, salesgrowth, cashflow, usfedfundrate, productivitygrowth, fqtr, uselectionyear, date, leverage))
  nonmissing = colSums(!is.na(data_productivity))
  data_productivity = data_productivity[(nrow(data_productivity) - nonmissing[i]):nrow(data_productivity), ]
  
  endog_variables = data_productivity[ ,c(i,9,10,12,11,7,8)] 
  exog_variables = data_productivity[ ,c(13,14)]
  uncertainty = toString(colnames(data_productivity[i]))
  
  # Optimal Lag Selection
  lagsselect = VARselect(endog_variables[complete.cases(endog_variables),], lag.max = 8, type = "cons", exog = exog_variables[complete.cases(endog_variables),], season = NULL)
  
  # Model Estimation
  VAR_productivity  = VAR(y = endog_variables[complete.cases(endog_variables),], p = lagsselect$selection[3], type = "cons", exogen = exog_variables[complete.cases(endog_variables),], season = NULL)
  #stargazer(VAR_productivity[["varresult"]], type = "text")
  
  # Diagnostics
  print(arch.test(VAR_productivity, lags.multi = lagsselect$selection[3]))
  #print(normality.test(VAR_productivity, multivariate.only = TRUE))
  #plot(stability(VAR_productivity, type = "OLS-CUSUM"))
  
  # Analysis
  irf_productivity = irf(VAR_productivity, impulse = colnames(data_productivity[i]), response = "productivitygrowth", boot = T, n.ahead = 20, runs = 250, ci = 0.90, ortho = TRUE)
  fevd_productivity = fevd(VAR_productivity, n.ahead = 20)
  granger_causality(VAR_productivity, var.y = "productivitygrowth", var.x = colnames(data_productivity[i]))
  
  # IRF Plots
  variablename_table = paste0("irf_productivity_table_", i)
  variablename_table_df = assign(variablename_table,extract_varirf(irf_productivity))
  colnames(variablename_table_df) = c("period","irf_actual_productivity","irf_lower_productivity","irf_upper_productivity")
  
  variablename_ggplot = paste0("irf_productivity_ggplot_", i)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(x = period, y = irf_actual_productivity, ymin = irf_lower_productivity, ymax = irf_upper_productivity)) +
                                      geom_hline(yintercept = 0, color = "black") + 
                                      geom_ribbon(fill = "grey", alpha = 0.3) + 
                                      geom_line(size = 1) + 
                                      coord_cartesian(ylim = c(-0.30, 0.30)) +
                                      theme_classic() + 
                                      ggtitle(toupper(uncertainty)) + 
                                      xlab("Number of Periods") + 
                                      ylab("Productivitty Growth (%)") +
                                      theme(plot.title = element_text(size = 10, hjust = 0.5),
                                            axis.title.y = element_text(size = 9),
                                            axis.title.x = element_text(size = 9)))
  
  # Variance Decomposition Plots
  variablename_table = paste0("fevd_productivity_table_", i)
  variablename_table_df = assign(variablename_table,fevd_productivity)
  variablename_table_df = as.data.frame(variablename_table_df$productivitygrowth)
  variablename_table_df$row = rownames(variablename_table_df)
  variablename_table_df1 = reshape2::melt(variablename_table_df, id.vars = "row", measure.vars = c(uncertainty,"salesgrowth","cashflow","productivitygrowth","usfedfundrate","sp500_dlog","tobinsq"))
  colnames(variablename_table_df1) = c("period","variable","value")
  
  variablename_table_df1 = variablename_table_df1[order(as.numeric(as.character(variablename_table_df1$period))), ]
  variablename_table_df = variablename_table_df1
  
  variablename_ggplot = paste0("fevd_productivity_ggplot_", i)
  condition = rep(c("Uncertainty","Sales Growth","Cash Flow","Productivity Growth","Monetary Policy Rate","Stock Market","Tobin's Q"), 20)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(fill = condition, x = period, y = value)) +
                                      geom_bar(position = "fill", stat = "identity") +
                                      scale_x_discrete(limits = c("1","5","10","15","20")) +
                                      scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
                                      theme_classic(base_size = 10) + 
                                      theme(legend.title=element_blank()) +
                                      scale_fill_manual(values = c("#FFFFB3", "#BEBADA", "#8DD3C7", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")) +
                                      ggtitle(toupper(uncertainty)) + 
                                      xlab("Number of Periods") + 
                                      ylab("Proportion of Variance Explained") +
                                      theme(plot.title = element_text(size = 10, hjust = 0.5),
                                            axis.title.y = element_text(size = 9),
                                            axis.title.x = element_text(size = 9)))
}

productivity_irf_plots = ggarrange(irf_productivity_ggplot_1,irf_productivity_ggplot_2,irf_productivity_ggplot_3,irf_productivity_ggplot_4,irf_productivity_ggplot_5,irf_productivity_ggplot_6,
                                 ncol = 3, nrow = 2)
productivity_irf_plots

productivity_fevd_plots = ggarrange(fevd_productivity_ggplot_1,fevd_productivity_ggplot_2,fevd_productivity_ggplot_3,fevd_productivity_ggplot_4,fevd_productivity_ggplot_5,fevd_productivity_ggplot_6,
                                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
productivity_fevd_plots

# Bankruptcy Risk -----------------
for (i in c(1:6)) {
  # Data Set
  dataset = read_dta("USVAR.dta")
  dataset = as.data.frame(dataset)
  
  # Keeping Relevant Variables
  data_bankruptcy = dataset %>% dplyr::select(c(canrsmv, usrsmv, vix, canepu, usepu, srv, sp500_dlog, tobinsq, salesgrowth, cashflow, usfedfundrate, bankruptcy, fqtr, uselectionyear, date, leverage))
  
  nonmissing = colSums(!is.na(data_bankruptcy))
  data_bankruptcy = data_bankruptcy[(nrow(data_bankruptcy) - nonmissing[i]):nrow(data_bankruptcy), ]
  
  endog_variables = data_bankruptcy[ ,c(i,9,10,12,11,7,8)]
  exog_variables = data_bankruptcy[ ,c(13,14)]
  uncertainty = toString(colnames(data_bankruptcy[i]))
  
  # Optimal Lag Selection
  lagsselect = VARselect(endog_variables[complete.cases(endog_variables),], lag.max = 8, type = "cons", exog = exog_variables[complete.cases(endog_variables),], season = NULL)
  
  # Model Estimation
  VAR_bankruptcy = VAR(y = endog_variables[complete.cases(endog_variables),], p = lagsselect$selection[3], type = "cons", exogen = exog_variables[complete.cases(endog_variables),], season = NULL)
  #stargazer(VAR_bankruptcy[["varresult"]], type = "text")
  
  # Diagnostics
  print(arch.test(VAR_bankruptcy, lags.multi = lagsselect$selection[3]))
  #print(normality.test(VAR_bankruptcy, multivariate.only = TRUE))
  #plot(stability(VAR_bankruptcy, type = "OLS-CUSUM"))
  
  # Analysis
  irf_bankruptcy = irf(VAR_bankruptcy, impulse = colnames(data_bankruptcy[i]), response = "bankruptcy", boot = T, n.ahead = 20, runs = 250, ci = 0.90, ortho = TRUE)
  fevd_bankruptcy = fevd(VAR_bankruptcy, n.ahead = 20)
  granger_causality(VAR_bankruptcy, var.y = "bankruptcy", var.x = colnames(data_bankruptcy[i]))
  
  # IRF Plots
  variablename_table = paste0("irf_bankruptcy_table_", i)
  variablename_table_df = assign(variablename_table,extract_varirf(irf_bankruptcy))
  colnames(variablename_table_df) = c("period","irf_actual_bankruptcy","irf_lower_bankruptcy","irf_upper_bankruptcy")
  
  variablename_ggplot = paste0("irf_bankruptcy_ggplot_", i)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(x = period, y = irf_actual_bankruptcy, ymin = irf_lower_bankruptcy, ymax = irf_upper_bankruptcy)) +
                                      geom_hline(yintercept = 0, color = "black") + 
                                      geom_ribbon(fill = "grey", alpha = 0.3) + 
                                      geom_line(size = 1) + 
                                      coord_cartesian(ylim = c(-0.10, 0.10)) +
                                      theme_classic() + 
                                      ggtitle(toupper(uncertainty)) + 
                                      xlab("Number of Periods") + 
                                      ylab("Altman Z-Score") +
                                      theme(plot.title = element_text(size = 10, hjust = 0.5),
                                            axis.title.y = element_text(size = 9),
                                            axis.title.x = element_text(size = 9)))
  
  # Variance Decomposition Plots
  variablename_table = paste0("fevd_bankruptcy_table_", i)
  variablename_table_df = assign(variablename_table,fevd_bankruptcy)
  variablename_table_df = as.data.frame(variablename_table_df$bankruptcy)
  variablename_table_df$row = rownames(variablename_table_df)
  variablename_table_df1 = reshape2::melt(variablename_table_df, id.vars = "row", measure.vars = c(uncertainty,"salesgrowth","cashflow","bankruptcy","usfedfundrate","sp500_dlog","tobinsq"))
  colnames(variablename_table_df1) = c("period","variable","value")
  
  variablename_table_df1 = variablename_table_df1[order(as.numeric(as.character(variablename_table_df1$period))), ]
  variablename_table_df = variablename_table_df1
  
  variablename_ggplot = paste0("fevd_bankruptcy_ggplot_", i)
  condition = rep(c("Uncertainty","Sales Growth","Cash Flow","Altman Z-Score","Monetary Policy Rate","Stock Market","Tobin's Q"), 20)
  variablename_ggplot_plot = assign(variablename_ggplot, ggplot(data = variablename_table_df, aes(fill = condition, x = period, y = value)) +
                                      geom_bar(position = "fill", stat = "identity") +
                                      scale_x_discrete(limits = c("1","5","10","15","20")) +
                                      scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
                                      theme_classic(base_size = 10) + 
                                      theme(legend.title=element_blank()) +
                                      scale_fill_manual(values = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")) +
                                      ggtitle(toupper(uncertainty)) + 
                                      xlab("Number of Periods") + 
                                      ylab("Proportion of Variance Explained") +
                                      theme(plot.title = element_text(size = 10, hjust = 0.5),
                                            axis.title.y = element_text(size = 9),
                                            axis.title.x = element_text(size = 9)))
}

bankruptcy_irf_plots = ggarrange(irf_bankruptcy_ggplot_1,irf_bankruptcy_ggplot_2,irf_bankruptcy_ggplot_3,irf_bankruptcy_ggplot_4,irf_bankruptcy_ggplot_5,irf_bankruptcy_ggplot_6,
                                 ncol = 3, nrow = 2)
bankruptcy_irf_plots

bankruptcy_fevd_plots = ggarrange(fevd_bankruptcy_ggplot_1,fevd_bankruptcy_ggplot_2,fevd_bankruptcy_ggplot_3,fevd_bankruptcy_ggplot_4,fevd_bankruptcy_ggplot_5,fevd_bankruptcy_ggplot_6,
                                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
bankruptcy_fevd_plots
