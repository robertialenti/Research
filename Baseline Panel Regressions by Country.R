# Preliminaries
rm(list = ls())
options(scipen = 999)
library(ggplot2)
library(dplyr)
library("readxl")

# Data
baseline <- read_excel("Subsample Analysis.xlsx", sheet = 3)
baseline$uncertainty = factor(baseline$uncertainty, levels = c("CAN RSMV","US RSMV","VIX","CAN EPU","US EPU","SRV"))

# Canada
baseline_canada = baseline %>%
  dplyr::filter(country == "canada")

# Investment
baseline_canada_investment = baseline_canada %>%
  dplyr::filter(outcome == "investment")
can_baseline_investment_plot = ggplot(data = baseline_canada_investment) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Investment Rate") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

# Productivity Growth
baseline_canada_productivity = baseline_canada %>%
  dplyr::filter(outcome == "productivity")
can_baseline_productivity_plot = ggplot(data = baseline_canada_productivity) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Productivity Growth") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

# Bankruptcy Risk
baseline_canada_bankruptcy = baseline_canada %>%
  dplyr::filter(outcome == "bankruptcy")
can_baseline_bankruptcy_plot = ggplot(data = baseline_canada_bankruptcy) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Bankruptcy Risk") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

can_baseline_plots = ggarrange(can_baseline_investment_plot, can_baseline_productivity_plot, can_baseline_bankruptcy_plot,
                                 ncol = 3, nrow = 1, font.label = list(size = 10, face = "plain"))
can_baseline_plots

# US
baseline_us = baseline %>%
  dplyr::filter(country == "us")

# Investment
baseline_us_investment = baseline_us %>%
  dplyr::filter(outcome == "investment")
us_baseline_investment_plot = ggplot(data = baseline_us_investment) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Investment Rate") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

# Productivity Growth
baseline_us_productivity = baseline_us %>%
  dplyr::filter(outcome == "productivity")
us_baseline_productivity_plot = ggplot(data = baseline_us_productivity) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Productivity Growth") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

# Bankruptcy Risk
baseline_us_bankruptcy = baseline_us %>%
  dplyr::filter(outcome == "bankruptcy")
us_baseline_bankruptcy_plot = ggplot(data = baseline_us_bankruptcy) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Bankruptcy Risk") +
  theme_classic() + 
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 10, hjust = 0.5))

us_baseline_plots = ggarrange(us_baseline_investment_plot,us_baseline_productivity_plot,us_baseline_bankruptcy_plot,
                               ncol = 3, nrow = 1, font.label = list(size = 10, face = "plain"))
us_baseline_plots
