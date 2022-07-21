# Preliminaries
rm(list = ls())
options(scipen = 999)
library(ggplot2)
library(dplyr)
library("readxl")

# Data
augmented <- read_excel("Subsample Analysis.xlsx", sheet = 4)
augmented$uncertainty = factor(augmented$uncertainty, levels = c("CAN RSMV","US RSMV","VIX","CAN EPU","US EPU","SRV"))

# Canada
augmented_canada = augmented %>%
  dplyr::filter(country == "canada")

# Investment
augmented_canada_investment = augmented_canada %>%
  dplyr::filter(outcome == "investment")
can_augmented_investment_plot = ggplot(data = augmented_canada_investment) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  ylim(-0.45,0.45) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Investment Rate") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

# Productivity Growth
augmented_canada_productivity = augmented_canada %>%
  dplyr::filter(outcome == "productivity")
can_augmented_productivity_plot = ggplot(data = augmented_canada_productivity) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  ylim(-0.45,0.45) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Productivity Growth") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

# Bankruptcy Risk
augmented_canada_bankruptcy = augmented_canada %>%
  dplyr::filter(outcome == "bankruptcy")
can_augmented_bankruptcy_plot = ggplot(data = augmented_canada_bankruptcy) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  ylim(-0.45,0.45) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Bankruptcy Risk") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

can_augmented_plots = ggarrange(can_augmented_investment_plot, can_augmented_productivity_plot, can_augmented_bankruptcy_plot,
                               ncol = 3, nrow = 1, font.label = list(size = 10, face = "plain"))
can_augmented_plots

# US
augmented_us = augmented %>%
  dplyr::filter(country == "us")

# Investment
augmented_us_investment = augmented_us %>%
  dplyr::filter(outcome == "investment")
us_augmented_investment_plot = ggplot(data = augmented_us_investment) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  ylim(-0.15,0.15) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Investment Rate") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

# Productivity Growth
augmented_us_productivity = augmented_us %>%
  dplyr::filter(outcome == "productivity")
us_augmented_productivity_plot = ggplot(data = augmented_us_productivity) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") + 
  ylim(-0.15,0.15) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Productivity Growth") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

# Bankruptcy Risk
augmented_us_bankruptcy = augmented_us %>%
  dplyr::filter(outcome == "bankruptcy")
us_augmented_bankruptcy_plot = ggplot(data = augmented_us_bankruptcy) +
  geom_point(aes(x = uncertainty, y = coeff), stat = "identity", fill = "black", alpha = 1) +
  geom_errorbar(aes(x = uncertainty, ymin = coeff - ic, ymax = coeff + ic), width = 0.2, colour = "black", alpha = 0.9, size = 0.50) +
  geom_hline(yintercept = 0, color = "black") +
  ylim(-0.15,0.15) +
  xlab("") + 
  ylab("Standard Deviations") +
  ggtitle("Bankruptcy Risk") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8, face = NULL),
        plot.title = element_text(size = 12, hjust = 0.5))

us_augmented_plots = ggarrange(us_augmented_investment_plot,us_augmented_productivity_plot,us_augmented_bankruptcy_plot,
                              ncol = 3, nrow = 1, font.label = list(size = 10, face = "plain"))
us_augmented_plots

