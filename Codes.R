##### IMPORTING NECCESSARY LIBRARIES #####
library(tidyverse)
library(patchwork)
library(plm)
library(urca)
library(lmtest)



##### IMPORTING DATASET AND MAKING THEM USEABLE #####
rice_df <- read.csv(file.choose(), header = TRUE)
rice_df$District <- as.factor(rice_df$District)



###### EXPLORATORY ANALYSIS #####

## SUMMARY STATISTICS ##
# Aus #
mean(rice_df$Aus)
sd(rice_df$Aus)
range(rice_df$Aus)
# Aman #
mean(rice_df$Aman)
sd(rice_df$Aman)
range(rice_df$Aman)
# Boro #
mean(rice_df$Boro)
sd(rice_df$Boro)
range(rice_df$Boro)
#change the formulae to get statistics for other variables

## TREND OF RICE PRODUCTIVITY ##
trend_df <- rice_df %>% 
  group_by(Year) %>% summarise(Aus = mean(Aus), Aman = mean(Aman), Boro = mean(Boro), 
                               min_T.Aus = mean(min_T.Aus), min_T.Aman = mean(min_T.Aman), min_T.Boro = mean(min_T.Boro), 
                               max_T.Aus = mean(max_T.Aus), max_T.Aman = mean(max_T.Aman), max_T.Boro = mean(max_T.Boro), 
                               rainfall.Aus = mean(rainfall.Aus),rainfall.Aman = mean(rainfall.Aman), rainfall.Boro = mean(rainfall.Boro))
# Productivity #
line_prod <- ggplot(trend_df, aes(x = Year)) + 
  geom_line(aes(y = Aus, color = "Aus"), size = 0.7) + geom_point(aes(y = Aus, color = "Aus")) + 
  geom_line(aes(y = Aman, color = "Aman"), size = 0.7) + geom_point(aes(y = Aman, color = "Aman")) + 
  geom_line(aes(y = Boro, color = "Boro"), size = 0.7) + geom_point(aes(y = Boro, color = "Boro")) + 
  geom_vline(xintercept = 2005, size = 0.7, lty = "longdash") + 
  geom_smooth(data = trend_df[trend_df$Year < 2006, ], aes(x = Year, y = Aus, color = "Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 2006, ], aes(x = Year, y = Aman, color = "Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 2006, ], aes(x = Year, y = Boro, color = "Boro"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2004, ], aes(x = Year, y = Aus, color = "Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2004, ], aes(x = Year, y = Aman, color = "Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2004, ], aes(x = Year, y = Boro, color = "Boro"), method = "lm", se = FALSE, size = 0.7) + 
  scale_color_manual(name = "Category", values = c("Aus" = "red", "Aman" = "blue", "Boro" = "green4")) + 
  labs(title = "Productivity", x = NULL, y = "Average Productivity (tonnes/hec)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "none", axis.text.x = element_blank())
# Minimum Temperature #
line_minT <- ggplot(trend_df, aes(x = Year)) +
  geom_line(aes(y = min_T.Aus, color = "min_T.Aus"), size = 0.7) + geom_point(aes(y = min_T.Aus, color = "min_T.Aus")) +
  geom_line(aes(y = min_T.Aman, color = "min_T.Aman"), size = 0.7) + geom_point(aes(y = min_T.Aman, color = "min_T.Aman")) +
  geom_line(aes(y = min_T.Boro, color = "min_T.Boro"), size = 0.7) + geom_point(aes(y = min_T.Boro, color = "min_T.Boro")) + 
  geom_vline(xintercept = 1990, size = 0.7, lty = "longdash") + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = min_T.Aus, color = "min_T.Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = min_T.Aman, color = "min_T.Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = min_T.Boro, color = "min_T.Boro"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = min_T.Aus, color = "min_T.Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = min_T.Aman, color = "min_T.Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = min_T.Boro, color = "min_T.Boro"), method = "lm", se = FALSE, size = 0.7) + 
  scale_color_manual(name = "Category", values = c("min_T.Aus" = "red", "min_T.Aman" = "blue", "min_T.Boro" = "green4")) + 
  labs(title = "Minimum Temperature", x = NULL, y = "Average Minimum Temperature (°C)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "none", axis.text.x = element_blank())
# Maximum Temperature #
line_maxT <- ggplot(trend_df, aes(x = Year)) +
  geom_line(aes(y = max_T.Aus, color = "max_T.Aus"), size = 0.7) + geom_point(aes(y = max_T.Aus, color = "max_T.Aus")) +
  geom_line(aes(y = max_T.Aman, color = "max_T.Aman"), size = 0.7) + geom_point(aes(y = max_T.Aman, color = "max_T.Aman")) +
  geom_line(aes(y = max_T.Boro, color = "max_T.Boro"), size = 0.7) + geom_point(aes(y = max_T.Boro, color = "max_T.Boro")) + 
  geom_vline(xintercept = 1990, size = 0.7, lty = "longdash") + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = max_T.Aus, color = "max_T.Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = max_T.Aman, color = "max_T.Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 1991, ], aes(x = Year, y = max_T.Boro, color = "max_T.Boro"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = max_T.Aus, color = "max_T.Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = max_T.Aman, color = "max_T.Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 1989, ], aes(x = Year, y = max_T.Boro, color = "max_T.Boro"), method = "lm", se = FALSE, size = 0.7) + 
  scale_color_manual(name = "Category", values = c("max_T.Aus" = "red", "max_T.Aman" = "blue", "max_T.Boro" = "green4")) + 
  labs(title = "Maximum Temperature", x = NULL, y = "Average Maximum Temperature (°C)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "none", axis.text.x = element_blank())
# Rainfall #
line_rainfall <- ggplot(trend_df, aes(x = Year)) +
  geom_line(aes(y = rainfall.Aus, color = "Aus"), size = 0.7) + geom_point(aes(y = rainfall.Aus, color = "Aus")) +
  geom_line(aes(y = rainfall.Aman, color = "Aman"), size = 0.7) + geom_point(aes(y = rainfall.Aman, color = "Aman")) +
  geom_line(aes(y = rainfall.Boro, color = "Boro"), size = 0.7) + geom_point(aes(y = rainfall.Boro, color = "Boro")) + 
  geom_vline(xintercept = 2009, size = 0.7, lty = "longdash") + 
  geom_smooth(data = trend_df[trend_df$Year < 2010, ], aes(x = Year, y = rainfall.Aus, color = "Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 2010, ], aes(x = Year, y = rainfall.Aman, color = "Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year < 2010, ], aes(x = Year, y = rainfall.Boro, color = "Boro"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2008, ], aes(x = Year, y = rainfall.Aus, color = "Aus"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2008, ], aes(x = Year, y = rainfall.Aman, color = "Aman"), method = "lm", se = FALSE, size = 0.7) + 
  geom_smooth(data = trend_df[trend_df$Year > 2008, ], aes(x = Year, y = rainfall.Boro, color = "Boro"), method = "lm", se = FALSE, size = 0.7) + 
  scale_color_manual(name = "Category", values = c("Aus" = "red", "Aman" = "blue", "Boro" = "green4")) + 
  labs(title = "Rainfall", x = "Year", y = "Average Rainfall (mm)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "bottom", legend.direction = "horizontal")
# Combined Plot #
line_plot <- (line_prod / line_minT / line_maxT / line_rainfall)
print(line_plot)

## VIOLIN PLOT OF PRODUCTIVITY ##
# Aus #
violin_aus <- ggplot(rice_df, aes(x = District, y = Aus)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Aus", x = NULL, y = "Average Productivity (in tonnes/hec)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), axis.text.x = element_blank())
# Aman #
violin_aman <- ggplot(rice_df, aes(x = District, y = Aman)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Aman", x = NULL, y = "Average Productivity (in tonnes/hec)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), axis.text.x = element_blank())
# Boro #
violin_boro <- ggplot(rice_df, aes(x = District, y = Boro)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Boro", x = "District", y = "Average Productivity (in tonnes/hec)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), axis.text.x = element_text())
# Combined Plot #
violin_plot <- (violin_aus / violin_aman / violin_boro)
print(violin_plot)

## DISTRIBUTION OF VARIOUS CATEGORIES OF RICE OVER DISTRICTS ##
# Aus #
aus_dist_df <- rice_df %>% group_by(District) %>% summarise(Productivity = sum(Aus))
aus_dist <- ggplot(aus_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")), position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Aus") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "none")
# Aman #
aman_dist_df <- rice_df %>% group_by(District) %>% summarise(Productivity = sum(Aman))
aman_dist <- ggplot(aman_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")), position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Aman") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "bottom", legend.direction = "horizontal")
# Boro #
boro_dist_df <- rice_df %>% group_by(District) %>% summarise(Productivity = sum(Boro))
boro_dist <- ggplot(boro_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")),position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Boro") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), legend.position = "none")
# Combined Plot #
pie_plot <- (aus_dist | aman_dist | boro_dist)
print(pie_plot)



##### DATA ANALYSIS #####

## MK TEST ##
mkttest(rice_df[rice_df$District == "Bankura", ]$Aus)
mkttest(rice_df[rice_df$District == "Bankura", ]$min_T.Aus)
mkttest(rice_df[rice_df$District == "Bankura", ]$max_T.Aus)
mkttest(rice_df[rice_df$District == "Bankura", ]$rainfall.Aus)

## HAUSMAN TEST ##
# Aus #
fe_model <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df, index = c("District", "Year"), 
                model = "within", effect = "individual")
re_model <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df, index = c("District", "Year"), 
                model = "random", effect = "individual")
phtest(fe_model, re_model)
# Aman #
fe_model <- plm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman, data = rice_df, index = c("District", "Year"), 
                model = "within", effect = "individual")
re_model <- plm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman, data = rice_df, index = c("District", "Year"), 
                model = "random", effect = "individual")
phtest(fe_model, re_model)
# Boro #
fe_model <- plm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro, data = rice_df, index = c("District", "Year"), 
                model = "within", effect = "individual")
re_model <- plm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro, data = rice_df, index = c("District", "Year"), 
                model = "random", effect = "individual")
phtest(fe_model, re_model)

## RANDOM EFFECTS MODEL: AUS ##
re_model_aus <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df, index = c("District", "Year"),
                    model = "random", effect = "individual")
summary(re_model_aus, robust = TRUE, vcov = vcovHC(re_model_aus, type = "HC1"))

## RANDOM EFFECTS MODEL: AMAN ##
re_model_aman <- plm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman, data = rice_df, index = c("District", "Year"),
                     model = "random", effect = "individual")
summary(re_model_aman, robust = TRUE, vcov = vcovHC(re_model_aman, type = "HC1"))

## FIXED EFFECTS LSDV MODEL: BORO ##
dist_dummy <- model.matrix(~ District, rice_df)
lsdv_model_boro <- lm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro + dist_dummy, data = rice_df)
summary(lsdv_model_boro, robust = TRUE, vcov = vcovHC(lsdv_model_boro, type = "HC1"))


