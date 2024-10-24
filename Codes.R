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

## PERCENTAGE OF RICE CATEGORY ##
rice_dist_df <- data.frame(Category = c("Aus", "Aman", "Boro"), 
                           Productivity = c(sum(rice_df$Aus), sum(rice_df$Aman), sum(rice_df$Boro)))
ggplot(rice_dist_df, aes(x = "", y = Productivity, fill = Category)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")),
            position = position_stack(vjust = 0.5)) + labs(x = NULL, y = NULL) + 
  theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

## TREND OF RICE PRODUCTIVITY ##
trend_df <- rice_df %>% 
  group_by(Year) %>% summarise(Aus = mean(Aus), Aman = mean(Aman), Boro = mean(Boro))
ggplot(trend_df, aes(x = Year)) +
  geom_line(aes(y = Aus, color = "Aus"), size = 0.7) + geom_point(aes(y = Aus, color = "Aus")) +
  geom_line(aes(y = Aman, color = "Aman"), size = 0.7) + geom_point(aes(y = Aman, color = "Aman")) +
  geom_line(aes(y = Boro, color = "Boro"), size = 0.7) + geom_point(aes(y = Boro, color = "Boro")) +
  scale_color_manual(name = "Category", 
                     values = c("Aus" = "red", "Aman" = "blue", "Boro" = "green4")) + 
  labs(x = "Year", y = "Average Productivity (tonnes/hec)") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

## VIOLIN PLOT OF PRODUCTIVITY ##
# Aus #
violin_aus <- ggplot(rice_df, aes(x = District, y = Aus)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Aus", x = "District", y = "Average Productivity (in tonnes/hec)") + 
  theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
                          axis.text.x = element_text(angle = 30, hjust = 1))
# Aman #
violin_aman <- ggplot(rice_df, aes(x = District, y = Aman)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Aman", x = "District", y = NULL) + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        axis.text.x = element_text(angle = 30, hjust = 1))
# Boro #
violin_boro <- ggplot(rice_df, aes(x = District, y = Boro)) + 
  geom_violin(fill = "grey", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_point(position = position_jitter(seed = 1, width = 0.2), color = "red", shape = 1) + 
  stat_summary(fun = mean, geom = "point", color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") + 
  labs(title = "Boro", x = "District", y = NULL) + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        axis.text.x = element_text(angle = 30, hjust = 1))
# Combined Plot #
violin_plot <- (violin_aus | violin_aman | violin_boro)
print(violin_plot)

## DISTRIBUTION OF VARIOUS CATEGORIES OF RICE OVER DISTRICTS ##
# Aus #
aus_dist_df <- rice_df %>% 
  group_by(District) %>% summarise(Productivity = sum(Aus))
aus_dist <- ggplot(aus_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Aus") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        legend.position = "none")
# Aman #
aman_dist_df <- rice_df %>% 
  group_by(District) %>% summarise(Productivity = sum(Aman))
aman_dist <- ggplot(aman_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Aman") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        legend.position = "bottom", legend.direction = "horizontal")
# Boro #
boro_dist_df <- rice_df %>% 
  group_by(District) %>% summarise(Productivity = sum(Boro))
boro_dist <- ggplot(boro_dist_df, aes(x = "", y = Productivity, fill = District)) + 
  geom_col(width = 1, color = "black") + coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Productivity * 100 / sum(Productivity), 2), "%")),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = NULL, y = NULL) + labs(title = "Boro") + theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        legend.position = "none")
# Combined Plot #
pie_plot <- (aus_dist | aman_dist | boro_dist)
print(pie_plot)



##### DATA ANALYSIS #####

## HAUSMAN TEST ##
# Aus #
fe_model <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df, 
                index = c("District", "Year"), model = "within", effect = "individual")
re_model <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df, 
                index = c("District", "Year"), model = "random", effect = "individual")
phtest(fe_model, re_model)
# Aman #
fe_model <- plm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman, data = rice_df, 
                index = c("District", "Year"), model = "within", effect = "individual")
re_model <- plm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman, data = rice_df, 
                index = c("District", "Year"), model = "random", effect = "individual")
phtest(fe_model, re_model)
# Boro #
fe_model <- plm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro, data = rice_df, 
                index = c("District", "Year"), model = "within", effect = "individual")
re_model <- plm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro, data = rice_df, 
                index = c("District", "Year"), model = "random", effect = "individual")
phtest(fe_model, re_model)

## RANDOM EFFECTS MODEL: AUS ##
re_model_aus <- plm(Aus ~ min_T.Aus + max_T.Aus + rainfall.Aus, data = rice_df,
                    index = c("District", "Year"),
                    model = "random", effect = "individual")
summary(re_model_aus, robust = TRUE, vcov = vcovHC(re_model_aus, type = "HC1"))

## FIXED EFFECTS LSDV MODEL: AMAN ##
dist_dummy <- model.matrix(~ District, rice_df)
lsdv_model_aman <- lm(Aman ~ min_T.Aman + max_T.Aman + rainfall.Aman + dist_dummy, data = rice_df)
summary(lsdv_model_aman, robust = TRUE, vcov = vcovHC(lsdv_model_aman, type = "HC1"))

## FIXED EFFECTS LSDV MODEL: BORO ##
lsdv_model_boro <- lm(Boro ~ min_T.Boro + max_T.Boro + rainfall.Boro + dist_dummy, data = rice_df)
summary(lsdv_model_boro, robust = TRUE, vcov = vcovHC(lsdv_model_boro, type = "HC1"))


