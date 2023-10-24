#Convertir csv a objeto
base_datos_objeto <- read.csv("Nueva_base.csv")
#Convertir objeto a data frame 
base_datos_df <- data.frame(base_datos_objeto)

#Data Data Frame saved as: base_datos_df

View(base_datos_df)


# Subset the data for each group (if you haven't already)
stem_cells_data <- subset(base_datos_df, Group == "STEM CELLS")

stem_cells_scaffold_data <- subset(base_datos_df, Group == "STEM CELLS + SCAFFOLD")
placebo_data <- subset(base_datos_df, Group == "PLACEBO")

# Descriptive statistics for IIEF-5 scores after the intervention for each group
summary(stem_cells_data$IIEF_5_score_after)
summary(stem_cells_scaffold_data$IIEF_5_score_after)
summary(placebo_data$IIEF_5_score_after)

str(base_datos_df)

# Subset the data for each group (if you haven't already)
stem_cells_data <- subset(base_datos_df, Group == "STEM CELLS")
stem_cells_scaffold_data <- subset(base_datos_df, Group == "STEM CELLS + SCAFFOLD")
placebo_data <- subset(base_datos_df, Group == "PLACEBO")

# Descriptive statistics for IIEF-5 scores after the intervention for each group
summary(stem_cells_data$IIEF_5_Score_After)
summary(stem_cells_scaffold_data$IIEF_5_Score_After)
summary(placebo_data$IIEF_5_Score_After)

# T-test for IIEF-5 scores before the intervention between groups
ttest_before <- aov(IIEF_5_score_before ~ Group, data = base_datos_df)
summary(ttest_before)

# T-test for IIEF-5 scores after the intervention between groups
ttest_after <- aov(IIEF_5_Score_After ~ Group, data = base_datos_df)
summary(ttest_after)

# T-test for penile length before the intervention between groups
ttest_length_before <- aov(Length_CM_before ~ Group, data = base_datos_df)
summary(ttest_length_before)

# T-test for penile length after the intervention between groups
ttest_length_after <- aov(Length_CM_After ~ Group, data = base_datos_df)
summary(ttest_length_after)

# T-test for penile thickness before the intervention between groups
ttest_thickness_before <- aov(Thickness_CM_before ~ Group, data = base_datos_df)
summary(ttest_thickness_before)

# T-test for penile thickness after the intervention between groups
ttest_thickness_after <- aov(Thickness_CM_After ~ Group, data = base_datos_df)
summary(ttest_thickness_after)




#Box plots to compare before and after



# Set graphical parameters with rotated x-axis labels and adjusted position of the axis label
par(cex.lab=1, cex.axis=0.8, cex.main=1)

# Define custom group names
group_names <- c("Stem Cells", "Scaffold", "Placebo")

# Box plots for IIEF-5 scores before and after the intervention
par(mfrow=c(1,2))
boxplot(IIEF_5_score_before ~ Group, data = base_datos_df, main = "", 
        xlab = "Group", ylab = "IIEF-5 Score", col = "lightblue", las=2, names = group_names)
title("Penile Length Before Intervention", line = 2.5, las = 2)

boxplot(IIEF_5_Score_After ~ Group, data = base_datos_df, main = "", 
        xlab = "Group", ylab = "IIEF-5 Score", col = "lightgreen", las=2, names = group_names)
title("Penile Length After Intervention", line = 2.5, las = 2)

par(mfrow=c(1,1))


# Set graphical parameters with rotated x-axis labels and adjusted position of the axis label
par(cex.lab=1, cex.axis=0.8, cex.main=1)

# Define custom group names
group_names <- c("Stem Cells", "Scaffold", "Placebo")

# Box plots for IIEF-5 scores before and after the intervention
par(mfrow=c(1,2))
boxplot(IIEF_5_score_before ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "IIEF-5 Score", col = "lightblue", las=2, names = group_names)
title("Penile Length Before", line = 2.5, las = 2)

boxplot(IIEF_5_Score_After ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "IIEF-5 Score", col = "lightgreen", las=2, names = group_names)
title("Penile Length After", line = 2.5, las = 2)

par(mfrow=c(1,1))

# Set graphical parameters with rotated x-axis labels and adjusted position of the axis label
par(cex.lab=1, cex.axis=0.8, cex.main=1)

# Define custom group names
group_names <- c("Stem Cells", "Scaffold", "Placebo")

# Box plots for penile length before and after the intervention
par(mfrow=c(1,2))
boxplot(Length_CM_before ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "Penile Length (cm)", col = "lightblue", las=2, names = group_names)
title("Penile Length Before", line = 2.5, las = 2)

boxplot(Length_CM_After ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "Penile Length (cm)", col = "lightgreen", las=2, names = group_names)
title("Penile Length After", line = 2.5, las = 2)

par(mfrow=c(1,1))


# Set graphical parameters with rotated x-axis labels and adjusted position of the axis label
par(cex.lab=1, cex.axis=0.8, cex.main=1)

# Define custom group names
group_names <- c("Stem Cells", "Scaffold", "Placebo")

# Box plots for penile thickness before and after the intervention
par(mfrow=c(1,2))
boxplot(Thickness_CM_before ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "Penile Thickness (cm)", col = "lightblue", las=2, names = group_names)
title("Penile Thickness Before", line = 2.5, las = 2)

boxplot(Thickness_CM_After ~ Group, data = base_datos_df, main = "", 
        xlab = "", ylab = "Penile Thickness (cm)", col = "lightgreen", las=2, names = group_names)
title("Penile Thickness After", line = 2.5, las = 2)

par(mfrow=c(1,1))


#HASTA AQUI YA TENEMOS LOS 3 BOX PLOTS COMPARANDO TAMAÑO, GROSOR , 
#Y CAMBIOS EN EL IEF SCORE 

#AHORA HARE ANALISIS LONGITUDINAL

# Paired t-tests for IIEF-5 scores within each group
stem_cells_diff <- stem_cells_data$IIEF_5_Score_After - stem_cells_data$IIEF_5_score_before
scaffold_diff <- stem_cells_scaffold_data$IIEF_5_Score_After - stem_cells_scaffold_data$IIEF_5_score_before
placebo_diff <- placebo_data$IIEF_5_Score_After - placebo_data$IIEF_5_score_before

t_test_stem_cells <- t.test(stem_cells_diff, alternative = "two.sided")
t_test_scaffold <- t.test(scaffold_diff, alternative = "two.sided")
t_test_placebo <- t.test(placebo_diff, alternative = "two.sided")

# Print the results
print("Paired t-test for Stem Cells Group:")
print(t_test_stem_cells)
print("Paired t-test for Scaffold Group:")
print(t_test_scaffold)
print("Paired t-test for Placebo Group:")
print(t_test_placebo)

# Wilcoxon signed-rank tests for penile length within each group
wilcox_test_length_stem_cells <- wilcox.test(stem_cells_data$Length_CM_After, stem_cells_data$Length_CM_before, paired = TRUE)
wilcox_test_length_scaffold <- wilcox.test(stem_cells_scaffold_data$Length_CM_After, stem_cells_scaffold_data$Length_CM_before, paired = TRUE)
wilcox_test_length_placebo <- wilcox.test(placebo_data$Length_CM_After, placebo_data$Length_CM_before, paired = TRUE)

# Wilcoxon signed-rank tests for penile thickness within each group
wilcox_test_thickness_stem_cells <- wilcox.test(stem_cells_data$Thickness_CM_After, stem_cells_data$Thickness_CM_before, paired = TRUE)
wilcox_test_thickness_scaffold <- wilcox.test(stem_cells_scaffold_data$Thickness_CM_After, stem_cells_scaffold_data$Thickness_CM_before, paired = TRUE)
wilcox_test_thickness_placebo <- wilcox.test(placebo_data$Thickness_CM_After, placebo_data$Thickness_CM_before, paired = TRUE)

# Print the results
print("Wilcoxon Signed-Rank Test for Penile Length (Stem Cells Group):")
print(wilcox_test_length_stem_cells)
print("Wilcoxon Signed-Rank Test for Penile Length (Scaffold Group):")
print(wilcox_test_length_scaffold)
print("Wilcoxon Signed-Rank Test for Penile Length (Placebo Group):")
print(wilcox_test_length_placebo)

print("Wilcoxon Signed-Rank Test for Penile Thickness (Stem Cells Group):")
print(wilcox_test_thickness_stem_cells)
print("Wilcoxon Signed-Rank Test for Penile Thickness (Scaffold Group):")
print(wilcox_test_thickness_scaffold)
print("Wilcoxon Signed-Rank Test for Penile Thickness (Placebo Group):")
print(wilcox_test_thickness_placebo)



#create side-by-side box plots for comparing the distribution of IIEF-5 
#scores and penile measurements before and after the intervention

# Set graphical parameters with rotated x-axis labels and adjusted position of the axis label
par(cex.lab=1, cex.axis=0.8, cex.main=1)

# Define custom group names
group_names <- c("Stem Cells", "Scaffold", "Placebo")

# Box plots for IIEF-5 scores before and after the intervention
par(mfrow=c(2,2))

# Box plot for IIEF-5 scores before the intervention
boxplot(IIEF_5_score_before ~ Group, data = base_datos_df, main = "IIEF-5 Score Before Intervention", 
        xlab = "", ylab = "IIEF-5 Score", col = "lightblue", las=2, names = group_names)

# Box plot for IIEF-5 scores after the intervention
boxplot(IIEF_5_Score_After ~ Group, data = base_datos_df, main = "IIEF-5 Score After Intervention", 
        xlab = "", ylab = "IIEF-5 Score", col = "lightgreen", las=2, names = group_names)

# Box plot for penile length before the intervention
boxplot(Length_CM_before ~ Group, data = base_datos_df, main = "Penile Length Before Intervention", 
        xlab = "", ylab = "Penile Length (CM)", col = "lightblue", las=2, names = group_names)

# Box plot for penile length after the intervention
boxplot(Length_CM_After ~ Group, data = base_datos_df, main = "Penile Length After Intervention", 
        xlab = "", ylab = "Penile Length (CM)", col = "lightgreen", las=2, names = group_names)

par(mfrow=c(1,1))

#Effect Size Calculation:
#Calculate effect sizes (Cohen's d or Hedges' g) to quantify the magnitude of 
#differences between groups and within groups for meaningful interpretation.
install.packages("effsize")
library(effsize)

# Calculate mean and standard deviation for each group before intervention
mean_before <- tapply(base_datos_df$IIEF_5_score_before, base_datos_df$Group, mean)
sd_before <- tapply(base_datos_df$IIEF_5_score_before, base_datos_df$Group, sd)

# Calculate Cohen's d for differences between groups before intervention
cohen_d_before <- (mean_before["STEM CELLS"] - mean_before["STEM CELLS + SCAFFOLD"]) / sqrt((sd_before["STEM CELLS"]^2 + sd_before["STEM CELLS + SCAFFOLD"]^2) / 2)

# Calculate mean and standard deviation for each group after intervention
mean_after <- tapply(base_datos_df$IIEF_5_Score_After, base_datos_df$Group, mean)
sd_after <- tapply(base_datos_df$IIEF_5_Score_After, base_datos_df$Group, sd)

# Calculate Cohen's d for differences between groups after intervention
cohen_d_after <- (mean_after["STEM CELLS"] - mean_after["STEM CELLS + SCAFFOLD"]) / sqrt((sd_after["STEM CELLS"]^2 + sd_after["STEM CELLS + SCAFFOLD"]^2) / 2)

# Print the results
print("Cohen's d for Differences Between Groups (Before Intervention):")
print(cohen_d_before)

print("Cohen's d for Differences Between Groups (After Intervention):")
print(cohen_d_after)

