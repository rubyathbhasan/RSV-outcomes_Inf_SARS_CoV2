# Install required packages if you don't have them
#install.packages(c("ggplot2", "dplyr"))

#library(ggplot2)
#library(dplyr)

# 1. Create the dataset based on image_19a1a8.png
df <- data.frame(
  Outcome = rep(c("30-day all cause mortality", 
                  "Intensive care unit admission", 
                  "Mechanical ventilation", 
                  "Length of stay (IRR)"), each = 6),
  Model = rep(rep(c("Unadjusted model", "Adjusted model"), each = 3), times = 4),
  Period = rep(c("Pre-pandemic", "Pandemic", "Post-pandemic"), times = 8),
  Estimate = c(
    # Mortality
    1.52, 1.73, 1.51, 1.36, 1.62, 1.09,
    # ICU
    1.47, 1.33, 0.76, 1.51, 1.25, 0.82,
    # Ventilation
    1.39, 1.35, 0.77, 1.32, 1.24, 0.86,
    # Length of stay
    1.05, 1.15, 1.09, 1.00, 1.06, 1.01
  ),
  Lower_CI = c(
    # Mortality
    1.04, 0.87, 1.00, 0.91, 0.80, 0.71,
    # ICU
    1.00, 0.60, 0.48, 1.00, 0.54, 0.51,
    # Ventilation
    0.73, 0.36, 0.37, 0.69, 0.32, 0.41,
    # Length of stay
    0.96, 0.97, 0.99, 0.93, 0.92, 0.92
  ),
  Upper_CI = c(
    # Mortality
    2.22, 3.45, 2.27, 2.04, 3.33, 1.67,
    # ICU
    2.16, 2.98, 1.21, 2.25, 2.89, 1.31,
    # Ventilation
    2.66, 5.06, 1.62, 2.55, 4.80, 1.83,
    # Length of stay
    1.15, 1.36, 1.19, 1.10, 1.23, 1.11
  )
)

# Enforce logical ordering for the factors so they plot in the correct sequence
df$Period <- factor(df$Period, levels = rev(c("Pre-pandemic", "Pandemic", "Post-pandemic")))
df$Outcome <- factor(df$Outcome, levels = c("30-day all cause mortality", 
                                            "Intensive care unit admission", 
                                            "Mechanical ventilation", 
                                            "Length of stay (IRR)"))

# 2. Generate the Forest Plot
ggplot(df, aes(x = Estimate, y = Period, color = Model)) +
  # Add a vertical dashed line at 1 (Null effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray") +
  
  # Add the points and horizontal error bars, dodged slightly so they don't overlap
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI), 
                  position = position_dodge(width = 0.5), 
                  size = 0.8, linewidth = 1) + # Increased point size and line thickness
  
  # Facet by outcome (creates a 2x2 grid for PowerPoint)
  facet_wrap(~ Outcome, ncol = 2) +
  
  # Custom professional color palette
  scale_color_manual(values = c("Unadjusted model" = "#1B4F72", "Adjusted model" = "#943126")) +
  
  # Apply a log scale with symmetrical limits around 1
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5), limits = c(0.18, 5.5)) +
  
  # Styling
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 16, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    
    # NEW: Increase font sizes for PowerPoint readability
    axis.text.y = element_text(size = 14, face = "bold"), # Makes the periods larger
    axis.text.x = element_text(size = 12),                # Makes the bottom numbers larger
    axis.title.x = element_text(size = 14, margin = margin(t = 10)), # X-axis title
    legend.text = element_text(size = 14),                # Legend text
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 15))
  ) +
  labs(
    title = "Comparison of clinical outcomes between RSV vs Influenza",
    x = "Estimate (Odds Ratio / Incidence Rate Ratio) with 95% CI",
    y = "",
    color = "Model Type"
  )

# Save with PowerPoint-friendly 16:9 dimensions
ggsave("forest_plot_ppt.png", width = 10, height = 5.625, dpi = 300)


# 1. Create the dataset based on image_34038d.png
df <- data.frame(
  Outcome = rep(c("30-day all cause mortality", 
                  "Intensive care unit admission", 
                  "Mechanical ventilation", 
                  "Length of stay (IRR)"), each = 4),
  Model = rep(rep(c("Unadjusted model", "Adjusted model"), each = 2), times = 4),
  Period = rep(c("Pandemic", "Post-pandemic"), times = 8),
  
  # Data extracted in order: Pandemic then Post-pandemic
  Estimate = c(
    # Mortality (Unadj then Adj)
    0.47, 0.94, 0.29, 1.08,
    # ICU
    0.29, 1.21, 0.33, 1.02,
    # Ventilation
    0.19, 2.42, 0.21, 1.85,
    # Length of stay
    0.57, 0.82, 0.61, 0.85
  ),
  Lower_CI = c(
    # Mortality
    0.29, 0.66, 0.17, 0.75,
    # ICU
    0.16, 0.78, 0.17, 0.64,
    # Ventilation
    0.07, 1.16, 0.08, 0.85,
    # Length of stay
    0.50, 0.75, 0.54, 0.79
  ),
  Upper_CI = c(
    # Mortality
    0.78, 1.34, 0.48, 1.55,
    # ICU
    0.54, 1.88, 0.62, 1.60,
    # Ventilation
    0.50, 5.07, 0.59, 4.03,
    # Length of stay
    0.66, 0.89, 0.69, 0.93
  )
)

# Enforce logical ordering for the factors
df$Period <- factor(df$Period, levels = rev(c("Pandemic", "Post-pandemic")))
df$Outcome <- factor(df$Outcome, levels = c("30-day all cause mortality", 
                                            "Intensive care unit admission", 
                                            "Mechanical ventilation", 
                                            "Length of stay (IRR)"))

# 2. Generate the PowerPoint-ready Forest Plot
ggplot(df, aes(x = Estimate, y = Period, color = Model)) +
  # Add a vertical dashed line at 1 (Null effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Points and horizontal error bars, dodged slightly
  geom_pointrange(aes(xmin = Lower_CI, xmax = Upper_CI), 
                  position = position_dodge(width = 0.5), 
                  size = 0.8, linewidth = 1) +
  
  # Facet by outcome (2x2 grid)
  facet_wrap(~ Outcome, ncol = 2) +
  
  # Professional color palette (Gray & Slate)
  scale_color_manual(values = c("Unadjusted model" = "#1B4F72", "Adjusted model" = "#943126")) +
  
  # Log scale with symmetrical limits (0.05 to 20 centers the line at 1)
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5), limits = c(0.05, 20)) +
  
  # PowerPoint styling
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 16, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 15))
  ) +
  labs(
    title = "Comparison of clinical outcomes between RSV vs SARS-CoV-2",
    x = "Estimate (Odds Ratio / Incidence Rate Ratio) with 95% CI",
    y = "",
    color = "Model Type"
  )

# Save with PowerPoint-friendly 16:9 dimensions
ggsave("forest_plot_RSV_vs_SARSCoV2.png", width = 10, height = 5.625, dpi = 300)