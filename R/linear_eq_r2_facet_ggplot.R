# Load necessary libraries
library(ggplot2)

# Sample dataset with categories
data <- data.frame(x = double(), y = double(), category = character())
for (let in letters[1:5]) {
  
  sub_data <- data.frame(
    x = sort(sample(1:100, 100, replace = TRUE)), 
    y = sort(rnorm(100, mean = sample(1:20, 1), sd = sample(1:8, 1))), 
    category = rep(let, 100)
  )
  
  data <- rbind(data, sub_data)
  
}

# data$category <- as.factor(data$category)

# Function to create linear model and plot
create_linear_model_plot <- function(data, category_value) {
  # Subset data based on category
  subset_data <- subset(data, category == category_value)
  
  # Fit linear model
  model <- lm(y ~ x, data = subset_data)
  
  # Extract coefficients
  coef <- coef(model)
  intercept <- coef[1]
  slope <- coef[2]
  
  # Calculate R-squared
  r2 <- summary(model)$r.squared
  
  # Create plot
  p <- ggplot(subset_data, aes(x = x, y = y)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "blue") + 
    labs(title = paste("Category:", category_value), 
         x = "X", y = "Y") + 
    ylim(min(data$y)*1.10, max(data$y)*1.10) + 
    xlim(0, 100) + 
    annotate("text", 
             x = 0, y = max(data$y)*1.05, 
             label = paste("y =", 
                           round(intercept, 2), "+", 
                           round(slope, 2), 
                           "x\nR^2 =", 
                           round(r2, 3)), 
             hjust = 0) + 
    theme_minimal()
  
  return(p)
}

# Create separate plots for each category
plots <- lapply(sort(unique(data$category)), 
                function(category) create_linear_model_plot(data = data, 
                                                            category_value = category))


# Combine plots and facet-wrap based on category
multiplot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

