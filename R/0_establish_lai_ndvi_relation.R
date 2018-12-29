# Calculate the change in NDVI for a given loss of biomass/lai
# 

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Reminder of how log transforms work

# log relation
# y = ab^x
# log(y) = log(a) + x log(b)
# If Y = log(y), A = log(a) and B = log(b), then Y = A + B(x)

# Back calculating from Gamon plot (x axis is logged instead of y-axis in above example)
# y = A + B(X)
# y = A + B * log(x)
# y = log(a) + log(b) * log (x)
# exp(y) = ab^log(x)
# y = log(ab^log(x))


# ---------------------------------------------------------------------
# Estimate the relationship between LAI and NDVI from Gamon et al. 1995

# Estimate of line values from Figure 4b and Figure 2d
x <- c(-3, -2, -1, 0, 1, 2)
y <- c(0.25, 0.35, 0.45, 0.55, 0.65, 0.75)

happy <- broom::tidy(lm(y~x))

A <- happy$estimate[1]
B <- happy$estimate[2]

# These following calculations aren't actually needed
#a <- exp(A)
#b <- exp(B)
# Check
#curve(log(a*b^log(x)), from=0.01, to=14)
#curve(log(a*b^log(x)), from=0.001, to=14, log="x")


# ---------------------------------------------------------------------
# Function for determining new NDVI from original NDVI given a fractino of biomass/lai loss

ndvi_adjust <- function(lai_change, A, B){
  # lai_change: New level of biomass/lai as fraction of original biomass/lai (e.g. 30% reduction in lai would be 0.7)
  # A: Intercept from fig 4b 
  # B: slope from fig 4b
  # NDVI_diff: Change in NDVI from original NDVI (NDVI units)
  
  NDVI_orig <- 0.5  # NDVI_diff is the same no matter the starting NDVI value 
  
  # For linear equation (4b), find X value associated with original NDVI 
  X_orig <- (NDVI_orig-A)/B
  # Find new X value
  X_new <- log(exp(X_orig)*lai_change)
  # Find NDVI value associated with new X value
  NDVI_new <- A + B*X_new
  NDVI_diff <- NDVI_new - NDVI_orig

  return(NDVI_diff)
}

# Example function
# ndvi_adjust(lai_change = 0.7, A = A, B = B)


