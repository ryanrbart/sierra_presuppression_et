# Investigate SNAMP NDVI-Canopy Cover relation
# Datasets from Christine Ma

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

snamp_data <- read_csv("data/LC_Plots_2008.csv")

# ---------------------------------------------------------------------

# Linear
data1 <- snamp_data$NDVI_Pre~snamp_data$CC_pre
lm1 <- lm(data1)
broom::glance(lm1)
broom::tidy(lm1)
a1 <- as.numeric(broom::tidy(lm1)[2,2])
b1 <- as.numeric(broom::tidy(lm1)[1,2])

# Exponential
data2 <- snamp_data$NDVI_Pre~exp(snamp_data$CC_pre)
lm2 <- lm(data2)
broom::glance(lm2)
broom::tidy(lm2)
a2 <- as.numeric(broom::tidy(lm2)[2,2])
b2 <- as.numeric(broom::tidy(lm2)[1,2])

# Log
data3 <- snamp_data$NDVI_Pre~log(snamp_data$CC_pre)
lm3 <- lm(data3)
broom::glance(lm3)
broom::tidy(lm3)
a3 <- as.numeric(broom::tidy(lm3)[2,2])
b3 <- as.numeric(broom::tidy(lm3)[1,2])

# Power
data4 <- snamp_data$NDVI_Pre~a*(snamp_data$CC_pre^b)
lm4 <- nls(formula=data4,data=snamp_data,start=list(a=0.8,b=0.2))
broom::glance(lm4)
broom::tidy(lm4)
a4 <- as.numeric(broom::tidy(lm4)[1,2])
b4 <- as.numeric(broom::tidy(lm4)[2,2])


# ---------------------------------------------------------------------
# plot CC-NDVI relations together on untransformed data

curve1 <- function(x,a,b)a*x+b
curve2 <- function(x,a,b)a*exp(x)+b
curve3 <- function(x,a,b)a*log(x)+b
curve4 <- function(x,a,b)a*(x^b)


# All curves
x <- ggplot(snamp_data) +
  geom_point(aes(x=CC_pre, y=NDVI_Pre)) +
  stat_function(fun=curve1,args=list(a=a1, b=b1), size=1.2) +
  #stat_function(fun=curve2,args=list(a=a2, b=b2)) +
  stat_function(fun=curve3,args=list(a=a3, b=b3),linetype=1,color="red", size=1.2) +
  stat_function(fun=curve4,args=list(a=a4, b=b4),linetype=1,color="blue", size=1.2) +
  NULL
plot(x)
ggsave("ndvi_cc_model_comparison.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)

# Linear
ggplot(snamp_data) +
  geom_point(aes(x=CC_pre, y=NDVI_Pre)) +
  stat_function(fun=curve1,args=list(a=a1, b=b1), size=1.2) +
  NULL

# Exponential
ggplot(snamp_data) +
  geom_point(aes(x=exp(CC_pre), y=NDVI_Pre)) +
  
  NULL

# Log
x <- ggplot(snamp_data) +
  geom_point(aes(x=CC_pre, y=NDVI_Pre)) +
  scale_x_continuous(trans='log') +
  stat_function(fun=curve3,args=list(a=a3, b=b3), size=1.2) +
  NULL
plot(x)
ggsave("ndvi_cc_model_log.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)


# Power
x <- ggplot(snamp_data) +
  geom_point(aes(x=CC_pre, y=NDVI_Pre)) +
  labs(x = "Canopy Cover (%)", y = "NDVI") +
  stat_function(fun=curve4,args=list(a=a4, b=b4), size=1.2) +
  theme_bw(base_size = 12) +
  NULL
plot(x)
ggsave("ndvi_cc_model_power.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)


# Power (logged axes)
x <- ggplot(snamp_data) +
  geom_point(aes(x=CC_pre, y=NDVI_Pre)) +
  scale_x_continuous(trans='log') +
  scale_y_continuous(trans='log') +
  stat_function(fun=curve4,args=list(a=a4, b=b4), size=1.2) +
  NULL
plot(x)
ggsave("ndvi_cc_model_power_logged.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Create functions for processing prehistorical changes in NDVI

# --------
# Linear model (breaks down when NDVI start is too low)

cc_ndvi_linear <- function(ndvi_start, cc_reduction, a, b){
  # NDVI_start: Initial NDVI value
  # cc_reduction: # New level of canopy cover as fraction of original canopy cover (e.g. 30% reduction in lai would be 0.7)
  # a: slope of ndvi/cc relation
  # b: intercept of ndvi/cc relation

  # Calculate corresponding canopy cover for ndvi
  cc_start <- (ndvi_start-b)/a
  # Calculate ending canopy cover
  cc_end <- cc_start*cc_reduction
  # Calculate corresponding ndvi for canopy cover
  ndvi_end <- a*cc_end+b
  return(ndvi_end)
}

# --------
# Exponential model (breaks down when NDVI start is below value of b, or ~0.454)

cc_ndvi_exp <- function(ndvi_start, cc_reduction, a, b){
  # NDVI_start: Initial NDVI value
  # cc_reduction: # New level of canopy cover as fraction of original canopy cover (e.g. 30% reduction in lai would be 0.7)
  # a: slope of ndvi/cc relation
  # b: intercept of ndvi/cc relation
  
  # Calculate corresponding canopy cover for ndvi
  cc_start <- log((ndvi_start-b)/a)
  # Calculate ending canopy cover
  cc_end <- cc_start*cc_reduction
  # Calculate corresponding ndvi for canopy cover
  ndvi_end <- a*exp(cc_end)+b
  return(ndvi_end)
}

# ------
# Log model 

cc_ndvi_log <- function(ndvi_start, cc_reduction, a, b){
  # NDVI_start: Initial NDVI value
  # cc_reduction: # New level of canopy cover as fraction of original canopy cover (e.g. 30% reduction in lai would be 0.7)
  # a: slope of ndvi/cc relation
  # b: intercept of ndvi/cc relation
  
  # Calculate corresponding canopy cover for ndvi
  cc_start <- exp((ndvi_start-b)/a)
  # Calculate ending canopy cover
  cc_end <- cc_start*cc_reduction
  # Calculate corresponding ndvi for canopy cover
  ndvi_end <- a*log(cc_end)+b
  return(ndvi_end)
}

# ------
# Power law model 

cc_ndvi_power <- function(ndvi_start, cc_reduction, a, b){
  # NDVI_start: Initial NDVI value
  # cc_reduction: # New level of canopy cover as fraction of original canopy cover (e.g. 30% reduction in lai would be 0.7)
  # a: multiplier of ndvi/cc relation
  # b: exponent of ndvi/cc relation
  
  # Calculate corresponding canopy cover for ndvi
  cc_start <- exp((log(ndvi_start)-log(a))/b)
  # Calculate ending canopy cover
  cc_end <- cc_start*cc_reduction
  # Calculate corresponding ndvi for canopy cover
  ndvi_end <- exp((b*log(cc_end)) + log(a))
  return(ndvi_end)
}



# --------
# Reduction in NDVI (Quantified)
# Adjust initial variable to see patterns 

# Linear
initial = 0.3
end <- cc_ndvi_linear(ndvi_start=initial, cc_reduction=0.5, a=a1, b=b1)
end-initial 
end/initial 

# Log (constant decrease in NDVI for given change in cc)
initial = 0.5
end <- cc_ndvi_log(ndvi_start=initial, cc_reduction=0.5, a=a3, b=b3)
end-initial 
end/initial 


# Power (constant percent decrease in NDVI for given change in cc)
initial = 0.2
end <- cc_ndvi_power(ndvi_start=initial, cc_reduction=0.5, a=a4, b=b4)
end-initial 
end/initial 


