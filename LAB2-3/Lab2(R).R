```r
# ==============================
# Load libraries and dataset
# ==============================
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)   # for skewness, kurtosis

# Load dataset
taxi <- read.csv("yellow_tripdata_sample.csv")

# Preview
glimpse(taxi)
```

```r
# ==============================
# Part A: Descriptive Statistics
# ==============================

num_vars <- c("passenger_count", "trip_distance", "fare_amount", 
              "total_amount", "tip_amount", "extra")

# Summary statistics
summary_stats <- taxi %>% 
  select(all_of(num_vars)) %>% 
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm=TRUE),
    median = ~median(.x, na.rm=TRUE),
    sd = ~sd(.x, na.rm=TRUE),
    var = ~var(.x, na.rm=TRUE),
    min = ~min(.x, na.rm=TRUE),
    max = ~max(.x, na.rm=TRUE),
    skew = ~skewness(.x, na.rm=TRUE),
    kurt = ~kurtosis(.x, na.rm=TRUE),
    n_missing = ~sum(is.na(.x))
  ), .names="{col}_{fn}"))

print(summary_stats)
```

```r
# ==================
# Visualizations
# ==================

# Histogram + Density
for (v in c("trip_distance", "fare_amount", "tip_amount")) {
  print(
    ggplot(taxi, aes_string(v)) + 
      geom_histogram(aes(y=..density..), fill="skyblue", bins=50, alpha=0.7) +
      geom_density(color="red", size=1) +
      ggtitle(paste("Histogram & Density of", v))
  )
}

# Boxplot
for (v in c("fare_amount", "trip_distance")) {
  print(
    ggplot(taxi, aes_string(y=v)) +
      geom_boxplot(fill="orange", alpha=0.6) +
      ggtitle(paste("Boxplot of", v))
  )
}

# Bar chart (categorical: payment_type)
ggplot(taxi, aes(factor(payment_type))) +
  geom_bar(fill="steelblue") +
  ggtitle("Distribution of Payment Type")

# Pie chart (VendorID)
taxi %>% count(VendorID) %>% 
  ggplot(aes(x="", y=n, fill=factor(VendorID))) +
  geom_col() +
  coord_polar(theta="y") +
  ggtitle("VendorID Proportions")
```

```r
# ==============================
# Part B: Inferential Statistics
# ==============================

# 95% CI for means
ci <- function(x) {
  t.test(x, conf.level=0.95)$conf.int
}

ci_trip <- ci(taxi$trip_distance)
ci_fare <- ci(taxi$fare_amount)
ci_tip <- ci(taxi$tip_amount)

print(list(CI_trip=ci_trip, CI_fare=ci_fare, CI_tip=ci_tip))
```

```r
# One-sample t-test: tip amount vs $2
t.test(taxi$tip_amount, mu=2)
```

```r
# Two-sample t-test: fare amount between credit(1) vs cash(2)
fare_credit <- taxi %>% filter(payment_type==1) %>% pull(fare_amount)
fare_cash   <- taxi %>% filter(payment_type==2) %>% pull(fare_amount)
t.test(fare_credit, fare_cash)
```

```r
# Chi-square test: payment_type vs RateCodeID
tab <- table(taxi$payment_type, taxi$RatecodeID)
chisq.test(tab)
```

```r
# ==================
# Correlation Analysis
# ==================

# Pearson correlations
cor_td_fare <- cor(taxi$trip_distance, taxi$fare_amount, use="complete.obs")
cor_fare_tip <- cor(taxi$fare_amount, taxi$tip_amount, use="complete.obs")

print(list(cor_trip_fare=cor_td_fare, cor_fare_tip=cor_fare_tip))

# Correlation heatmap
library(GGally)
taxi %>% 
  select(trip_distance, fare_amount, tip_amount) %>%
  ggcorr(label=TRUE)
```

```r
# ==================
# Bonus: Time series
# ==================

# Convert pickup_datetime if present
if("tpep_pickup_datetime" %in% names(taxi)) {
  taxi$tpep_pickup_datetime <- as.POSIXct(taxi$tpep_pickup_datetime)
  taxi %>% 
    mutate(date = as.Date(tpep_pickup_datetime)) %>%
    count(date) %>%
    ggplot(aes(date, n)) + geom_line(color="blue") +
    ggtitle("Trip Count per Day")
}
```
