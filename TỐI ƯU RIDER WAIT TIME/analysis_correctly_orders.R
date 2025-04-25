
# ---- Bước 1: Đọc dữ liệu ----
library(readr)
library(dplyr)

df <- read_csv("order_history_kaggle_data.csv")

# ---- Bước 2: Lọc các đơn hàng 'Correctly' ----
df_correct <- df %>%
  filter(`Order Ready Marked` == "Correctly")

total_orders <- nrow(df)
correct_orders <- nrow(df_correct)
percent_correct <- correct_orders / total_orders * 100
cat("Tỷ lệ đơn hàng 'Correctly':", round(percent_correct, 2), "%\n")

# ---- Bước 3: Thống kê Rider Wait Time ----
summary(df_correct$`Rider Wait Time (minutes)`)

# ---- Bước 4: Chọn ngưỡng và tính toán ----
threshold <- 10
acceptable_count <- df_correct %>%
  filter(`Rider Wait Time (minutes)` <= threshold) %>%
  nrow()

acceptable_percent <- acceptable_count / correct_orders * 100
cat("Tỷ lệ đơn 'Correctly' có Rider Wait Time ≤", threshold, "phút:", round(acceptable_percent, 2), "%\n")

# ---- Bước 5: Kết luận ngắn gọn ----
cat("\nKết luận:\n")
cat("- Có", round(percent_correct, 2), "% đơn được đánh dấu 'Correctly'\n")
cat("- Trong đó,", round(acceptable_percent, 2), "% Rider không phải chờ quá 10 phút\n")
