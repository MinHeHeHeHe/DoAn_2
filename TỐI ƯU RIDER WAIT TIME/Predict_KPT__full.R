# Dự đoán thời gian chuẩn bị đơn hàng (KPT) bằng Random Forest
# Mục tiêu: Dự đoán KPT để tối ưu thời gian thông báo tài xế, giảm thời gian chờ
# Dữ liệu: data_has_Correctly.csv, sample_orders.csv
# Công cụ: dplyr, tidyr, lubridate, randomForest, fastDummies
setwd("D:")
# 1. Nhập thư viện
library(dplyr)
library(tidyr)
library(lubridate)
library(randomForest)
library(fastDummies)
library(ggplot2)


# 2. Đọc dữ liệu
# Đọc file data_has_Correctly.csv và kiểm tra 5 dòng đầu
df_raw <- read.csv("data_has_Correctly.csv")
head(df_raw, 5)

# 3. Chuyển đổi thời gian
# Chuyển cột Order.Placed.At sang định dạng datetime, loại bỏ NA
df_raw$Order.Placed.At <- as.POSIXct(df_raw$Order.Placed.At, format="%I:%M %p, %B %d %Y", tz="UTC")
df_raw <- df_raw %>% filter(!is.na(Order.Placed.At))
head(df_raw[, "Order.Placed.At", drop=FALSE], 5)

# 4. Làm sạch dữ liệu
# Loại bỏ dòng thiếu giá trị ở các cột quan trọng
df <- df_raw %>% 
  filter(!is.na(Order.ID) & !is.na(Restaurant.ID) & !is.na(Order.Placed.At) & 
           !is.na(Items.in.order) & !is.na(KPT.duration..minutes.))
str(df)

# 5. Tạo đặc trưng
# Hàm đếm số món từ Items.in.order
count_items <- function(item_str) {
  if (is.na(item_str) || item_str == "") return(0)
  matches <- gregexpr("\\d+x", item_str)[[1]]
  if (matches[1] != -1) {
    quantities <- as.numeric(gsub("x", "", regmatches(item_str, matches)))
    return(sum(quantities))
  } else {
    return(length(strsplit(item_str, ",")[[1]]))
  }
}

# Hàm tách tên món
extract_item_names <- function(item_str) {
  if (is.na(item_str) || item_str == "") return(character(0))
  items <- trimws(gsub("^\\s*\\d+x\\s*", "", strsplit(item_str, ",")[[1]]))
  return(tolower(items))
}

# Áp dụng: tạo num_items, item_list, hour, weekday
df$num_items <- sapply(df$Items.in.order, count_items)
df$item_list <- lapply(df$Items.in.order, extract_item_names)
df$hour <- hour(df$Order.Placed.At)
df$weekday <- wday(df$Order.Placed.At, week_start=1) - 1

# 6. Mã hóa đặc trưng
# 1. Đếm tần suất món ăn và chọn top 50 món phổ biến
item_freq <- table(unlist(df$item_list))
top_items <- names(sort(item_freq, decreasing = TRUE))[1:32]

# 2. Giữ lại các món thuộc top_items
df$item_list <- lapply(df$item_list, function(x) intersect(x, top_items))

# 3. Tạo ma trận one-hot
df_items <- df %>% 
  mutate(item_list = lapply(item_list, function(x) paste(x, collapse=","))) %>%
  dummy_cols(select_columns="item_list", split=",")

# Gộp đặc trưng số và món ăn
df_features <- df %>% 
  select(num_items, hour, weekday) %>%
  bind_cols(df_items %>% select(starts_with("item_list_")))

names(df_features)


# 7. Xử lý outlier và chuẩn bị dữ liệu
# Loại outlier của KPT.duration..minutes. bằng IQR
Q1 <- quantile(df$KPT.duration..minutes., 0.25)
Q3 <- quantile(df$KPT.duration..minutes., 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

df_features <- df_features[df$KPT.duration..minutes. >= lower & df$KPT.duration..minutes. <= upper, ]
df <- df[df$KPT.duration..minutes. >= lower & df$KPT.duration..minutes. <= upper, ]

# Chuẩn bị X (đặc trưng) và y (mục tiêu)
X <- df_features
y <- df$KPT.duration..minutes.

# 8. Chia dữ liệu huấn luyện và kiểm tra
# Chia 80% huấn luyện, 20% kiểm tra
set.seed(42)
train_indices <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]
names(X_train) <- make.names(names(X_train))
names(X_test) <- make.names(names(X_test))
# 9. Huấn luyện mô hình Random Forest
# Khởi tạo và huấn luyện mô hình
rf_model <- randomForest(y ~ ., data=cbind(X_train, y=y_train), ntree=100)

# 10. Dự đoán trên dữ liệu mẫu
# Đọc và tiền xử lý sample_orders.csv
sample_df <- read.csv("sample_orders.csv")
sample_df$Order.Placed.At <- as.POSIXct(sample_df$Order.Placed.At, format="%I:%M %p, %B %d %Y", tz="UTC")
sample_df <- sample_df %>% filter(!is.na(Order.Placed.At))
sample_df$hour <- hour(sample_df$Order.Placed.At)
sample_df$weekday <- wday(sample_df$Order.Placed.At, week_start=1) - 1
sample_df$num_items <- sapply(sample_df$Items.in.order, count_items)
sample_df$item_list <- lapply(sample_df$Items.in.order, extract_item_names)

# Mã hóa món ăn cho dữ liệu mẫu
sample_items <- sample_df %>% 
  mutate(item_list = lapply(item_list, function(x) paste(x, collapse=","))) %>%
  dummy_cols(select_columns="item_list", split=",")

# Gộp đặc trưng
X_sample <- sample_df %>% 
  select(num_items, hour, weekday) %>%
  bind_cols(sample_items %>% select(starts_with("item_list_")))

# Đảm bảo cột khớp với X_train
missing_cols <- setdiff(names(X_train), names(X_sample))
for (col in missing_cols) {
  X_sample[[col]] <- 0
}
X_sample <- X_sample[, names(X_train)]

# Dự đoán KPT
sample_df$Predicted.KPT <- predict(rf_model, X_sample)

# Hiển thị kết quả
sample_df %>% 
  select(Order.ID, Items.in.order, Predicted.KPT, KPT.duration..minutes.) %>%
  head(10)

# 11. Tối ưu hóa thời gian chờ tài xế
# Lọc các đơn có thời gian chờ tài xế > 7 phút
df_wait <- df[df$Rider.wait.time..minutes. > 7, ]

# Tạo lại đặc trưng tương tự như X
df_wait$num_items <- sapply(df_wait$Items.in.order, count_items)
df_wait$item_list <- lapply(df_wait$Items.in.order, extract_item_names)
df_wait$hour <- hour(df_wait$Order.Placed.At)
df_wait$weekday <- wday(df_wait$Order.Placed.At, week_start=1) - 1
df_wait$item_list <- lapply(df_wait$item_list, function(x) intersect(x, top_items))

# Tạo ma trận one-hot
df_wait_items <- df_wait %>% 
  mutate(item_list = lapply(item_list, function(x) paste(x, collapse=","))) %>%
  dummy_cols(select_columns="item_list", split=",")

# Gộp đặc trưng
X_wait <- df_wait %>% 
  select(num_items, hour, weekday) %>%
  bind_cols(df_wait_items %>% select(starts_with("item_list_")))

# Thêm các cột bị thiếu so với X_train
missing_cols <- setdiff(names(X_train), names(X_wait))
for (col in missing_cols) {
  X_wait[[col]] <- 0
}
X_wait <- X_wait[, names(X_train)]  # Sắp xếp lại cột

# Dự đoán KPT
df_wait$KPT_pred <- predict(rf_model, X_wait)

# Hiển thị kết quả
df_wait %>% 
  select(Order.ID, Items.in.order, Rider.wait.time..minutes., KPT.duration..minutes., KPT_pred) %>%
  head(20)
