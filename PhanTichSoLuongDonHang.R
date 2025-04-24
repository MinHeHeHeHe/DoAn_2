#File này dùng để phân tích số lượng đơn hàng theo các khung thời gian cách nhau 6 tiếng.
#Áp dụng auto sarima model.

# Thư viện cho việc tiền xử lý
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#Thư viện cho ADF Test
library(tseries)

#Thư viện cho mô hình
library(forecast)   

#Thư viện tính 4 chỉ số MSE, MAE, R squared, Adjusted
library(Metrics)
library(ModelMetrics)
# Đọc dữ liệu từ tệp CSV
df <- read.csv("cleaned_order_history.csv", stringsAsFactors = FALSE)
head(df)

# Chuyển sang datetime đúng format
df$Order.Placed.At <- as.POSIXct(df$Order.Placed.At, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata")
head(df$Order.Placed.At, 20)
class(df$Order.Placed.At)


# Tạo cột 'Order Hour Range' để nhóm theo từng khung giờ 6 tiếng
df <- df %>%
  mutate(Order_Hour_Range = floor_date(Order.Placed.At, unit = "6 hours"))

# Đếm số lượng đơn hàng theo từng khung giờ
order_counts <- df %>%
  group_by(Order_Hour_Range) %>%
  summarise(Order_Count = n()) %>%
  ungroup()

# Tạo một dãy thời gian đầy đủ từ thời điểm sớm nhất đến muộn nhất với bước 6 tiếng
full_time_seq <- seq(from = min(order_counts$Order_Hour_Range),
                     to = max(order_counts$Order_Hour_Range),
                     by = "6 hours")

# Tạo DataFrame đầy đủ tất cả các khung giờ
full_df <- data.frame(Order_Hour_Range = full_time_seq)

# Merge với bảng order_counts để điền các khung giờ không có đơn hàng
full_order_counts <- full_df %>%
  left_join(order_counts, by = "Order_Hour_Range") %>%
  arrange(Order_Hour_Range) %>%
  mutate(Order_Count = replace_na(Order_Count, 0))

# Vẽ biểu đồ đường thể hiện số đơn hàng theo từng khung giờ theo thời gian
ggplot(order_counts, aes(x = Order_Hour_Range, y = Order_Count)) +
  geom_line(color = "steelblue") +
  labs(title = "Số lượng đơn hàng theo từng khung giờ 6 tiếng",
       x = "Khung giờ", y = "Số lượng đơn hàng") +
  theme_minimal()

# Vẽ biểu đồ Top 20 khung giờ có số lượng đơn hàng cao nhất
top20 <- order_counts %>%
  arrange(desc(Order_Count)) %>%
  slice_head(n = 20)

ggplot(top20, aes(x = reorder(as.character(Order_Hour_Range), -Order_Count), y = Order_Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Top 20 khung giờ có nhiều đơn hàng nhất",
       x = "Khung giờ", y = "Số lượng đơn hàng") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Vẽ biểu đồ Bottom 20 khung giờ có số lượng đơn hàng ít nhất
bottom20 <- order_counts %>%
  arrange(Order_Count) %>%
  slice_head(n = 20)

ggplot(bottom20, aes(x = reorder(as.character(Order_Hour_Range), Order_Count), y = Order_Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Bottom 20 khung giờ có ít đơn hàng nhất",
       x = "Khung giờ", y = "Số lượng đơn hàng") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Xuất bảng sắp xếp khung giờ theo số lượng đơn hàng tăng dần (hiển thị 100 dòng đầu tiên)
sorted_full_order_counts <- full_order_counts %>%
  arrange(Order_Count)

head(sorted_full_order_counts, 20)

head(full_order_counts,20)
class(full_order_counts$Order_Hour_Range)

#Thư viện để vẽ ACF và PACF
library(forecast)

acf(full_order_counts$Order_Count, main = "ACF of Full Order Counts")
pacf(full_order_counts$Order_Count, main = "PACF of Full Order Counts")

#Phân rã chuỗi thời gian bằng STL
full_order_counts_ts <- ts(full_order_counts$Order_Count, frequency = 4) 
stl_decomposition <- stl(full_order_counts_ts, s.window = "periodic")
#Vẽ kết quả
plot(stl_decomposition$time.series[, "seasonal"], main = "Seasonal Component", col = "green")
plot(stl_decomposition)



# Vẽ biểu đồ và hiển thị nhãn tại từng điểm
filtered_df <- full_order_counts[full_order_counts$Order_Count >= 150 & full_order_counts$Order_Count <= 200, ]
ggplot(filtered_df, aes(x = Order_Hour_Range, y = Order_Count)) +
  geom_line(color = "blue") +
  geom_point(color = "darkblue", size = 2) +  # thêm điểm để dễ thấy
  geom_text(aes(label = paste0(Order_Hour_Range, ": ", Order_Count)),
            vjust = -0.5, size = 3, angle = 45) +  # nhãn hơi nghiêng cho đỡ đụng nhau
  labs(title = "Khung giờ có từ 150 đến 200 đơn hàng",
       x = "Thời gian", y = "Số đơn hàng") +
  theme_minimal()

#Kiểm định độ dừng nhờ ADF Test
adf.test(full_order_counts$Order_Count)


library(dplyr)
# Giữ chỉ cột 'Order Count' và loại bỏ 'Order Hour Range'
full_order_counts <- full_order_counts %>% select(Order_Count)
head(full_order_counts)



# DÙNG SARIMA ĐỂ HUẤN LUYỆN MÔ HÌNH VÀ DỰ BÁO CHO FOLD 5
library(forecast)  # Để sử dụng auto.arima và forecast
library(caret)     # Để sử dụng createTimeSlices

# Bước 1: Tạo chuỗi thời gian với frequency = 4 (mỗi ngày 4 điểm => chu kỳ 1 ngày)
ts_data <- ts(full_order_counts$Order_Count, frequency = 4)

# Bước 2: Tạo fold cho TimeSeriesCrossValidation
n_splits <- 5
tscv <- createTimeSlices(1:length(ts_data), initialWindow = floor(length(ts_data) / n_splits), 
                         horizon = floor(length(ts_data) / n_splits), fixedWindow = TRUE)

# Khởi tạo danh sách để lưu chỉ số
mse_list <- c()
mae_list <- c()
r2_list <- c()
adj_r2_list <- c()

# Bước 3: Lặp qua từng fold
for (i in 1:n_splits) {
  # Lấy chỉ số train/test cho fold hiện tại
  train_indices <- tscv$train[[i]]
  test_indices <- tscv$test[[i]]
  
  # Tạo chuỗi thời gian cho tập train/test
  train_ts <- ts(ts_data[train_indices], frequency = 4)
  test_ts <- ts(ts_data[test_indices], frequency = 4)
  
  # Huấn luyện mô hình ARIMA
  model <- auto.arima(train_ts, seasonal = TRUE)
  forecast_values <- forecast(model, h = length(test_ts))
  
  # So sánh dự báo với thực tế
  actual <- as.numeric(test_ts)
  predicted <- as.numeric(forecast_values$mean)
  
  # Tính toán các chỉ số
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  ss_total <- sum((actual - mean(actual))^2)
  ss_res <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_res / ss_total)
  
  n_obs <- length(actual)
  p <- length(model$coef)
  adj_r_squared <- 1 - ((1 - r_squared) * (n_obs - 1)) / (n_obs - p - 1)
  
  # Lưu kết quả
  mse_list <- c(mse_list, mse)
  mae_list <- c(mae_list, mae)
  r2_list <- c(r2_list, r_squared)
  adj_r2_list <- c(adj_r2_list, adj_r_squared)
}

# Bước 4: Tính trung bình các chỉ số qua 5 fold
cat("==== Kết quả trung bình qua 5 fold ====\n")
cat("MSE trung bình:", round(mean(mse_list), 2), "\n")
cat("MAE trung bình:", round(mean(mae_list), 2), "\n")
cat("R-squared trung bình:", round(mean(r2_list), 4), "\n")
cat("Adjusted R-squared trung bình:", round(mean(adj_r2_list), 4), "\n")



#SỬ DỤNG SARIMA VỚI CÁC CHỈ SỐ ĐỊNH SẴN
# Cập nhật thông số SARIMA mong muốn
p <- 0; d <- 0; q <- 1
P <- 1; D <- 0; Q <- 2
seasonal_period <- 4

# Bước 1: Tạo chuỗi thời gian
ts_data <- ts(full_order_counts$Order_Count, frequency = seasonal_period)

# Bước 2: Tạo fold cho TimeSeries Cross-Validation
n_splits <- 5
tscv <- createTimeSlices(1:length(ts_data), initialWindow = floor(length(ts_data) / n_splits), 
                         horizon = floor(length(ts_data) / n_splits), fixedWindow = TRUE)

# Khởi tạo danh sách lưu chỉ số
mse_list <- c()
mae_list <- c()
r2_list <- c()
adj_r2_list <- c()

# Bước 3: Lặp qua từng fold
for (i in 1:n_splits) {
  train_indices <- tscv$train[[i]]
  test_indices <- tscv$test[[i]]
  
  train_ts <- ts(ts_data[train_indices], frequency = seasonal_period)
  test_ts <- ts(ts_data[test_indices], frequency = seasonal_period)
  
  # Huấn luyện mô hình SARIMA với thông số cụ thể
  model <- Arima(train_ts, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = seasonal_period), method = "ML")
  forecast_values <- forecast(model, h = length(test_ts))
  
  # So sánh với dữ liệu thực tế
  actual <- as.numeric(test_ts)
  predicted <- as.numeric(forecast_values$mean)
  
  # Tính toán các chỉ số
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  ss_total <- sum((actual - mean(actual))^2)
  ss_res <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_res / ss_total)
  
  n_obs <- length(actual)
  p_model <- length(model$coef)
  adj_r_squared <- 1 - ((1 - r_squared) * (n_obs - 1)) / (n_obs - p_model - 1)
  
  # Lưu kết quả
  mse_list <- c(mse_list, mse)
  mae_list <- c(mae_list, mae)
  r2_list <- c(r2_list, r_squared)
  adj_r2_list <- c(adj_r2_list, adj_r_squared)
}

# Bước 4: Trung bình kết quả
cat("==== Kết quả trung bình qua 5 fold ====\n")
cat("MSE trung bình:", round(mean(mse_list), 2), "\n")
cat("MAE trung bình:", round(mean(mae_list), 2), "\n")
cat("R-squared trung bình:", round(mean(r2_list), 4), "\n")
cat("Adjusted R-squared trung bình:", round(mean(adj_r2_list), 4), "\n")


library(forecast)
library(lubridate)

# --- Đảm bảo dữ liệu thời gian ---
full_order_counts$Order_Hour_Range <- as.POSIXct(full_order_counts$Order_Hour_Range)

# --- Chuyển dữ liệu thành chuỗi thời gian ---
seasonal_period <- 28  # 7 ngày * 4 khung giờ
ts_data <- ts(full_order_counts$Order_Count, frequency = seasonal_period)

# --- Huấn luyện mô hình SARIMA với tham số cụ thể ---
model <- Arima(ts_data, 
               order = c(1, 0, 2), 
               seasonal = list(order = c(1, 0, 2), period = seasonal_period))

# --- Dự báo 28 bước tiếp theo (7 ngày) ---
forecast_steps <- 28
forecast_values <- forecast(model, h = forecast_steps)

# ====== 🎯 Tính các chỉ số thủ công ======
fitted_values <- fitted(model)  # Giá trị dự đoán in-sample
actual_values <- as.numeric(ts_data)  # Giá trị thực tế

# MSE
mse_value <- mean((actual_values - fitted_values)^2)

# MAE
mae_value <- mean(abs(actual_values - fitted_values))

# R2
ss_res <- sum((actual_values - fitted_values)^2)  # Tổng bình phương sai số còn lại
ss_tot <- sum((actual_values - mean(actual_values))^2)  # Tổng bình phương sai số tổng
r2_value <- 1 - ss_res/ss_tot

# Adjusted R2
n <- length(actual_values)   # số lượng mẫu
k <- length(model$coef)      # số tham số ước lượng
r2_adj_value <- 1 - (1 - r2_value) * (n - 1) / (n - k - 1)

# In kết quả
cat("\n🔍 Các chỉ số đánh giá trên tập huấn luyện:\n")
cat(sprintf("MSE        : %.2f\n", mse_value))
cat(sprintf("MAE        : %.2f\n", mae_value))
cat(sprintf("R2         : %.2f\n", r2_value))
cat(sprintf("Adjusted R2: %.2f\n", r2_adj_value))

# --- Chuẩn hóa lại dữ liệu vẽ ---
last_time <- full_order_counts$Order_Hour_Range[nrow(full_order_counts)]
future_times <- seq(last_time + hours(6), by = "6 hours", length.out = forecast_steps)

plot_times <- c(full_order_counts$Order_Hour_Range, future_times)
plot_values <- c(full_order_counts$Order_Count, as.numeric(forecast_values$mean))

# --- Vẽ biểu đồ ---
plot(plot_times, plot_values, type = "n", 
     main="Dự báo số lượng đơn hàng", xlab="Thời gian", ylab="Số đơn hàng")

# Vẽ dữ liệu thực tế
points(full_order_counts$Order_Hour_Range, full_order_counts$Order_Count, col="blue", pch=16, cex=0.5)

# Vẽ đường dự báo
lines(future_times, as.numeric(forecast_values$mean), col="red", lwd=2, lty=2)

# Vẽ vùng dự báo 95%
polygon(c(future_times, rev(future_times)),
        c(forecast_values$lower[,2], rev(forecast_values$upper[,2])),
        col=rgb(1,0,0,0.2), border=NA)

# Thêm chú thích
legend("topleft", legend=c("Dữ liệu thực tế", "Dự báo 7 ngày tiếp theo", "Vùng dự báo 95%"),
       col=c("blue", "red", rgb(1,0,0,0.2)), lwd=c(1,2,10), lty=c(NA,2,NA), pch=c(16,NA,15))


install.packages("xgboost")
#XGBoost dự đoán fold 5
library(xgboost)
library(dplyr)
library(lubridate)
library(Metrics)

# ====== Chuẩn bị dữ liệu ======
df <- full_order_counts %>%
  mutate(Order_Hour_Range = as.POSIXct(Order_Hour_Range)) %>%
  arrange(Order_Hour_Range)

# ====== Tạo lag features ======
for (lag in 1:4) {
  df[[paste0("lag_", lag)]] <- lag(df$Order_Count, lag)
}

# Xóa NA do lag
df <- df %>% drop_na()

# ====== Chia fold ======
n <- nrow(df)
fold_size <- floor(n / 5)

fold_1 <- 1:fold_size
fold_2 <- (fold_size + 1):(2 * fold_size)
fold_3 <- (2 * fold_size + 1):(3 * fold_size)
fold_4 <- (3 * fold_size + 1):(4 * fold_size)
fold_5 <- (4 * fold_size + 1):n

train_idx <- c(fold_1, fold_2, fold_3, fold_4)
test_idx <- fold_5

# ====== Train/Test data ======
train_data <- df[train_idx, ]
test_data <- df[test_idx, ]

X_train <- as.matrix(train_data %>% select(starts_with("lag_")))
y_train <- train_data$Order_Count

X_test <- as.matrix(test_data %>% select(starts_with("lag_")))
y_test <- test_data$Order_Count

# ====== Train XGBoost model ======
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)

# ====== Dự báo ======
y_pred <- predict(model, dtest)

# ====== Tính các chỉ số ======
mse_value <- mean((y_test - y_pred)^2)
mae_value <- mean(abs(y_test - y_pred))
r2_value <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

# Adjusted R²
n_test <- length(y_test)    # số lượng mẫu trong test
k_features <- ncol(X_test)  # số lượng biến đầu vào (lag_1 ~ lag_4)

r2_adj_value <- 1 - (1 - r2_value) * (n_test - 1) / (n_test - k_features - 1)

# ====== In kết quả ======
cat("\n🔍 Các chỉ số đánh giá trên fold 5:\n")
cat(sprintf("MSE        : %.2f\n", mse_value))
cat(sprintf("MAE        : %.2f\n", mae_value))
cat(sprintf("R2         : %.2f\n", r2_value))
cat(sprintf("Adjusted R2: %.2f\n", r2_adj_value))

# ====== Vẽ biểu đồ ======
plot(test_data$Order_Hour_Range, y_test, type = "l", col = "blue", lwd = 2, 
     main = "Dự báo fold 5 bằng XGBoost", xlab = "Thời gian", ylab = "Số đơn hàng")
lines(test_data$Order_Hour_Range, y_pred, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Thực tế", "Dự báo"), col = c("blue", "red"), lty = c(1,2), lwd = 2)


library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)
library(lubridate)

# === 1. TẠO ĐẶC TRƯNG LAG ===
create_lag_features <- function(df, n_lags = 4) {
  for (i in 1:n_lags) {
    df[[paste0("lag_", i)]] <- dplyr::lag(df$`Order_Count`, i)
  }
  df <- na.omit(df)
  return(df)
}

# === 2. GIẢ SỬ full_order_counts ĐÃ TỒN TẠI ===
n_lags <- 27  # 7 ngày = 27 giờ
df_lagged <- create_lag_features(full_order_counts, n_lags)

X <- df_lagged[, !names(df_lagged) %in% "Order_Count"]
y <- df_lagged$`Order_Count`

# === 3. CROSS-VALIDATION VỚI TimeSeriesSplit ===
library(dplyr)

# Đảm bảo X là kiểu số
X_numeric <- X %>% mutate(across(everything(), as.numeric))

n_splits <- 5  # Số lượng fold
n_samples <- nrow(X_numeric)  # Tổng số mẫu
initialWindow <- floor(0.8 * n_samples)  # Huấn luyện trên 80% dữ liệu
horizon <- floor(0.2 * n_samples)  # Kiểm tra trên 20% dữ liệu còn lại

# Tạo các chỉ số huấn luyện và kiểm tra
tscv <- createTimeSlices(1:n_samples, initialWindow = initialWindow, horizon = horizon, fixedWindow = TRUE)

mse_scores <- c()
mae_scores <- c()
r2_scores <- c()
r2_adj_scores <- c()

for (fold in seq_along(tscv$train)) {
  train_idx <- tscv$train[[fold]]
  test_idx <- tscv$test[[fold]]
  
  X_train <- as.matrix(X_numeric[train_idx, ])
  y_train <- y[train_idx]
  
  X_test <- as.matrix(X_numeric[test_idx, ])
  y_test <- y[test_idx]
  
  model <- xgboost(
    data = X_train, label = y_train,
    nrounds = 100, eta = 0.1, max_depth = 3,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  y_pred <- predict(model, X_test)
  
  mse <- mean((y_test - y_pred)^2)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
  n <- length(y_test)
  p <- ncol(X_test)
  r2_adj <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  
  mse_scores <- c(mse_scores, mse)
  mae_scores <- c(mae_scores, mae)
  r2_scores <- c(r2_scores, r2)
  r2_adj_scores <- c(r2_adj_scores, r2_adj)
  
  cat(sprintf("\nFold %d\n", fold))
  cat(sprintf("🔹 MSE: %.4f\n", mse))
  cat(sprintf("🔹 MAE: %.4f\n", mae))
  cat(sprintf("🔹 R²: %.4f\n", r2))
  cat(sprintf("🔹 R² adjusted: %.4f\n", r2_adj))
}

# === 4. KẾT QUẢ TRUNG BÌNH SAU 5 FOLD ===
cat("\n🔍 Trung bình các chỉ số cross-validation:\n")
cat(sprintf("Avg MSE        : %.4f\n", mean(mse_scores)))
cat(sprintf("Avg MAE        : %.4f\n", mean(mae_scores)))
cat(sprintf("Avg R2         : %.4f\n", mean(r2_scores)))
cat(sprintf("Avg R2 Adjusted: %.4f\n", mean(r2_adj_scores)))


# === 5. HUẤN LUYỆN TRÊN TOÀN BỘ DỮ LIỆU ===
X_matrix <- as.matrix(X_numeric)

model_final <- xgboost(
  data = X_matrix, label = y,
  nrounds = 100, eta = 0.1, max_depth = 3,
  objective = "reg:squarederror",
  verbose = 0
)

# === 6. DỰ BÁO 7 NGÀY TƯƠI LAI ===
# Tạo các đặc trưng lag cho 7 ngày tiếp theo
n_future_days <- 28
df_future <- tail(full_order_counts, n_future_days)

# Tạo các giá trị lag cho mỗi ngày trong tương lai
for (i in 1:n_lags) {
  lag_values <- rep(NA, n_future_days)  # Tạo NA cho tất cả các ngày trong tương lai
  if (i <= n_future_days) {
    lag_values[(i+1):n_future_days] <- df_future$`Order_Count`[1:(n_future_days - i)]  # Gán các giá trị lag cho các ngày sau
  }
  df_future[[paste0("lag_", i)]] <- lag_values
}

df_future <- df_future[complete.cases(df_future), ]  # Loại bỏ các dòng có NA

# Dự báo 7 ngày tiếp theo
X_future <- df_future[, !names(df_future) %in% "Order_Count"]
X_future_numeric <- X_future %>% mutate(across(everything(), as.numeric))
X_future_matrix <- as.matrix(X_future_numeric)

y_future_pred <- predict(model_final, X_future_matrix)

# === 7. VẼ BIỂU ĐỒ DỰ BÁO 7 NGÀY ===
# Tạo một vector thời gian cho các ngày tiếp theo
future_dates <- seq.Date(from = Sys.Date() + 1, by = "day", length.out = n_future_days)

# Vẽ biểu đồ
df_pred <- data.frame(Date = future_dates, Predicted_Order_Count = y_future_pred)

ggplot(df_pred, aes(x = Date, y = Predicted_Order_Count)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Dự Báo Số Lượng Đơn Hàng 7 Ngày Tương Lai",
       x = "Ngày", y = "Số Lượng Đơn Hàng Dự Báo") +
  theme_minimal()




setwd("D:/Nam 3/HK2/PhanTichDuLieuKinhDoanh/DoAn2")
