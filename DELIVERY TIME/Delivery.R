library(dplyr)
library(lubridate)
library(readr)

# Đọc và xử lý dữ liệu từ order_history_kaggle_data.csv
df <- read_csv('C:/Users/ACER/Desktop/DoAn_2/DELIVERY TIME/order_history_kaggle_data.csv')

# Kiểm tra và làm sạch dữ liệu
df <- df %>%
  # Loại bỏ các hàng có Order Placed At không hợp lệ
  filter(!is.na(`Order Placed At`) & `Order Placed At` != "") %>%
  mutate(`Order Placed At` = as.POSIXct(`Order Placed At`, format = '%I:%M %p, %B %d %Y', tz = "UTC"),
         Hour = hour(`Order Placed At`),
         Day = wday(`Order Placed At`, label = TRUE, abbr = TRUE),
         # Xử lý Distance, đảm bảo không tạo NA
         Distance_km = ifelse(grepl('<', Distance, fixed = TRUE), 0.5, 
                              as.numeric(gsub('km', '', Distance)))) %>%
  # Loại bỏ các hàng có Distance_km là NA
  filter(!is.na(Distance_km)) %>%
  filter(`Order Status` == 'Delivered')

# Đọc và xử lý dữ liệu từ SpeedPerHour.csv
speed_df <- read_csv('C:/Users/ACER/Desktop/DoAn_2/DELIVERY TIME/SpeedPerHour.csv', col_names = TRUE)
speed_df <- speed_df %>%
  rename(Hour = 1) %>%
  mutate(Hour = as.integer(gsub('h', '', Hour))) %>%
  mutate(across(-Hour, ~ as.numeric(gsub(' km/h', '', .))))

# Hàm tính thời gian giao hàng
calculate_delivery_time <- function(row) {
  user_distance <- row[['Distance_km']]
  user_hour <- row[['Hour']]
  day <- row[['Day']]
  
  # Kiểm tra xem Order Placed At có hợp lệ không
  if (is.na(row[['Order Placed At']])) {
    return(c(NA, NA))
  }
  
  user_time_in_minutes <- hour(row[['Order Placed At']]) * 60 + minute(row[['Order Placed At']])
  
  # Tra cứu tốc độ dựa trên giờ và ngày
  speed_at_hour_day <- speed_df[speed_df$Hour == user_hour, as.character(day)][[1]]
  
  # Tính thời gian giao hàng (phút)
  delivery_time_minutes <- (user_distance / speed_at_hour_day) * 60
  
  return(c(delivery_time_minutes, speed_at_hour_day))
}

# Áp dụng hàm
df <- df %>%
  rowwise() %>%
  mutate(Delivery_Time_minutes = calculate_delivery_time(cur_data())[1],
         Speed_kmph = calculate_delivery_time(cur_data())[2]) %>%
  ungroup()

# In 20 hàng ngẫu nhiên
print(sample_n(df, 20) %>% 
        select(`Order ID`, Distance_km, Hour, Day, Delivery_Time_minutes, Speed_kmph))

# Đếm và sắp xếp Distance_km
distance_counts <- df %>%
  count(Distance_km, name = "count") %>%
  arrange(desc(count))

print(distance_counts)