# Đọc dữ liệu
df <- read.csv("C:/Users/krizb/Documents/DoAn2/cleaned_order_history.csv")

colnames(df)

# Lọc những đơn bị đánh dấu là incorrectly
incorrected_rows <- subset(df, Order.Ready.Marked == "Incorrectly")
nrow(incorrected_rows)

# Lọc các đơn hàng có thời gian chờ > 10 phút
incorrected_gt10 <- subset(incorrected_rows, Rider.wait.time..minutes. > 10)

# Tính tỷ lệ phần trăm
rate <- nrow(incorrected_gt10) / nrow(incorrected_rows) * 100
print(paste("Tỷ lệ đơn hàng incorrectly có Rider Wait Time > 10 phút:", round(rate, 2), "%"))

# Tạo biểu đồ tròn
slices <- c(nrow(incorrected_gt10), nrow(incorrected_rows) - nrow(incorrected_gt10))
labels <- c("Wait > 10 mins", "Wait ≤ 10 mins")
percent_labels <- paste0(labels, ": ", round(slices / sum(slices) * 100, 1), "%")

pie(slices, labels = percent_labels, main = "Incorrect Orders - Rider Wait Time > 10 mins")
