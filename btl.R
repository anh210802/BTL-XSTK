# if (!require(dplyr)) install.packages("dplyr")
# if (!require(lubridate)) install.packages("lubridate")
# if (!require(car)) install.packages("car")
library(dplyr)
library(lubridate)
library(car)

###################################### Xử lý dữ liệu ###########################################

# Đường dẫn đến file
file_path <- 'F:/xstk/BTL/dirty_data.csv'

# Đọc và xử lý dữ liệu
dirt <- read.csv(file_path)
View(dirt)

# Sửa định dạng các giá trị trong các cột
dirt <- dirt %>%
  mutate(
    nearest_warehouse = recode(nearest_warehouse, 
                               "nickolson" = "Nickolson", 
                               "thompson" = "Thompson"),
    season = recode(season,
                    "spring" = "Spring",
                    "summer" = "Summer",
                    "autumn" = "Autumn",
                    "winter" = "Winter"),
    date = recode(date,                                                                     
                  "06/29/2019" = "2019-06-29",
                  "09/04/2019" = "2019-09-04",
                  "04/20/2019" = "2019-04-20",
                  "05/06/2019" = "2019-05-06",
                  "05/12/2019" = "2019-05-12",
                  "03/27/2019" = "2019-03-27",
                  "10 12 2019" = "2019-10-12",
                  "03/16/2019" = "2019-03-16",
                  "02 03 2019" = "2019-02-03",
                  "03 10 2019" = "2019-03-10",
                  "03 29 2019" = "2019-03-29",
                  "01 27 2019" = "2019-01-27",
                  "03-13-2019" = "2019-03-13",
                  "07-13-2019" = "2019-07-13",
                  "02-24-2019" = "2019-02-24",
                  "06-18-2019" = "2019-06-18",
                  "08-18-2019" = "2019-08-18",
                  "03-28-2019" = "2019-03-28",
                  "04-01-2019" = "2019-04-01",
                  "12-09-2019" = "2019-12-09"),
    shopping_cart = recode(shopping_cart, 
                           "[('IASSIST LINE', 2), ('lucent 330s', 2)]"="[('iAssit Line', ', 2), ('Lucent 3305', 2)]", 
                           "[('PEARTV', 2), ('CANDLE INFERNO', 1), ('THUNDER LINE', 2), ('ALCON 10', 1)]" = "[('pearTV', 2), ('Candle inferno', 1), ('Thunder line', 2), ('Alcon 10', 1)]",              
                           "[('ISTREAM', 1), ('ALCON 10', 1), ('PEARTV', 1), ('TOSHIKA 750', 2)]"="[('iStream', 1), (' Alcon 10', 1), ('pearTV', 1), ('Toshika 750', 2)]",                               
                           "[('iassist line', 1), ('alcon 10', 2), ('candle inferno', 2), ('istream', 2)]"="[('iAssist Line', 1), (' Alcon 10', 2), ('Candle inferno', 2), ('iStream', 2)]",             
                           "[('iassist line', 2), ('toshika 750', 2), ('olivia x460', 1), ('universe note', 1)]"="[('iAssist Line', 2), ('Toshika 750', 2), ('Olivia x460', 1), ('Universe Note', 1)]",
                           "[('IASSIST LINE', 1), ('ALCON 10', 2), ('OLIVIA X460', 1), ('ISTREAM', 1)] "="[('iAssist Line', 1), ('Alcon 10', 2), ('Olivia x460', 1), ('iStream', 1)]",                   
                           "[('ALCON 10', 1), ('thunder line', 1)]"="[('Alcon 10', 1), ('Thunder line', 1)]",                                                                                            
                           "[('PEARTV', 2), ('LUCENT 330S', 1), ('OLIVIA X460', 2)]"="[('pearTV', 2), ('Lucent 330S', 1), ('Olivia x460', 1), ('iStream', 1)]",                                          
                           "[('toshika 750', 2), ('istream', 1), ('peartv', 2), ('universe note', 1)]" = "[('Toshika 750', 2), ('iStream', 1), ('pearTV', 2), ('Universe Note', 1)]",                    
                           "[('ALCON 10', 2), ('istream', 2)]" = "[('Alcon 10', 2), ('iStream', 2)]",                                                                                                    
                           "[('ISTREAM', 1), ('peartv', 1)]" = "[('iStream', 1), ('pearTV', 1)]",                                                                                                        
                           "[('universe note', 2), ('iassist line', 2), ('lucent 330s', 2)]" = "[('Universe Note', 2), ('iAssist Line', 2), ('Lucent 330s', 2)]",                                        
                           "[('THUNDER LINE', 1), ('OLIVIA X460', 2), ('LUCENT 330S', 1), ('IASSIST LINE', 1)]" = "[('Thunder line', 1), ('Olivia X460', 2), ('Lucent 330S', 1), ('iAssist Line', 1)]",  
                           "[('LUCENT 330S', 1), ('THUNDER LINE', 2), ('ISTREAM', 1)]" = "[('Lucent 330S', 1), ('Thunder line', 2), ('iStream', 1)]",                                                    
                           "[('ISTREAM', 1), ('ALCON 10', 1), ('PEARTV', 1), ('TOSHIKA 750', 2)]" = "[('iStream', 1), ('Alcon 10', 1), ('pearTV', 1), ('Toshika 750', 2)]",                            
                           "[('ALCON 10', 1), ('iassist line', 1)]" = "[('Alcon 10', 1), ('iAssist Line', 1)]",),  
    season = case_when(
      month(date) %in% 1:3 ~ "Spring",
      month(date) %in% 4:6 ~ "Summer",
      month(date) %in% 7:9 ~ "Autumn",
      month(date) %in% 10:12 ~ "Winter",
      TRUE ~ season
    )
  )

summary(dirt)

# Sửa lại các biến phân loại chưa được định dạng
dirt$order_id_new <- factor(dirt$order_id)
dirt$customer_id_new <- factor(dirt$customer_id)
dirt$date_new <- factor(dirt$date)
dirt$nearest_warehouse <- factor(dirt$nearest_warehouse)
dirt$shopping_cart <- factor(dirt$shopping_cart)
dirt$season <- factor(dirt$season)
dirt$is_expedited_delivery <- factor(dirt$is_expedited_delivery)
dirt$is_happy_customer <- factor(dirt$is_happy_customer)
summary(dirt)


# Tính lại cột `order_total`
dirt <- dirt %>%
  mutate(order_total_new = order_price * (100 - coupon_discount) / 100 + delivery_charges)

# Tìm các hàng có giá trị `order_total_new` khác với `order_total`
diff_rows <- which(dirt$order_total_new != dirt$order_total)
if (length(diff_rows) > 0) {
  for (i in diff_rows) {
    cat("Row:", i, " - value new:", dirt$order_total_new[i], ", value old:", dirt$order_total[i], "\n")
  }
}

# Các biến ngoại lai dựa theo latest_customer_review và is_happy_customer 
# (Khi latest_customer_review = "good" thì is_happy_customer = "False" và ngược lại)
##cac bien ngoai lai dua theo new_customer_review và is_happy_customer

# "ORD157353"
# "ORD439821"
# "ORD370065"
# "ORD226452"
# "ORD452012"
# "ORD452642"
# "ORD057521"
# "ORD073993"
# "ORD413882"
# "ORD355763"
# "ORD371242"
# "ORD051673"
# "ORD319178"
# "ORD051670"
# "ORD121712"
# "ORD267220"
# "ORD374251"
# "ORD083728"
# "ORD243905"

# Tìm và tính tỉ lệ các giá trị ngoại lai trong `order_total_new`
boxplot(dirt$order_total_new, main = "Boxplot of order_total_new", col = "blue")
outliers <- boxplot.stats(dirt$order_total_new)$out
length(outliers)
outliers
length(outliers) / length(dirt$order_total_new)

# Biến đổi các giá trị True/False thành 1/0
dirt <- dirt %>%
  mutate(
    is_expedited_delivery_num = ifelse(is_expedited_delivery == "True", 1, 0),
    is_happy_customer_num = ifelse(is_happy_customer == "True", 1, 0)
  )

# Lọc biến sử dụng
dirt_new <- dirt[,c("order_price", "order_total_new", "delivery_charges", "coupon_discount", "distance_to_nearest_warehouse", "is_expedited_delivery_num", "is_happy_customer_num")]
View(dirt_new)

# Lấy log của các biến cần thiết
dirt_log <- dirt_new
dirt_log[,c("order_price", "order_total_new", "delivery_charges", "distance_to_nearest_warehouse")] <- 
  log(dirt_log[,c("order_price", "order_total_new", "delivery_charges", "distance_to_nearest_warehouse")])
View(dirt_log)

# Tính thống kê mô tả
mean = apply(dirt_new, 2, mean)
sd = apply(dirt_new, 2, sd)
min = apply(dirt_new, 2, min)
max = apply(dirt_new, 2, max)
median = apply(dirt_new, 2, median)
Q1 = apply(dirt_new, 2, quantile, 0.25)
Q3 = apply(dirt_new, 2, quantile, 0.75)
bang_thong_ke <- data.frame(mean, sd, min, max, median, Q1, Q3)
View(bang_thong_ke)

# Tính thống kê mô tả sau khi lấy log
mean_log = apply(dirt_log, 2, mean)
sd_log = apply(dirt_log, 2, sd)
min_log = apply(dirt_log, 2, min)
max_log = apply(dirt_log, 2, max)
median_log = apply(dirt_log, 2, median)
Q1_log = apply(dirt_log, 2, quantile, 0.25)
Q3_log = apply(dirt_log, 2, quantile, 0.75)
bang_thong_ke_log <- data.frame(mean_log, sd_log, min_log, max_log, median_log, Q1_log, Q3_log)
View(bang_thong_ke_log)

# Lập bảng tần số và mode cho các biến số nhị phân
table_is_expedited_delivery <- table(dirt$is_expedited_delivery_num)
View(table_is_expedited_delivery)
table_is_happy_customer <- table(dirt$is_happy_customer_num)
View(table_is_happy_customer)
mode_table <- data.frame(
  mode_is_expedited_delivery = names(table_is_expedited_delivery)[which.max(table_is_expedited_delivery)],
  mode_is_happy_customer = names(table_is_happy_customer)[which.max(table_is_happy_customer)]
)
View(mode_table)

# Biểu đồ cho các biến
# Biểu đồ phân phối của biến is_expedited_delivery_num và is_happy_customer_num
hist(dirt_new$is_expedited_delivery_num, main = "Histogram of is_expedited_delivery", xlab = "is_expedited_delivery", col = "red", border = "white", breaks = 15, labels = TRUE)

hist(dirt_new$is_happy_customer_num, main = "Histogram of is_happy_customer", xlab = "is_happy_customer", col = "blue", border = "white", breaks = 15, labels = TRUE)

# Biểu đồ phân phối của biến order_total_new và order_total_new log
hist(dirt_new$order_total_new, main = "Histogram order_total_new (Before Log)", xlab = "order_total_new", col = "green", border = "white", breaks = 15, labels = TRUE)

hist(dirt_log$order_total_new, main = "Histogram order_total_new (After Log)", xlab = "order_total_new", col = "yellow", border = "white", breaks = 15, labels = TRUE, ylim = c(0, 200))

# Biểu đồ boxplot của biến order_total_new và order_total_new log theo is_expedited_delivery_num và is_happy_customer_num
boxplot(order_total_new ~ is_expedited_delivery_num, data = dirt_new, main = "Histogram order_total_new (Before Log) by is_expedited_delivery", col = "blue")

boxplot(order_total_new ~ is_expedited_delivery_num, data = dirt_log, main = "Histogram order_total_new (After Log) by is_expedited_delivery", col = "red")

boxplot(order_total_new ~ is_happy_customer_num, data = dirt_new, main = "Histogram order_total_new by (Before Log) is_happy_customer", col = "blue")

boxplot(order_total_new ~ is_happy_customer_num, data = dirt_log, main = "Histogram order_total_new (After Log) by is_happy_customer", col = "red")

# Biểu đồ boxplot của biến delivery_charges và delivery_charges log theo is_expedited_delivery_num và is_happy_customer_num
boxplot(delivery_charges ~ is_expedited_delivery_num, data = dirt_new, main = "Histogram delivery_charges (Before Log) by is_expedited_delivery", col = "blue")

boxplot(delivery_charges ~ is_expedited_delivery_num, data = dirt_log, main = "Histogram delivery_charges (After Log) by is_expedited_delivery", col = "red")

boxplot(delivery_charges ~ is_happy_customer_num, data = dirt_new, main = "Histogram delivery_charges (Before Log) by is_happy_customer", col = "blue")

boxplot(delivery_charges ~ is_happy_customer_num, data = dirt_log, main = "Histogram delivery_charges (After Log) by is_happy_customer", col = "red")

# Biểu đồ của biến order_total_new lần lượt theo các biến order_price, delivery_charges, 
pairs(dirt_new$order_total_new ~ dirt_new$order_price + dirt_new$delivery_charges + dirt_new$distance_to_nearest_warehouse, main = "Scatterplot of order_total_new by order_price, delivery_charges, distance_to_nearest_warehouse", col = "blue",
      labels = c("order_price", "delivery_charges", "distance_to_nearest_warehouse"))
pairs(dirt_log$order_total_new ~ dirt_log$order_price + dirt_log$delivery_charges + dirt_log$distance_to_nearest_warehouse, main = "Scatterplot of order_total_new by order_price_log, delivery_charges_log, distance_to_nearest_warehouse_log", col = "red",
      labels = c("order_price_log", "delivery_charges_log", "distance_to_nearest_warehouse_log"))

####################################### Thống kê và suy diễn ###############################################

# Kiểm định order_price với mức ý nghĩa 5%
# Chưa bỏ ngoại lai
# t_result <- t.test(dirt$order_price)
# print(t_result)

#   # Bỏ ngoại lai order_total_new
# dirt$order_total_new[dirt$order_total_new %in% outliers] <- NA
# dirt <- na.omit(dirt)
# t_result <- t.test((dirt$order_price))
# print(t_result)

#   # Bỏ ngoại lai total_new và is_happy_customer
# dirt <- dirt[-c(38, 53, 57, 84, 94, 100, 120, 201, 222, 223, 251, 265, 296, 383, 415, 419, 424, 434),]
# t_result <- t.test(dirt$order_price)
# print(t_result)

################################################ ANOVA ####################################################

# So sánh giá trị đơn đặt hàng theo từng kho. Mức ý nghĩa 5%
# Chưa bỏ ngoại lai
# anova1 <- aov(dirt$order_total_new ~ dirt$nearest_warehouse)
# summary(anova1)
#   # Bỏ ngoại lai order_total_new
# dirt$order_total_new[dirt$order_total_new %in% outliers] <- NA
# dirt <- na.omit(dirt)
# anova1 <- aov(dirt$order_total_new ~ dirt$nearest_warehouse)
# summary(anova1)
#   # Bỏ ngoại lai total_new và is_happy_customer
# dirt <- dirt[-c(38, 53, 57, 84, 94, 100, 120, 201, 222, 223, 251, 265, 296, 383, 415, 419, 424, 434),]
# anova1 <- aov(dirt$order_total_new ~ dirt$nearest_warehouse)
# summary(anova1)

# # So sánh giá trị đơn đặt hàng theo mùa. Mức ý nghĩa 5%
#   # Chưa bỏ ngoại lai
# anoval2 <- aov(dirt$order_total_new ~ dirt$season)
# summary(anoval2)
#   # Bỏ ngoại lai order_total_new
# dirt$order_total_new[dirt$order_total_new %in% outliers] <- NA
# dirt <- na.omit(dirt)
# anoval2 <- aov(dirt$order_total_new ~ dirt$season)
# summary(anoval2)
#   # Bỏ ngoại lai total_new và is_happy_customer
# dirt <- dirt[-c(38, 53, 57, 84, 94, 100, 120, 201, 222, 223, 251, 265, 296, 383, 415, 419, 424, 434),]
# anoval2 <- aov(dirt$order_total_new ~ dirt$season)
# summary(anoval2)

# So sánh giá trị đơn đặt hàng hài lòng và người không hài lòng với mức đơn hàng gần nhất. Mức ý nghĩa 5%
# Chưa bỏ ngoại lai 
# anoval3 <- aov(dirt$order_total_new ~ dirt$is_happy_customer)
# summary(anoval3)
# Bỏ ngoại lai order_total_new
# dirt$order_total_new[dirt$order_total_new %in% outliers] <- NA
# dirt <- na.omit(dirt)
# anoval3 <- aov(dirt$order_total_new ~ dirt$is_happy_customer)
# summary(anoval3)
# Bỏ ngoại lai total_new và is_happy_customer
# dirt <- dirt[-c(38, 53, 57, 84, 94, 100, 120, 201, 222, 223, 251, 265, 296, 383, 415, 419, 424, 434),]
# anoval3 <- aov(dirt$order_total_new ~ dirt$is_happy_customer)
# summary(anoval3)

################################################# Hồi quy tuyến tính ############################################

# model = lm(order_total_new ~ order_price + delivery_charges + coupon_discount + distance_to_nearest_warehouse, data = dirt)
# summary(model)

# Kiểm định các biến delivery_charges và distance_to_nearest_warehouse đồng thời không ảnh hưởng tới giá trị đơn đặt hàng
# linearHypothesis(model, c("delivery_charges=0", "distance_to_nearest_warehouse=0"))

# Chạy lại khi bỏ biến delivery_charges và distance_to_nearest_warehouse
# model = lm(order_total_new ~ order_price + coupon_discount, data = dirt)
# summary(model)

# Hồi quy
# dirt$order_total_new[dirt$order_total_new %in% outliers] <- NA
# dirt <- na.omit(dirt)
# dirt <- dirt[-c(38, 53, 57, 84, 94, 100, 120, 201, 222, 223, 251, 265, 296, 383, 415, 419, 424, 434),]
# model = lm(order_total_new ~ order_price + delivery_charges + coupon_discount + distance_to_nearest_warehouse, data = dirt)
# summary(model)

# Kiểm định các biến delivery_charges đồng thời không ảnh hưởng tới giá trị đơn đặt hàng
# linearHypothesis(model, c("delivery_charges=0"))

# Chạy lại khi bỏ biến delivery_charges
# model = lm(order_total_new ~ order_price + coupon_discount + distance_to_nearest_warehouse, data = dirt)
# summary(model)