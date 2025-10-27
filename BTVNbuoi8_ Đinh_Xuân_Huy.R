#Bài 1: Dữ liệu (dulieu2.csv) đã cho là thông tin về doanh số bán hàng của một
#công ty qua các năm.
#Thực hiện các thao tác sau với phần mềm R.
#a. Nhập dữ liệu (“dulieu2.csv”) vào R và loại bỏ dữ liệu trống.
data1 <- read.csv("D:\\PTDL\\dulieu2.csv", header=TRUE)
data1
data1 <- na.omit(data1)
#b. Trích ra một dữ liệu có bộ phận (Segment) là “Government”, đặt nó là data1
#và “Midmarket” đặt tên nó là data2.
data1_1 <- subset(data1, Segment == "Government")
data1_2 <- subset(data1, Segment == "Midmarket")
#c. Hãy sử dụng các câu lệnh if/for để tính tổng doanh thu (Gross.sales) của từng
#loại mặt hàng (Product). Hãy cho biết sản phẩm nào có doanh thu lớn nhất?
max <- 0
name <- " "
for(j in unique(data1$Product)){
  sum_product <- sum(data1$Gross.Sales[data1$Product==j])
  print(j)
  print(sum_product)
  if(max < sum_product){
    max <- sum_product
    name = j
  }
}
max
name
#  d. Kiểm tra sản phẩm “VTT” được bán tại “Mexico” vào tháng và năm nào?
vtt_mexico <- subset(data1, Product == "VTT" & Country == "Mexico")
vtt_mexico
if (nrow(vtt_mexico) > 0) {
  cat("Sản phẩm VTT được bán tại Mexico vào:\n")
  print(unique(vtt_mexico[, c("Month.Number", "Month.Name", "Year")]))
} else {
  print("Không có sản phầm VTT nào bán tại Mexico.\n")
}
#e. Hãy cho biết sản phẩm nào đã bán được nhiều nhất?
products <- unique(data1$Product);products
so_lan_ban <- numeric(length(products))

for (i in 1:length(products)) {
  sp = products[i]
  sl = 0
  
  for (j in 1:nrow(data1)) {
    if (data1$Product[j] == sp) {
      sl = sl + 1
    }
  }
  
  so_lan_ban[i] = sl
}
so_lan_ban
max_index <- which.max(so_lan_ban); max_index
cat("Sản phẩm được bán nhiều nhất là:", products[max_index], ", với số lần bán:", so_lan_ban[max_index], "\n" )

# f. Hãy so sánh tổng doanh thu của hai bộ phận Government và midmarket.
sum_gov <- sum(data1_1$Gross.Sales)
sum_mid <- sum(data1_2$Gross.Sales)

cat("Tổng doanh thu Government:", sum_gov, "\n")
cat("Tổng doanh thu Midmarket:", sum_mid, "\n")

#Bài 2:
#a. Nhập dữ liệu “WHO1” vào R và đặt tên nó là who
setwd("D:\\PTDL")
who = read.csv("WHO1.csv")
who
#b. Loại bỏ các dữ liệu trống trong và đặt tên nó là who1.
who1 = na.omit(who)
#c. Thực hiện bổ sung dữ liệu trống trong dữ liệu who bằng cách thêm giá trị
#trung bình cộng của giá trị lớn nhất và giá trị nhỏ nhất
#Cách 1.
colSums(is.na(who))
#Thay giá trị thiếu trong cột Tỷ lệ sinh sản
gttb1 <- 0.5*(max(who$FertilityRate..Tylesinhsan.,na.rm = TRUE)+
                min(who$FertilityRate..Tylesinhsan.,na.rm = TRUE))
vitritrong <- which(is.na(who$FertilityRate..Tylesinhsan.))
for(i in vitritrong){
  who$FertilityRate..Tylesinhsan.[i] <- gttb1
}
colSums(is.na(who))
#Thay giá trị thiếu trong cột Tỷ lệ biết đọc viết
gttb2 <- 0.5*(max(who$LiteracyRate..Tylebietdocviet.,na.rm = TRUE)+
                min(who$LiteracyRate..Tylebietdocviet.,na.rm = TRUE))
vitritrong <- which(is.na(who$LiteracyRate..Tylebietdocviet.))
for(i in vitritrong){
  who$LiteracyRate..Tylebietdocviet.[i] <- gttb2
}
colSums(is.na(who))
#Thay giá trị thiếu trong cột Thu nhập quốc dân
gttb3 <- 0.5*(max(who$GNI..Thunhapquocdan.,na.rm = TRUE)+
                min(who$GNI..Thunhapquocdan.,na.rm = TRUE))
vitritrong <- which(is.na(who$GNI..Thunhapquocdan.))
for(i in vitritrong){
  who$GNI..Thunhapquocdan.[i] <- gttb2
}
colSums(is.na(who))

#Cách 2:
for (col in names(who)) {
  # Kiểm tra nếu cột là dạng số
  if (is.numeric(who[[col]])) {
    max_val <- max(who[[col]], na.rm = TRUE)  
    min_val <- min(who[[col]], na.rm = TRUE)  
    fill_value <- (max_val + min_val) / 2     
    who[[col]][is.na(who[[col]])] <- fill_value
  }
}
print(who)
#d. Hãy tính thu nhập quốc dân trung bình của hai dữ liệu who và who1, và đưa
#ra nhận xét.
GNITB = mean(who$GNI..Thunhapquocdan.,na.rm = TRUE)
GNITB1 = mean(who1$GNI..Thunhapquocdan.,na.rm = TRUE)
if(GNITB > GNITB1){
  print("Thu nhập trung bình của who lớn hơn who1")
} else {
  print("Thu nhập trung bình của who1 lớn hơn who")
}
#e. Từ dữ liệu trích ra dữ liệu của hai nước “Bahrain” và “Mexico” và so sánh
#tuổi thọ của hai nước này.
mexico <- who[who$Country == "Mexico", ]
bahrain <- who[who$Country == "Bahrain", ]
#Tuổi thọ của hai nước
bahrain$LifeExpectancy..Tuoitho.
mexico$LifeExpectancy..Tuoitho.
#so sánh
if (bahrain$LifeExpectancy > mexico$LifeExpectancy) {
  print("Bahrain có tuổi thọ cao hơn Mexico")
} else {
  print("Mexico có tuổi thọ cao hơn Bahrain")
}
#f. Hãy cho biết tỷ lệ sinh sản của nước “Canada”.
canada <- who[who$Country == "Canada", ];canada
canada$FertilityRate..Tylesinhsan.
#g. Viết thêm cột phân nhóm các quốc gia vào dữ liệu. Hãy cho biết nước
#“China” thuộc nhóm nào?
who$phannhom <- cut(who$GNI..Thunhapquocdan.,
                    breaks = c(-Inf,10000,20000,Inf),
                    labels = c("Thấp","Trung bình","Cao"))
who$phannhom
#Tìm xem nước "China" thuộc nhóm nào
who[who$Country=="China",]$phannhom
#Cách khác
who2 <- who
country <- unique(who$Country)
who2$Group <- NA
for(i in 1:length(country)){
  current_country <- country[i]
  current_gni <- who2[who2$Country == current_country, ]$GNI..Thunhapquocdan.
  
  if(current_gni > 20000){
    who2[who2$Country == current_country, ]$Group <- "Phat trien"
  } else if(current_gni >= 10000 & current_gni <= 20000){
    who2[who2$Country == current_country, ]$Group <- "Dang phat trien"
  } else if(current_gni < 10000){
    who2[who2$Country == current_country, ]$Group <- "Chua phat trien"
  } else {
    who2[who2$Country == current_country, ]$Group <- "Khac"
  }
}
print(who2)
#h. Hãy tìm nước có dân số cao nhất.
who[who$Population..Danso. == max(who$Population..Danso.), c("Country")]

#Bài 3
#a Sinh ra một bộ dữ liệu về thông tin của 300 sinh viên: bao gồm ID, giới
#tính, tuổi (từ 18-24), chiều cao, cân nặng, điểm thường xuyên, điểm giữa
#kỳ, điểm cuối kỳ.
set.seed(123)
n <- 300

ID <- 1:n
ID

gender <- sample(c("Nam","Nữ"),n,replace = TRUE)
gender 

age <- sample(18:24,n,replace = TRUE)
age

height <- round(rnorm(n,mean =165,sd = 20),1)
height

weight <- round(rnorm(n,mean = 60,sd = 20),1)
weight

tx <- round(runif(n,min = 5, max = 10),1)
tx

gk <- round(runif(n,min = 1,max = 10),1)
gk

ck <- round(runif(n,min = 1,max = 10),1)
ck

stu <- data.frame(ID,gender,age,height,weight,tx,gk,ck)
stu
#b Lấy từ dữ liệu trên danh sách sinh viên của điểm thi cuối kỳ lớn hơn hoặc
#bằng 9.
b <- subset(stu,stu$ck >= 9)
b
#c Bạn sinh viên có ID=5 có điểm chuyên cần bị sai, phải sửa lại là 10. Thực
#hiện thao tác đó với R
stu$tx[stu$ID == 5] <- 10
stu
#d Thêm một cột tên là “tổng điểm” được tính theo công thức tổng= điểm
#thường xuyên*0.2+điểm giữa kỳ*0.2+điểm cuối kỳ*0.6.
stu$tong <- stu$tx * 0.2 + stu$gk * 0.2 + stu$ck * 0.6
stu
#e Nếu tổng điểm lớn hơn hoặc bằng 9.0 được xếp điểm A+, 8.5<= điểm
#tổng < 9 được xếp điểm A, 8.0<= điểm tổng< 8.5 được xếp điểm B+,
#7.0 <= điểm tổng <8.0 được xếp điểm B, điểm tổng< 7.0 được xếp điểm
#C. Hãy kiểm tra xem bạn có ID=10 được điểm chữ là bao nhiêu?
stu$diemchu <- ifelse(stu$tong >= 9,"A+",
                      ifelse(stu$tong >= 8.5,"A",
                             ifelse(stu$tong >= 8,"B+",
                                    ifelse(stu$tong >= 7,"B","C"))))
stu
#Cách khác
stu$diemchu2 <- cut(stu$tong,
                    breaks = c(-Inf,7,8,8.5,9,Inf),
                    labels = c("C","B","B+","A","A+"))

e<-stu$diemchu[stu$ID == 10]
e
#f Hãy đưa ra điểm chữ cho các bạn sinh viên có điểm thi cuối kỳ lớn hơn
#hoặc bằng 9.0.
f<-subset(stu,stu$ck >= 9)
f$diemchu
#g Có bao nhiêu bạn sinh viên có điểm giữa kì là 0 nhưng lại qua môn?
g <- subset(stu,stu$tong >= 4)
g
count_g <- sum(g$gk == 0)
count_g
#h Hãy tính tỉ lệ phần trăm của các bạn được điểm “B”
h <- sum(stu$diemchu == "B")
tyle <- h / n * 100
tyle
#i Tỉ lệ này sẽ thay đổi như nào nếu ta cộng thêm mỗi sinh viên 2đ giữa kỳ.
#(Bạn nào quá 10 điểm thì chỉ đạt 10đ). Câu hỏi này tương tự với ý g)
stu$gk <- stu$gk + 2
stu
stu$gk[stu$gk > 10] <- 10
stu

stu$tong <- stu$tx * 0.2 + stu$gk * 0.2 + stu$ck * 0.6
stu
stu$diemchu2 <- cut(stu$tong,
                    breaks = c(-Inf,7,8,8.5,9,Inf),
                    labels = c("C","B","B+","A","A+"))

h <- sum(stu$diemchu2 == "B")
tyle <- h / n * 100
tyle

# bài 4
#a. Thực hiện sinh ra dữ liệu và đặt tên nó là hanoi.
n <- 506
crim <- rnorm(n, mean = 3.613, sd = 8.6)

indus <- rnorm(n,mean = 11.14, sd = 6.86)

rm <- sample(2:6, n, replace = TRUE); rm

tax <- rnorm(n, mean = 408, sd = 167);tax

nox <- rnorm(n, mean = 0.55, sd = 0.116); nox
for(i in 1:length(nox)){
  if(nox[i] > 0.87 || nox[i] < 0.38){
    repeat {
      new_value <- rnorm(1, mean = 0.55, sd = 0.116)
      if(new_value <= 0.87 && new_value >= 0.38){
        nox[i] <- new_value
        break } 
        }
    }
  }
nox

chas <- rbinom(n, size = 1, prob = 1/5)

age <- rnorm(n, mean = 68, sd = 28); age
for(i in 1:length(age)){
  if(age[i] > 100 || age[i] < 0){
    repeat {
      new_value <- rnorm(1, mean = 68, sd = 28)
      if(new_value <= 100 && new_value >= 0){
        age[i] <- new_value
        break} 
        }
    }
  }
age


hanoi <- data.frame(crim, indus, rm, tax, nox, chas, age)

#b Hãy tạo ra biến medv1 bằng công thức trên, sau đó hãy tính độ lệch tiêu chuẩn
#cho hiệu số giữa medv1 và medv trong Boston.
hanoi$medv1 = - 0.138108*hanoi$crim - 0.068990*hanoi$indus 
                - 4.716034*hanoi$nox + 7.677527*hanoi$rm - 
                0.007923*hanoi$tax - 18.599942

library(MASS)
data("Boston")
Boston
Boston$medv = - 0.138108*Boston$crim - 0.068990*Boston$indus
              - 4.716034*Boston$nox + 7.677527*Boston$rm -
                     0.007923*Boston$tax - 18.599942

dolechtieuchuan <- sd(hanoi$medv1 - Boston$medv)
dolechtieuchuan

#c. Hãy so sánh giá trị trung bình của những ngôi nhà gần sông và những ngôi
#nhà không gần sông.
mean_gansong <- mean(hanoi$medv1[hanoi$chas ==1])
mean_xasong <- mean(hanoi$medv1[hanoi$chas ==0])
if(mean_gansong < mean_xasong){
  print("mean_gansong < mean_xasong")} else {
    if(mean_gansong == mean_xasong){
      print("mean_gansong = mean_xasong")} else {
        print("mean_gansong > mean_xasong")
      }
  }

# Bài 5 -------------------------------------------------------------------

setwd("D:\\PTDL")
benhnhan = read.csv("COVID19.csv")
View(benhnhan)
#a. viết hàm tính tuổi trung bình của bệnh nhân theo giới tính 
tbtuoi <- function(x){
  nam <- subset(x, x$Sex == "Male")
  nu <- subset(x, x$Sex == "Female")
  return(c(mean(nam$Age),mean(nu$Age)))
}
tbtuoi(benhnhan)
#b. Viết một hàm để tính tỷ lệ bệnh nhân đã khỏi bệnh
#trên tổng số bệnh nhân mắc bệnh ở các địa điểm
unique(benhnhan$Status)
kb <- function(x){
  k=0
  for(i in 1 : length(x)){
    if(x[i]=="Recovered"){
      k = k +1
    }
  }
  return(k/length(x))
}
tp = c()
tl = c()

for(i in unique(benhnhan$Location)){
  tp = c(tp,i)
  tl =c(tl, kb(benhnhan[benhnhan$Location==i,"Status"]))
  
}
tilekhoibenh = data.frame(tp,tl)
tilekhoibenh

#So sánh tỉ lệ khỏi bệnh ở TP HCM và HN
if(tilekhoibenh[tilekhoibenh$tp=="Ho Chi Minh","tl"]>tilekhoibenh[tilekhoibenh$tp=="Ha Noi","tl"]){
  print("Tỉ lệ bệnh nhân được chữa khỏi ở tp.HCM cao hơn Hà Nội")
}else if(tilekhoibenh[tilekhoibenh$tp=="Ho Chi Minh","tl"]>tilekhoibenh[tilekhoibenh$tp=="Ha Noi","tl"]){
  print("tỉ lệ bênh nhân được chữa khỏi ở Hà Nội cao hơn tp.HCM")
}else{
  print("tỉ lệ chữa khỏi ở hai thành phố bằng nhau")
}

#c. Có bao nhiêu bệnh nhân quốc tịch nước ngoài đã được chữa khỏi bệnh.
sobn = 0
for(i in unique(benhnhan$Nationality)){
  if(i != "Vietnamese"){
    sobn = sobn + length(benhnhan[benhnhan$Nationality==i&benhnhan$Status=="Recovered","ID"])
  }
}
cat ("Số bênh nhân nước ngoài được chữa khỏi :",sobn,"\n")
#d.Tính toán tỷ lệ bệnh nhân khỏi bệnh, đang điều trị, và đã tử vong trong
#từng nhóm tuổi khác nhau (ví dụ: dưới 18, 18-40, 41-60, trên 60).
#Nhóm tuổi nào có tỷ lệ hồi phục cao nhất.
tlbn <- function(dl,x){
  sbn=0
  for(i in unique(dl$ID)){
    if(dl[dl$ID==i,"Status"]==x){
      sbn = sbn + 1
    }
  }
  return(round(((sbn/length(dl$ID))*100),1))
}
cat("tỉ lệ người dưới 18 tuôi được chữa khỏi ,đang điều trị ,đã tử vong lần lượt là :")
for(i in unique(benhnhan$Status)){
  print(tlbn(benhnhan[benhnhan$Age<18,],i))
}

cat("tỉ lệ người trên 18 dưới 40 tuôi được chữa khỏi ,đang điều trị ,đã tử vong lần lượt là :")
for(i in unique(benhnhan$Status)){
  print(tlbn(benhnhan[benhnhan$Age>18&benhnhan$Age<=40,],i))
}

cat("tỉ lệ người dưới từ 41 đến 60 được chữa khỏi ,đang điều trị ,đã tử vong lần lượt là :")
for(i in unique(benhnhan$Status)){
  print(tlbn(benhnhan[benhnhan$Age<60&benhnhan$Age>40,],i))
}

cat("tỉ lệ người trên 60 tuôi được chữa khỏi ,đang điều trị ,đã tử vong lần lượt là :")
for(i in unique(benhnhan$Status)){
  print(tlbn(benhnhan[benhnhan$Age>=60,],i))
}

#Bai6
#a
setwd("D:\\PTDL")
data = na.omit(read.csv("Product.csv"))
#b Xác định tổng số sản phẩm “Chocolate” được bán ra tại thành phố San Diego.
chocolate = sum(data$City == "San Diego" & data$Product == "Chocolate Chip")
chocolate
#c Trích ra một bộ dữ liệu có loại sản phẩm là “Bars”, đặt tên là data1 và một bộ dữ liệu có loại sản phẩm là “Crackers”, đặt tên là data2.
#So sánh doanh thu trung bình trong ngày của hai loại sản phẩm trên.
data1 = subset(data, data$Category == "Bars")
data2 = subset(data, data$Category == "Crackers")

if(mean(data1$TotalPrice) > mean(data2$TotalPrice)){
  print("Doanh thu trung bình  của “Bars” lớn hơn")
}else {
  print("Doanh thu trung bình  của “Bars” nhỏ hơn")
}
#d Viết một hàm trong R để kiểm tra tổng số lượng bán ra của một sản phẩm tại thành phố New York.
#Áp dụng hàm để kiểm tra với sản phẩm “Carrot”.
data_ny <- subset(data, data$City =="New York")
data_ny

so_luong_ban_ra <- function(data, ten_sp) {
  tong_sl <- 0
  for (i in 1:nrow(data)) {
    if (data$Product[i] == ten_sp) {
      tong_sl <- tong_sl + data$Quantity[i]
    }
  }
  return(tong_sl)
}
so_luong_ban_ra(data_ny, "Carrot")
#e để đưa ra tên thành phố có tổng số lượng sản phẩm thuộc một loại nhất định bán ra nhiều nhất
get_city_with_max_quantity = function(x, df) {
  max_sum <- 0
  max_city <- " "
  for(i in unique(df$City)){
    total <- sum(df[df$City== i & df$Category== x,]$Quantity)
    if(total > max_sum){
      max_sum <- total
      max_city <- i
    }
  }
  return(max_city)
}
get_city_with_max_quantity("Cookies",data)

#e. TotalPrice=Quantity×UnitPrice 
data$TotalPrice1 <- data$Quantity*data$UnitPrice
data

for(i in 1:length(data$TotalPrice)){
  if(data$TotalPrice[i] != data$TotalPrice1[i]){
    data$TotalPrice[i] <- data$TotalPrice1[i]
  }
}
data

