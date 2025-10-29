# câu 1 
#a. Nhập từ bàn phím dữ liệu của 35 hộ vào R.
tien_dien_thu_dien = c(1700,1800,1900,2000,2100,2200,2300)
so_ho = c(3,4,5,7,8,5,3)
data_dien = data.frame(tien_dien_thu_dien, so_ho)
data_dien
n = sum(data_dien$so_ho)
#bTính các giá trị trung bình và độ lệch tiêu chuẩn mẫu (s), biết trung bìnhvà độ lệch tiêu chuẩn mẫu được tính theo công thức
gttb = sum(data_dien$tien_dien_thu_dien * data_dien$so_ho) / n
gttb
phuong_sai = sum(data_dien$so_ho*(data_dien$tien_dien_thu_dien- gttb)*(data_dien$tien_dien_thu_dien- gttb)) / (n-1)
do_lech_chuan = sqrt(phuong_sai)

#c) Có ý kiến cho rằng: “Số điện trung bình của một hộ ở Hà Nội là 0190
#số điện”. Để kiểm tra ý kiến trên có đúng không, ta đặt T được tính bằng
#công thức dưới đây là Test thống kê.
#với n là cỡ mẫu (length)
#Nếu 1.757T thì ta có thể đưa ra kết luận ý kiến trên là sai.
T = (gttb - 1900)* sqrt(n)/sqrt(do_lech_chuan)
T
ifelse(T > 1.757,print("ket qua tren la sai"),print("ket qua tren la dung"))

#d. Hãy viết một hàm với biến đầu vào là dữ liệu x và hằng số 0 để kiểm tra
#tính chính xác của các khẳng định như trên.
kiem_tra = function(du_lieu_x,Mo){
  n = length(du_lieu_x)
  gttb = mean(du_lieu_x)
  do_lech_chuan = sd(du_lieu_x)
  T = (gttb - Mo)* sqrt(n)/ do_lech_chuan
  ifelse(T > 1.757,print("ket qua tren la sai"),print("ket qua tren la dung"))
}

#e. Hãy sinh ra một véc tơ mới, gồm 150 giá trị, là tiền điện tiêu thụ của 150
#hộ dân. Biết rằng, véc tơ này có phân phối chuẩn với cùng trung bình và
#độ lệch tiêu chuẩn của dữ liệu ban đầu
vecto_moi = rnorm(150,mean= gttb, sd = do_lech_chuan)
vecto_moi
#fHãy kiểm tra khẳng định : “Số điện trung bình của một hộ ở Hà Nội là
#0190
#số điện” có chính xác trên dữ liệu vừa sinh ra không ?
Mo = 1900
kiem_tra = kiem_tra(vecto_moi,Mo)
kiem_tra


#bài 2
#Nhập từ bàn phím dữ liệu của 100 ống tuýp vào R và đặt tên là
chieu_dai = c(178,179,180,181,182)
so_ong = c(12,18,35,20,15)
du_lieu = data.frame(chieu_dai,so_ong)
#bTính các giá trị trung bình và độ lệch tiêu chuẩn mẫu (s), biết trung
#bình và độ lệch tiêu chuẩn mẫu được tính theo công thức
n1 = sum(so_ong)
mean = sum(chieu_dai*so_ong) / n1
phuong_sai1 = sum(so_ong*(chieu_dai-mean)*(chieu_dai - mean)) / (n1-1)
do_lech_chuan1 = sqrt(phuong_sai1)
#c Viết một hàm để xác định xem một số 0 có thể đại diện cho chiều dài
#của các ống tuýp do xí nghiệp x sản xuất hay không, biết 0 có thể đại
#diện cho chiều dài của các ống tuýp do xí nghiệp trên sản xuất nếu 0
#nằm trong khoảng
# ta sử dụng dữ liệu thô x
xac_dinh = function(Mo,x){
  mean = mean(x)
  sd = sd(x)
  n = length(x)
  g1 = mean - 1.96*sd/ sqrt(n)
  g2 = mean + 1.96*sd/ sqrt(n)
  ifelse (g1< Mo & Mo > g2,print("Mo đại diện cho chiều dài các ống tuýt"),print("Mo không đại diện cho chiều dài các ống tuýt"))
}
#d Số Mo = 185 , có đại diện cho dữ liệu ban đầu hay không?
#chuyen du lieu ban dau sang du lieu tho
dulieu1 = c(rep(178,12),rep(179,18),rep(180,35),rep(181,20),rep(182,15))
Mo = 185
kiem_tra1 = xac_dinh(Mo,dulieu1)
kiem_tra1

#e Hãy sinh ra một véc tơ mới (vecto y), gồm 1500 giá trị, là chiều dài của
#1500 bóng tuýp. Biết rằng, véc tơ này có phân phối chuẩn với cùng
#trung bình và độ lệch tiêu chuẩn của dữ liệu ban đầu.
y = rnorm(1500,mean = mean, sd = do_lech_chuan1 )
y
#f
kiem_tra2 = xac_dinh(median(y), y)
kiem_tra2
# bai 3
#a Nhập từ bàn phím dữ liệu của 350 nhân viên vào R.
Thoi_gian = c(40,42,44,46,48,50)
So_nhan_vien = c(25,50,60,110,90,15)
du_lieu_thoi_gian_di_lam = data.frame(Thoi_gian,So_nhan_vien)
#b Tính các giá trị trung bình và độ lệch tiêu chuẩn mẫu (s), biết trung bình và
#độ lệch tiêu chuẩn mẫu được tính theo công thức
n2 = sum(So_nhan_vien)
gttb_thoi_gian = sum(Thoi_gian*So_nhan_vien)/ n2
phuong_sai2 = sum(So_nhan_vien*(Thoi_gian - gttb_thoi_gian)*(Thoi_gian - gttb_thoi_gian)) /(n2-1)
do_lech_chuan2 = sqrt(phuong_sai2)
# c và d thiếu đề bài 

#bài 4
#a Nhập dữ liệu từ tệp đã cho vào R và đặt tên là “red”. Giải thích ý nghĩa của
#lệnh dim.
red = read.csv("C:/Users/pc/Downloads/winequality.red.5.csv")
colnames(red)
red
#Lệnh dim là công cụ cơ bản để kiểm tra và quản lý hình dạng (shape) của dữ liệu có cấu trúc trong R.
#b Trong dữ liệu về total.sulfur.dioxide ta thấy các giá trị 29 và 102 bị nhập sai,
#nó phải là 92. Tìm và thay thế nó.
red$total.sulfur.dioxide[red$total.sulfur.dioxide== 29 & red$total.sulfur.dioxide == 102] = 92 

#c Trong dữ liệu về citric.acid, để khắc phục lỗi kĩ thuật khi đo đạc ta sẽ cộng
#thêm một lượng là 0,01 với những giá trị Không lớn hơn 0,02. Viết một hàm để
#thực hiện thao tác đó.
cong_them = function(x){
  for (i in 1:length(x)){
    if(x[i] <= 0.02){
      x[i] = 0.01 +x[i]
    }
  }
}
cong_them(red$citric.acid)
#d Giả sử rằng: rượu được đánh giá là tốt nếu hằng số k &gt; 8,3, trong đó
.# Viết một hàm để phân loại chất lượng rượu từ dữ liệu đã cho
#(“Tốt”, “Xấu”).
phan_loai = function(alcohol,quality,pH){
  phan_loai_Tot_xau = c()
  for (i in 1:length(pH)){
    k = alcohol[i] * quality[i] / pH[i]
    if(k >8.3){
      phan_loai_Tot_xau = c(phan_loai_Tot_xau,"Tốt")
    }
    else{
      phan_loai_Tot_xau = c(phan_loai_Tot_xau,"Xấu")
    }
    
  }
  phan_loai_Tot_xau
}
phan_loai = phan_loai(red$alcohol,red$quality,red$pH)
phan_loai
red$phan_loai = phan_loai
red
#e Từ việc phân loại trên, hãy tính trung bình, độ lệch tiêu chuẩn của độ pH và
#mật độ rượu density cho mỗi nhóm.
mean_tot_pH = mean(red$pH[red$phan_loai == "Tốt"])
mean_xau_pH = mean(red$pH[red$phan_loai == "Xấu"])
dlc_tot_pH = sd 
for (i in unique(red$phan_loai)){
  mean = mean(red$pH[red$phan_loai == i])
  sd = sd(red$pH[red$phan_loai == i])
  mean_density = mean(red$density[red$phan_loai == i])
  sd_density = sd(red$density[red$phan_loai == i])
  print(i)
  print(mean)
  print(sd)
  print(mean_density)
  print(sd_density)
}
# bài 5
#a) Nhập dữ liệu (“Sampledatasafety.csv”) và đặt tên là data vào R. Loại bỏ dữ
#liệu trống nếu có.
data = read.csv("C:/Users/pc/Downloads/Sampledatasafety.csv")
data = na.omit(data)
colnames(data)
#Trích ra một bộ dữ liệu con về nguyên nhân chấn thương do bỏng (Burn) và
#tính tổng chi phí điều trị (Incident Cost).
Burn = subset(data,Incident.Type="Burn")
sum(Burn$Incident.Cost)