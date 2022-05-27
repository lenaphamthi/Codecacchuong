#Bài tập Chương 2
#BT1: Sử dụng hàm c, tạo một biến chứa các giá trị độ dài của bảy con vật. Đồng thời tạo một biến chứa các giá trị Tb (bao gồm cả NAs). Chiều dài trung bình của bảy con vật là bao nhiêu?
LengthCT <- c(75, 85, 91.6, 95, NA, 105.5, 106)
Tb <- c(0, 0, 1, NA, 0, 0, 0)
mean(LengthCT, na.rm = TRUE)
#BT2: Sử dụng lệnh cbind để kết hợp dữ liệu tháng, độ dài và Tb, và lưu trữ kết quả trong biến Boar. Đảm bảo rằng bạn có thể trích xuất các hàng, cột và các phần tử của Boar. Sử dụng các hàm dim, nrow và ncol để xác định số lượng động vật và các biến trong Boar
Farm <- c("MO", "MO", "MO", "MO", "LN", "SE", "QM")
Month <- c(11, 7, 7, NA, 9, 9, 11)
Boar <- cbind(Month, LengthCT, Tb)
dim(Boar)
nrow(Boar)
ncol(Boar)
Boar
#BT3: Chúng ta tiếp tục với hươu từ bài 1. Thay vì hàm c mà bạn đã sử dụng trong bài 2 để kết hợp dữ liệu Tb. Bạn có thể làm tương tự với hàm vectơ không? Đặt tên khác cho vectơ, chẳng hạn như Tb2.
Tb2 <- vector(length = 7)
Tb2[1] = 0
Tb2[2] = 0
Tb2[3] = 1
Tb2[4] = NA
Tb2[5] = 0
Tb2[6] = 0
Tb2[7] = 0
cbind(Month, LengthCT, Tb2)
#BT 4: Tạo một ma trận sau R và xác định chuyển vị, nghịch đảo của nó và bội với nghịch đảo của chính nó.
D <- as.matrix(cbind(c(1, 2, 3), c(4, 2, 1), c(2, 3, 0)))
D
##chuyển vị 
t(D)
##nghịch đảo 
solve(D)
##bội với nghịch 
D%*%solve(D)
#BT 5: Hãy tạo một data frame chứa tất cả các dữ liệu được trình bày trong bảng ở bài 1. Giả sử rằng bạn quyết định bình phương dữ liệu độ dài. Thêm dữ liệu đó vào data frame. Làm bài tập tương tự với list thay vì data.frame.
df <- data.frame(
  Farm,
  Month,
  Year <- c(00, 00, 01, NA, 03, 03, 02),
  Sex <- c(1, 2, 2, 2, 1, 2, 2),
  LengthClass <- c(1, 1, 1, 1, 1, 1, 1),
  LengthCT,
  Ecervi <- c(0, 0, 0, NA, 0, 0, 0),
  Tb,
  LengthCT_Square <- LengthCT*LengthCT
)
df
#BT 6: Nhập dữ liệu vào R trước tiên bằng cách sử dụng hàm read.table và sau đó là hàm scan. Sử dụng hai tên khác nhau để lưu trữ dữ liệu. Sự khác biệt giữa hai hàm là gì? Sử dụng các hàm is.matrix và is.data.frame để trả lời câu hỏi này.
library(readxl)
ISIT <- read_excel("C:/Users/LENOVO/Downloads/ISIT.xls")
View(ISIT)
is.matrix(ISIT)
is.data.frame(ISIT)
#scan 
ISIT <- scan("C:/Users/LENOVO/Downloads/ISIT.xls",what = "numeric", dec = ".")
is.matrix(ISIT)
is.data.frame(ISIT)
#Bài tập chương 3
#BT1: Tệp BirdFlu.xls chứa số trường hợp xác nhận hàng năm về cúm gia cầm A(H5N1) ở người từ một số quốc gia đã báo cáo cho tổ chức y tế thế giới (WHO). In số ca mắc cúm gia cầm năm 2003. Tổng số ca mắc cúm gia cầm năm 2003 và năm 2005 là bao nhiêu? Quốc gia nào đã có nhiều trường hợp nhất? Quốc gia nào có ít ca tử vong do cúm gia cầm nhất? Cho biết tổng số ca mắc cúm gia cầm của mỗi quốc gia là bao nhiêu? Tổng số vụ mỗi năm là bao nhiêu?
BF <- read_excel("C:/Users/LENOVO/Downloads/BirdFlu.xls")
View(BF)
# In số ca mắc cúm gia cầm năm 2003.
data.frame(BF$Country, BF$cases...2)
# Tổng số ca mắc cúm gia cầm năm 2003 và tổng số năm 2005
sum(BF$cases...2)
sum(BF$cases...6)
#Thêm cột tổng số ca tử vong và số trường hợp mắc cúm gia cầm
BF$Total.Cases<-BF$cases...2 + BF$cases...4 + BF$cases...6 + BF$cases...8 + BF$cases...10 + BF$cases...12
BF$Total.Deaths<- BF$deaths...3 + BF$deaths...5 + BF$deaths...7 + BF$deaths...9 + BF$deaths...11 + BF$deaths...13
# Quốc gia nào đã có nhiều trường hợp mắc bệnh nhất? 
BF[BF$Total.Cases == max(BF$Total.Cases), ]$Country
# Quốc gia nào có ít ca tử vong do cúm gia cầm nhất?
BF[BF$Total.Deaths == min(BF$Total.Deaths), ]$Country
#Tổng số ca mắc cúm gia cầm của mỗi quốc gia
data.frame(BF$Country, BF$Total.Cases)
#Tổng số ca mắc mỗi năm
df<-data.frame(c(2003, 2004, 2005, 2006, 2007, 2008), 
               c(sum(BF$cases...2), sum(BF$cases...4), sum(BF$cases...6), sum(BF$cases...8), sum(BF$cases...10), sum(BF$cases...12)))
names(df)<- c("Year", "Case")
df
#BT2: Trích xuất dữ liệu từ trạm 1. Có bao nhiêu quan sát được tại trạm này? Độ sâu lấy mẫu tối thiểu, trung vị, trung bình và tối đa tại trạm 1 là bao nhiêu?
#Xác định xem trạm nào có ít quan sát hơn đáng kể. Tạo một khung dữ liệu mới bỏ qua các trạm này.
#Trích xuất dữ liệu từ năm 2002. Trích xuất dữ liệu từ tháng 4. Trích xuất dữ liệu được đo ở độ sâu lớn hơn 2000 mét. Hiển thị dữ liệu theo giá trị độ sâu tăng dần. Hiển thị dữ liệu được đo ở độ sâu hơn 2000 mét vào tháng Tư.
ISIT <- read_excel("C:/Users/LENOVO/Downloads/ISIT.xls")
View(ISIT)
#Trích xuất dữ liệu từ trạm 1
ISIT.Station1 <- ISIT[ISIT$Station == 1,]
head(ISIT.Station1)
nrow(ISIT.Station1)
# GTLN, GTNN, Giá trị trung bình, Giá trị trung vị của độ sâu tại trạm 1
min(ISIT.Station1$`Sample Depth`)
median(ISIT.Station1$`Sample Depth`)
mean(ISIT.Station1$`Sample Depth`)
max(ISIT.Station1$`Sample Depth`)
#Xác định số quan sát của các trạm
tapply(ISIT$Sources, FUN = length, INDEX = ISIT$Station)
#Ta thấy trạm 4 và trạm 5 có số quan sát ít hơn so với phần còn lại. Tạo khung dữ liệu mới bỏ qua 2 trạm này. 
ISIT2 <- ISIT [ISIT$Station!= 4 & ISIT$Station != 5,]
#file tài xuống bị lỗi, không có năm và tháng cho tập dữ liệu nên không thể tiến hành code được 
#BT3: Trong bước cuối cùng của bài tập trước, dữ liệu đo được ở độ sâu hơn 2000 mét vào tháng 4 đã được trích xuất. Xuất những dữ liệu này sang một tệp ascii mới.
#BT4: Các trạm từ 1 đến 5 được lấy mẫu vào tháng 4 năm 2001, các trạm từ 6 đến 11 vào tháng 8 năm 2001, các trạm từ 12 đến 15 vào tháng 3 năm 2002 và các trạm từ 16 đến 19 vào tháng 10 năm 2002. Tạo hai biến mới trong R để xác định tháng và năm. Thực hiện việc này bằng cách thêm các biến mới bên trong data frame.
#Bài tập chương 4
#BT1: Tính toán giá trị trung bình hàng tháng một lần bằng cách sử dụng dữ liệu từ tất cả các trạm. Kết quả cuối cùng phải là một ma trận 16 x 12. Đồng thời tính độ lệch chuẩn và số lần quan sát mỗi tháng
Tem<-read_excel("C:/Users/LENOVO/Downloads/RBook/RBook/Temperature.xls")
View(Tem)
mean<-matrix(nrow = 16, ncol = 12)
std <- matrix(nrow = 16, ncol = 12)
count<- matrix(nrow = 16, ncol = 12)
allyear <- unique(Tem$Year)
for (i in 1:16) {
  year = allyear[i]
  val = Tem[Tem$Year == year,]
  mean[i,] = tapply(val$Temperature,val$Month,mean, na.rm = TRUE)
  std[i,] = tapply(val$Temperature,val$Month, sd, na.rm = TRUE)
  count[i,] = tapply(val$Temperature,val$Month, length)
}
#Độ lệch chuẩn của nhiệt độ mỗi tháng từng năm 
std
#Số lần quan sát mỗi tháng từng năm 
count
#BT4: Sử dụng dữ liệu trong bài 1, xác định số lần quan sát trên mỗi trạm. Có bao nhiêu quan sát được thực hiện mỗi năm? Có bao nhiêu quan sát được thực hiện ở mỗi trạm mỗi năm?
#Số lần quan sát trên mỗi trạm
table(Tem$Station)
#Số quan sát được thực hiện mỗi năm
table(Tem$Year)
#Số quan sát được thực hiện ở mỗi trạm theo từng năm
table(Tem$Year, Tem$Station)
#Bài tập chương 5
#BT1:Tạo một plot của TOT_N so với D_park. Sử dụng nhãn thích hợp.Thêm một đường cong làm mịn. Tạo lại plot nhưng sử dụng điểm tỷ lệ với giá trị của OLIVE (điều này có thể cho biết liệu OLIVE có ảnh hưởng hay không).
Amphibian_road_Kills <- read_excel("C:/Users/LENOVO/Downloads/RBook/RBook/Amphibian_road_Kills.xls")
View(Amphibian_road_Kills)
plot(x=Amphibian_road_Kills$D.PARK, y=Amphibian_road_Kills$TOT.N, xlab = "Distance to park", ylab = "Number of dead animal")
M.Loess = loess(TOT.N ~ D.PARK, data = Amphibian_road_Kills)
Fit = fitted(M.Loess)
Ord1 = order(Amphibian_road_Kills$D.PARK)
lines(Amphibian_road_Kills$D.PARK[Ord1], Fit[Ord1], lwd = 3, lty = 2)
plot(x = Amphibian_road_Kills$D.PARK, y=Amphibian_road_Kills$TOT.N, cex = 0.5 + 2*Amphibian_road_Kills$OLIVE/max(Amphibian_road_Kills$OLIVE), xlab = "Distance to park", ylab = "Number of dead animals")
lines(Amphibian_road_Kills$D.PARK[Ord1], Fit[Ord1], lwd = 3, lty = 2)
#Bài tập chương 6
#BT1: Vẽ biểu đồ dữ liệu nhiệt độ theo thời gian (tháng) cho từng trạm và lưu đồ thị dưới dạng tệp jpg
TemP<- read_excel("C:/Users/LENOVO/Downloads/RBook/RBook/Temperature.xls")
View(TemP)
AllStations <- unique(TemP$Station)
for (i in 1:30) {
  Station.i = AllStations[i]
  filename = paste(Station.i, ".jpg", sep = "")
  jpeg(file = filename)
  TemP.i = TemP[TemP$Station == Station.i, ]
  temp = tapply(X = TemP.i$Temperature, INDEX = TemP.i$Month, FUN = mean, na.rm = T)
  plot(temp)
  dev.off()
}
#BT2: Sử dụng ifelse tạo một biến phân loại mới xác định những quan sát từ chế độ ăn tại một tổ cụ thể. Thử chạy lại mã từ bài 1 để lập biểu đồ thương lượng giữa anh chị em với thời gian đến cùng một tổ và và phân loại chế độ ăn.
Owls <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/Owls.txt")
View(Owls)
AllNest <- unique(Owls$Nest)
for (i in 1:27) {
  Owls.i = Owls[Owls$Nest == AllNest[i], ]
  filename = paste(AllNest[i], ".jpg", sep = "")
  jpeg(file = filename)
  plot(x = Owls.i$ArrivalTime, y = Owls.i$SiblingNegotiation, col = c("blue", "red")[ifelse(Owls.i$FoodTreatment == "Deprived", 1, 2)])
  legend(x= "right", c("Deprived", "Satiated"), col = c("blue", "red"), pch = 1)
  dev.off()
}
#BT3: Viết lại hàm tính độ phong phú và độ đa dạng của sinh vật
RIKZ <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/RIKZ.txt", header=FALSE)
View(RIKZ)
RIKZ <- RIKZ[ ,2:76]
Veg <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/Vegetation2.txt")
View(Veg)
Veg <- Veg[ ,5:9]
diversity = function(X, Choice = "Shannon") {
  if (Choice == "Richness")
  {
    index <-rowSums(X > 0, na.rm = TRUE)
  } 
  else if (Choice == "Total Abundance")
  {
    index <- rowSums(X, na.rm = TRUE)
  }
  else if (Choice == "Shannon")
  {
    RS <- rowSums(X, na.rm = TRUE) # this gives # animals counted per site.
    Prop <- X/RS # this gives the % of a given species relative to total species per site.
    H <- -rowSums(Prop * log10(Prop), na.rm = TRUE)
    index <- H
  }
  else
  {
    print("Incorrect value, please enter new choice")
    index <- NA
  }
  list(index = index, MyChoice = Choice)
}
diversity(RIKZ, "Shannon")
#Bài tập chương 7 
#BT1 :  Lập biểu đồ tròn thể hiện tổng số ca tử vong mỗi năm và một biểu đồ biểu thị số ca tử vong trên mỗi quốc gia.
BirdFluDeaths <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/BirdFluDeaths.txt")
View(BirdFluDeaths)
CBF = colSums(BirdFluDeaths[,2:16])
YBF=  rowSums(BirdFluDeaths[,2:16])
pie(CBF, labels = CBF, col = rainbow(length(CBF)),  main="s??? ca t??? vong c???a m???i qu???c gia", clockwise = TRUE, radius = .8)
legend(x = 0.82, y = 0.5, c('Azerbaijan','Bangladesh','Cambodia','China','Djibouti','Egypt','Indonesia','Iraq','LaoPDR','Myanmar','Nigeria','Pakistan','Thailand','Turkey','VietNam'), cex = 0.65, fill = rainbow(length(CBF)))
pie(YBF, labels = YBF, col = rainbow(length(YBF)),  main="S??? ca t??? vong theo t???ng nãm", clockwise = TRUE, radius = .8)
legend(x = 0.82, y = 0.6, c(2003:2008), cex = 0.65, fill = rainbow(length(YBF)))
#BT2: Tạo biểu đồ thanh theo giá trị trung bình ứng với từng lát cắt và thêm một đường thẳng đứng cho lỗi tiêu chuẩn.
r??? trung b??nh ???ng v???i t???ng lát c???t và thêm m???t ðý???ng th???ng ð???ng cho l???i tiêu chu???n.
Veg <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/Vegetation2.txt")
View(Veg)
mean <- tapply(Veg$R, Veg$Transect, mean)
std<- tapply(Veg$R, Veg$Transect, sd)
len<- tapply(Veg$R, Veg$Transect, length)
Se <- std/sqrt(len)
bar <- barplot(mean, xlab = "Transect", ylab = "Richness", ylim = c(0,20))
arrows(bar, mean, bar, mean + std, lwd = 1.5, angle=90, length=0.1)
box()
#Lập biểu đồ trong đó trung bình được vẽ dưới dạng điểm đen, sai số chuẩn dưới dạng các đường xung quanh giá trị trung bình và các dữ liệu quan sát được thể hiện dưới dạng dấu chấm trắng.
plot(x=Veg$Transect, y = jitter(Veg$R), pch=1,  xlab = "Transect", ylab = "Richness")
points(1:8, mean, pch = 16, cex = 1.5)
arrows(1:8,mean, 1:8, mean + Se, lwd = 1.5, angle=90, length=0.1)
arrows(1:8, mean, 1:8, mean- Se, lwd = 1.5, angle=90, length=0.1)
#BT3: Sử dụng số liệu về thảm thực vật trong Bài tập 2, hãy lập biểu đồ hình hộp thể hiện sự phong phú các giá trị.
boxplot(Veg$R)
#BT4: Tạo một ô vuông của số lượng ký sinh trùng (Cường độ) tùy theo khu vực, giới tính, giai đoạn hoặc năm. Thử các kết hợp để phát hiện các tương tác.
CodP <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/CodParasite.txt")
View(CodP)
boxplot(Intensity ~ Area, data = CodP)
boxplot(Intensity ~ Sex, data = CodP)
boxplot(Intensity ~ Year, data = CodP)
boxplot(Intensity ~ Stage, data = CodP)
##Thử kết hợp nhiều yếu tố để tìm sự tương tác
boxplot(Intensity ~ Area*Stage, data = CodP)
#BT5: Lập hai biểu đồ chấm về thương lượng anh chị em và thời gian đến.
Owls <- read.delim("C:/Users/LENOVO/Downloads/RBook/RBook/Owls.txt")
View(Owls)
dotchart(Owls$NegPerChick, main = "Negotiation", xlab = "Value of variable", ylab = "Order of the data")
dotchart(Owls$ArrivalTime, main = "Arrival time", xlab = "Value of variable", ylab= "Order of the data")
#Lập biểu đồ chấm hiển thị thời gian đến nơi mỗi đêm. Và phân biệt chế độ ăn của của mỗi đêm.
dotchart(Owls$ArrivalTime,
          main = "Arrival time",
          xlab = "Value of variable",
          ylab = "Order of the data",
          groups = ifelse(Owls$FoodTreatment == "Deprived", 1, 2),
          color = ifelse(Owls$FoodTreatment == "Deprived", 1, 2),)
#BT6 : Tạo biểu đồ chấm thể hiện cường độ theo khu vực, giới tính, giai đoạn, năm.
dotchart(CodP$Intensity, groups = factor(CodP$Area))
dotchart(CodP$Intensity, groups = factor(CodP$Year))
dotchart(CodP$Intensity, groups = factor(CodP$Sex))
dotchart(CodP$Intensity, groups = factor(CodP$Stage))
#Tạo biểu đồ chấm thể hiện độ sâu theo mức độ phổ biến
dotchart(CodP$Depth, groups = factor(CodP$Prevalence))
         
         
