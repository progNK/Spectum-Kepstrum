data <- read.csv('/Users/NK/Desktop/Prac6sem/5/Test5/var_6_Task_5.rds',header = T)
head(data)

library(TSA)

#выборка series1

x1 <- data[,1]
matplot(x1,type='p',main='Series1',pch=21)

#Вычислить периодограмму данных выборки 1. 
#По ней определить 2 частоты гармонических колебаний, присутствующих в данных 

pgm<- periodogram(x1, col = "blue",lwd = 3,main = 'Series1. Periodogram',xlab='Frequency',ylab='Intensity',panel.first=grid(nx=NULL,ny = NULL))

#Исследовать ряд на предмет наличия Эхо эффекта. 
#При наличии эха определить задержку и величину кепстра на задержке.

x2 <- data[,2]
matplot(x2,type='b',main='Series2',pch=21,col='purple')

#Ряд нестационарный, поэтому берем разности

dx2 <-diff(x2)
matplot(dx2,type='b',main='Diff. Series2',pch=21,col='purple')

#Строим периодограмму для разностей:

pgm<- periodogram(dx2, col = "blue",lwd = 3,main = 'Diff. Series2. Periodogram',xlab='Frequency',ylab='Intensity',panel.first=grid(nx=NULL,ny = NULL))

#В периодограмме едва заметны косинусоидальные колебания.
#Они станут заметнее, если провести сглаживание по 3-х кратной свертке окна Даниелла ширины 7

specc <-spec.pgram(dx2,c(7,7,7),col = "blue",lwd = 3,main = 'Diff. Series2. Smoothed Periodogram',xlab='Frequency',ylab='Intensity',panel.first=grid(nx=NULL,ny = NULL))

#Это явный признак наличия эхо в ряде разностей. 
#Чтобы убедится в этом проведем оценку кепстра.

l <- Re(fft(log(abs(fft(dx2))), inverse = TRUE))
l[1]=0
barplot(l[1:100],col = "blue",main="Kepstr",panel.first=grid(nx=NULL,ny = NULL))

(kep_val <-l[8])

#Провести спектральный анализ ряда. 
#Построить периодограмму и провести сглаживание с помощью 3-х кратной свертки окна Даниэля. 
#Оценить стандарное отклонение периодограммы и сглаженной периодограммы.

x3 <- data[,3]
matplot(x3,type='b',main='Series3',pch=20,col='blue')

#Оценим периодограмму:

prgrm<- periodogram(x3, col = "blue",lwd = 3,plot = FALSE)
plot(prgrm$freq,prgrm$spec,main = "Series3. Periodogram", type = 'b',pch = 20, col='blue')

#Вычислим стандарное отклонение периодограммы:
(sd_prgrm<- sd(prgrm$spec)) 

#Оценим сглаженную периодограмму методом spec.pgram() по 3-х кратной свертке окна Даниеля.
#Ширина окна равна 7.

sm_prgrm <-spec.pgram(x3,c(7,7,7),plot=FALSE)
plot(sm_prgrm$freq,sm_prgrm$spec,type = 'b',pch = 20,main = "Series3. Smoothed Periodogram",col='blue')

#Вычислим стандарное отклонение сглаженной периодограммы:
(sd_sm_prgrm<- sd(sm_prgrm$spec))

plot(sm_prgrm$freq,sm_prgrm$spec,type = 'b',pch = 20,main = "Series3. Periodogram and Smoothed Periodogram",col='blue')
lines(prgrm$freq,prgrm$spec, type = 'b',pch = 20, col='Magenta')
legend("topright",legend= c("Smoothed Periodogram","Periodogram"),col = c("blue","magenta"),pch = 20)

res <- list(
  kep_val <- l[8],
  sd_prgrm<- sd(prgrm$spec),
  sd_sm_prgrm<- sd(sm_prgrm$spec)
)
res
