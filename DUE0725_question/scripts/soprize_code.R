#soprize
dev.off()
#par(family="AppleGothic")
#set.seed(1)
#colors<- colors()[sample(502,10, replace=FALSE)]

pdf.options(family = "Korea1deb")

allyears <- c(2007:2021)
all.df <- data.frame();
for (i in allyears){
  filename = paste(i,".txt",sep="")
  print(filename)
  tmp <- read.csv(filename, header=T)
  all.df <- rbind(all.df,tmp)
}
#data filt (remove ???)
test <- all.df$프로그램명
test[test=="여성???아동인권보호및가족지원"] = "여성및아동인권보호및가족지원"
test[test=="여성???아동인권보호"] = "여성및아동인권보호"
test[test=="여성???아동폭력예방및보호지원"] = "여성및아동폭력예방및보호지원"
test[test=="여유자금 운용"] = "기타"
test[test=="기금간거래"] = "기타"
test[test=="여유자금운용"] = "기타"
test[test=="기금간거래(전출금)"] = "기타"
all.df$프로그램명<- test

all_yesan_in_a_year <- c();
for (i in allyears){
  yesan_in_a_year <- sum(all.df[all.df$회계년도==i,12])
  all_yesan_in_a_year <- c(all_yesan_in_a_year ,yesan_in_a_year)
  print(paste(i," 국회확정금액: ", yesan_in_a_year, sep=""))
}
pdf("bar.pdf")
barplot(all_yesan_in_a_year,ylab = "all_budget", xlab="years", main="all_budget per year", names.arg=allyears)
dev.off()

union_programs <- c();
for (i in allyears){
  test <- all.df[all.df$회계년도==i,c(7)]
  union_programs<-c(union_programs,unique(test))
}
union_programs <- unique(union_programs)
set.seed(22214)
program_colors<- colors()[sample(502,length(union_programs), replace=FALSE)]

df.22mat <- data.frame()
pdf("./pies.pdf",width=20,height=10)
for (i in allyears){
  print(i)
  bar_data <- c();
  bar_label <- c();
  programs <- all.df[all.df$회계년도==i,c(7)]
  all_programs <- unique(programs)
  use_colors <- program_colors[match(all_programs,union_programs)]
  for (j in all_programs){
    if(length(all.df[all.df$회계년도==i & all.df$프로그램명==j,c(12)])!=0){
      val <- sum(all.df[all.df$회계년도==i & all.df$프로그램명==j,c(12)])
    }else{
      val = 0;
    }
    index <- match(i,allyears)
    perc <- round(val/all_yesan_in_a_year[index],2)
    #percs <- c(percs,perc)
    
    bar_data <- c(bar_data,val)
    bar_label <- c(bar_label, paste(j,"(",perc,"%)"))
    #print (paste(all_programs, val))
  }
  names(bar_data)<- all_programs
  pie(bar_data, main=i,col=use_colors, labels = bar_label)
  tmp.vec <- c(rep(0,length(union_programs)))
  tmp.vec[match(names(bar_data),union_programs)] <- bar_data
  df.tmp <- data.frame(tmp.vec)
  colnames(df.tmp) <- i
  if(length(df.22mat)==0){
    df.22mat<-df.tmp
  }else{
    df.22mat<-cbind(df.22mat,df.tmp)  
  }
}

dev.off();
rownames(df.22mat) <- union_programs

df.22mat2<-t(df.22mat)
pcm = melt(df.22mat2)
colours <- program_colors
pcm$Sample <- factor(union_programs)
xx = ggplot(pcm, aes(x = Var1, y = Var2)) + 
  geom_point(aes(size=ifelse(value==0, NA, value), fill = Var2), alpha = 0.75, shape = 21) + 
  #scale_size_continuous(limits = c(0,1417780052), range = c(1,17))) + 
  labs( x= "Years", y = "Programs", size = "국회확정예산\n(천원)", fill = "")  + 
  #theme(legend.key=element_blank(), 
  #      axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
  #      axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
  #      legend.text = element_text(size = 10, face ="bold", colour ="black"), 
  #      legend.title = element_text(size = 12, face = "bold"), 
  #      panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
  #      legend.position = "right") +  
  scale_fill_manual(values = colours, guide = "none") + 
  scale_y_discrete(limits = rev(levels(pcm$variable))) 

pdf("bubble.pdf",width=10)
xx
dev.off()

####SEE detail
for (i in allyears)
  print(unique(all.df[all.df$회계년도==i,c(7,8)]))
}

#### 2021
unique(all.df[all.df$회계년도==2021,c(7,8)])
write.csv(unique(all.df[all.df$회계년도==2021,c(7,8)]),file="test.txt")

sum_print <- c()
ar_money <- c()
ar_labels<- c()
sub1 <- unique(all.df[all.df$회계년도==2021,c(8)])
for(i in c(1:length(sub1))){
  subname <- sub1[i]
  sum_sub1 <- sum(all.df[all.df$회계년도==2021&all.df$단위사업명==subname,c(12)])
  sum_print <- c(sum_print,paste(subname,":",sum_sub1))
  ar_money<- c(ar_money,sum_sub1)
  ar_labels<-c(ar_labels,subname)
}
write.csv(sum_print,file="sum_sub1.csv")

############TOP5
top5_sub1<-names(sort(ar_money,decreasing = T)[c(1:5)])
for (i in c(1:length(top5_sub1))){
  sub_name <- top5_sub1[i]
  print(sub_name)
  sub2_list <- paste(unique(all.df[all.df$회계년도==2021&all.df$단위사업명==sub_name,c(10)]),sep=",")
  print(sub2_list)
}

length(all.df[all.df$회계년도==2021,c(8)])


#########################NOUSE

#make value
for (i in allyears){
  for (j in all_programs){
    test <- all.df[all.df$회계년도==i & all.df$프로그램명==j,c(12)]
  }
}

#makelabels
labs <-c();
for (i in allyears){
  for (j in all_programs){
    labs <- c(nodes,paste(i,"_",j,sep=""))
  }
}
vals <-c();
df.perprog = data.frame()
