#리스트 만들기
s_can<-function(){
  text<-"null"
  i=1
  values <-list()
  while(text!=""){
    text = readline(paste(i,": "))
    i =i+1
    if(!is.na(as.numeric(text))){
      text<-as.numeric(text)
    }
   if(text!="") 
      values[length(values)+1]<-text
  }
  return(values)
}

#자료 분석
an1<-function(x,tf){
  print(paste("Class :",class(x)))
  print(paste("dim :",dim(x)))
  print(paste("colnames :",colnames(x)))
  if(tf){
    print("head==========")
    print(head(x))
    print("tail==========")
    print(tail(x))
  }
}
#자료분석
an2<-function(x){
  if(is.character(x)) print("Class : character")
  else{
    print(paste("Min :",min(x,na.rm = TRUE)))
    print(paste("1st :",quantile(x,1/4,na.rm = TRUE)))
    print(paste("Median :",median(x,na.rm = TRUE)))
    print(paste("3st :",quantile(x,3/4,na.rm = TRUE)))
    print(paste("Max :",max(x,na.rm = TRUE)))
    print(paste("Mean :",mean(x,na.rm = TRUE)))
    print(paste("n :",sum(!is.na(x))))
    print(paste("na :",sum(is.na(x))))
    print(paste("IQR :",IQR(x,na.rm = TRUE)))
    hist(x)
  }
}

gp=geom_point(aes(color=SP))

#그래프 lab 이름 설정
lab<-function()
{
  title=readline("title :")
  sub=readline("subtitle :")
  cap=readline("caption :")
  tag=readline("tag :")
  alt=readline("alt :")
  alt_insight=readline("alt_insight :")
 
   labs(title = title,
       subtitle = sub,
       caption = cap,
       tag = tag,
       alt = alt,
       alt_insight =alt_insight 
       )
}
lab<-function()
{
  title=readline("title :")
  sub=readline("subtitle :")
  x=readline("xlab : ")
  y=readline("ylab : ")
  la=labs(title = title,
       subtitle = sub,
  )
  xl=xlab(x)
  yl=ylab(y)
  la+xl+yl
}
#파이 그래프
pi<-function(x,s=2){
  t_cn=table(x)
  NAME=names(t_cn)
  VALUE=c(unname(t_cn))
  df<-data.frame(NAME,VALUE)
  df$lab=paste(round(((df$VALUE/sum(df$VALUE))*100),2),"%")
  
  plt=ggplot(df,aes(x=1,y=VALUE,fill=NAME))
  pl=plt+coord_polar(theta = 'y')+theme_void()
  pl=pl+geom_bar(stat = "identity")
  pl=pl+geom_text(size=s,
                  aes(label =lab),
                  position=position_stack(vjust = 0.5)
  )
  
  return(pl)
}

pi2<-function(df,x,y){
 
  df$lab=paste(round(((y/sum(y))*100),2),"%")
  
  plt=ggplot(df,aes(x=1,y=y,fill=x))
  pl=plt+coord_polar(theta = 'y')+theme_void()
  pl=pl+geom_bar(stat = "identity")
  pl=pl+geom_text(size=2,
                  aes(label =lab),
                  position=position_stack(vjust = 0.5)
  )
  
  return(pl)
}

#바 그래프
bar<-function(data,x,co="white"){
  plt<-ggplot(data,aes(x))
  gb<-geom_bar(aes(fill=x))
  gt<- geom_text(aes(label=..count..),stat="count",vjust=2,color=co)
  plt+gb+gt
}

#col 그래프
co= function(a,b){
  plt<-ggplot(df_per2,aes(x=name,y=a))
  gb<-geom_col(aes(fill=name))
  df_sr=filter(df_per,df_per$name==b)
  la=labs(title=sprintf("%s",df_sr$name[1]),
          subtitle = sprintf("score :%d rank :%d",df_sr$score[1],df_sr$rank[1]),ylab='(%)')
  
  plt+gb+ylim(0,20)+la+theme(legend.position = "none",axis.title.y=element_blank())
}

