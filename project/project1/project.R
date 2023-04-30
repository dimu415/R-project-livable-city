
#  초등학교 데이터 ------------------------------------------------------------
#서울시 초등학교 공립 남녀 공학 데이터 , 구 초등학교 개수 
df=read.csv('school_ele.csv' ,header = T, fileEncoding = "euc-kr")
df=data.frame(df)

table(df$학교종류명) #초등학교만있는지 확인
table(df$남녀공학구분명) #남녀공학만있는지 확인
#필요한 데이터만 가져오기
df1=filter(df,df$설립구분=="공립") # 공립인 학교만 가져오기
df1=dplyr::select(df,관할조직명,도로명주소)

sum(is.na(df1)) #null값 확인
sum(duplicated(df1$도로명주소)) #중복값 확인
sl = strsplit(df1$도로명주소, ' ') #구 구하기
f = vector(mode='character')

i=1
for(s in sl)
{
  f[i] =s[2]
  i = i+1
}
f
df1$도로명주소=f
sc=table(unlist(df1$도로명주소)) #빈도표 생성
df_sc=as.data.frame(sc,stringsAsFactors = FALSE) # 프레임 변환
df_sc=rename(df_sc,주소=Var1,학교수=Freq) #이름변경
df_sc=arrange(df_sc,df_sc$주소)


# 범죄 ----------------------------------------------------------------------


#구 범죄
bd1=read_xlsx('crime2021.xlsx') #2021
bd2=read_xlsx('crime2020.xlsx') #2020
bd3=read_xlsx('crime2019.xlsx') #2019


bd1=bd1[c(-1,-2,-3),]
bd1=bd1[,c(3,4)]
names(bd1)=c("주소","to21")


bd2=bd2[c(-1,-2,-3),]
bd2=bd2[,c(3,4)]
names(bd2)=c("주소","to20")

bd3=bd3[c(-1,-2,-3),]
bd3=bd3[,c(3,4)]
names(bd3)=c("주소","to19")


bd<-left_join(bd1,bd2,by="주소")

bd<-left_join(bd,bd3,by="주소")
bd=arrange(bd,bd$주소)

bd=data.frame(bd)


cr1=read_xlsx('crime2021.xlsx') #2021
cr1=cr1[c(-2,-3),]
names(cr1)=c(cr1[1,])
cr1=cr1[,c(3,6,8,10,12,14)]
cr1=cr1[-1,]

# 병원 ----------------------------------------------------------------------


#병원
hospital=read.csv("hospital.csv",header = T, fileEncoding = "euc-kr")
hospital=filter(hospital,
                !grepl("치과의원",hospital$병원분류명),
                !grepl("한방병원",hospital$병원분류명),
                !grepl("요양병원",hospital$병원분류명),
                !grepl("한의원",hospital$병원분류명),
                !grepl("성형",hospital$기관명),
                !grepl("한의원",hospital$기관명),
                !grepl(365,hospital$기관명))

hos=hospital

sl = strsplit(hos$주소, ' ') #구 구하기
f = vector(mode='character')

i=1
for(s in sl)
{
  f[i] =s[2]
  i = i+1
}
hos$주소=f
ta_hos=table(unlist(hos$주소))
df_hos=as.data.frame(ta_hos,stringsAsFactors = FALSE) # 프레임 변환
df_hos=arrange(df_hos,df_hos$Var1)
names(df_hos)=c("주소","병원수")
#응급실
table(hos$응급실운영여부.1.2.)
a=filter(hos,hos$응급실운영여부.1.2.==1)
df_a=table(unlist(a$주소))
df_a=as.data.frame(df_a,stringsAsFactors = FALSE)
names(df_a)=c("주소","응급실수")
df_hos=left_join(df_hos,df_a,by="주소")
# 유치원 ---------------------------------------------------------------------


#유치원
kind=read.csv("kindergarden.csv")
kind=kind[c(-1,-2,-3),c(2,3)]

for(i in 2:length(kind)){
  kind[,i]=as.numeric(kind[,i])
}
df_kind=arrange(kind,kind$자치구별.2.)


# 학원 ----------------------------------------------------------------------


#학원
aca=read.csv("academy.csv",fileEncoding = "euc-kr")
aca=arrange(aca,aca$행정구역명)
aca=filter(aca,aca$행정구역명!="",
           !grepl("입시",aca$분야명),
           !grepl("직업기술",aca$분야명),
           !grepl("정보",aca$분야명),
           !grepl("특수교육",aca$분야명),
           !grepl("인문사회",aca$분야명),
           !grepl("성인",aca$교습과정목록명),
           !grepl("성인",aca$교습과정명),
           !grepl("대학",aca$교습과정목록명),
           aca$학원.교습소=="학원" )
table(aca$등록상태명)
df_aca=aca[,c(1,3)]

aca_sc=table(unlist(df_aca$행정구역명))
aca_cc=table(unlist(df_aca$분야명))
df_aca=data.frame(aca_sc)



# 공원 ----------------------------------------------------------------------
library(dplyr)
pa=read.csv("park.csv",fileEncoding = "euc-kr")
pa=data.frame(pa)
dd=table(unlist(pa$지역))
df_pa=data.frame(dd)
df_pa=df_pa[c(-1,-6),]
df_pa=arrange(df_pa,df_pa$Var1)
pa=filter(pa,pa$지역!="")
pa=rename(pa,"경"="X좌표.WGS84.","위"="Y좌표.WGS84.")





# 교통통만족도 ---------------------------------------------------------------------

traffic = read.csv('traffic.csv',fileEncoding = 'euc-kr')
abc=traffic[29:53,c(3,4)]
names(abc)=c("주소","만족도")
abc=arrange(abc,abc$주소)
# 면적 ----------------------------------------------------------------------


# 자치구별 면적
size = read.csv('size.csv')
size = size[-c(1:3),c(2,3)]
size$X2021 <- gsub(",", "", size$X2021)
size$X2021 = as.numeric(size$X2021)

ct_size = size[c(order(size$자치구별.2.)),]

# 유통업체 --------------------------------------------------------------------


# 유통업체 현황

mart = read.csv('mart1.csv',fileEncoding = 'euc-kr')

mart =dplyr:: filter(mart,(mart$영업상태명 == c("영업/정상")))
mart =dplyr:: filter(mart,!(mart$사업장명 == c("명품관")))

mart = mart[c('지번주소','도로명주소','사업장명','업태구분명')]

i=1
for(i in 1:length(mart$지번주소)){
  if(mart$지번주소[i]=="")mart$지번주소[i]= mart$도로명주소[i]
}

sl7 = strsplit(mart$지번주소,' ') #도로명주소를 공백으로 구분
v7 = vector(mode='character')

i=1
for(s in sl7)
{
  v7[i] = s[2]
  i = i+1}

mart$city = v7
mart = mart[c(3,5)]
ct_mart = table(unlist(mart$city))
ct_mart = as.data.frame(ct_mart)
ct_mart = filter(ct_mart,!(ct_mart$Var1==''))




# 나이별 인구수 ---------------------------------------------------------------------
pop=read.csv('pop.csv' ,header = T, fileEncoding = "euc-kr")
pop=pop[,c(1,4:16)]
pop=pop[-1,]
sl = strsplit(pop$행정구역, ' ') #구 구하기
f = vector(mode='character')

i=1
for(s in sl)
{
  f[i] =s[2]
  i = i+1
}
class(pop$a)
for(r in 1:nrow(pop)){
  for(c in 2:ncol(pop)){
    pop[r,c] = paste(pop[r,c])
    pop[r,c] = str_replace_all(pop[r,c],",","")
  }
}
for(i in 2:length(pop)){
  pop[,i]=as.numeric(pop[,i])
}

#대중교통 주민인구 병원 
pop$행정구역=f
pop=arrange(pop,행정구역)
names(pop)=c('name','a','b','c','d','e','f','g','h','i','j','k','l','m')

pop=pop %>% mutate(pop,유아=d+e+f+g)
pop=pop %>% mutate(pop,초등학생=h+i+j+k+l+m)

df_pop=pop[,c(1,15,16)]

# 총인구수 --------------------------------------------------------------------
to_pop=read.csv('human.csv')
to_pop=to_pop[-c(1:3),c(2,4)]
names(to_pop)=c("name","to")
to_pop=arrange(to_pop,to_pop$name)
# all_df ------------------------------------------------------------------
df_hos
all_df=data.frame(df_hos,
                  
                  학교수= df_sc$학교수,
                  범죄=bd$to21,
                  공원수=df_pa$Freq,
                  면적=ct_size$X2021,
                  유통업체=ct_mart$Freq,
                  유아=df_pop$유아,
                  어린이=df_pop$초등학생,
                  유치원수=df_kind$X2022,
                  총인구수=to_pop$to,
                  교통만족도=abc$만족도)
ct_mart$Freq
for(i in 2:length(all_df)){
  all_df[,i]=as.numeric(all_df[,i])
}
all_df$인구밀도도=all_df$총인구수/all_df$면적
all_df$어린이=all_df$어린이/all_df$면적
all_df$유아=all_df$유아/all_df$면적
for(i in 2:length(all_df)){
  all_df[,i]=as.numeric(all_df[,i])
}


# 정규화->가중치 ----------------------------------------------------------------


df = all_df
a = (df$학교수/df$어린이) #초등생 수 대비 초등학교 수 

b = (df$유치원수/df$유아)#유아 수 대비 유치원 수

c = (df$병원수/df$인구밀도)# 인구 수 대비 병원 수

d = (df$병원수/df$인구밀도) # 인구 수 대비 병원 수

e = (df$범죄/df$인구밀도) # 주민등록인구 대비 범죄발생건수

f = (df$유통업체/df$인구밀도) # 주민등록인구 대비 유통시설 수

g = (df$교통만족도) # 대중교통만족도

h = (df$공원수/df$인구밀도) #인구 수 대비 공원수수 



#정규화 x 가중치
a = round(scale(a, center = min(a), scale = max(a)-min(a)),3)

b = round(scale(b, center = min(b), scale = max(b)-min(b)),3)

c = round(scale(c,center = min(c), scale = max(c)-min(c))*0.6,3)

d = round(scale(d ,center = min(d), scale = max(d)-min(d))*0.6,3)

e = round(scale(e,center = min(e), scale = max(e)-min(e))*(-1),3)

f = round(scale(f, center = min(f), scale = max(f)-min(f))*0.2,3)

g = round(scale(g, center = min(g), scale = max(g)-min(g))*0.8,3)

h = round(scale(h, center = min(h), scale = max(h)-min(h))*0.2,3)


df2 = data.frame(df$주소,a,b,c,d,e,f,g,h)
names(df2) = c('자치구','초등학교','유치원','병원','응급실','범죄발생','유통시설','대중교통','공원')
#df2$가중치평균 = rowSums(df[,2:9])/9
df2$가중치평균 = round(rowSums(df2[2:9]/8),3)
df2 = arrange(df2,desc(가중치평균))

df3 = head(df2,10)

# 결과 그래프
df3_plt = ggplot(df3, aes(reorder(x=자치구,-가중치평균),y=가중치평균, fill=자치구))+ 
  geom_col()+
  geom_text(aes(label = 가중치평균), hjust=0.1)

df3_plt



# 범죄 시각화 ---------------------------------------------------------------------

#시각화
l=data.frame(cr1)
tab_t=t(l)
df_tab=data.frame(tab_t)
names(df_tab)=c(df_tab[1,])
df_tab=df_tab[-1,]
a=c(
  "살인",         
  "강도"    ,       "강간·강제추행", "절도" ,         
  "폭력"  )
df_tab$구분=a



q=vector(mode='character')
w=vector(mode='character')
e=vector(mode='character')
a=1
x=1
c=1
for(i in 1:length(l[,1])){#25
  for(j in 2:length(l[i,])){
    q[a]=l[i,1]
    a=a+1
    
  }
}
len=length(df_tab[1,])-1
len
for(i in 1:len){
  for(j in 1:length(df_tab[,i])){
    w[x]=df_tab[j,i]
    e[c]=df_tab[j,26]
    x=x+1
    c=c+1
  }
}
Sys.sleep(1) 

qwe=data.frame(q,w,e)
qwe$w=as.numeric(qwe$w)
qwe=arrange(qwe,qwe$q)


crime =qwe


