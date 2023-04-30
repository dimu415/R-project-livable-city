source('yr_home.R')

# 서울시 초등학교O==============================================================
ele_school = read.csv('C:/RWorkspace/project_1/school_ele.csv', header = T, encoding = 'euc-kr')
ele_school <- subset(ele_school, 설립구분=='공립')
ele_school <- ele_school[c(4,8)] # 학교명, 주소

sl = strsplit(ele_school$도로명주소,' ') #도로명주소를 공백으로 구분
v = vector(mode='character')
i=1
for(s in sl)
{
  v[i] = s[2]
  i = i+1
}
ele_school$city = v #자지구 칼럼 추가

ct_school = table(unlist(ele_school$city)) #구별 초등학교 개수
ct_school = as.data.frame(ct_school)

# 서울시 유치원O=================================================================
kids = read.csv("C:/RWorkspace/project_1/kingdergarden.csv")
kids = kids[-c(1:3),c(2,3)]

ct_kids = kids[c(order(kids$자치구별.2.)),]

# 서울시 학원O==================================================================
aca=read.csv("C:/RWorkspace/project_1/academy.csv")

aca = filter(aca,aca$행정구역명!="")
aca=filter(aca,!grepl("입시",aca$분야명))
aca=filter(aca,!grepl("직업기술",aca$분야명))
aca=filter(aca,!grepl("정보",aca$분야명))
aca=filter(aca,!grepl("특수교육",aca$분야명))
aca=filter(aca,!grepl("인문사회",aca$분야명))
aca=filter(aca,!grepl("성인",aca$교습과정목록명))
aca=filter(aca,!grepl("성인",aca$교습과정명))
aca=filter(aca,!grepl("대학",aca$교습과정목록명))
aca=filter(aca,aca$학원.교습소=="학원")

df_aca=aca[,c(1,3)]

aca_sc=table(unlist(df_aca$행정구역명))
aca_cc=table(unlist(df_aca$분야명))
ct_academy = data.frame(aca_sc)

# 서울시 병원O===================================================================
hospital = read.csv('C:/RWorkspace/project_1/hospital.csv')

emergency = hospital[c(2,4,7,11)]
hospital = hospital[c(2,4,11)] # 주소, 병원분류,기관명

hospital=filter(hospital,!grepl("치과의원",hospital$병원분류명))
hospital=filter(hospital,!grepl("한방병원",hospital$병원분류명))
hospital=filter(hospital,!grepl("요양병원",hospital$병원분류명))
hospital=filter(hospital,!grepl("한의원",hospital$병원분류명))
hospital=filter(hospital,!grepl("성형",hospital$기관명))
hospital=filter(hospital,!grepl("한의원",hospital$기관명))
hospital=filter(hospital,!grepl(365,hospital$기관명))

sl2 = strsplit(hospital$주소, ' ')
v2 = vector(mode='character')

i=1
for(s in sl2)
{
  v2[i] = s[2]
  i = i+1
}

hospital$city = v2
View(hospital)
ct_hospital = table(unlist(hospital$city)) #구별 병원개수
ct_hospital = as.data.frame(ct_hospital)

# 응급실O========================================================================
emergency = filter(emergency,emergency$응급실운영여부.1.2.==1)

sl3 = strsplit(emergency$주소, ' ')
v3 = vector(mode='character')

i=1
for(s in sl3)
{
  v3[i] = s[2]
  i = i+1
}

emergency$city = v3

ct_emergency = table(unlist(emergency$city)) #구별 소아전문병원 개수
ct_emergency = as.data.frame(ct_emergency)

#서울시 공원O====================================================================
park = read.csv('C:/RWorkspace/project_1/park.csv')
park = park[c('공원명','지역')]
park = filter(park,park$지역!='과천시')

park$지역[park$지역=='']<-NA # empty string -> NA
park$지역 <- ifelse(is.na(park$지역),'노원구',park$지역) #결측치 대체

ct_park = table(unlist(park$지역))
ct_park = as.data.frame(ct_park)

# 서울시 범죄율O=================================================================
crime = read.csv('C:/RWorkspace/project_1/crime.csv')
crime = crime[-c(1:4),c(2,3,5,7,9,11,13)]

crime$X2021 = as.numeric(crime$X2021)
crime$X2021.2 = as.numeric(crime$X2021.2)
crime$X2021.4 = as.numeric(crime$X2021.4)
crime$X2021.6 = as.numeric(crime$X2021.6)
crime$X2021.8 = as.numeric(crime$X2021.8)
crime$X2021.10 = as.numeric(crime$X2021.10)

crime = rename(crime,
               c('자치구별.2.'='city',
                 'X2021'='발생합계','X2021.2'='살인','X2021.4'='강도',
                 'X2021.6'='성범죄','X2021.8'='절도','X2021.10'='폭력'))

ct_crime = crime[c(1,2)] #발생합계
ct_crime = ct_crime[c(order(ct_crime$city)),]

# 2023 구별 주민등록인구(0~6세/ 7세~12세)O=======================================
child = read.csv('C:/RWorkspace/project_1/kids.csv')

child = child[,c(1,4,5,6,7,8,9,10,11,12,13,14,15,16)]
child = child[-1,]

sl4 = strsplit(child$행정구역, ' ') #구 구하기
v4 = vector(mode='character')

i=1
for(s in sl4)
{
  v4[i] =s[2]
  i = i+1
}

for(r in 1:nrow(child)){
  for(c in 2:ncol(child)){
    child[r,c] = paste(child[r,c])
    child[r,c] = str_replace_all(child[r,c],",","")
  }
}
for(i in 2:length(child)){
  child[,i]=as.numeric(child[,i])
}

child$행정구역 = v4
child = arrange(child,행정구역)
names(child) = c('name','a','b','c','d','e','f','g','h','i','j','k','l','m')

child = child %>% mutate(child,유아=d+e+f+g)
child = child %>% mutate(child,초등학생=h+i+j+k+l+m)

ct_child = child[,c(1,15,16)]

# 주민등록인구 수O===============================================================
human = read.csv('C:/RWorkspace/project_1/human.csv')
human = human[-c(1:3),c(2,4)]
ct_human = arrange(human,동별.2.)

#서울시 지하철 위치정보O=========================================================
subway = read.csv('C:/RWorkspace/project_1/subway_map.csv')
subway = subway[c(3,4,6)]

sl6 = strsplit(subway$도로명주소,' ') #도로명주소를 공백으로 구분
v6 = vector(mode='character')

i=1
for(s in sl6)
{
  v6[i] = s[2]
  i = i+1
}

subway$city = v6 #자치구 칼럼 추가

ct_subway = table(unlist(subway$city)) 
ct_subway = ct_subway[-c(5,7,18,26,30)]
ct_subway = as.data.frame(ct_subway)

#대중교통 만족도================================================================
traffic = read.csv('C:/RWorkspace/project_1./traffic.csv')
ct_traffic = traffic[29:53,c(3,4)]
ct_traffic = arrange(ct_traffic, ct_traffic$구분별.3.)

# 자치구별 면적O=================================================================
size = read.csv('C:/RWorkspace/project_1./size.csv')
size = size[-c(1:3),c(2,3)]
size$X2021 <- gsub(",", "", size$X2021)
size$X2021 = as.numeric(size$X2021)

ct_size = size[c(order(size$자치구별.2.)),]

# 유통업체 현황O=================================================================
mart = read.csv('C:/RWorkspace/project_1./mart.csv')

mart = filter(mart,(mart$영업상태명 %in% c("영업/정상")))
mart = filter(mart,!(mart$사업장명 %in% c("명품관")))

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
#===============================================================================

all_df = data.frame(ct_human,
                  ct_child[-1],
                  ct_size[2],
                  ct_hospital[2],
                  ct_emergency[2],
                  ct_school[2],
                  ct_kids[2],
                  ct_crime[2],
                  ct_park[2],
                  ct_mart[2],
                  ct_traffic[2])

all_df = rename(all_df, c('동별.2.'='자치구', 
                          'X2022..12.1'='주민등록인구',
                          'X2021'='면적',
                          'Freq'='병원 수',
                          'Freq.1'='응급실 수',
                          'Freq.2'='초등학교 수',
                          'X2022'='유치원 수',
                          '발생합계'='범죄발생',
                          'Freq.3'='공원 수',
                          'Freq.4'='유통시설 수',
                          'X2021.1'='대중교통 만족도'))

all_df$`주민등록인구`= as.numeric(all_df$`주민등록인구`)
all_df$`초등학교 수`= as.numeric(all_df$`초등학교 수`)
all_df$`유치원 수`= as.numeric(all_df$`유치원 수`)
all_df$`병원 수`= as.numeric(all_df$`병원 수`)
all_df$`응급실 수`= as.numeric(all_df$`응급실 수`)
all_df$`공원 수`= as.numeric(all_df$`공원 수`)
all_df$`유통시설 수`= as.numeric(all_df$`유통시설 수`)
all_df$`대중교통 만족도`= as.numeric(all_df$`대중교통 만족도`)


all_df$인구밀도 = all_df$주민등록인구/all_df$면적 #구별 인구밀도
all_df$초등학생 = all_df$초등학생/all_df$면적 # 구별 초등생밀도
all_df$유아 = all_df$유아/all_df$면적 #구별 유아밀도

df = all_df
a = (df$`초등학교 수`/df$초등학생) #초등생 수 대비 초등학교 수 

b = (df$`유치원 수`/df$유아)#유아 수 대비 유치원 수

c = (df$`병원 수`/df$인구밀도)# 인구 수 대비 병원 수

d = (df$`응급실 수`/df$인구밀도) # 인구 수 대비 병원 수

e = (df$범죄발생/df$인구밀도) # 주민등록인구 대비 범죄발생건수

f = (df$`유통시설 수`/df$인구밀도) # 주민등록인구 대비 유통시설 수

g = (df$`대중교통 만족도`) # 대중교통만족도

h = (df$`공원 수`/df$인구밀도)



#정규화 x 가중치
a = round(scale(a, center = min(a), scale = max(a)-min(a)),3)

b = round(scale(b, center = min(b), scale = max(b)-min(b)),3)

c = round(scale(c,center = min(c), scale = max(c)-min(c))*0.6,3)

d = round(scale(d ,center = min(d), scale = max(d)-min(d))*0.6,3)

e = round(scale(e,center = min(e), scale = max(e)-min(e))*(-1),3)

f = round(scale(f, center = min(f), scale = max(f)-min(f))*0.2,3)

g = round(scale(g, center = min(g), scale = max(g)-min(g))*0.8,3)

h = round(scale(h, center = min(h), scale = max(h)-min(h))*0.2,3)


df2 = data.frame(df$자치구,a,b,c,d,e,f,g,h)
df2 = rename(df2, c('df.자치구'='자치구','a'='초등학교','b'='유치원','c'='병원','d'='응급실','e'='범죄발생',
              'f'='유통시설','g'='대중교통','h'='공원'))

#df2$가중치평균 = rowSums(df[,2:9])/9
df2$가중치평균 = round(rowSums(df2[2:9]/8),3)
df2 = arrange(df2,desc(가중치평균))
View(df2)

df3 = head(df2,10)

# 결과 그래프
df3_plt = ggplot(df3, aes(reorder(x=자치구,-가중치평균),y=가중치평균, fill=자치구))+ 
  geom_col()+
  geom_text(aes(label = 가중치평균), hjust=0.1)
  
df3_plt
