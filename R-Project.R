install.packages("foreign")
library(foreign)
install.packages("haven")
library(haven)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("xlsx")
library(xlsx)
raw_data <- read_spss("Koweps_hpda15_2020_beta1.1.sav")
View(raw_data)

# 1. 직종별 시간제 비율 상위 10개 ((2020년 15차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx 엑셀 파일을 가져온 후 기존의 데이터와 레프트 조인을 한다, 그 후 숫자로 되어 있는 시간제, 전일제를 문자로 변경하고, 직종별로 시간제/(전일제+시간제)를 구해 이 비율이 높은 상위 10개의 직종을 출력한다.) 

data1 <- raw_data
jobData <- as.data.frame(read_excel("(2020년 15차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx", sheet=4, range="C2:D157", col_names=c("code_job","job_name")))
jobData$code_job <- as.double(jobData$code_job)
data1 <- rename(data1, code_job = h15_eco9, working_hours_type= h15_eco6)
data1 <- left_join(data1, jobData, id = "code_job")
data1$working_hours_type <- ifelse(data1$working_hours_type == 1, "시간제", ifelse(data1$working_hours_type == 2, "전일제", NA))

job_working <- data1 %>%
  filter(!is.na(working_hours_type)) %>%
  group_by(job_name, working_hours_type) %>%
  summarise(n =n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group * 100, 2))

job_working <- job_working %>%
  filter(working_hours_type == "시간제") %>%
  arrange(desc(pct)) %>%
  select(job_name, working_hours_type, pct) %>%
  head(10)

ggplot(data = job_working, aes(x = reorder(job_name, pct), y = pct)) + geom_col(fill = 'blue') + coord_flip() + ggtitle("직종별 시간제 비율 상위 10개") + xlab("직종") + ylab("시간제 비율")


# 2 나이, 균등화소득에 의한 가구구분 형태에 따른 연간 자원봉사활동 횟수 평균 분석
data2 <- raw_data
data2 <- rename(data2, date_of_birth = h15_g4, household_type_by_income= h15_hc_all, volunteer_activities = p1504_6)

# 모름/무응답 처리
data2$volunteer_activities <- ifelse(data2$volunteer_activities == 999, NA, data2$volunteer_activities)
# 생년월일을 통해 나이 컬럼을 추가
data2$age <- 2020 - data2$date_of_birth + 1
data2 <- data2 %>%
  mutate(household_type = ifelse(data2$household_type_by_income == 1, "일반가구", ifelse(data2$household_type_by_income == 2, "저소득층 가구", NA)))

age_household_volunteer <- data2 %>%
  filter(!is.na(volunteer_activities)) %>%
  group_by(age, household_type) %>%
  summarise(mean_volunteer = mean(volunteer_activities))

ggplot(data = age_household_volunteer, aes(x = age, y = mean_volunteer, col=household_type)) + geom_line()+ ggtitle("나이, 균등화소득에 의한 가구구분 형태에 따른 자원봉사활동 연간 횟수 평균 분석") + xlab("나이") + ylab("자원봉사활동 연간횟수 평균") + labs(col = "가구구분 형태")


# 3. 한달 근로일 수, 성별에 따른 월 평균 임금 분석
data3 <- raw_data
data3 <- rename(data3, working_day = p1502_7, gender= h15_g3, income = p1502_8aq1)
# 데이터에 한달근로일수인데, 40일 이라고 적힌 데이터가 있어 결측치 처리했음
data3$working_day <- ifelse(data3$working_day == 99 | data3$working_day == 40 , NA, data3$working_day)
data3$gender <- ifelse(data3$gender == 9, NA, data3$gender)
data3$income <- ifelse(data3$income == 9999, NA, data3$income)
data3 <- data3 %>%
  mutate(gender_type = ifelse(data3$gender == 1, "남자", ifelse(data3$gender == 2, "여자", NA)))

working_gender_income <- data3 %>%
  filter(!is.na(income) & !is.na(working_day) & !is.na(gender_type)) %>%
  group_by(working_day, gender_type) %>%
  summarise(mean_income = mean(income))

ggplot(data = working_gender_income, aes(x=working_day, y=mean_income, colour=gender_type)) + geom_point(shape=15, size=2) + geom_smooth(formula = y ~ x, method=lm, level=0.95) + ggtitle("한달의 근로일 수, 성별에 따른 월 평균 임금 분석") + xlab("한달 근로일 수") + ylab("월 평균 임금") + labs(col = "성별")

# 4. 금연할 계획이 있는 사람 구분, 하루 평균 흡연량 빈도수 분석 
data4 <- raw_data  # 총 흡연 기간 
data4 <- rename(data4, average_amount_of_smoking = p1505_3aq6, smoking_cessation_plan = p1505_3aq8)
  table(data4$average_amount_of_smoking)
  
data4 <- data4 %>%
  mutate(smoking_cessation_plan_type = ifelse(data4$smoking_cessation_plan == 1 | data4$smoking_cessation_plan == 2 | data4$smoking_cessation_plan == 3, "금연 계획 있음", ifelse(data4$smoking_cessation_plan == 4, "금연 계획 없음", NA)))

smoking_count <- data4 %>%
  filter(!is.na(average_amount_of_smoking))

ggplot(data=smoking_count, aes(x=average_amount_of_smoking, fill = smoking_cessation_plan_type)) + geom_histogram(binwidth = 5) + ggtitle("하루 평균 흡연량 빈도수 분석") + xlab("하루 평균 흡연량") + ylab("빈도 수") + labs(fill = "금연 계획 여부")

# 5. 5개 권역별 지역구분에 따른 집의 가격 
data5 <- raw_data

data5 <- rename(data5, region = h15_reg5, home_price = h1506_6)
data5$home_price <- ifelse(data5$home_price == 0 | data5$home_price == 999999, NA, data5$home_price)

data5$region <- ifelse(data5$region == 1, "서울", ifelse(data5$region == 2, "광역시", ifelse(data5$region == 3, "시", ifelse(data5$region == 4, "군", ifelse(data5$region == 5, "도농복합군", NA)))))

region_price <- data5 %>%
  filter(!is.na(region) & !is.na(home_price)) %>%
  select(region, home_price)

ggplot(data = region_price, aes(x = region, y = home_price)) + geom_boxplot(, width=0.8, outlier.size=1, outlier.shape=16, outlier.colour="red") + stat_summary(fun="mean", geom="point", shape=22, size=1, fill="blue") + ggtitle("5개 권역별 지역구분에 따른 집의 가격") + xlab("지역") + ylab("집 가격(단위: 만원)")  + scale_y_continuous(limits = c(0, 2e+05))

# 6. 출신학교 소재지별 인문계 고등학교 비율 하위 10개
data6 <- raw_data
regionData <- as.data.frame(read_excel("(2020년 15차 한국복지패널조사) 조사설계서-가구원용(beta1).xlsx", sheet=2, range="B3:C22", col_names=c("code_region","region_name")))
data6 <- rename(data6, code_region = p1507_3aq3, school_type= p1507_3aq2)
data6 <- right_join(regionData, data6, id = "code_region")

data6$region_name <- ifelse(data6$region_name == 99, NA, data6$region_name)
data6$school_type <- ifelse(data6$school_type == 99, NA, data6$school_type)

region_school <- data6 %>%
  filter(!is.na(school_type)) %>%
  group_by(region_name, school_type) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n)) %>%
  mutate(per_general = round(n / sum *100, 2))

region_school <- region_school %>%
  filter(school_type == 1) %>%
  select(region_name, per_general) %>%
  arrange(per_general) %>%
  head(10)

ggplot(data = region_school, aes(x = reorder(region_name, -per_general), y = per_general)) + geom_col(fill='orange') + coord_flip() + ggtitle("소재지별 일반계 고등학교 비율 하위 10개") + xlab("소재지") + ylab("일반계 고등학교 비율")

# 7. 장애등급, 차별에 따른 남의도움이 필요하다고 생각하는 사람의 비율
data7 <- raw_data
data7 <- rename(data7, disability_rating = h15_g9, discrimination= da15_15aq2, helping_status = da15_48)

data7$disability_rating <- ifelse(data7$disability_rating == 0, NA, data7$disability_rating)

data7$helping_status <- ifelse(data7$helping_status == 1 | data7$helping_status == 2, "도움필요없음", ifelse(data7$helping_status == 3 | data7$helping_status == 4 | data7$helping_status == 5, "도움필요함", NA))

data7$discrimination <- ifelse(data7$discrimination == 1, "전혀 없다", ifelse(data7$discrimination == 2, "별로 없다", ifelse(data7$discrimination == 3, "약간 많다", ifelse(data7$discrimination == 4, "매우 많다", NA)))) 

disability_help <- data7 %>%
  filter(!is.na(data7$helping_status)) %>%
  group_by(disability_rating, helping_status, discrimination) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(help_per = (n / tot_group * 100))

disability_help <- disability_help %>%
  filter(helping_status == "도움필요함") %>%
  select(disability_rating, helping_status, help_per, discrimination)

disability_help$disability_rating <- as.factor(disability_help$disability_rating)

ggplot(data = disability_help, aes(x = disability_rating, y = help_per, fill = discrimination)) + geom_col(position = "dodge") + ggtitle("장애등급, 차별에 따른 남의도움이 필요하다고 생각하는 사람의 비율") + xlab("장애등급 (단위: 등급)") + ylab("남의 도움이 필요하다고 답한 장애인의 비율") + labs(fill = "장애인에 대한 차별 정도")
