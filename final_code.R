# 패키지 불러오기
library(tidyverse)
library(readxl)
library(writexl)
library(utf8)
rm(list=ls())

# 코퍼스 불러오기,  필요없는 행 없애기
sejong <- read_excel("/Users/oni/R/final_term/10.1a(명사).xlsx", col_names =  F)
colnames(sejong) <- c("list", "name", "mean", "type", "freq", "percent", "cum")
sejong_corpus <- sejong[-c(1:5), ]

# 명사(NNG) 추출 
sejong_NNG <- sejong_corpus %>% 
  filter(sejong_corpus$type == "NNG") %>%
  mutate(NNG.2 = str_replace_all(name, "\\__[0-9]{1,3}", "")) %>%
  mutate(syllable = str_length(NNG.2))

# 음절 분리: 2음절 단어를 대상으로 할 것이기 때문에 sy1, sy2만 추출
sejong_NNG2 <- sejong_NNG %>%
  mutate(syl1 = str_sub(NNG.2, 1, 1),
         syl2 = str_sub(NNG.2, 2, 2))

# 2음절 단어 filter
sejong_NNG3 <- sejong_NNG2 %>%
  filter(str_length(NNG.2) == 2)

# 음절 별 자소 분리
initial <- c('ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ', 
             'ㅆ', '@', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ') # 모음으로 시작할 경우 '@'로 대체.
midial <- c('ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ', 'ㅙ', 
            'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ')
final <- c('', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ', 
           'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ', 'ㅆ', 
           'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ')

jaso.decom <- function(syl_vector){
  jaso.decom.letter <- function(letter){
    if (str_detect(letter, "[가-힣]") == T) {
      a <-  utf8ToInt(utf8_encode(letter))
      Chr_ord <- a - 44032  # ord('가') = 44032
      ini_index <- Chr_ord %/% (21*28) # 중성 21개, 종성 28
      mid_index <- (Chr_ord %/% 28) %% 21 # %/% = 몫
      final_index <- Chr_ord %% 28 # %% = 나머지
      b <- c(c(initial[ini_index+1], midial[mid_index+1], final[final_index+1]))
    } else
      b <- c(letter) # 투입되는 입력이 한글이 아닌 경우 분해하지 말 것을 명령.
  }
  c <- map(syl_vector, jaso.decom.letter)  
}


syl1_split <- map(map(sejong_NNG3$syl1, jaso.decom), unlist) %>%
  map(function(x) str_c(x, collapse = "")) %>%
  map_dfr(as_tibble) %>%
  rename(syl1_split = value)

syl2_split <- map(map(sejong_NNG3$syl2, jaso.decom), unlist) %>%
  map(function(x) str_c(x, collapse = "")) %>%
  map_dfr(as_tibble) %>%
  rename(syl2_split = value)


# 첫 음절을 CVC 구조로 한정
sejong_NNG4 <- sejong_NNG3 %>%
  mutate(syl1_split)  %>%
  mutate(syl2_split) %>%
  mutate(leng = str_length(syl1_split)) %>%
  filter(leng == 3)

sejong_NNG5 <- sejong_NNG4 %>%
  select(c("NNG.2", "type", "freq", "percent", "cum", "syllable",
           "syl1", "syl2", "syl1_split","syl2_split", "leng"))


# 실험 1: 음운 변동 조건
# 음운 변동 조건 합치기 
# 음운 변동 조건 (1) : set_b: 비음화 조건 
set_b <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[ㄱ|ㄷ|ㅂ]$")) 

set_b <- set_b %>%
  filter(str_detect(set_b$syl2_split, "^[ㄴ|ㅁ]"))

# 음운 변동 조건 (2) : set_c: 끝소리 규칙
set_c <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[^ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅇ]$" ))


# 음운 변동 조건 (3) : set_d: 유음화
set_d_1 <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[ㄴ]$") & str_detect(sejong_NNG5$syl2_split, "^ㄹ")) 

set_d_2 <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[ㄹ]$") & str_detect(sejong_NNG5$syl2_split, "^ㄴ"))

# 음운 변동 조건 (4) : set_e: 구개음화
set_e <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[ㄷ|ㅌ]$") & str_detect(sejong_NNG5$syl2_split, "^@ㅣ$")) 

# 음운 변동 조건 합치기 
exp1_set <- rbind(set_b, set_c, set_d_1, set_d_2, set_e) %>%
  unique()

# 첫 음절 토큰 빈도 구하기 
as <- as_tibble(aggregate(freq ~ syl1,sejong_NNG2, sum))
exp1_syl1_freq2 <- as %>%
  filter(syl1 %in% exp1_set$syl1) %>%
  arrange(desc(freq))
View(exp1_syl1_freq2)

# 실험 1(음운변동 o & 고빈도  조건) : 첫음절, 두번째 음절 컬럼 추가 후 엑셀 저장
# 토큰빈도 기준으로 고빈도와 저빈도로 나누기
# 선행연구 기준 고빈도 log5, 저빈도 log3
exp1_syl1_highfreq <- exp1_syl1_freq2[1:10, ]
exp1_syl1_lowfreq <- exp1_syl1_freq2[145:196, ]


exp1_highfreq_total1 <- exp1_set %>%
  filter(syl1 %in% exp1_syl1_highfreq$syl1)
# 첫 번째 음절 컬럼 추가
exp1_highfreq_total2 <- exp1_highfreq_total1 %>%
  full_join(exp1_syl1_highfreq, by = "syl1") %>%
  rename("syl1_freq" = "freq.y")

# 두 번째 음절 컬럼 추가 
syl2_freq <- as_tibble(aggregate(freq ~ syl2,sejong_NNG2, sum))

exp1_high_set_syl2 <- syl2_freq %>%
  filter(syl2 %in% exp1_highfreq_total2$syl2) %>%
  arrange(desc(freq)) 

# 실험 1 음운변동 & 고빈도 조건 빈도 합 
exp1_highfreq_freqtotal <-exp1_highfreq_total2 %>%
  full_join(exp1_high_set_syl2, by ="syl2") %>%
  rename("syl2_freq" = "freq") %>%
  rename("freq" = "freq.x")
write_xlsx(exp1_highfreq_freqtotal, path ="exp1_highfreq_freqtotal.xlsx")

# 실험 1(음운변동 o & 저빈도  조건) : 첫음절, 두번째 음절 컬럼 추가 후 엑셀 저장
# 첫 번째 음절 컬럼 추가
exp1_lowfreq_total <- exp1_set %>%
  filter(syl1 %in% exp1_syl1_lowfreq$syl1) 

exp1_lowfreq_total2 <- exp1_lowfreq_total %>%
  full_join(exp1_syl1_lowfreq, by = "syl1") %>%
  rename("syl1_freq" = "freq.y")

# 두 번째 음절 컬럼 추가 
syl2_freq <- as_tibble(aggregate(freq ~ syl2,sejong_NNG2, sum))
exp1_low_set_syl2 <- syl2_freq %>%
  filter(syl2 %in% exp1_lowfreq_total2$syl2) %>%
  arrange(desc(freq)) 

# 실험 1 음운변동 & 저빈도 조건 빈도 합 
exp1_lowfreq_freqtotal <-exp1_lowfreq_total2 %>%
  full_join(exp1_low_set_syl2, by ="syl2") %>%
  rename("syl2_freq" = "freq") %>%
  rename("freq" = "freq.x")
write_xlsx(exp1_lowfreq_freqtotal, path ="exp1_lowfreq_freqtotal.xlsx")

# 실험 2: 음운 변동이 없는 조건
# 첫음절이 cvc 구성된 것 중 끝소리규칙이 적용되지 않는 단어
set_a <- sejong_NNG5 %>%
  filter(str_detect(sejong_NNG5$syl1_split, "[ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅇ]$" )) 

# set_a 에서 set_b를 빼야 함(set_b를 먼저 실행해야 함! )
exp2_set <- set_a %>%
  anti_join(set_b, by = "NNG.2")
View(exp2_set)

# 첫 음절 토큰 빈도 구하기 
as <- as_tibble(aggregate(freq ~ syl1,sejong_NNG2, sum))

exp2_syl1_freq2 <- as %>%
  filter(syl1 %in% exp2_set$syl1) %>%
  arrange(desc(freq)) 

# 토큰빈도 기준으로 고빈도와 저빈도로 나누기
# 선행연구 기준 고빈도 log5, 저빈도 log3
View(exp2_syl1_freq2)
exp2_syl1_highfreq <- exp2_syl1_freq2[1:16, ]
exp2_syl1_lowfreq <- exp2_syl1_freq2[246:335, ]

# 실험 2(음운변동 x & 고빈도) : 첫음절, 두번째 음절 컬럼 추가 후 엑셀 저장
# 첫 번째 음절 컬럼 추가
exp2_highfreq_total <- exp2_set %>%
  filter(syl1 %in% exp2_syl1_highfreq$syl1)

exp2_highfreq_total2 <- exp2_highfreq_total %>%
  full_join(exp2_syl1_highfreq, by = "syl1") %>%
  rename("syl1_freq" = "freq.y")
View(exp2_highfreq_total2)

# 두 번째 음절 컬럼 추가 후 엑셀 저장 
syl2_freq <- as_tibble(aggregate(freq ~ syl2,sejong_NNG2, sum))
exp2_high_set_syl2 <- syl2_freq %>%
  filter(syl2 %in% exp2_highfreq_total2$syl2) %>%
  arrange(desc(freq)) 
exp2_highfreq_freqtotal <-exp2_highfreq_total2 %>%
  full_join(exp2_high_set_syl2, by ="syl2") %>%
  rename("syl2_freq" = "freq") %>%
  rename("freq" = "freq.x")
write_xlsx(exp2_highfreq_freqtotal, path ="exp2_highfreq_freqtotal.xlsx")

# 실험2(음운변동 x & 저빈도) : 첫음절, 두번째 음절 컬럼 추가 후 엑셀 저장
# 첫 번째 음절 컬럼 추가
exp2_lowfreq_total <- exp2_set %>%
  filter(syl1 %in% exp2_syl1_lowfreq$syl1)

exp2_lowfreq_total2 <- exp2_lowfreq_total %>%
  full_join(exp2_syl1_lowfreq, by = "syl1") %>%
  rename("syl1_freq" = "freq.y")

# 두 번째 음절 컬럼 추가 
syl2_freq <- as_tibble(aggregate(freq ~ syl2,sejong_NNG2, sum))

exp2_low_set_syl2 <- syl2_freq %>%
  filter(syl2 %in% exp2_lowfreq_total2$syl2) %>%
  arrange(desc(freq)) 

# 실험 2 음운변동x & 저빈도 조건 빈도 합 
exp2_lowfreq_freqtotal <-exp2_lowfreq_total2 %>%
  full_join(exp2_low_set_syl2, by ="syl2") %>%
  rename("syl2_freq" = "freq") %>%
  rename("freq" = "freq.x")
write_xlsx(exp2_lowfreq_freqtotal, path ="exp2_lowfreq_freqtotal.xlsx")


