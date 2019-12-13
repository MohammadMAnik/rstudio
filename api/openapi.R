#rm(list = ls())

## word cloud 관련 패키지
install.packages("KoNLP")
install.packages("RColorBrewer")
install.packages("wordcloud")

## API 관련 라이브러리 설치
install.packages("RCurl")
install.packages("XML")

## library loading
library(XML)
library(ggmap)

# 검색조건
returnType <- "XML"
pageNum <- "1"
pageSize <- "10"
StDt <- "20190101"
EndDt <- "20190131"
outType <- "1"
sort <- "ASC"
sortCol <- "TR_STT_DT"

## API URL 설정
API_key <- "lyAS5SxzXAW0vlHkRiigT3bsjwTrkUQS"

## API URL 설정
url <- paste("http://hrd.go.kr/jsp/HRDP/HRDPO00/HRDPOA60/HRDPOA60_1.jsp?returnType=XML&authKey=", API_key, "&pageNum=1&pageSize=10&srchTraStDt=20190426&srchTraEndDt=20190726&outType=1&sort=ASC&sortCol=TR_STT_DT")

url <- paste("http://www.hrd.go.kr/jsp/HRDP/HRDPO00/HRDPOA60/HRDPOA60_1.jsp?authKey=", API_key, "&srchTraStDt=", StDt, "&srchTraEndDt=", EndDt, sep="")

#"&pageNum=", 1, "&pageSize=", 20, "&srchTraStDt=", 20141001, "&srchTraEndDt=", 20141231, sep="")

## 웹에서 XML 파일 다운로드
xmefile <- xmlParse(url)

## XML 접근 및 출력
xmlRoot(xmefile)

## XML 노드를 이용하여 데이터 프레임 전환
df <- xmlToDataFrame(getNodeSet(xmefile, "//itemList"))
head(df)


# Naver API
library(XML)
library(RCurl)
