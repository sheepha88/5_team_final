# 치안안전도 예측 경진대회

## 1.사용언어
    -python, R, jupyter notebook


## 2. 목적 및 개요
  -17~19년도 치안관련 데이터를 통해 20년도 치안안전도 예측모델을 개발하고 치안안전활동 방향성 제시
  
## 3. 기술 list
  (1). 전처리
    -결측치 처리
    -shapely.geometry와 geopandas를 이용한 위치데이터 처리
    -one-hot encoding
    
  (2). feture engineering
    -R의 All subset regression
    
  (3). Modeling
    -Machine learing(regression)
    
## 4. 세부내용
 1. 전처리  
    (1) 결측치처리  
    -결측치 삭제 및 결측치 범주형 변수처리

<img src="https://user-images.githubusercontent.com/87842980/131620635-da4732b5-6010-46a8-94cc-487a37f153de.png" width="30%"><img src="https://user-images.githubusercontent.com/87842980/131620637-e9bd5e7b-4c7b-47a5-bb77-7e31abdf090a.png" width="30%"><img src="https://user-images.githubusercontent.com/87842980/131620638-2e59446c-4fcb-4898-94e4-a5d6fa41bb55.png" width="30%">

    (2)위치데이터 처리
    -shapely.geometry와 geopandas를 이용한 위치데이터 처리
![그림5](https://user-images.githubusercontent.com/87842980/131621758-64e04355-e1e0-4e23-b191-decd03fe759b.png)


    (3)
