;; 1. the same as run039
;; 2. Description: Sim from run33 + Effect +2yr +800sub
;; x1. Author: user
;; 3. Label:       IRT model
;; 4. Estimation:  FOCE -2LL 


$SIZE LTH=200
$PROBLEM IRT model for all visit include time effect BL ON SLOPE
;----------------------------------
$INPUT 
ID             ;Patient No
TIME           ;Days after Baseline
ITEM           ;Item indicator 1-33
DV             ;Score of item
EVENT          ;Visit indicator
TIMEM          ;Months after Baseline
DRUGE          ;Drug Effect
DID            ;Indicator for group
EID            ;Indicator for different Effect
;----------------------------------
$DATA PDSIME000.csv IGNORE=@  
;----------------------------------
$PRED
;-----Item parameter selection-----
IF(ITEM.EQ.1) THEN 
DIS=THETA(1)                            ;I1DIS 
DISF1=THETA(2)                           ;I1DISF1 
DISF2=DISF1+THETA(3)                      ;I1DISF2 
DISF3=DISF2+THETA(4)                      ;I1DISF3 
DISF4=DISF3+THETA(5)                      ;I1DISF4 
ENDIF
IF(ITEM.EQ.2) THEN 
DIS=THETA(6)                            ;I2DIS 
DISF1=THETA(7)                           ;I2DISF1 
DISF2=DISF1+THETA(8)                      ;I2DISF2 
DISF3=DISF2+THETA(9)                      ;I2DISF3 
DISF4=DISF3+THETA(10)                     ;I2DISF4 
ENDIF
IF(ITEM.EQ.3) THEN 
DIS=THETA(11)                            ;I3DIS 
DISF1=THETA(12)                           ;I3DISF1 
DISF2=DISF1+THETA(13)                      ;I3DISF2 
DISF3=DISF2+THETA(14)                      ;I3DISF3 
DISF4=DISF3+THETA(15)                      ;I3DISF4 
ENDIF
IF(ITEM.EQ.4) THEN 
DIS=THETA(16)                            ;I4DIS 
DISF1=THETA(17)                           ;I4DISF1 
DISF2=DISF1+THETA(18)                      ;I4DISF2 
DISF3=DISF2+THETA(19)                      ;I4DISF3 
DISF4=DISF3+THETA(20)                      ;I4DISF4 
ENDIF
IF(ITEM.EQ.5) THEN 
DIS=THETA(21)                            ;I5DIS 
DISF1=THETA(22)                           ;I5DISF1 
DISF2=DISF1+THETA(23)                      ;I5DISF2 
DISF3=DISF2+THETA(24)                      ;I5DISF3 
DISF4=DISF3+THETA(25)                      ;I5DISF4 
ENDIF
IF(ITEM.EQ.6) THEN 
DIS=THETA(26)                            ;I6DIS 
DISF1=THETA(27)                           ;I6DISF1 
DISF2=DISF1+THETA(28)                      ;I6DISF2 
DISF3=DISF2+THETA(29)                      ;I6DISF3 
DISF4=DISF3+THETA(30)                      ;I6DISF4 
ENDIF
IF(ITEM.EQ.7) THEN 
DIS=THETA(31)                            ;I7DIS 
DISF1=THETA(32)                           ;I7DISF1 
DISF2=DISF1+THETA(33)                      ;I7DISF2 
DISF3=DISF2+THETA(34)                      ;I7DISF3 
DISF4=DISF3+THETA(35)                      ;I7DISF4 
ENDIF
IF(ITEM.EQ.8) THEN 
DIS=THETA(36)                            ;I8DIS 
DISF1=THETA(37)                           ;I8DISF1 
DISF2=DISF1+THETA(38)                      ;I8DISF2 
DISF3=DISF2+THETA(39)                      ;I8DISF3 
DISF4=DISF3+THETA(40)                      ;I8DISF4 
ENDIF
IF(ITEM.EQ.9) THEN 
DIS=THETA(41)                            ;I9DIS 
DISF1=THETA(42)                           ;I9DISF1 
DISF2=DISF1+THETA(43)                      ;I9DISF2 
DISF3=DISF2+THETA(44)                      ;I9DISF3 
DISF4=DISF3+THETA(45)                      ;I9DISF4 
ENDIF
IF(ITEM.EQ.10) THEN 
DIS=THETA(46)                            ;I10DIS 
DISF1=THETA(47)                           ;I10DISF1 
DISF2=DISF1+THETA(48)                      ;I10DISF2 
DISF3=DISF2+THETA(49)                      ;I10DISF3 
DISF4=DISF3+THETA(50)                      ;I10DISF4 
ENDIF
IF(ITEM.EQ.11) THEN 
DIS=THETA(51)                            ;I11DIS 
DISF1=THETA(52)                           ;I11DISF1 
DISF2=DISF1+THETA(53)                      ;I11DISF2 
DISF3=DISF2+THETA(54)                      ;I11DISF3 
DISF4=DISF3+THETA(55)                      ;I11DISF4 
ENDIF
IF(ITEM.EQ.12) THEN 
DIS=THETA(56)                            ;I12DIS 
DISF1=THETA(57)                           ;I12DISF1 
DISF2=DISF1+THETA(58)                      ;I12DISF2 
DISF3=DISF2+THETA(59)                      ;I12DISF3 
DISF4=DISF3+THETA(60)                      ;I12DISF4 
ENDIF
IF(ITEM.EQ.13) THEN 
DIS=THETA(61)                            ;I13DIS 
DISF1=THETA(62)                           ;I13DISF1 
DISF2=DISF1+THETA(63)                      ;I13DISF2 
DISF3=DISF2+THETA(64)                      ;I13DISF3 
DISF4=DISF3+THETA(65)                      ;I13DISF4 
ENDIF
IF(ITEM.EQ.14) THEN 
DIS=THETA(66)                            ;I14DIS 
DISF1=THETA(67)                           ;I14DISF1 
DISF2=DISF1+THETA(68)                      ;I14DISF2 
DISF3=DISF2+THETA(69)                      ;I14DISF3 
DISF4=DISF3+THETA(70)                      ;I14DISF4 
ENDIF
IF(ITEM.EQ.15) THEN 
DIS=THETA(71)                            ;I15DIS 
DISF1=THETA(72)                           ;I15DISF1 
DISF2=DISF1+THETA(73)                      ;I15DISF2 
DISF3=DISF2+THETA(74)                      ;I15DISF3 
DISF4=DISF3+THETA(75)                      ;I15DISF4 
ENDIF
IF(ITEM.EQ.16) THEN 
DIS=THETA(76)                            ;I16DIS 
DISF1=THETA(77)                           ;I16DISF1 
DISF2=DISF1+THETA(78)                      ;I16DISF2 
DISF3=DISF2+THETA(79)                      ;I16DISF3 
DISF4=DISF3+THETA(80)                      ;I16DISF4 
ENDIF
IF(ITEM.EQ.17) THEN 
DIS=THETA(81)                            ;I17DIS 
DISF1=THETA(82)                           ;I17DISF1 
DISF2=DISF1+THETA(83)                      ;I17DISF2 
DISF3=DISF2+THETA(84)                      ;I17DISF3 
DISF4=DISF3+THETA(85)                      ;I17DISF4 
ENDIF
IF(ITEM.EQ.18) THEN 
DIS=THETA(86)                            ;I18DIS 
DISF1=THETA(87)                           ;I18DISF1 
DISF2=DISF1+THETA(88)                      ;I18DISF2 
DISF3=DISF2+THETA(89)                      ;I18DISF3 
DISF4=DISF3+THETA(90)                      ;I18DISF4 
ENDIF
IF(ITEM.EQ.19) THEN 
DIS=THETA(91)                            ;I19DIS 
DISF1=THETA(92)                           ;I19DISF1 
DISF2=DISF1+THETA(93)                      ;I19DISF2 
DISF3=DISF2+THETA(94)                      ;I19DISF3 
DISF4=DISF3+THETA(95)                      ;I19DISF4 
ENDIF
IF(ITEM.EQ.20) THEN 
DIS=THETA(96)                            ;I20DIS 
DISF1=THETA(97)                           ;I20DISF1 
DISF2=DISF1+THETA(98)                      ;I20DISF2 
DISF3=DISF2+THETA(99)                      ;I20DISF3 
DISF4=DISF3+THETA(100)                      ;I20DISF4 
ENDIF
IF(ITEM.EQ.21) THEN 
DIS=THETA(101)                            ;I21DIS 
DISF1=THETA(102)                           ;I21DISF1 
DISF2=DISF1+THETA(103)                      ;I21DISF2 
DISF3=DISF2+THETA(104)                      ;I21DISF3 
DISF4=DISF3+THETA(105)                      ;I21DISF4 
ENDIF
IF(ITEM.EQ.22) THEN 
DIS=THETA(106)                            ;I22DIS 
DISF1=THETA(107)                           ;I22DISF1 
DISF2=DISF1+THETA(108)                      ;I22DISF2 
DISF3=DISF2+THETA(109)                      ;I22DISF3 
DISF4=DISF3+THETA(110)                      ;I22DISF4 
ENDIF
IF(ITEM.EQ.23) THEN 
DIS=THETA(111)                            ;I23DIS 
DISF1=THETA(112)                           ;I23DISF1 
DISF2=DISF1+THETA(113)                      ;I23DISF2 
DISF3=DISF2+THETA(114)                      ;I23DISF3 
DISF4=DISF3+THETA(115)                      ;I23DISF4 
ENDIF
IF(ITEM.EQ.24) THEN 
DIS=THETA(116)                            ;I24DIS 
DISF1=THETA(117)                           ;I24DISF1 
DISF2=DISF1+THETA(118)                      ;I24DISF2 
DISF3=DISF2+THETA(119)                      ;I24DISF3 
DISF4=DISF3+THETA(120)                      ;I24DISF4 
ENDIF
IF(ITEM.EQ.25) THEN 
DIS=THETA(121)                            ;I25DIS 
DISF1=THETA(122)                           ;I25DISF1 
DISF2=DISF1+THETA(123)                      ;I25DISF2 
DISF3=DISF2+THETA(124)                      ;I25DISF3 
DISF4=DISF3+THETA(125)                      ;I25DISF4 
ENDIF
IF(ITEM.EQ.26) THEN 
DIS=THETA(126)                            ;I26DIS 
DISF1=THETA(127)                           ;I26DISF1 
DISF2=DISF1+THETA(128)                      ;I26DISF2 
DISF3=DISF2+THETA(129)                      ;I26DISF3 
DISF4=DISF3+THETA(130)                      ;I26DISF4 
ENDIF
IF(ITEM.EQ.27) THEN 
DIS=THETA(131)                            ;I27DIS 
DISF1=THETA(132)                           ;I27DISF1 
DISF2=DISF1+THETA(133)                      ;I27DISF2 
DISF3=DISF2+THETA(134)                      ;I27DISF3 
DISF4=DISF3+THETA(135)                      ;I27DISF4 
ENDIF
IF(ITEM.EQ.28) THEN 
DIS=THETA(136)                            ;I28DIS 
DISF1=THETA(137)                           ;I28DISF1 
DISF2=DISF1+THETA(138)                      ;I28DISF2 
DISF3=DISF2+THETA(139)                      ;I28DISF3 
DISF4=DISF3+THETA(140)                      ;I28DISF4 
ENDIF
IF(ITEM.EQ.29) THEN 
DIS=THETA(141)                            ;I29DIS 
DISF1=THETA(142)                           ;I29DISF1 
DISF2=DISF1+THETA(143)                      ;I29DISF2 
DISF3=DISF2+THETA(144)                      ;I29DISF3 
DISF4=DISF3+THETA(145)                      ;I29DISF4 
ENDIF
IF(ITEM.EQ.30) THEN 
DIS=THETA(146)                            ;I30DIS 
DISF1=THETA(147)                           ;I30DISF1 
DISF2=DISF1+THETA(148)                      ;I30DISF2 
DISF3=DISF2+THETA(149)                      ;I30DISF3 
DISF4=DISF3+THETA(150)                      ;I30DISF4 
ENDIF
IF(ITEM.EQ.31) THEN 
DIS=THETA(151)                            ;I31DIS 
DISF1=THETA(152)                           ;I31DISF1 
DISF2=DISF1+THETA(153)                      ;I31DISF2 
DISF3=DISF2+THETA(154)                      ;I31DISF3 
DISF4=DISF3+THETA(155)                      ;I31DISF4 
ENDIF
IF(ITEM.EQ.32) THEN 
DIS=THETA(156)                            ;I32DIS 
DISF1=THETA(157)                           ;I32DISF1 
DISF2=DISF1+THETA(158)                      ;I32DISF2 
DISF3=DISF2+THETA(159)                      ;I32DISF3 
DISF4=DISF3+THETA(160)                      ;I32DISF4 
ENDIF
IF(ITEM.EQ.33) THEN 
DIS=THETA(161)                            ;I33DIS 
DISF1=THETA(162)                           ;I33DISF1 
DISF2=DISF1+THETA(163)                      ;I33DISF2 
DISF3=DISF2+THETA(164)                      ;I33DISF3 
DISF4=DISF3+THETA(165)                      ;I33DISF4 
ENDIF

IF (EVENT .EQ. 1) IOVD=ETA(3)
IF (EVENT .EQ. 2) IOVD=ETA(4)
IF (EVENT .EQ. 3) IOVD=ETA(5)
IF (EVENT .EQ. 4) IOVD=ETA(6)
IF (EVENT .EQ. 5) IOVD=ETA(7)
IF (EVENT .EQ. 6) IOVD=ETA(8)
IF (EVENT .EQ. 7) IOVD=ETA(9)
IF (EVENT .EQ. 8) IOVD=ETA(10)
IF (EVENT .EQ. 9) IOVD=ETA(11)
IF (EVENT .EQ. 10) IOVD=ETA(12)
IF (EVENT .EQ. 11) IOVD=ETA(13)
IF (EVENT .EQ. 12) IOVD=ETA(14)

;-----Hidden variable--------------
DIABL  = ETA(1)
DSLOPE = (THETA(166) + THETA(167)*DIABL)/100+ETA(2)
IF (EVENT .EQ. 0)  DIAB = DIABL
IF (EVENT .NE. 0)  DIAB = DIABL + DSLOPE*(1-DRUGE)*TIME/360 + IOVD

;-----2 parameter logit model------
PL1 = EXP(DIS*DIAB - DISF1)/(1+EXP(DIS*DIAB - DISF1)) ;>=1
PL2 = EXP(DIS*DIAB - DISF2)/(1+EXP(DIS*DIAB - DISF2)) ;>=2
PL3 = EXP(DIS*DIAB - DISF3)/(1+EXP(DIS*DIAB - DISF3)) ;>=3
PL4 = EXP(DIS*DIAB - DISF4)/(1+EXP(DIS*DIAB - DISF4)) ;>=4

P0 = 1 - PL1        ; Probability of Score=0
P1 = PL1 - PL2      ; Probability of Score=1
P2 = PL2 - PL3      ; Probability of Score=2
P3 = PL3 - PL4      ; Probability of Score=3
P4 = PL4            ; Probability of Score=4

IF(DV.EQ.0) P = P0
IF(DV.EQ.1) P = P1
IF(DV.EQ.2) P = P2
IF(DV.EQ.3) P = P3
IF(DV.EQ.4) P = P4

TP = 1E-10
IF(P.LT.TP) P=TP	
IF(P.GT.(1-TP)) P=1-TP

Y=-2*LOG(P)

;Simulation of item-level data
PG0 = P0
PG1 = PG0 + P1
PG2 = PG1 + P2
PG3 = PG2 + P3
IF (ICALL.EQ.4) THEN
   CALL RANDOM (2,R)
   DV=0
    IF(R.GE.PG0) DV=1
    IF(R.GE.PG1) DV=2
    IF(R.GE.PG2) DV=3
    IF(R.GE.PG3) DV=4
ENDIF
;----------------------------------
; fixed value of 50 for items in which there was no observed response
$THETA
(0, 0.758,5) FIX ;1.I1DIS     
(-30, -0.0776,30) FIX ;2.I1DISF1   
(0, 2.75,60) FIX ;3.I1DISF2   
(0, 3.22,90) FIX ;4.I1DISF3    
(25) FIX ;5.I1DIF4      
(0, 1.06,5) FIX ;6.I2DIS     
(-30, -2.18,30) FIX ;7.I2DISF1   
(0, 3.12,60) FIX ;8.I2DISF2   
(0, 2.65,90) FIX ;9.I2DISF3   
(0, 3.25,150) FIX ;10.I2DISF4  
(0, 1.06,5) FIX ;11.I3DIS    
(-30, -0.141,30) FIX ;12.I3DISF1  
(0, 1.65,60) FIX ;13.I3DISF2  
(0, 2.92,90) FIX ;14.I3DISF3  
(0, 2.32,150) FIX ;15.I3DISF4  
(0, 0.188,5) FIX ;16.I4DIS    
(-30, -1.17,30) FIX ;17.I4DISF1  
(0, 1.56,60) FIX ;18.I4DISF2  
(0, 2.47,90) FIX ;19.I4DISF3  
(0, 4.44,150) FIX ;20.I4DISF4  
(0, 1.93,5) FIX ;21.I5DIS    
(-30, -0.562,30) FIX ;22.I5DISF1  
(0, 2.16,60) FIX ;23.I5DISF2  
(0, 3.53,90) FIX ;24.I5DISF3  
(0, 5.79,150) FIX ;25.I5DISF4  
(0, 0.416,5) FIX ;26.I6DIS    
(-30, -0.0165,30) FIX ;27.I6DISF1  
(0, 1.34,60) FIX ;28.I6DISF2  
(0, 2.17,90) FIX ;29.I6DISF3  
(0, 3.26,150) FIX ;30.I6DISF4  
(0, 1.61,5) FIX ;31.I7DIS    
(-30, 0.629,30) FIX ;32.I7DISF1  
(0, 1.61,60) FIX ;33.I7DISF2  
(0, 2.57,90) FIX ;34.I7DISF3  
(0, 4.27,150) FIX ;35.I7DISF4  
(0, 0.303,5) FIX ;36.I8DIS    
(-30, -1.1,30) FIX ;37.I8DISF1  
(0, 1.71,60) FIX ;38.I8DISF2  
(0, 1.76,90) FIX ;39.I8DISF3  
(0, 2.91,150) FIX ;40.I8DISF4  
(0, 3,5) FIX ;41.I9DIS    
(-30, -1.14,30) FIX ;42.I9DISF1  
(0, 3.19,60) FIX ;43.I9DISF2  
(0, 3.08,90) FIX ;44.I9DISF3  
(0, 4.65,150) FIX ;45.I9DISF4  
(0, 0.325,5) FIX ;46.I10DIS   
(-30, -0.642,30) FIX ;47.I10DISF1 
(0, 1.74,60) FIX ;48.I10DISF2 
(0, 1.76,90) FIX ;49.I10DISF3 
(0, 3.54,150) FIX ;50.I10DISF4 
(0, 3.07,5) FIX ;51.I11DIS   
(-30, -0.456,30) FIX ;52.I11DISF1 
(0, 3.25,60) FIX ;53.I11DISF2 
(0, 3.3,90) FIX ;54.I11DISF3 
(0, 4.76,150) FIX ;55.I11DISF4 
(0, 0.161,5) FIX ;56.I12DIS   
(-30, -0.625,30) FIX ;57.I12DISF1 
(0, 1.66,60) FIX ;58.I12DISF2 
(0, 1.79,90) FIX ;59.I12DISF3 
(0, 3.06,150) FIX ;60.I12DISF4 
(0, 2.53,5) FIX ;61.I13DIS   
(-30, -0.249,30) FIX ;62.I13DISF1 
(0, 2.68,60) FIX ;63.I13DISF2 
(0, 2.71,90) FIX ;64.I13DISF3 
(0, 4.2,150) FIX ;65.I13DISF4 
(0, 0.334,5) FIX ;66.I14DIS   
(-30, -0.692,30) FIX ;67.I14DISF1 
(0, 1.77,60) FIX ;68.I14DISF2 
(0, 1.89,90) FIX ;69.I14DISF3 
(0, 2.73,150) FIX ;70.I14DISF4 
(0, 2.19,5) FIX ;71.I15DIS   
(-30, -0.78,30) FIX ;72.I15DISF1 
(0, 2.57,60) FIX ;73.I15DISF2 
(0, 2.68,90) FIX ;74.I15DISF3 
(0, 3.99,150) FIX ;75.I15DISF4 
(0, 0.517,5) FIX ;76.I16DIS   
(-30, 0.153,30) FIX ;77.I16DISF1 
(0, 1.99,60) FIX ;78.I16DISF2 
(0, 1.96,90) FIX ;79.I16DISF3 
(0, 2.72,150) FIX ;80.I16DISF4 
(0, 2.13,5) FIX ;81.I17DIS   
(-30, 0.495,30) FIX ;82.I17DISF1 
(0, 2.54,60) FIX ;83.I17DISF2 
(0, 2.59,90) FIX ;84.I17DISF3 
(0, 3.13,150) FIX ;85.I17DISF4 
(0, 0.915,5) FIX ;86.I18DIS   
(-30, 1.8,30) FIX ;87.I18DISF1 
(0, 1.98,60) FIX ;88.I18DISF2 
(0, 1.43,90) FIX ;89.I18DISF3 
(0, 1.75,150) FIX ;90.I18DISF4 
(0, 0.739,5) FIX ;91.I19DIS   
(-30, -0.628,30) FIX ;92.I19DISF1 
(0, 3.27,60) FIX ;93.I19DISF2 
(0, 2.01,90) FIX ;94.I19DISF3 
(0, 2.24,150) FIX ;95.I19DISF4 
(0, 1.48,5) FIX ;96.I20DIS   
(-30, 5.03,30) FIX ;97.I20DISF1 
(0, 1.57,60) FIX ;98.I20DISF2 
(0, 0.878,90) FIX ;99.I20DISF3 
(0, 0.485,150) FIX ;100.I20DISF4
(0, 0.689,5) FIX ;101.I21DIS  
(-30, 2.24,30) FIX ;102.I21DISF1
(0, 1.06,60) FIX ;103.I21DISF2
(0, 0.652,90) FIX ;104.I21DISF3
(0, 2.17,150) FIX ;105.I21DISF4
(0, 0.77,5) FIX ;106.I22DIS  
(-30, -0.422,30) FIX ;107.I22DISF1
(0, 2.35,60) FIX ;108.I22DISF2
(0, 2.14,90) FIX ;109.I22DISF3
(0, 2.48,150) FIX ;110.I22DISF4
(0, 1.32,5) FIX ;111.I23DIS  
(-30, -2.45,30) FIX ;112.I23DISF1
(0, 2.97,60) FIX ;113.I23DISF2
(0, 2.61,90) FIX ;114.I23DISF3
(0, 5.42,150) FIX ;115.I23DISF4
(0) FIX ;116.I24DIS  
(-30, 0.619,30) FIX ;117.I24DISF1
(0, 1.78,60) FIX ;118.I24DISF2
(0, 1.7,90) FIX ;119.I24DISF3
(0, 3.12,150) FIX ;120.I24DISF4
(0, 0.759,5) FIX ;121.I25DIS  
(-30, 1.05,30) FIX ;122.I25DISF1
(0, 2.14,60) FIX ;123.I25DISF2
(0, 2.3,90) FIX ;124.I25DISF3 
(25) FIX ;125.I25DIF4   
(0, 0.0946,5) FIX ;126.I26DIS  
(-30, 0.77,30) FIX ;127.I26DISF1
(0, 2.26,60) FIX ;128.I26DISF2
(0, 3.01,90) FIX ;129.I26DISF3 
(25) FIX ;130.I26DIF4   
(0, 0.82,5) FIX ;131.I27DIS  
(-30, 0.904,30) FIX ;132.I27DISF1
(0, 2.28,60) FIX ;133.I27DISF2
(0, 3.77,90) FIX ;134.I27DISF3
(0, 1.95,150) FIX ;135.I27DISF4
(0) FIX ;136.I28DIS  
(-30, 0.33,30) FIX ;137.I28DISF1
(0, 0.617,60) FIX ;138.I28DISF2
(0, 1.36,90) FIX ;139.I28DISF3
(0, 4.63,150) FIX ;140.I28DISF4
(0, 0.789,5) FIX ;141.I29DIS  
(-30, 0.883,30) FIX ;142.I29DISF1
(0, 0.933,60) FIX ;143.I29DISF2
(0, 1.87,90) FIX ;144.I29DISF3 
(25) FIX ;145.I29DIF4   
(0) FIX ;146.I30DIS  
(-30, 1.92,30) FIX ;147.I30DISF1
(0, 1.27,60) FIX ;148.I30DISF2
(0, 1.94,90) FIX ;149.I30DISF3
(25) FIX ;150.I30DIF4   
(0, 0.39,5) FIX ;151.I31DIS  
(-30, 2.23,30) FIX ;152.I31DISF1
(0, 1.19,60) FIX ;153.I31DISF2
(0, 2.69,90) FIX ;154.I31DISF3 
(25) FIX ;155.I31DIF4   
(0, 0.345,5) FIX ;156.I32DIS  
(-30, 2.59,30) FIX ;157.I32DISF1
(0, 1.44,60) FIX ;158.I32DISF2
(0, 2.66,90) FIX ;159.I32DISF3
(25) FIX ;160.I32DIF4   
(0, 0.0521,5) FIX ;161.I33DIS  
(-30, -0.742,30) FIX ;162.I33DISF1
(0, 0.972,60) FIX ;163.I33DISF2
(0, 0.766,90) FIX ;164.I33DISF3
(0, 1.26,150) FIX ;165.I33DISF4 
(0, 26.8,50)  ;166.DSLOPE
(-20, -7.55,0);167.BLONSL

$OMEGA
 1 FIX  ;1 Disability BL  
 0.0308 ;2 DSLOPE  
$OMEGA BLOCK(1) 0.151 ;3 IOV V1
$OMEGA BLOCK(1) SAME        ;4 IOV V2
$OMEGA BLOCK(1) SAME        ;5 IOV V3
$OMEGA BLOCK(1) SAME        ;6 IOV V4
$OMEGA BLOCK(1) SAME        ;7 IOV V5
$OMEGA BLOCK(1) SAME        ;8 IOV V6
$OMEGA BLOCK(1) SAME        ;9 IOV V7
$OMEGA BLOCK(1) SAME        ;10 IOV V8
$OMEGA BLOCK(1) SAME        ;11 IOV V9
$OMEGA BLOCK(1) SAME        ;12 IOV V10
$OMEGA BLOCK(1) SAME        ;13 IOV V11
$OMEGA BLOCK(1) SAME        ;14 IOV V12
;----------------------------------

;----------------------------------
$SIMULATION (953751) (1478963 UNIFORM) ONLYSIM NOPRED SUBPROBLEM=100
;----------------------------------
$TABLE ID TIME ITEM DV DIAB DRUGE EID
NOPRINT NOAPPEND ONEHEADER FILE=sdtab000








