EESchema Schematic File Version 2
LIBS:power_flag
LIBS:conn-2x5
LIBS:jtag
LIBS:thermistor
LIBS:max9938
LIBS:conn-4
LIBS:ferrite
LIBS:conn-2
LIBS:ant
LIBS:nmos
LIBS:vin
LIBS:tactile-4
LIBS:xtal
LIBS:r4
LIBS:r
LIBS:mic5205
LIBS:led
LIBS:l
LIBS:gnd
LIBS:d
LIBS:cp
LIBS:c
LIBS:+5v
LIBS:+3v3
LIBS:+1v8
LIBS:+1v2
LIBS:2450BM14E0003
LIBS:nrf51822
LIBS:xo2-256
LIBS:pulsar-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 5
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L nrf51822 U4
U 1 1 54134283
P 8350 1200
F 0 "U4" H 8350 1350 50  0000 L CNN
F 1 "nrf51822" H 8350 1250 50  0000 L CNN
F 2 "pulsar:nrf51822-qfn48" H 8700 -700 50  0001 C CNN
F 3 "DOCUMENTATION" H 8700 -600 50  0001 C CNN
	1    8350 1200
	1    0    0    -1  
$EndComp
$Comp
L C C22
U 1 1 54134389
P 9350 1950
F 0 "C22" H 9375 1975 30  0000 L CNN
F 1 "47nF" H 9375 1925 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 9250 2000 60  0001 C CNN
F 3 "" H 9250 2000 60  0000 C CNN
	1    9350 1950
	-1   0    0    -1  
$EndComp
$Comp
L C C24
U 1 1 541343DA
P 9450 1950
F 0 "C24" H 9475 1975 30  0000 L CNN
F 1 "100nF" H 9475 1925 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 9350 2000 60  0001 C CNN
F 3 "" H 9350 2000 60  0000 C CNN
	1    9450 1950
	1    0    0    -1  
$EndComp
$Comp
L XTAL-4 Y2
U 1 1 54134AE5
P 7950 2500
F 0 "Y2" H 8050 2525 40  0000 C CNN
F 1 "16MHz" H 8075 2450 40  0000 C CNN
F 2 "osc2520:OSC2520" H 7950 2500 60  0001 C CNN
F 3 "" H 7950 2500 60  0000 C CNN
	1    7950 2500
	1    0    0    -1  
$EndComp
$Comp
L C C20
U 1 1 54134E31
P 7800 2600
F 0 "C20" H 7825 2625 30  0000 L CNN
F 1 "12pF" H 7825 2575 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 7700 2650 60  0001 C CNN
F 3 "" H 7700 2650 60  0000 C CNN
	1    7800 2600
	0    -1   -1   0   
$EndComp
$Comp
L C C19
U 1 1 54134EAF
P 7800 2400
F 0 "C19" H 7825 2425 30  0000 L CNN
F 1 "12pF" H 7825 2375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 7700 2450 60  0001 C CNN
F 3 "" H 7700 2450 60  0000 C CNN
	1    7800 2400
	0    -1   -1   0   
$EndComp
$Comp
L C C23
U 1 1 5413709A
P 9400 2700
F 0 "C23" H 9425 2725 30  0000 L CNN
F 1 "2.2nF" H 9425 2675 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 9300 2750 60  0001 C CNN
F 3 "" H 9300 2750 60  0000 C CNN
	1    9400 2700
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR01
U 1 1 54137131
P 9400 2800
F 0 "#PWR01" V 9520 2800 60  0001 C CNN
F 1 "DGND" H 9400 2700 40  0000 C CNN
F 2 "" H 9400 2750 60  0000 C CNN
F 3 "" H 9400 2750 60  0000 C CNN
	1    9400 2800
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR02
U 1 1 54137FFF
P 10500 2750
F 0 "#PWR02" V 10620 2750 60  0001 C CNN
F 1 "DGND" H 10500 2650 40  0000 C CNN
F 2 "" H 10500 2700 60  0000 C CNN
F 3 "" H 10500 2700 60  0000 C CNN
	1    10500 2750
	1    0    0    -1  
$EndComp
$Comp
L C C18
U 1 1 5413A1E6
P 7800 1400
F 0 "C18" H 7825 1425 30  0000 L CNN
F 1 "100nF" H 7825 1375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 7700 1450 60  0001 C CNN
F 3 "" H 7700 1450 60  0000 C CNN
	1    7800 1400
	-1   0    0    -1  
$EndComp
$Comp
L C C25
U 1 1 5413B494
P 9700 1550
F 0 "C25" H 9725 1575 30  0000 L CNN
F 1 "1nF" H 9725 1525 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 9600 1600 60  0001 C CNN
F 3 "" H 9600 1600 60  0000 C CNN
	1    9700 1550
	1    0    0    -1  
$EndComp
$Comp
L C C21
U 1 1 54143341
P 7900 1400
F 0 "C21" H 7925 1425 30  0000 L CNN
F 1 "100nF" H 7925 1375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 7800 1450 60  0001 C CNN
F 3 "" H 7800 1450 60  0000 C CNN
	1    7900 1400
	1    0    0    -1  
$EndComp
$Comp
L nRF-BALUN B1
U 1 1 541B24EE
P 9950 2450
F 0 "B1" H 9650 2750 50  0000 L CNN
F 1 "nRF-BALUN" H 9650 2700 50  0000 L CNN
F 2 "" H 9950 2200 50  0001 C CNN
F 3 "DOCUMENTATION" H 9950 2100 50  0001 C CNN
	1    9950 2450
	1    0    0    -1  
$EndComp
$Comp
L nrf51822 U4
U 2 1 541B5100
P 4700 900
F 0 "U4" H 4700 1050 50  0000 L CNN
F 1 "nrf51822" H 4700 950 50  0000 L CNN
F 2 "pulsar:nrf51822-qfn48" H 5050 -1000 50  0001 C CNN
F 3 "DOCUMENTATION" H 5050 -900 50  0001 C CNN
	2    4700 900 
	1    0    0    -1  
$EndComp
Text Label 10750 2350 2    60   ~ 0
ANT
$Comp
L +3V3 #PWR03
U 1 1 541B55C5
P 7700 1200
F 0 "#PWR03" H 7710 1200 20  0001 C CNN
F 1 "+3V3" H 7700 1260 40  0000 C CNN
F 2 "" H 7700 1200 60  0000 C CNN
F 3 "" H 7700 1200 60  0000 C CNN
	1    7700 1200
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR04
U 1 1 541B5693
P 9800 1350
F 0 "#PWR04" H 9810 1350 20  0001 C CNN
F 1 "+3V3" H 9800 1410 40  0000 C CNN
F 2 "" H 9800 1350 60  0000 C CNN
F 3 "" H 9800 1350 60  0000 C CNN
	1    9800 1350
	1    0    0    -1  
$EndComp
NoConn ~ 9200 1300
$Comp
L DGND #PWR05
U 1 1 541B5E26
P 7650 2600
F 0 "#PWR05" V 7770 2600 60  0001 C CNN
F 1 "DGND" H 7650 2500 40  0000 C CNN
F 2 "" H 7650 2550 60  0000 C CNN
F 3 "" H 7650 2550 60  0000 C CNN
	1    7650 2600
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR06
U 1 1 541B6302
P 9700 1600
F 0 "#PWR06" V 9820 1600 60  0001 C CNN
F 1 "DGND" H 9700 1500 40  0000 C CNN
F 2 "" H 9700 1550 60  0000 C CNN
F 3 "" H 9700 1550 60  0000 C CNN
	1    9700 1600
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR07
U 1 1 541B6968
P 9400 2050
F 0 "#PWR07" V 9520 2050 60  0001 C CNN
F 1 "DGND" H 9400 1950 40  0000 C CNN
F 2 "" H 9400 2000 60  0000 C CNN
F 3 "" H 9400 2000 60  0000 C CNN
	1    9400 2050
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR08
U 1 1 541B72A5
P 8100 1900
F 0 "#PWR08" V 8220 1900 60  0001 C CNN
F 1 "DGND" H 8100 1800 40  0000 C CNN
F 2 "" H 8100 1850 60  0000 C CNN
F 3 "" H 8100 1850 60  0000 C CNN
	1    8100 1900
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR09
U 1 1 541B750A
P 7850 1500
F 0 "#PWR09" V 7970 1500 60  0001 C CNN
F 1 "DGND" H 7850 1400 40  0000 C CNN
F 2 "" H 7850 1450 60  0000 C CNN
F 3 "" H 7850 1450 60  0000 C CNN
	1    7850 1500
	1    0    0    -1  
$EndComp
$Comp
L XO2-256 U2
U 1 1 541B634E
P 1700 5500
F 0 "U2" H 1750 5550 50  0000 L CNN
F 1 "XO2-256" H 3600 5550 50  0000 R CNN
F 2 "pulsar:xo2-256-qfn32" H 2000 3800 50  0001 C CNN
F 3 "DOCUMENTATION" H 2000 3800 50  0001 C CNN
	1    1700 5500
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR010
U 1 1 541B6444
P 2700 7350
F 0 "#PWR010" V 2820 7350 60  0001 C CNN
F 1 "DGND" H 2700 7250 40  0000 C CNN
F 2 "" H 2700 7300 60  0000 C CNN
F 3 "" H 2700 7300 60  0000 C CNN
	1    2700 7350
	1    0    0    -1  
$EndComp
$Comp
L MIC5205 U3
U 1 1 5427FBF9
P 1750 2250
F 0 "U3" H 1500 2650 60  0000 L CNN
F 1 "MIC5205" H 1500 2550 60  0000 L CNN
F 2 "sot-23-5:SOT-23-5" H 1750 2250 60  0001 C CNN
F 3 "" H 1750 2250 60  0000 C CNN
	1    1750 2250
	1    0    0    -1  
$EndComp
$Comp
L TACTILE-4 S2
U 1 1 54282DCD
P 4550 4250
F 0 "S2" H 4425 4175 40  0000 L CNN
F 1 "TACTILE-4" H 4550 4295 60  0001 C CNN
F 2 "" H 4550 4250 60  0001 C CNN
F 3 "" H 4550 4250 60  0000 C CNN
	1    4550 4250
	-1   0    0    -1  
$EndComp
$Comp
L C C1
U 1 1 542AC103
P 1100 2200
F 0 "C1" H 1125 2225 30  0000 L CNN
F 1 "1u" H 1125 2175 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1000 2250 60  0001 C CNN
F 3 "" H 1000 2250 60  0000 C CNN
	1    1100 2200
	1    0    0    -1  
$EndComp
$Comp
L C C6
U 1 1 542AC208
P 2250 2200
F 0 "C6" H 2275 2225 30  0000 L CNN
F 1 "2.2u" H 2275 2175 30  0000 L CNN
F 2 "SMD-1608:SMD-1608" H 2150 2250 60  0001 C CNN
F 3 "" H 2150 2250 60  0000 C CNN
	1    2250 2200
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR011
U 1 1 542AC5B5
P 1100 2300
F 0 "#PWR011" V 1220 2300 60  0001 C CNN
F 1 "DGND" H 1100 2200 40  0000 C CNN
F 2 "" H 1100 2250 60  0000 C CNN
F 3 "" H 1100 2250 60  0000 C CNN
	1    1100 2300
	1    0    0    -1  
$EndComp
NoConn ~ 2100 2200
$Comp
L DGND #PWR012
U 1 1 542AC775
P 2250 2300
F 0 "#PWR012" V 2370 2300 60  0001 C CNN
F 1 "DGND" H 2250 2200 40  0000 C CNN
F 2 "" H 2250 2250 60  0000 C CNN
F 3 "" H 2250 2250 60  0000 C CNN
	1    2250 2300
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR013
U 1 1 542AC7FE
P 1750 2450
F 0 "#PWR013" V 1870 2450 60  0001 C CNN
F 1 "DGND" H 1750 2350 40  0000 C CNN
F 2 "" H 1750 2400 60  0000 C CNN
F 3 "" H 1750 2400 60  0000 C CNN
	1    1750 2450
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR014
U 1 1 542AC997
P 2450 2050
F 0 "#PWR014" H 2460 2050 20  0001 C CNN
F 1 "+3V3" H 2450 2110 40  0000 C CNN
F 2 "" H 2450 2050 60  0000 C CNN
F 3 "" H 2450 2050 60  0000 C CNN
	1    2450 2050
	1    0    0    -1  
$EndComp
$Comp
L VIN #PWR015
U 1 1 542ACA8B
P 650 2050
F 0 "#PWR015" H 660 2050 20  0001 C CNN
F 1 "VIN" H 650 2110 40  0000 C CNN
F 2 "" H 650 2050 60  0000 C CNN
F 3 "" H 650 2050 60  0000 C CNN
	1    650  2050
	1    0    0    -1  
$EndComp
$Comp
L ANT AE1
U 1 1 542AD70F
P 10900 2150
F 0 "AE1" H 11000 2100 60  0000 L CNN
F 1 "ANT" H 11000 2000 60  0001 L CNN
F 2 "" H 10450 2200 60  0001 C CNN
F 3 "" H 10450 2200 60  0000 C CNN
	1    10900 2150
	1    0    0    -1  
$EndComp
$Comp
L CONN-2 J1
U 1 1 542AE2CF
P 1050 800
F 0 "J1" H 1300 850 40  0000 L CNN
F 1 "POWER" H 1300 750 40  0000 L CNN
F 2 "" H 1150 600 60  0001 C CNN
F 3 "" H 1150 600 60  0000 C CNN
	1    1050 800 
	-1   0    0    -1  
$EndComp
$Comp
L VIN #PWR016
U 1 1 542AE7EA
P 2350 700
F 0 "#PWR016" H 2360 700 20  0001 C CNN
F 1 "VIN" H 2350 760 40  0000 C CNN
F 2 "" H 2350 700 60  0000 C CNN
F 3 "" H 2350 700 60  0000 C CNN
	1    2350 700 
	1    0    0    -1  
$EndComp
$Comp
L FERRITE FB1
U 1 1 542AEA61
P 850 2100
F 0 "FB1" H 775 2025 40  0000 L CNN
F 1 "FERRITE" H 850 2200 60  0001 C CNN
F 2 "SMD-1005:SMD-1005" H 850 2000 60  0001 C CNN
F 3 "" H 850 2000 60  0000 C CNN
	1    850  2100
	1    0    0    -1  
$EndComp
$Sheet
S 5750 5500 550  300 
U 542B257F
F0 "rgb1" 40
F1 "rgb-nmos.sch" 40
F2 "CH1" I L 5750 5550 50 
F3 "CH2" I L 5750 5650 50 
F4 "CH3" I L 5750 5750 50 
$EndSheet
$Sheet
S 5750 6000 550  300 
U 542BC4ED
F0 "rgb2" 40
F1 "rgb-nmos.sch" 40
F2 "CH1" I L 5750 6050 50 
F3 "CH2" I L 5750 6150 50 
F4 "CH3" I L 5750 6250 50 
$EndSheet
$Sheet
S 5750 6500 550  300 
U 542BC4EF
F0 "rgb3" 40
F1 "rgb-nmos.sch" 40
F2 "CH1" I L 5750 6550 50 
F3 "CH2" I L 5750 6650 50 
F4 "CH3" I L 5750 6750 50 
$EndSheet
$Sheet
S 5750 7000 550  300 
U 542BC4F0
F0 "rgb4" 40
F1 "rgb-nmos.sch" 40
F2 "CH1" I L 5750 7050 50 
F3 "CH2" I L 5750 7150 50 
F4 "CH3" I L 5750 7250 50 
$EndSheet
$Comp
L +3V3 #PWR017
U 1 1 542C089D
P 4350 5250
F 0 "#PWR017" H 4360 5250 20  0001 C CNN
F 1 "+3V3" H 4350 5310 40  0000 C CNN
F 2 "" H 4350 5250 60  0000 C CNN
F 3 "" H 4350 5250 60  0000 C CNN
	1    4350 5250
	1    0    0    -1  
$EndComp
$Comp
L C C12
U 1 1 542C0E3C
P 4050 5400
F 0 "C12" H 4075 5425 30  0000 L CNN
F 1 "C" H 4075 5375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 3950 5450 60  0001 C CNN
F 3 "" H 3950 5450 60  0000 C CNN
	1    4050 5400
	1    0    0    -1  
$EndComp
$Comp
L C C14
U 1 1 542C1604
P 4200 5400
F 0 "C14" H 4225 5425 30  0000 L CNN
F 1 "C" H 4225 5375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 4100 5450 60  0001 C CNN
F 3 "" H 4100 5450 60  0000 C CNN
	1    4200 5400
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR018
U 1 1 542C160A
P 4350 5500
F 0 "#PWR018" V 4470 5500 60  0001 C CNN
F 1 "DGND" H 4350 5400 40  0000 C CNN
F 2 "" H 4350 5450 60  0000 C CNN
F 3 "" H 4350 5450 60  0000 C CNN
	1    4350 5500
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR019
U 1 1 542C2BB7
P 3200 4900
F 0 "#PWR019" H 3210 4900 20  0001 C CNN
F 1 "+3V3" H 3200 4960 40  0000 C CNN
F 2 "" H 3200 4900 60  0000 C CNN
F 3 "" H 3200 4900 60  0000 C CNN
	1    3200 4900
	1    0    0    -1  
$EndComp
$Comp
L C C7
U 1 1 542C2BBD
P 2900 5050
F 0 "C7" H 2925 5075 30  0000 L CNN
F 1 "C" H 2925 5025 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 2800 5100 60  0001 C CNN
F 3 "" H 2800 5100 60  0000 C CNN
	1    2900 5050
	1    0    0    -1  
$EndComp
$Comp
L C C8
U 1 1 542C2BC3
P 3050 5050
F 0 "C8" H 3075 5075 30  0000 L CNN
F 1 "C" H 3075 5025 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 2950 5100 60  0001 C CNN
F 3 "" H 2950 5100 60  0000 C CNN
	1    3050 5050
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR020
U 1 1 542C2BC9
P 3200 5150
F 0 "#PWR020" V 3320 5150 60  0001 C CNN
F 1 "DGND" H 3200 5050 40  0000 C CNN
F 2 "" H 3200 5100 60  0000 C CNN
F 3 "" H 3200 5100 60  0000 C CNN
	1    3200 5150
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR021
U 1 1 542C399C
P 1000 5250
F 0 "#PWR021" H 1010 5250 20  0001 C CNN
F 1 "+3V3" H 1000 5310 40  0000 C CNN
F 2 "" H 1000 5250 60  0000 C CNN
F 3 "" H 1000 5250 60  0000 C CNN
	1    1000 5250
	-1   0    0    -1  
$EndComp
$Comp
L C C4
U 1 1 542C39A2
P 1300 5400
F 0 "C4" H 1325 5425 30  0000 L CNN
F 1 "C" H 1325 5375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1200 5450 60  0001 C CNN
F 3 "" H 1200 5450 60  0000 C CNN
	1    1300 5400
	-1   0    0    -1  
$EndComp
$Comp
L C C2
U 1 1 542C39A8
P 1150 5400
F 0 "C2" H 1175 5425 30  0000 L CNN
F 1 "C" H 1175 5375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1050 5450 60  0001 C CNN
F 3 "" H 1050 5450 60  0000 C CNN
	1    1150 5400
	-1   0    0    -1  
$EndComp
$Comp
L DGND #PWR022
U 1 1 542C39AE
P 1000 5500
F 0 "#PWR022" V 1120 5500 60  0001 C CNN
F 1 "DGND" H 1000 5400 40  0000 C CNN
F 2 "" H 1000 5450 60  0000 C CNN
F 3 "" H 1000 5450 60  0000 C CNN
	1    1000 5500
	-1   0    0    -1  
$EndComp
Text Label 4350 5900 2    50   ~ 0
SPI_CLK
Text Label 4350 6000 2    50   ~ 0
SPI_MISO
Text Label 4350 6600 2    50   ~ 0
SPI_MOSI
Text Label 4350 6500 2    50   ~ 0
SPI_CS
Text Label 800  6100 0    50   ~ 0
~XO2_JTAG_EN
Text Label 800  6400 0    50   ~ 0
XO2_JTAG_TMS
Text Label 800  6500 0    50   ~ 0
XO2_JTAG_TCK
Text Label 800  6600 0    50   ~ 0
XO2_JTAG_TDI
Text Label 800  5800 0    50   ~ 0
XO2_JTAG_TDO
Text Label 4200 6800 2    50   ~ 0
CH1
Text Label 4200 6900 2    50   ~ 0
CH2
Text Label 1150 5900 0    50   ~ 0
CH10
Text Label 1150 6200 0    50   ~ 0
CH11
Text Label 800  6000 0    50   ~ 0
~XO2_RESET
Text Label 4200 5800 2    50   ~ 0
CH3
Text Label 4200 6100 2    50   ~ 0
CH4
Text Label 4200 6200 2    50   ~ 0
CH5
Text Label 4200 6300 2    50   ~ 0
CH6
Text Label 4200 6400 2    50   ~ 0
CH7
Text Label 1150 6800 0    50   ~ 0
CH8
Text Label 1150 6900 0    50   ~ 0
CH9
Text Label 1150 6300 0    50   ~ 0
CH12
Text Label 5300 5550 0    50   ~ 0
CH1
Text Label 5300 5650 0    50   ~ 0
CH2
Text Label 5300 5750 0    50   ~ 0
CH3
Text Label 5300 6050 0    50   ~ 0
CH4
Text Label 5300 6150 0    50   ~ 0
CH5
Text Label 5300 6250 0    50   ~ 0
CH6
Text Label 5300 6550 0    50   ~ 0
CH7
Text Label 5300 6650 0    50   ~ 0
CH8
Text Label 5300 6750 0    50   ~ 0
CH9
Text Label 5300 7050 0    50   ~ 0
CH10
Text Label 5300 7150 0    50   ~ 0
CH11
Text Label 5300 7250 0    50   ~ 0
CH12
$Comp
L +3V3 #PWR023
U 1 1 542F2D0A
P 4350 7150
F 0 "#PWR023" H 4360 7150 20  0001 C CNN
F 1 "+3V3" H 4350 7210 40  0000 C CNN
F 2 "" H 4350 7150 60  0000 C CNN
F 3 "" H 4350 7150 60  0000 C CNN
	1    4350 7150
	1    0    0    -1  
$EndComp
$Comp
L C C13
U 1 1 542F2D10
P 4050 7300
F 0 "C13" H 4075 7325 30  0000 L CNN
F 1 "C" H 4075 7275 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 3950 7350 60  0001 C CNN
F 3 "" H 3950 7350 60  0000 C CNN
	1    4050 7300
	1    0    0    -1  
$EndComp
$Comp
L C C15
U 1 1 542F2D16
P 4200 7300
F 0 "C15" H 4225 7325 30  0000 L CNN
F 1 "C" H 4225 7275 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 4100 7350 60  0001 C CNN
F 3 "" H 4100 7350 60  0000 C CNN
	1    4200 7300
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR024
U 1 1 542F2D1C
P 4350 7400
F 0 "#PWR024" V 4470 7400 60  0001 C CNN
F 1 "DGND" H 4350 7300 40  0000 C CNN
F 2 "" H 4350 7350 60  0000 C CNN
F 3 "" H 4350 7350 60  0000 C CNN
	1    4350 7400
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR025
U 1 1 542F3150
P 1000 7150
F 0 "#PWR025" H 1010 7150 20  0001 C CNN
F 1 "+3V3" H 1000 7210 40  0000 C CNN
F 2 "" H 1000 7150 60  0000 C CNN
F 3 "" H 1000 7150 60  0000 C CNN
	1    1000 7150
	-1   0    0    -1  
$EndComp
$Comp
L C C5
U 1 1 542F3156
P 1300 7300
F 0 "C5" H 1325 7325 30  0000 L CNN
F 1 "C" H 1325 7275 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1200 7350 60  0001 C CNN
F 3 "" H 1200 7350 60  0000 C CNN
	1    1300 7300
	-1   0    0    -1  
$EndComp
$Comp
L C C3
U 1 1 542F315C
P 1150 7300
F 0 "C3" H 1175 7325 30  0000 L CNN
F 1 "C" H 1175 7275 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1050 7350 60  0001 C CNN
F 3 "" H 1050 7350 60  0000 C CNN
	1    1150 7300
	-1   0    0    -1  
$EndComp
$Comp
L DGND #PWR026
U 1 1 542F3162
P 1000 7400
F 0 "#PWR026" V 1120 7400 60  0001 C CNN
F 1 "DGND" H 1000 7300 40  0000 C CNN
F 2 "" H 1000 7350 60  0000 C CNN
F 3 "" H 1000 7350 60  0000 C CNN
	1    1000 7400
	-1   0    0    -1  
$EndComp
Text Label 7350 2100 0    50   ~ 0
NRF_SWDCLK
Text Label 7350 2200 0    50   ~ 0
NRF_SWDIO/~RESET
NoConn ~ 4750 4250
Text Label 5400 4150 2    50   ~ 0
BUTTON2
Text Label 6300 1600 2    50   ~ 0
SPI_CLK
Text Label 6300 1700 2    50   ~ 0
SPI_MISO
Text Label 6300 1900 2    50   ~ 0
SPI_MOSI
Text Label 6300 1800 2    50   ~ 0
SPI_CS
Text Label 5850 3150 2    50   ~ 0
~XO2_JTAG_EN
Text Label 6300 1300 2    50   ~ 0
XO2_JTAG_TMS
Text Label 6300 1400 2    50   ~ 0
XO2_JTAG_TCK
Text Label 6300 1500 2    50   ~ 0
XO2_JTAG_TDI
Text Label 6300 1000 2    50   ~ 0
XO2_JTAG_TDO
Text Label 5850 3050 2    50   ~ 0
~XO2_RESET
$Comp
L R R1
U 1 1 542F803B
P 700 3600
F 0 "R1" H 725 3625 30  0000 L CNN
F 1 "190k" H 725 3575 30  0000 L CNN
F 2 "SMD-1608:SMD-1608" H 700 3600 60  0001 C CNN
F 3 "" H 700 3600 60  0000 C CNN
	1    700  3600
	1    0    0    -1  
$EndComp
$Comp
L R R2
U 1 1 542F813C
P 700 3800
F 0 "R2" H 725 3825 30  0000 L CNN
F 1 "10k" H 725 3775 30  0000 L CNN
F 2 "SMD-1608:SMD-1608" H 700 3800 60  0001 C CNN
F 3 "" H 700 3800 60  0000 C CNN
	1    700  3800
	1    0    0    -1  
$EndComp
Text Label 1300 3700 2    50   ~ 0
VIN_SENSE
$Comp
L VIN #PWR027
U 1 1 542F83FF
P 700 3500
F 0 "#PWR027" H 710 3500 20  0001 C CNN
F 1 "VIN" H 700 3560 40  0000 C CNN
F 2 "" H 700 3500 60  0000 C CNN
F 3 "" H 700 3500 60  0000 C CNN
	1    700  3500
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR028
U 1 1 542F8413
P 700 3900
F 0 "#PWR028" V 820 3900 60  0001 C CNN
F 1 "DGND" H 700 3800 40  0000 C CNN
F 2 "" H 700 3850 60  0000 C CNN
F 3 "" H 700 3850 60  0000 C CNN
	1    700  3900
	1    0    0    -1  
$EndComp
Text Notes 850  3500 0    40   ~ 0
VIN max: 16V\nsense max: 1.2V
$Comp
L R R3
U 1 1 542F924D
P 1600 750
F 0 "R3" H 1625 775 30  0000 L CNN
F 1 "5m" H 1625 725 30  0000 L CNN
F 2 "SMD-2012:SMD-2012" H 1600 750 60  0001 C CNN
F 3 "" H 1600 750 60  0000 C CNN
	1    1600 750 
	0    -1   -1   0   
$EndComp
$Comp
L PGND #PWR029
U 1 1 542F9EBD
P 1200 1200
F 0 "#PWR029" V 1320 1200 60  0001 C CNN
F 1 "PGND" H 1200 1100 40  0000 C CNN
F 2 "" H 1200 1150 60  0000 C CNN
F 3 "" H 1200 1150 60  0000 C CNN
	1    1200 1200
	1    0    0    -1  
$EndComp
$Comp
L TS1102 U1
U 1 1 542FBD60
P 1400 900
F 0 "U1" H 1800 450 50  0000 R CNN
F 1 "TS1102" H 1600 350 50  0000 L CNN
F 2 "sot-23-5:SOT-23-5" H 950 1050 50  0001 C CNN
F 3 "" H 950 1050 50  0000 C CNN
	1    1400 900 
	1    0    0    -1  
$EndComp
Text Label 2450 1200 2    50   ~ 0
IIN_SENSE
$Comp
L PGND #PWR030
U 1 1 542FCB41
P 1450 1450
F 0 "#PWR030" V 1570 1450 60  0001 C CNN
F 1 "PGND" H 1450 1350 40  0000 C CNN
F 2 "" H 1450 1400 60  0000 C CNN
F 3 "" H 1450 1400 60  0000 C CNN
	1    1450 1450
	1    0    0    -1  
$EndComp
Text Label 3750 1200 0    50   ~ 0
VIN_SENSE
Text Label 3750 1300 0    50   ~ 0
IIN_SENSE
$Comp
L NTC RT1
U 1 1 5431503A
P 1650 3750
F 0 "RT1" H 1675 3805 30  0000 L CNN
F 1 "47k" H 1675 3755 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1650 3750 60  0001 C CNN
F 3 "" H 1650 3750 60  0000 C CNN
	1    1650 3750
	1    0    0    -1  
$EndComp
$Comp
L R R4
U 1 1 543153CF
P 1650 3550
F 0 "R4" H 1675 3575 30  0000 L CNN
F 1 "22k" H 1675 3525 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 1650 3550 60  0001 C CNN
F 3 "" H 1650 3550 60  0000 C CNN
	1    1650 3550
	1    0    0    -1  
$EndComp
Text Label 2350 3650 2    50   ~ 0
TEMP_SENSE
$Comp
L PGND #PWR031
U 1 1 543158BA
P 1650 3850
F 0 "#PWR031" V 1770 3850 60  0001 C CNN
F 1 "PGND" H 1650 3750 40  0000 C CNN
F 2 "" H 1650 3800 60  0000 C CNN
F 3 "" H 1650 3800 60  0000 C CNN
	1    1650 3850
	1    0    0    -1  
$EndComp
$Comp
L +3V3 #PWR032
U 1 1 543158CE
P 1650 3450
F 0 "#PWR032" H 1660 3450 20  0001 C CNN
F 1 "+3V3" H 1650 3510 40  0000 C CNN
F 2 "" H 1650 3450 60  0000 C CNN
F 3 "" H 1650 3450 60  0000 C CNN
	1    1650 3450
	1    0    0    -1  
$EndComp
Text Label 3750 1400 0    50   ~ 0
TEMP_SENSE
Wire Wire Line
	9200 2350 9500 2350
Wire Wire Line
	9200 2450 9500 2450
Wire Wire Line
	9200 2550 9500 2550
Wire Wire Line
	9400 2650 9400 2550
Connection ~ 9400 2550
Wire Wire Line
	9400 2750 9400 2800
Wire Wire Line
	10400 2550 10500 2550
Wire Wire Line
	10500 2450 10500 2750
Wire Wire Line
	10400 2450 10500 2450
Connection ~ 10500 2550
Wire Wire Line
	10400 2350 10750 2350
Wire Wire Line
	7700 1300 8200 1300
Wire Wire Line
	9800 1450 9800 1350
Wire Wire Line
	9200 1450 9800 1450
Wire Wire Line
	9200 1550 9250 1550
Wire Wire Line
	9250 1550 9250 1450
Connection ~ 9250 1450
Wire Wire Line
	7700 1200 7700 1300
Wire Wire Line
	8200 1400 8150 1400
Wire Wire Line
	8150 1400 8150 1300
Connection ~ 8150 1300
Wire Wire Line
	7750 2400 7650 2400
Wire Wire Line
	7650 2400 7650 2600
Wire Wire Line
	7650 2600 7750 2600
Wire Wire Line
	7850 2600 8200 2600
Wire Wire Line
	7850 2400 8200 2400
Wire Wire Line
	8200 2400 8200 2450
Connection ~ 7950 2400
Wire Wire Line
	8200 2600 8200 2550
Connection ~ 7950 2600
Wire Wire Line
	9700 1500 9700 1450
Connection ~ 9700 1450
Wire Wire Line
	9200 1750 9450 1750
Wire Wire Line
	9450 1750 9450 1900
Wire Wire Line
	9200 1850 9350 1850
Wire Wire Line
	9350 2000 9350 2050
Wire Wire Line
	9350 2050 9450 2050
Wire Wire Line
	9450 2050 9450 2000
Connection ~ 9400 2050
Wire Wire Line
	9350 1850 9350 1900
Wire Wire Line
	8200 1550 8100 1550
Wire Wire Line
	8100 1550 8100 1900
Wire Wire Line
	8100 1850 8200 1850
Wire Wire Line
	8200 1750 8100 1750
Connection ~ 8100 1750
Connection ~ 8100 1850
Wire Wire Line
	8200 1650 8100 1650
Connection ~ 8100 1650
Wire Wire Line
	7800 1450 7800 1500
Wire Wire Line
	7800 1500 7900 1500
Wire Wire Line
	7900 1500 7900 1450
Connection ~ 7850 1500
Wire Wire Line
	7900 1350 7900 1300
Connection ~ 7900 1300
Wire Wire Line
	7800 1350 7800 1300
Connection ~ 7800 1300
Wire Wire Line
	2600 7250 2600 7350
Wire Wire Line
	2600 7350 2800 7350
Wire Wire Line
	2700 7350 2700 7250
Wire Wire Line
	2800 7350 2800 7250
Connection ~ 2700 7350
Wire Wire Line
	1400 2200 1350 2200
Wire Wire Line
	1350 2200 1350 2100
Wire Wire Line
	950  2100 1400 2100
Connection ~ 1350 2100
Wire Wire Line
	1100 2100 1100 2150
Wire Wire Line
	1100 2250 1100 2300
Wire Wire Line
	2100 2100 2450 2100
Wire Wire Line
	2250 2100 2250 2150
Wire Wire Line
	2250 2300 2250 2250
Wire Wire Line
	1750 2450 1750 2400
Wire Wire Line
	2450 2100 2450 2050
Connection ~ 2250 2100
Wire Wire Line
	650  2050 650  2100
Connection ~ 1100 2100
Wire Wire Line
	1050 850  1200 850 
Wire Wire Line
	1200 850  1200 1200
Wire Wire Line
	1700 750  2350 750 
Wire Wire Line
	2350 750  2350 700 
Wire Wire Line
	650  2100 750  2100
Wire Wire Line
	3900 5700 3800 5700
Wire Wire Line
	3900 5300 3900 5700
Connection ~ 3900 5600
Wire Wire Line
	3900 5600 3800 5600
Wire Wire Line
	4350 5300 4350 5250
Wire Wire Line
	4050 5350 4050 5300
Connection ~ 4050 5300
Wire Wire Line
	3900 5300 4350 5300
Wire Wire Line
	4200 5300 4200 5350
Connection ~ 4200 5300
Wire Wire Line
	4050 5450 4050 5500
Wire Wire Line
	4050 5500 4350 5500
Wire Wire Line
	4200 5500 4200 5450
Connection ~ 4200 5500
Wire Wire Line
	3200 4950 3200 4900
Wire Wire Line
	2900 5000 2900 4950
Connection ~ 2900 4950
Wire Wire Line
	2750 4950 3200 4950
Wire Wire Line
	3050 4950 3050 5000
Connection ~ 3050 4950
Wire Wire Line
	2900 5100 2900 5150
Wire Wire Line
	2900 5150 3200 5150
Wire Wire Line
	3050 5150 3050 5100
Connection ~ 3050 5150
Wire Wire Line
	2750 4950 2750 5350
Wire Wire Line
	2650 5350 2650 5300
Wire Wire Line
	2650 5300 2750 5300
Connection ~ 2750 5300
Wire Wire Line
	1000 5300 1000 5250
Wire Wire Line
	1300 5300 1300 5350
Connection ~ 1300 5300
Wire Wire Line
	1000 5300 1450 5300
Wire Wire Line
	1150 5300 1150 5350
Connection ~ 1150 5300
Wire Wire Line
	1300 5500 1300 5450
Wire Wire Line
	1000 5500 1300 5500
Wire Wire Line
	1150 5500 1150 5450
Connection ~ 1150 5500
Wire Wire Line
	1450 5300 1450 5700
Wire Wire Line
	1450 5700 1550 5700
Wire Wire Line
	1550 5600 1450 5600
Connection ~ 1450 5600
Wire Wire Line
	3800 5900 4350 5900
Wire Wire Line
	3800 6000 4350 6000
Wire Wire Line
	3800 6500 4350 6500
Wire Wire Line
	3800 6600 4350 6600
Wire Wire Line
	1550 6100 800  6100
Wire Wire Line
	1550 6400 800  6400
Wire Wire Line
	1550 6500 800  6500
Wire Wire Line
	800  6600 1550 6600
Wire Wire Line
	1550 5800 800  5800
Wire Wire Line
	3800 6800 4200 6800
Wire Wire Line
	3800 6900 4200 6900
Wire Wire Line
	1550 6800 1150 6800
Wire Wire Line
	1550 6900 1150 6900
Wire Wire Line
	1550 6000 800  6000
Wire Wire Line
	3800 5800 4200 5800
Wire Wire Line
	3800 6100 4200 6100
Wire Wire Line
	3800 6200 4200 6200
Wire Wire Line
	3800 6300 4200 6300
Wire Wire Line
	3800 6400 4200 6400
Wire Wire Line
	1550 6200 1150 6200
Wire Wire Line
	1550 6300 1150 6300
Wire Wire Line
	1550 5900 1150 5900
Wire Wire Line
	5300 5550 5750 5550
Wire Wire Line
	5300 5650 5750 5650
Wire Wire Line
	5300 5750 5750 5750
Wire Wire Line
	5300 6050 5750 6050
Wire Wire Line
	5300 6150 5750 6150
Wire Wire Line
	5300 6250 5750 6250
Wire Wire Line
	5300 6550 5750 6550
Wire Wire Line
	5300 6650 5750 6650
Wire Wire Line
	5300 6750 5750 6750
Wire Wire Line
	5300 7050 5750 7050
Wire Wire Line
	5300 7150 5750 7150
Wire Wire Line
	5300 7250 5750 7250
Wire Wire Line
	4350 7200 4350 7150
Wire Wire Line
	4050 7250 4050 7200
Connection ~ 4050 7200
Wire Wire Line
	3900 7200 4350 7200
Wire Wire Line
	4200 7200 4200 7250
Connection ~ 4200 7200
Wire Wire Line
	4050 7350 4050 7400
Wire Wire Line
	4050 7400 4350 7400
Wire Wire Line
	4200 7400 4200 7350
Connection ~ 4200 7400
Wire Wire Line
	3900 7200 3900 7000
Wire Wire Line
	3900 7000 3800 7000
Wire Wire Line
	1000 7200 1000 7150
Wire Wire Line
	1300 7200 1300 7250
Connection ~ 1300 7200
Wire Wire Line
	1000 7200 1450 7200
Wire Wire Line
	1150 7200 1150 7250
Connection ~ 1150 7200
Wire Wire Line
	1300 7400 1300 7350
Wire Wire Line
	1000 7400 1300 7400
Wire Wire Line
	1150 7400 1150 7350
Connection ~ 1150 7400
Wire Wire Line
	1450 7200 1450 7000
Wire Wire Line
	1450 7000 1550 7000
Wire Wire Line
	7350 2100 8200 2100
Wire Wire Line
	7350 2200 8200 2200
Wire Wire Line
	4350 4150 4350 4300
Connection ~ 4350 4250
Wire Wire Line
	5400 4150 4750 4150
Wire Wire Line
	5100 3150 5850 3150
Wire Wire Line
	5550 1300 6300 1300
Wire Wire Line
	5550 1400 6300 1400
Wire Wire Line
	6300 1500 5550 1500
Wire Wire Line
	5550 1000 6300 1000
Wire Wire Line
	5100 3050 5850 3050
Wire Wire Line
	6300 1600 5550 1600
Wire Wire Line
	6300 1700 5550 1700
Wire Wire Line
	6300 1900 5550 1900
Wire Wire Line
	6300 1800 5550 1800
Connection ~ 700  3700
Wire Wire Line
	700  3700 1300 3700
Wire Wire Line
	1500 750  1050 750 
Wire Wire Line
	1950 1200 2450 1200
Wire Wire Line
	1450 1450 1500 1450
Wire Wire Line
	3350 1200 4550 1200
Wire Wire Line
	3500 1300 4550 1300
Wire Wire Line
	1650 3650 2350 3650
Wire Wire Line
	3650 1400 4550 1400
$Comp
L C C9
U 1 1 54318645
P 3350 1300
F 0 "C9" H 3375 1325 30  0000 L CNN
F 1 "47n" H 3375 1275 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 3250 1350 60  0001 C CNN
F 3 "" H 3250 1350 60  0000 C CNN
	1    3350 1300
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR033
U 1 1 54318666
P 3350 1350
F 0 "#PWR033" V 3470 1350 60  0001 C CNN
F 1 "DGND" H 3350 1250 40  0000 C CNN
F 2 "" H 3350 1300 60  0000 C CNN
F 3 "" H 3350 1300 60  0000 C CNN
	1    3350 1350
	1    0    0    -1  
$EndComp
$Comp
L C C10
U 1 1 5431877C
P 3500 1400
F 0 "C10" H 3525 1425 30  0000 L CNN
F 1 "47n" H 3525 1375 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 3400 1450 60  0001 C CNN
F 3 "" H 3400 1450 60  0000 C CNN
	1    3500 1400
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR034
U 1 1 54318782
P 3500 1450
F 0 "#PWR034" V 3620 1450 60  0001 C CNN
F 1 "DGND" H 3500 1350 40  0000 C CNN
F 2 "" H 3500 1400 60  0000 C CNN
F 3 "" H 3500 1400 60  0000 C CNN
	1    3500 1450
	1    0    0    -1  
$EndComp
$Comp
L C C11
U 1 1 54318832
P 3650 1500
F 0 "C11" H 3675 1525 30  0000 L CNN
F 1 "47n" H 3675 1475 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 3550 1550 60  0001 C CNN
F 3 "" H 3550 1550 60  0000 C CNN
	1    3650 1500
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR035
U 1 1 54318838
P 3650 1550
F 0 "#PWR035" V 3770 1550 60  0001 C CNN
F 1 "DGND" H 3650 1450 40  0000 C CNN
F 2 "" H 3650 1500 60  0000 C CNN
F 3 "" H 3650 1500 60  0000 C CNN
	1    3650 1550
	1    0    0    -1  
$EndComp
Wire Wire Line
	3350 1250 3350 1200
Wire Wire Line
	3500 1350 3500 1300
Wire Wire Line
	3650 1450 3650 1400
Text Notes 1850 3800 0    50   ~ 0
place NTC on heat sink GND pour
$Comp
L PGND #PWR036
U 1 1 5431BAF7
P 1500 3000
F 0 "#PWR036" V 1620 3000 60  0001 C CNN
F 1 "PGND" H 1500 2900 40  0000 C CNN
F 2 "" H 1500 2950 60  0000 C CNN
F 3 "" H 1500 2950 60  0000 C CNN
	1    1500 3000
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR037
U 1 1 5431BB0B
P 1750 3000
F 0 "#PWR037" V 1870 3000 60  0001 C CNN
F 1 "DGND" H 1750 2900 40  0000 C CNN
F 2 "" H 1750 2950 60  0000 C CNN
F 3 "" H 1750 2950 60  0000 C CNN
	1    1750 3000
	1    0    0    -1  
$EndComp
Wire Wire Line
	1500 3000 1500 2950
Wire Wire Line
	1500 2950 1750 2950
Wire Wire Line
	1750 2950 1750 3000
Text Notes 1250 2900 0    50   ~ 0
XXX or star ground?
Wire Wire Line
	4550 2000 3750 2000
Text Label 3750 2000 0    50   ~ 0
BUTTON1
$Comp
L LED D1
U 1 1 5432CB85
P 6150 4400
F 0 "D1" H 6175 4425 30  0000 L CNN
F 1 "LED" H 6175 4375 30  0000 L CNN
F 2 "libs:SMD-1608-LED" H 6150 4400 60  0001 C CNN
F 3 "" H 6150 4400 60  0000 C CNN
	1    6150 4400
	1    0    0    -1  
$EndComp
$Comp
L LED D2
U 1 1 5432F164
P 6350 4400
F 0 "D2" H 6375 4425 30  0000 L CNN
F 1 "LED" H 6375 4375 30  0000 L CNN
F 2 "libs:SMD-1608-LED" H 6350 4400 60  0001 C CNN
F 3 "" H 6350 4400 60  0000 C CNN
	1    6350 4400
	1    0    0    -1  
$EndComp
$Comp
L LED D3
U 1 1 5432F1A7
P 6550 4400
F 0 "D3" H 6575 4425 30  0000 L CNN
F 1 "LED" H 6575 4375 30  0000 L CNN
F 2 "libs:SMD-1608-LED" H 6550 4400 60  0001 C CNN
F 3 "" H 6550 4400 60  0000 C CNN
	1    6550 4400
	1    0    0    -1  
$EndComp
$Comp
L R R5
U 1 1 5432F50D
P 6150 4200
F 0 "R5" H 6175 4225 30  0000 L CNN
F 1 "1k" H 6175 4175 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 6150 4200 60  0001 C CNN
F 3 "" H 6150 4200 60  0000 C CNN
	1    6150 4200
	1    0    0    -1  
$EndComp
$Comp
L R R6
U 1 1 5432F7A4
P 6350 4200
F 0 "R6" H 6375 4225 30  0000 L CNN
F 1 "1k" H 6375 4175 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 6350 4200 60  0001 C CNN
F 3 "" H 6350 4200 60  0000 C CNN
	1    6350 4200
	1    0    0    -1  
$EndComp
$Comp
L R R7
U 1 1 5432F7E8
P 6550 4200
F 0 "R7" H 6575 4225 30  0000 L CNN
F 1 "1k" H 6575 4175 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 6550 4200 60  0001 C CNN
F 3 "" H 6550 4200 60  0000 C CNN
	1    6550 4200
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR038
U 1 1 5432FA3D
P 6150 4500
F 0 "#PWR038" V 6270 4500 60  0001 C CNN
F 1 "DGND" H 6150 4400 40  0000 C CNN
F 2 "" H 6150 4450 60  0000 C CNN
F 3 "" H 6150 4450 60  0000 C CNN
	1    6150 4500
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR039
U 1 1 5432FB95
P 6350 4500
F 0 "#PWR039" V 6470 4500 60  0001 C CNN
F 1 "DGND" H 6350 4400 40  0000 C CNN
F 2 "" H 6350 4450 60  0000 C CNN
F 3 "" H 6350 4450 60  0000 C CNN
	1    6350 4500
	1    0    0    -1  
$EndComp
$Comp
L DGND #PWR040
U 1 1 5432FBD4
P 6550 4500
F 0 "#PWR040" V 6670 4500 60  0001 C CNN
F 1 "DGND" H 6550 4400 40  0000 C CNN
F 2 "" H 6550 4450 60  0000 C CNN
F 3 "" H 6550 4450 60  0000 C CNN
	1    6550 4500
	1    0    0    -1  
$EndComp
Wire Wire Line
	6150 4100 6150 3750
Text Label 6150 3750 3    50   ~ 0
LED1
Wire Wire Line
	6350 4100 6350 3750
Text Label 6350 3750 3    50   ~ 0
LED2
Wire Wire Line
	6550 4100 6550 3750
Text Label 6550 3750 3    50   ~ 0
LED3
Wire Wire Line
	4550 2300 3750 2300
Text Label 3750 2300 0    50   ~ 0
LED1
Text Label 3750 2400 0    50   ~ 0
LED2
Text Label 3750 2500 0    50   ~ 0
LED3
Wire Wire Line
	3750 2400 4550 2400
Wire Wire Line
	4550 2500 3750 2500
$Comp
L DGND #PWR041
U 1 1 543316EC
P 4350 4300
F 0 "#PWR041" V 4470 4300 60  0001 C CNN
F 1 "DGND" H 4350 4200 40  0000 C CNN
F 2 "" H 4350 4250 60  0000 C CNN
F 3 "" H 4350 4250 60  0000 C CNN
	1    4350 4300
	1    0    0    -1  
$EndComp
$Comp
L TACTILE-4 S1
U 1 1 5433179B
P 4550 3750
F 0 "S1" H 4425 3675 40  0000 L CNN
F 1 "TACTILE-4" H 4550 3795 60  0001 C CNN
F 2 "" H 4550 3750 60  0001 C CNN
F 3 "" H 4550 3750 60  0000 C CNN
	1    4550 3750
	-1   0    0    -1  
$EndComp
NoConn ~ 4750 3750
Text Label 5400 3650 2    50   ~ 0
BUTTON1
Wire Wire Line
	4350 3650 4350 3800
Connection ~ 4350 3750
Wire Wire Line
	5400 3650 4750 3650
$Comp
L DGND #PWR042
U 1 1 543317A7
P 4350 3800
F 0 "#PWR042" V 4470 3800 60  0001 C CNN
F 1 "DGND" H 4350 3700 40  0000 C CNN
F 2 "" H 4350 3750 60  0000 C CNN
F 3 "" H 4350 3750 60  0000 C CNN
	1    4350 3800
	1    0    0    -1  
$EndComp
Text Label 3750 2100 0    50   ~ 0
BUTTON2
Wire Wire Line
	3750 2100 4550 2100
$Comp
L JTAG J2
U 1 1 54335A28
P 9400 4250
F 0 "J2" H 9000 3900 50  0000 L CNN
F 1 "NRF-SWD" H 9800 3900 50  0000 R CNN
F 2 "libs:CONN-2x5-1.27-SMT" H 9300 4050 50  0001 C CNN
F 3 "" H 9300 4050 50  0000 C CNN
	1    9400 4250
	1    0    0    -1  
$EndComp
$Comp
L JTAG J3
U 1 1 54335C03
P 9400 5100
F 0 "J3" H 9000 4750 50  0000 L CNN
F 1 "XO2-JTAG" H 9800 4750 50  0000 R CNN
F 2 "libs:CONN-2x5-1.27-SMT" H 9300 4900 50  0001 C CNN
F 3 "" H 9300 4900 50  0000 C CNN
	1    9400 5100
	1    0    0    -1  
$EndComp
Text Label 8050 5300 0    50   ~ 0
~XO2_JTAG_EN
Text Label 10750 4900 2    50   ~ 0
XO2_JTAG_TMS
Text Label 10750 5000 2    50   ~ 0
XO2_JTAG_TCK
Text Label 10750 5200 2    50   ~ 0
XO2_JTAG_TDI
Text Label 10750 5100 2    50   ~ 0
XO2_JTAG_TDO
Text Label 10750 5300 2    50   ~ 0
~XO2_RESET
Wire Wire Line
	8800 5300 8050 5300
Wire Wire Line
	10000 4900 10750 4900
Wire Wire Line
	10000 5000 10750 5000
Wire Wire Line
	10750 5200 10000 5200
Wire Wire Line
	10000 5100 10750 5100
Wire Wire Line
	10000 5300 10750 5300
$Comp
L +3V3 #PWR043
U 1 1 5433643A
P 8600 4850
F 0 "#PWR043" H 8610 4850 20  0001 C CNN
F 1 "+3V3" H 8600 4910 40  0000 C CNN
F 2 "" H 8600 4850 60  0000 C CNN
F 3 "" H 8600 4850 60  0000 C CNN
	1    8600 4850
	1    0    0    -1  
$EndComp
Wire Wire Line
	8600 4850 8600 4900
Wire Wire Line
	8600 4900 8800 4900
$Comp
L DGND #PWR044
U 1 1 54336638
P 8600 5050
F 0 "#PWR044" V 8720 5050 60  0001 C CNN
F 1 "DGND" H 8600 4950 40  0000 C CNN
F 2 "" H 8600 5000 60  0000 C CNN
F 3 "" H 8600 5000 60  0000 C CNN
	1    8600 5050
	0    1    1    0   
$EndComp
Wire Wire Line
	8600 5000 8800 5000
Wire Wire Line
	8600 5000 8600 5100
Wire Wire Line
	8600 5100 8800 5100
Connection ~ 8600 5050
$Comp
L DGND #PWR045
U 1 1 54337106
P 8600 4200
F 0 "#PWR045" V 8720 4200 60  0001 C CNN
F 1 "DGND" H 8600 4100 40  0000 C CNN
F 2 "" H 8600 4150 60  0000 C CNN
F 3 "" H 8600 4150 60  0000 C CNN
	1    8600 4200
	0    1    1    0   
$EndComp
Wire Wire Line
	8600 4150 8800 4150
Wire Wire Line
	8600 4150 8600 4250
Wire Wire Line
	8600 4250 8800 4250
Connection ~ 8600 4200
$Comp
L +3V3 #PWR046
U 1 1 543372B9
P 8600 4000
F 0 "#PWR046" H 8610 4000 20  0001 C CNN
F 1 "+3V3" H 8600 4060 40  0000 C CNN
F 2 "" H 8600 4000 60  0000 C CNN
F 3 "" H 8600 4000 60  0000 C CNN
	1    8600 4000
	1    0    0    -1  
$EndComp
Wire Wire Line
	8600 4000 8600 4050
Wire Wire Line
	8600 4050 8800 4050
NoConn ~ 8800 4450
NoConn ~ 10000 4450
Text Label 10850 4150 2    50   ~ 0
NRF_SWDCLK
Text Label 10850 4050 2    50   ~ 0
NRF_SWDIO/~RESET
Wire Wire Line
	10850 4150 10000 4150
Wire Wire Line
	10850 4050 10000 4050
$Comp
L CONN-2x5 J4
U 1 1 54337D42
P 9400 6000
F 0 "J4" H 9400 5700 50  0000 C CNN
F 1 "CONN-2x5" H 9400 5600 50  0000 C CNN
F 2 "libs:CONN-2x5-1.27-SMT" H 10000 6800 50  0001 C CNN
F 3 "" H 10000 6800 50  0000 C CNN
	1    9400 6000
	1    0    0    -1  
$EndComp
Text Label 10400 5800 2    50   ~ 0
SPI_CLK
Text Label 10400 5900 2    50   ~ 0
SPI_MISO
Text Label 10400 6100 2    50   ~ 0
SPI_MOSI
Text Label 10400 6000 2    50   ~ 0
SPI_CS
Wire Wire Line
	10400 5800 9650 5800
Wire Wire Line
	10400 5900 9650 5900
Wire Wire Line
	10400 6100 9650 6100
Wire Wire Line
	10400 6000 9650 6000
Wire Wire Line
	9650 6200 9850 6200
Wire Wire Line
	9850 6200 9850 6250
$Comp
L DGND #PWR047
U 1 1 543382B5
P 9850 6250
F 0 "#PWR047" V 9970 6250 60  0001 C CNN
F 1 "DGND" H 9850 6150 40  0000 C CNN
F 2 "" H 9850 6200 60  0000 C CNN
F 3 "" H 9850 6200 60  0000 C CNN
	1    9850 6250
	1    0    0    -1  
$EndComp
Wire Wire Line
	4550 1000 4100 1000
Text Label 4100 1000 0    50   ~ 0
AUX1
Wire Wire Line
	4100 1100 4550 1100
Wire Wire Line
	5550 2200 6000 2200
Wire Wire Line
	6000 2300 5550 2300
Wire Wire Line
	5550 2400 6000 2400
Text Label 4100 1100 0    50   ~ 0
AUX2
Text Label 6000 2200 2    50   ~ 0
AUX3
Text Label 6000 2300 2    50   ~ 0
AUX4
Text Label 6000 2400 2    50   ~ 0
AUX5
Wire Wire Line
	9150 5800 8700 5800
Text Label 8700 5800 0    50   ~ 0
AUX1
Wire Wire Line
	8700 5900 9150 5900
Wire Wire Line
	9150 6000 8700 6000
Wire Wire Line
	8700 6100 9150 6100
Wire Wire Line
	9150 6200 8700 6200
Text Label 8700 5900 0    50   ~ 0
AUX2
Text Label 8700 6000 0    50   ~ 0
AUX3
Text Label 8700 6100 0    50   ~ 0
AUX4
Text Label 8700 6200 0    50   ~ 0
AUX5
Wire Wire Line
	5550 1100 6100 1100
Text Label 6100 1200 2    50   ~ 0
RX
Wire Wire Line
	6100 1200 5550 1200
Text Label 6100 1100 2    50   ~ 0
TX
Wire Wire Line
	10000 4250 10550 4250
Text Label 10550 4350 2    50   ~ 0
RX
Wire Wire Line
	10550 4350 10000 4350
Text Label 10550 4250 2    50   ~ 0
TX
$Comp
L XTAL Y1
U 1 1 54370330
P 6500 2050
F 0 "Y1" H 6600 2075 40  0000 C CNN
F 1 "32kHz" H 6625 2000 40  0000 C CNN
F 2 "" H 6500 2050 60  0001 C CNN
F 3 "" H 6500 2050 60  0000 C CNN
	1    6500 2050
	-1   0    0    1   
$EndComp
$Comp
L C C17
U 1 1 54370336
P 6650 2150
F 0 "C17" H 6675 2175 30  0000 L CNN
F 1 "12pF" H 6675 2125 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 6550 2200 60  0001 C CNN
F 3 "" H 6550 2200 60  0000 C CNN
	1    6650 2150
	0    1    -1   0   
$EndComp
$Comp
L C C16
U 1 1 5437033C
P 6650 1950
F 0 "C16" H 6675 1975 30  0000 L CNN
F 1 "12pF" H 6675 1925 30  0000 L CNN
F 2 "SMD-1005:SMD-1005" H 6550 2000 60  0001 C CNN
F 3 "" H 6550 2000 60  0000 C CNN
	1    6650 1950
	0    1    -1   0   
$EndComp
$Comp
L DGND #PWR048
U 1 1 54370342
P 6800 2150
F 0 "#PWR048" V 6920 2150 60  0001 C CNN
F 1 "DGND" H 6800 2050 40  0000 C CNN
F 2 "" H 6800 2100 60  0000 C CNN
F 3 "" H 6800 2100 60  0000 C CNN
	1    6800 2150
	-1   0    0    -1  
$EndComp
Wire Wire Line
	6700 1950 6800 1950
Wire Wire Line
	6800 2150 6700 2150
Wire Wire Line
	6250 2150 6600 2150
Wire Wire Line
	6250 1950 6600 1950
Connection ~ 6500 1950
Connection ~ 6500 2150
Wire Wire Line
	5550 2000 6250 2000
Wire Wire Line
	6250 2000 6250 1950
Wire Wire Line
	5550 2100 6250 2100
Wire Wire Line
	6250 2100 6250 2150
Wire Wire Line
	6800 1950 6800 2150
NoConn ~ 4550 1900
NoConn ~ 4550 1800
NoConn ~ 4550 2200
NoConn ~ 4550 1700
$Comp
L POWER_FLAG #PWR049
U 1 1 543739EE
P 1200 1100
F 0 "#PWR049" H 1200 1100 50  0001 C CNN
F 1 "POWER_FLAG" H 1200 1225 30  0000 C CNN
F 2 "" H 1200 1100 50  0001 C CNN
F 3 "" H 1200 1100 50  0000 C CNN
	1    1200 1100
	0    -1   -1   0   
$EndComp
Wire Wire Line
	1200 1100 1200 1100
Connection ~ 1200 1100
$Comp
L POWER_FLAG #PWR050
U 1 1 54374211
P 2100 750
F 0 "#PWR050" H 2100 750 50  0001 C CNN
F 1 "POWER_FLAG" H 2100 875 30  0000 C CNN
F 2 "" H 2100 750 50  0001 C CNN
F 3 "" H 2100 750 50  0000 C CNN
	1    2100 750 
	1    0    0    -1  
$EndComp
Connection ~ 2100 750 
$Comp
L POWER_FLAG #PWR051
U 1 1 5437480B
P 1000 2100
F 0 "#PWR051" H 1000 2100 50  0001 C CNN
F 1 "POWER_FLAG" H 1000 2225 30  0000 C CNN
F 2 "" H 1000 2100 50  0001 C CNN
F 3 "" H 1000 2100 50  0000 C CNN
	1    1000 2100
	1    0    0    -1  
$EndComp
Connection ~ 1000 2100
Wire Wire Line
	7850 2450 7850 2550
Wire Wire Line
	7850 2450 7650 2450
Connection ~ 7650 2450
$EndSCHEMATC
