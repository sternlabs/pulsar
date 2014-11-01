EESchema Schematic File Version 2
LIBS:testpoint
LIBS:mounthole
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
Sheet 2 5
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
L NMOS Q1
U 1 1 542BBC0D
P 3650 2300
AR Path="/542B257F/542BBC0D" Ref="Q1"  Part="1" 
AR Path="/542BC4ED/542BBC0D" Ref="Q4"  Part="1" 
AR Path="/542BC4EF/542BBC0D" Ref="Q7"  Part="1" 
AR Path="/542BC4F0/542BBC0D" Ref="Q10"  Part="1" 
F 0 "Q10" H 3750 2425 40  0000 L CNN
F 1 "NMOS" H 3750 2325 40  0000 L CNN
F 2 "libs:SOT-23-FET" H 2850 2150 60  0001 C CNN
F 3 "" H 2850 2150 60  0000 C CNN
	1    3650 2300
	1    0    0    -1  
$EndComp
Text HLabel 3150 2300 0    50   Input ~ 0
CH1
Wire Wire Line
	3150 2300 3400 2300
$Comp
L PGND #PWR058
U 1 1 542BBCA3
P 3650 2450
AR Path="/542B257F/542BBCA3" Ref="#PWR058"  Part="1" 
AR Path="/542BC4ED/542BBCA3" Ref="#PWR065"  Part="1" 
AR Path="/542BC4EF/542BBCA3" Ref="#PWR072"  Part="1" 
AR Path="/542BC4F0/542BBCA3" Ref="#PWR079"  Part="1" 
F 0 "#PWR079" V 3770 2450 60  0001 C CNN
F 1 "PGND" H 3650 2350 40  0000 C CNN
F 2 "" H 3650 2400 60  0000 C CNN
F 3 "" H 3650 2400 60  0000 C CNN
	1    3650 2450
	1    0    0    -1  
$EndComp
$Comp
L CONN-4 J5
U 1 1 542BBCB9
P 5050 2300
AR Path="/542B257F/542BBCB9" Ref="J5"  Part="1" 
AR Path="/542BC4ED/542BBCB9" Ref="J6"  Part="1" 
AR Path="/542BC4EF/542BBCB9" Ref="J7"  Part="1" 
AR Path="/542BC4F0/542BBCB9" Ref="J8"  Part="1" 
F 0 "J8" H 5300 2450 40  0000 L CNN
F 1 "CONN-4" H 5300 2350 40  0000 L CNN
F 2 "libs:CONN-4-2.54-0.8-PTH" H 5150 2200 60  0001 C CNN
F 3 "" H 5150 2200 60  0000 C CNN
	1    5050 2300
	1    0    0    -1  
$EndComp
$Comp
L NMOS Q2
U 1 1 542BBD72
P 3650 3000
AR Path="/542B257F/542BBD72" Ref="Q2"  Part="1" 
AR Path="/542BC4ED/542BBD72" Ref="Q5"  Part="1" 
AR Path="/542BC4EF/542BBD72" Ref="Q8"  Part="1" 
AR Path="/542BC4F0/542BBD72" Ref="Q11"  Part="1" 
F 0 "Q11" H 3750 3125 40  0000 L CNN
F 1 "NMOS" H 3750 3025 40  0000 L CNN
F 2 "libs:SOT-23-FET" H 2850 2850 60  0001 C CNN
F 3 "" H 2850 2850 60  0000 C CNN
	1    3650 3000
	1    0    0    -1  
$EndComp
Text HLabel 3150 3000 0    50   Input ~ 0
CH2
Wire Wire Line
	3150 3000 3400 3000
$Comp
L PGND #PWR059
U 1 1 542BBD7A
P 3650 3150
AR Path="/542B257F/542BBD7A" Ref="#PWR059"  Part="1" 
AR Path="/542BC4ED/542BBD7A" Ref="#PWR066"  Part="1" 
AR Path="/542BC4EF/542BBD7A" Ref="#PWR073"  Part="1" 
AR Path="/542BC4F0/542BBD7A" Ref="#PWR080"  Part="1" 
F 0 "#PWR080" V 3770 3150 60  0001 C CNN
F 1 "PGND" H 3650 3050 40  0000 C CNN
F 2 "" H 3650 3100 60  0000 C CNN
F 3 "" H 3650 3100 60  0000 C CNN
	1    3650 3150
	1    0    0    -1  
$EndComp
$Comp
L NMOS Q3
U 1 1 542BBE20
P 3650 3700
AR Path="/542B257F/542BBE20" Ref="Q3"  Part="1" 
AR Path="/542BC4ED/542BBE20" Ref="Q6"  Part="1" 
AR Path="/542BC4EF/542BBE20" Ref="Q9"  Part="1" 
AR Path="/542BC4F0/542BBE20" Ref="Q12"  Part="1" 
F 0 "Q12" H 3750 3825 40  0000 L CNN
F 1 "NMOS" H 3750 3725 40  0000 L CNN
F 2 "libs:SOT-23-FET" H 2850 3550 60  0001 C CNN
F 3 "" H 2850 3550 60  0000 C CNN
	1    3650 3700
	1    0    0    -1  
$EndComp
Text HLabel 3150 3700 0    50   Input ~ 0
CH3
Wire Wire Line
	3150 3700 3400 3700
$Comp
L PGND #PWR060
U 1 1 542BBE28
P 3650 3850
AR Path="/542B257F/542BBE28" Ref="#PWR060"  Part="1" 
AR Path="/542BC4ED/542BBE28" Ref="#PWR067"  Part="1" 
AR Path="/542BC4EF/542BBE28" Ref="#PWR074"  Part="1" 
AR Path="/542BC4F0/542BBE28" Ref="#PWR081"  Part="1" 
F 0 "#PWR081" V 3770 3850 60  0001 C CNN
F 1 "PGND" H 3650 3750 40  0000 C CNN
F 2 "" H 3650 3800 60  0000 C CNN
F 3 "" H 3650 3800 60  0000 C CNN
	1    3650 3850
	1    0    0    -1  
$EndComp
$Comp
L VIN #PWR061
U 1 1 542BBE40
P 4950 2050
AR Path="/542B257F/542BBE40" Ref="#PWR061"  Part="1" 
AR Path="/542BC4ED/542BBE40" Ref="#PWR068"  Part="1" 
AR Path="/542BC4EF/542BBE40" Ref="#PWR075"  Part="1" 
AR Path="/542BC4F0/542BBE40" Ref="#PWR082"  Part="1" 
F 0 "#PWR082" H 4960 2050 20  0001 C CNN
F 1 "VIN" H 4950 2110 40  0000 C CNN
F 2 "" H 4950 2050 60  0000 C CNN
F 3 "" H 4950 2050 60  0000 C CNN
	1    4950 2050
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 2050 4950 2150
Wire Wire Line
	4950 2150 5050 2150
$Comp
L CONN-2 J12
U 1 1 544070AC
P 5050 2950
AR Path="/542BC4ED/544070AC" Ref="J12"  Part="1" 
AR Path="/542B257F/544070AC" Ref="J9"  Part="1" 
AR Path="/542BC4EF/544070AC" Ref="J15"  Part="1" 
AR Path="/542BC4F0/544070AC" Ref="J18"  Part="1" 
F 0 "J18" H 5300 3000 40  0000 L CNN
F 1 "CONN-2" H 5300 2900 40  0000 L CNN
F 2 "" H 5150 2750 60  0001 C CNN
F 3 "" H 5150 2750 60  0000 C CNN
	1    5050 2950
	1    0    0    -1  
$EndComp
$Comp
L VIN #PWR062
U 1 1 544072E5
P 4950 2800
AR Path="/542B257F/544072E5" Ref="#PWR062"  Part="1" 
AR Path="/542BC4ED/544072E5" Ref="#PWR069"  Part="1" 
AR Path="/542BC4EF/544072E5" Ref="#PWR076"  Part="1" 
AR Path="/542BC4F0/544072E5" Ref="#PWR083"  Part="1" 
F 0 "#PWR083" H 4960 2800 20  0001 C CNN
F 1 "VIN" H 4950 2860 40  0000 C CNN
F 2 "" H 4950 2800 60  0000 C CNN
F 3 "" H 4950 2800 60  0000 C CNN
	1    4950 2800
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 2800 4950 2900
Wire Wire Line
	4950 2900 5050 2900
Wire Wire Line
	3650 2000 4150 2000
Text Label 4150 2000 2    50   ~ 0
CH1_OUT
Wire Wire Line
	3650 2700 4150 2700
Text Label 4150 2700 2    50   ~ 0
CH2_OUT
Wire Wire Line
	3650 3400 4150 3400
Text Label 4150 3400 2    50   ~ 0
CH3_OUT
Wire Wire Line
	5050 2250 4550 2250
Text Label 4550 2250 0    50   ~ 0
CH1_OUT
Text Label 4550 2350 0    50   ~ 0
CH2_OUT
Text Label 4550 2450 0    50   ~ 0
CH3_OUT
Wire Wire Line
	4550 2350 5050 2350
Wire Wire Line
	4550 2450 5050 2450
Wire Wire Line
	5050 3000 4550 3000
Text Label 4550 3000 0    50   ~ 0
CH1_OUT
$Comp
L CONN-2 J10
U 1 1 5440756A
P 5050 3350
AR Path="/542B257F/5440756A" Ref="J10"  Part="1" 
AR Path="/542BC4ED/5440756A" Ref="J13"  Part="1" 
AR Path="/542BC4EF/5440756A" Ref="J16"  Part="1" 
AR Path="/542BC4F0/5440756A" Ref="J19"  Part="1" 
F 0 "J19" H 5300 3400 40  0000 L CNN
F 1 "CONN-2" H 5300 3300 40  0000 L CNN
F 2 "" H 5150 3150 60  0001 C CNN
F 3 "" H 5150 3150 60  0000 C CNN
	1    5050 3350
	1    0    0    -1  
$EndComp
$Comp
L VIN #PWR063
U 1 1 54407570
P 4950 3200
AR Path="/542B257F/54407570" Ref="#PWR063"  Part="1" 
AR Path="/542BC4ED/54407570" Ref="#PWR070"  Part="1" 
AR Path="/542BC4EF/54407570" Ref="#PWR077"  Part="1" 
AR Path="/542BC4F0/54407570" Ref="#PWR084"  Part="1" 
F 0 "#PWR084" H 4960 3200 20  0001 C CNN
F 1 "VIN" H 4950 3260 40  0000 C CNN
F 2 "" H 4950 3200 60  0000 C CNN
F 3 "" H 4950 3200 60  0000 C CNN
	1    4950 3200
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 3200 4950 3300
Wire Wire Line
	4950 3300 5050 3300
Wire Wire Line
	5050 3400 4550 3400
Text Label 4550 3400 0    50   ~ 0
CH2_OUT
$Comp
L CONN-2 J11
U 1 1 544075A8
P 5050 3750
AR Path="/542B257F/544075A8" Ref="J11"  Part="1" 
AR Path="/542BC4ED/544075A8" Ref="J14"  Part="1" 
AR Path="/542BC4EF/544075A8" Ref="J17"  Part="1" 
AR Path="/542BC4F0/544075A8" Ref="J20"  Part="1" 
F 0 "J20" H 5300 3800 40  0000 L CNN
F 1 "CONN-2" H 5300 3700 40  0000 L CNN
F 2 "" H 5150 3550 60  0001 C CNN
F 3 "" H 5150 3550 60  0000 C CNN
	1    5050 3750
	1    0    0    -1  
$EndComp
$Comp
L VIN #PWR064
U 1 1 544075AE
P 4950 3600
AR Path="/542B257F/544075AE" Ref="#PWR064"  Part="1" 
AR Path="/542BC4ED/544075AE" Ref="#PWR071"  Part="1" 
AR Path="/542BC4EF/544075AE" Ref="#PWR078"  Part="1" 
AR Path="/542BC4F0/544075AE" Ref="#PWR085"  Part="1" 
F 0 "#PWR085" H 4960 3600 20  0001 C CNN
F 1 "VIN" H 4950 3660 40  0000 C CNN
F 2 "" H 4950 3600 60  0000 C CNN
F 3 "" H 4950 3600 60  0000 C CNN
	1    4950 3600
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 3600 4950 3700
Wire Wire Line
	4950 3700 5050 3700
Wire Wire Line
	5050 3800 4550 3800
Text Label 4550 3800 0    50   ~ 0
CH3_OUT
Text Notes 5700 3600 1    50   ~ 0
stuff sometimes
$EndSCHEMATC
