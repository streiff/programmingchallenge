1   ' GW-BASIC program to calculate the number or landmasses (and size)
2   ' in a given file.
3   '
4   ' GW-BASIC interpreter can be found at:
5   ' http://www.geocities.com/KindlyRat/GWBASIC.html
6   '
10  DEF FNCHARAT$(X$, Y) = MID$(X$, Y, 1)
20  DIM LINES$(255)
30  DIM RESULTS(26)
40  ROWNUM = 0 
50  ON ERROR GOTO 9000
60  LINE INPUT "Filename (or quit to exit): "; FILENAME$
70  IF FILENAME$ = "quit" THEN GOTO 500
80    ' Load data from file
81    GOSUB 1000
90    COLNUM = LEN(LINES$(0))
100    ' Print the map
101   GOSUB 1500
110   ' Mark the land masses with different characters
111   GOSUB 3500
120   ' Count the land masses
121   GOSUB 4000
130   ' print the results
131   GOSUB 4175
140   ERASE LINES$
150   ERASE RESULTS
160   ' Prompt for next file
161   GOTO 20
500 SYSTEM

1000 ' Load the data
1010 OPEN "I", 1, FILENAME$
1020 ROWNUM = 0
1030 IF EOF(1) THEN GOTO 1080
1040 INPUT#1, LINES$(ROWNUM)
1050 ROWNUM = ROWNUM + 1
1060 IF ROWNUM > 255 THEN GOTO 1100
1070 GOTO 1030
1080 CLOSE 1
1090 RETURN
1100 PRINT "Error: input exceeded 255 lines"
1110 SYSTEM

1500 ' print the lines
1501 ' relies on: LINES$ - map data
1510 FOR I = 0 TO ROWNUM - 1
1520 PRINT LINES$(I)
1530 NEXT I
1540 PRINT
1550 RETURN

2500 ' replace adjacent squares matching a character to another character
2501 ' relies on: SOURCE$ - character that is being expanded
2502 '            TARGET$ - character that will be replaced
2003 '            ROWNUM - number of rows in the map
2004 '            COLNUM - number of columns in the map
2505 '            LINES$ - map data
2506 '
2510 SQFND = 0
2411 '
2512 ' from the top to bottom
2513 FOR I = 0 TO ROWNUM - 1
2520 FOR J = 1 TO COLNUM
2530 IF MID$(LINES$(I), J, 1) <> SOURCE$ THEN GOTO 2860
2540 ' northwest
2541 IF I = 0 OR J = 1 THEN GOTO 2580
2550 IF MID$(LINES$(I - 1), J - 1, 1) <> TARGET$ THEN GOTO 2580
2560 MID$(LINES$(I - 1), J - 1, 1) = SOURCE$
2570 SQFND = SQFND + 1
2580 ' north
2581 IF I = 0 THEN GOTO 2620
2590 IF MID$(LINES$(I - 1), J, 1) <> TARGET$ THEN GOTO 2620
2600 MID$(LINES$(I - 1), J, 1) = SOURCE$
2610 SQFND = SQFND + 1
2620 ' northeast
2621 IF I = 0 OR J = COLNUM THEN GOTO 2660
2630 IF MID$(LINES$(I - 1), J + 1, 1) <> TARGET$ THEN GOTO 2660
2640 MID$(LINES$(I - 1), J + 1, 1) = SOURCE$
2650 SQFND = SQFND + 1
2660 ' west
2661 IF J = 1 THEN GOTO 2700
2670 IF MID$(LINES$(I), J - 1, 1) <> TARGET$ THEN GOTO 2700
2680 MID$(LINES$(I), J - 1, 1) = SOURCE$
2690 SQFND = SQFND + 1
2700 ' northeast
2701 IF J = COLNUM THEN GOTO 2740
2710 IF MID$(LINES$(I), J + 1, 1) <> TARGET$ THEN GOTO 2740
2720 MID$(LINES$(I), J + 1, 1) = SOURCE$
2730 SQFND = SQFND + 1
2740 ' southwest
2741 IF I = ROWNUM - 1 OR J = 1 THEN GOTO 2780
2750 IF MID$(LINES$(I + 1), J - 1, 1) <> TARGET$ THEN GOTO 2780
2760 MID$(LINES$(I + 1), J - 1, 1) = SOURCE$
2770 SQFND = SQFND + 1
2780 ' south
2781 IF I = ROWNUM - 1 THEN GOTO 2820
2790 IF MID$(LINES$(I + 1), J, 1) <> TARGET$ THEN GOTO 2820
2800 MID$(LINES$(I + 1), J, 1) = SOURCE$
2810 SQFND = SQFND + 1
2820 ' southeast
2821 IF I = ROWNUM - 1 OR J = COLNUM THEN GOTO 2860
2830 IF MID$(LINES$(I + 1), J + 1, 1) <> TARGET$ THEN GOTO 2860
2840 MID$(LINES$(I + 1), J + 1, 1) = SOURCE$
2850 SQFND = SQFND + 1
2860 NEXT
2870 NEXT
2880 '
2881 ' from the top to bottom
2882 FOR I = ROWNUM TO 0 STEP -1
2890 FOR J = COLNUM TO 1 STEP -1
2900 IF MID$(LINES$(I), J, 1) <> SOURCE$ THEN GOTO 3190
2910 ' from bottom to top
2911 ' north
2912 IF I = 0 THEN GOTO 2950
2920 IF MID$(LINES$(I - 1), J, 1) <> TARGET$ THEN GOTO 2950
2930 MID$(LINES$(I - 1), J, 1) = SOURCE$
2940 SQFND = SQFND + 1
2950 ' northeast
2951 IF I = 0 OR J = COLNUM THEN GOTO 2990
2960 IF MID$(LINES$(I - 1), J + 1, 1) <> TARGET$ THEN GOTO 2990
2970 MID$(LINES$(I - 1), J + 1, 1) = SOURCE$
2980 SQFND = SQFND + 1
2990 ' west
2991 IF J = 1 THEN GOTO 3030
3000 IF MID$(LINES$(I), J - 1, 1) <> TARGET$ THEN GOTO 3030
3010 MID$(LINES$(I), J - 1, 1) = SOURCE$
3020 SQFND = SQFND + 1
3030 ' northeast
3031 IF J = COLNUM THEN GOTO 3070
3040 IF MID$(LINES$(I), J + 1, 1) <> TARGET$ THEN GOTO 3070
3050 MID$(LINES$(I), J + 1, 1) = SOURCE$
3060 SQFND = SQFND + 1
3070 ' southwest
3071 IF I = ROWNUM - 1 OR J = 1 THEN GOTO 3110
3080 IF MID$(LINES$(I + 1), J - 1, 1) <> TARGET$ THEN GOTO 3110
3090 MID$(LINES$(I + 1), J - 1, 1) = SOURCE$
3100 SQFND = SQFND + 1
3110 ' south
3111 IF I = ROWNUM - 1 THEN GOTO 3150
3120 IF MID$(LINES$(I + 1), J, 1) <> TARGET$ THEN GOTO 3150
3130 MID$(LINES$(I + 1), J, 1) = SOURCE$
3140 SQFND = SQFND + 1
3150 ' southeast
3151 IF I = ROWNUM - 1 OR J = COLNUM THEN GOTO 3190
3160 IF MID$(LINES$(I + 1), J + 1, 1) <> TARGET$ THEN GOTO 3190
3170 MID$(LINES$(I + 1), J + 1, 1) = SOURCE$
3180 SQFND = SQFND + 1
3190 NEXT
3200 NEXT
3210 IF SQFND = 0 THEN GOTO 3230
3220 GOTO 2510
3230 RETURN

3500 ' Marks all the contenents with different numbers
3501 ' relies on: LINES$ - map data
3502 '            ROWNUM - number of rows in the map
3503 '            COLNUM - number of columns in the map
3504 '
3505 MARK = ASC("A")
3510 MARK$ = CHR$(MARK)
3560 FOR I = 0 TO ROWNUM - 1
3570 FOR J = 1 TO COLNUM
3580 IF MID$(LINES$(I), J, 1) = "+" THEN GOTO 3620
3590 NEXT
3600 NEXT
3610 RETURN
3620 MID$(LINES$(I), J, 1) = MARK$
3630 SOURCE$ = MARK$
3640 TARGET$ = "+"
3650 GOSUB 2500
3660 MARK = MARK + 1
3670 IF MARK = ASC("Z") GOTO 3690
3680 GOTO 3510
3690 PRINT "Error: Too many landmasses. This is basic after all!"
3700 GOTO 500 

4000 ' Prints out the output of the program
4001 ' relies on: LINES$ - map data that has been marked
4002 '            ROWNUM - number of rows in the map
4003 '            COLNUM - number of columns in the map
4004 CONTCOUNT = 0
4020 FOR I = 0 TO ROWNUM - 1
4030 FOR J = 1 TO COLNUM
4040 TOKEN = ASC(MID$(LINES$(I), J, 1)) - ASC("A")
4050 IF TOKEN < 0 OR TOKEN > 26 THEN GOTO 4140
4060 IF RESULTS(TOKEN) > 0 THEN GOTO 4080
4070 CONTCOUNT = CONTCOUNT + 1
4080 RESULTS(TOKEN) = RESULTS(TOKEN) + 1
4140 NEXT
4150 NEXT
4155 RETURN

4175 IF CONTCOUNT <> 1 THEN GOTO 4185
4180 PRINT STR$(CONTCOUNT) + " continent"
4181 GOTO 4190
4185 PRINT STR$(CONTCOUNT) + " continents"
4190 FOR I = 0 TO 26
4200 IF RESULTS(I) = 0 THEN GOTO 4220
4210 PRINT STR$(RESULTS(I))
4220 NEXT
4230 RETURN


9000 ' Error handler for File not found.
9010 A=ERR: B=ERL
9020 IF A <> 53 THEN GOTO 9050
9030 PRINT "File not found."
9040 RESUME 50
9050 PRINT "Error " + STR$(A) + " on line " + STR$(B)
9060 SYSTEM



