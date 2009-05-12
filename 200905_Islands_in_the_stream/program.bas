1   ' GW-BASIC program to calculate the number or landmasses (and size)
2   ' in a given file.
3   '
4   ' GW-BASIC interpreter can be found at:
5   ' http://www.geocities.com/KindlyRat/GWBASIC.html
6   '
10  DEF FNCHARAT$(X$, Y) = MID$(X$, Y, 1)
20  ON ERROR GOTO 9000
30  LINE INPUT "Filename (or quit to exit): "; FILENAME$
40  WHILE FILENAME$ <> "quit"
50    DIM LINES$(255)
60    DIM RESULTS(26)
70    ROWNUM = 0 
80    ' Load data from file
81    GOSUB 1000
90    COLNUM = LEN(LINES$(0))
100    ' Print the map
101   GOSUB 2000
110   ' Mark the land masses with different characters
111   GOSUB 3000
120   ' Count the land masses
121   GOSUB 4000
130   ' print the results
131   GOSUB 5000
140   ERASE RESULTS
150   ERASE LINES$
160   LINE INPUT "Filename (or quit to exit): "; FILENAME$
170 WEND 
500 SYSTEM

1000 ' Load the data
1010 OPEN "I", 1, FILENAME$
1020 ROWNUM = 0
1030 WHILE EOF(1) <> -1
1040   INPUT#1, L$
1050   IF LEN(L$) = 0 THEN GOTO 1080
1060     LINES$(ROWNUM) = L$
1070     ROWNUM = ROWNUM + 1
1080   IF ROWNUM < 255 THEN GOTO 1110
1090     PRINT "Error: input exceeded 255 lines"
1100     SYSTEM
1110 WEND
1120 CLOSE 1
1130 RETURN

2000 ' print the map
2010 FOR X2000 = 0 TO ROWNUM - 1
2020   PRINT LINES$(X2000)
2030 NEXT X2000
2040 PRINT
2050 RETURN

3000 ' Marks all the contenents with different numbers
3001 ' relies on: LINES$ - map data
3002 '            ROWNUM - number of rows in the map
3003 '            COLNUM - number of columns in the map
3004 '
3005 MARK = ASC("A") - 1
3010 FOR X3000 = 0 TO ROWNUM - 1
3020   FOR Y3000 = 1 TO COLNUM
3030     IF FNCHARAT$(LINES$(X3000), Y3000) <> "+" THEN GOTO 3130
3040       MARK = MARK + 1
3050       MARK$ = CHR$(MARK)
3060       MID$(LINES$(X3000), Y3000, 1) = MARK$
3070       SOURCE$ = MARK$
3080       TARGET$ = "+"
3090       GOSUB 6000
3100       IF MARK <= ASC("Z") GOTO 3130
3110         PRINT "Error: Too many landmasses. This is basic after all!"
3120         SYSTEM
3130   NEXT
3140 NEXT
3150 RETURN

4000 ' Counts the continents and thier sizes
4001 ' relies on: LINES$ - map data that has been marked
4002 '            ROWNUM - number of rows in the map
4003 '            COLNUM - number of columns in the map
4004 '            RESULTS - array for continent count results
4005 '            CONTCOUNT - count for the number of continents
4006 CONTCOUNT = 0
4010 FOR X4000 = 0 TO ROWNUM - 1
4020   FOR Y4000 = 1 TO COLNUM
4030     TOKEN = ASC(FNCHARAT$(LINES$(X4000), Y4000)) - ASC("A")
4040     IF TOKEN < 0 OR TOKEN > 26 THEN GOTO 4080
4050       IF RESULTS(TOKEN) > 0 THEN GOTO 4070
4060         CONTCOUNT = CONTCOUNT + 1
4070       RESULTS(TOKEN) = RESULTS(TOKEN) + 1
4080   NEXT
4090 NEXT
4100 RETURN

5000 ' Prints the number of continents and thier sizes
5001 ' relies on: RESULTS - array for continent count results
5002 '            CONTCOUNT - count for the number of continents
5003 IF CONTCOUNT <> 1 THEN GOTO 5030
5010   PRINT STR$(CONTCOUNT) + " continent"
5020   GOTO 5040
5030 ' ELSE
5031   PRINT STR$(CONTCOUNT) + " continents"
5040 ' END IF
5041 FOR X5000 = 0 TO 26
5050   IF RESULTS(X5000) = 0 THEN GOTO 5070
5060     PRINT STR$(RESULTS(X5000))
5070 NEXT
5080 PRINT
5090 RETURN

6000 ' replace adjacent squares matching a character to another character
6001 ' relies on: SOURCE$ - character that is being expanded
6002 '            TARGET$ - character that will be replaced
6003 '            ROWNUM - number of rows in the map
6004 '            COLNUM - number of columns in the map
6005 '            LINES$ - map data
6006 '
6007 SQFND = 0
6010 ' from the top to bottom
6011 FOR X6000 = 0 TO ROWNUM - 1
6020   FOR Y6000 = 1 TO COLNUM
6030     ' Skip to next iteration if this square does not match source
6030     IF FNCHARAT$(LINES$(X6000), Y6000) <> SOURCE$ THEN GOTO 6360
6040       ' northwest
6041       IF X6000 = 0 OR Y6000 = 1 THEN GOTO 6080
6050         IF FNCHARAT$(LINES$(X6000 - 1), Y6000 - 1) <> TARGET$ THEN GOTO 6080
6060           MID$(LINES$(X6000 - 1), Y6000 - 1, 1) = SOURCE$
6070           SQFND = SQFND + 1
6080       '
6081       ' north
6082       IF X6000 = 0 THEN GOTO 6120
6090         IF FNCHARAT$(LINES$(X6000 - 1), Y6000) <> TARGET$ THEN GOTO 6120
6100           MID$(LINES$(X6000 - 1), Y6000, 1) = SOURCE$
6110           SQFND = SQFND + 1
6120       '
6121       ' northeast
6122       IF X6000 = 0 OR Y6000 = COLNUM THEN GOTO 6160
6130         IF FNCHARAT$(LINES$(X6000 - 1), Y6000 + 1) <> TARGET$ THEN GOTO 6160
6140           MID$(LINES$(X6000 - 1), Y6000 + 1, 1) = SOURCE$
6150           SQFND = SQFND + 1
6160       '
6161       ' west
6162       IF Y6000 = 1 THEN GOTO 6200
6170         IF FNCHARAT$(LINES$(X6000), Y6000 - 1) <> TARGET$ THEN GOTO 6200
6180           MID$(LINES$(X6000), Y6000 - 1, 1) = SOURCE$
6190           SQFND = SQFND + 1
6200       '
6201       ' east
6202       IF Y6000 = COLNUM THEN GOTO 6240
6210         IF FNCHARAT$(LINES$(X6000), Y6000 + 1) <> TARGET$ THEN GOTO 6240
6220           MID$(LINES$(X6000), Y6000 + 1, 1) = SOURCE$
6230           SQFND = SQFND + 1
6240       '
6241       ' southwest
6242       IF X6000 = ROWNUM - 1 OR Y6000 = 1 THEN GOTO 6280
6250         IF FNCHARAT$(LINES$(X6000 + 1), Y6000 - 1) <> TARGET$ THEN GOTO 6280
6260           MID$(LINES$(X6000 + 1), Y6000 - 1, 1) = SOURCE$
6270           SQFND = SQFND + 1
6280       '
6281       ' south
6282       IF X6000 = ROWNUM - 1 THEN GOTO 6320
6290         IF FNCHARAT$(LINES$(X6000 + 1), Y6000) <> TARGET$ THEN GOTO 6320
6300           MID$(LINES$(X6000 + 1), Y6000, 1) = SOURCE$
6310           SQFND = SQFND + 1
6320       '
6321       ' southeast
6322       IF X6000 = ROWNUM - 1 OR Y6000 = COLNUM THEN GOTO 6360
6330         IF FNCHARAT$(LINES$(X6000 + 1), Y6000 + 1) <> TARGET$ THEN GOTO 6360
6340           MID$(LINES$(X6000 + 1), Y6000 + 1, 1) = SOURCE$
6350           SQFND = SQFND + 1
6360   NEXT
6370 NEXT
6380 '
6381 ' from bottom to top
6382 FOR X6000 = ROWNUM TO 0 STEP -1
6390   FOR Y6000 = COLNUM TO 1 STEP -1
6400     ' Skip to next iteration if this square does not match source
6401     IF FNCHARAT$(LINES$(X6000), Y6000) <> SOURCE$ THEN GOTO 6690
6410     ' north
6411       IF X6000 = 0 THEN GOTO 6450
6420         IF FNCHARAT$(LINES$(X6000 - 1), Y6000) <> TARGET$ THEN GOTO 6450
6430           MID$(LINES$(X6000 - 1), Y6000, 1) = SOURCE$
6440           SQFND = SQFND + 1
6450     '
6451     ' northeast
6452     IF X6000 = 0 OR Y6000 = COLNUM THEN GOTO 6490
6460       IF FNCHARAT$(LINES$(X6000 - 1), Y6000 + 1) <> TARGET$ THEN GOTO 6490
6470         MID$(LINES$(X6000 - 1), Y6000 + 1, 1) = SOURCE$
6480         SQFND = SQFND + 1
6490     '
6491     ' west
6492     IF Y6000 = 1 THEN GOTO 6530
6500       IF FNCHARAT$(LINES$(X6000), Y6000 - 1) <> TARGET$ THEN GOTO 6530
6510         MID$(LINES$(X6000), Y6000 - 1, 1) = SOURCE$
6520         SQFND = SQFND + 1
6530     '
6531     ' east
6532     IF Y6000 = COLNUM THEN GOTO 6570
6540       IF FNCHARAT$(LINES$(X6000), Y6000 + 1) <> TARGET$ THEN GOTO 6570
6550         MID$(LINES$(X6000), Y6000 + 1, 1) = SOURCE$
6560         SQFND = SQFND + 1
6570     '
6571     ' southwest
6572     IF X6000 = ROWNUM - 1 OR Y6000 = 1 THEN GOTO 6610
6580       IF FNCHARAT$(LINES$(X6000 + 1), Y6000 - 1) <> TARGET$ THEN GOTO 6610
6590         MID$(LINES$(X6000 + 1), Y6000 - 1, 1) = SOURCE$
6600         SQFND = SQFND + 1
6610     '
6611     ' south
6612     IF X6000 = ROWNUM - 1 THEN GOTO 6650
6620       IF FNCHARAT$(LINES$(X6000 + 1), Y6000) <> TARGET$ THEN GOTO 6650
6630         MID$(LINES$(X6000 + 1), Y6000, 1) = SOURCE$
6640         SQFND = SQFND + 1
6650     '
6651     ' southeast
6652     IF X6000 = ROWNUM - 1 OR Y6000 = COLNUM THEN GOTO 6690
6660       IF FNCHARAT$(LINES$(X6000 + 1), Y6000 + 1) <> TARGET$ THEN GOTO 6690
6680         MID$(LINES$(X6000  + 1), Y6000 + 1, 1) = SOURCE$
6690         SQFND = SQFND + 1
6690   NEXT
6700 NEXT
6710 IF SQFND = 0 THEN RETURN
6720 GOTO 6000

9000 ' Error handler for File not found.
9010 A=ERR: B=ERL
9020 IF A <> 53 THEN GOTO 9050
9030 PRINT "File not found."
9031 ERASE RESULTS
9032 ERASE LINES$
9040 RESUME 30
9050 PRINT "Error " + STR$(A) + " on line " + STR$(B)
9060 SYSTEM
