20 FOR I = 3 TO 4
30   FOR J = 1 TO 2
40     GOSUB 80
50   NEXT J
60 NEXT I
70 END
80 PRINT (I * 10)^RND(X)
90 RETURN
