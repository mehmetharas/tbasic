DEFDBL A-Z
 5 DIM SAYI$(100)
10000 DATA BIR,IKI,UC,DORT,BES,ALTI,YEDI,SEKIZ,DOKUZ,ON
10001 DATA ONBIR,ONIKI,ONUC,ONDORT,ONBES,ONALTI,ONYEDI,ONSEKIZ,ONDOKUZ,YIRMI
10002 DATA YIRMIBIR,YIRMIIKI,YIRMIUC,YIRMIDORT,YIRMIBES,YIRMIALTI,YIRMIYEDI,YIRMISEKIZ,YIRMIDOKUZ,OTUZ
10003 DATA OTUZBIR,OTUZIKI,OTUZUC,OTUZDORT,OTUZBES,OTUZALTI,OTUZYEDI,OTUZSEKIZ,OTUZDOKUZ,KIRK
10004 DATA KIRKBIR,KIRKIKI,KIRKUC,KIRKDORT,KIRKBES,KIRKALTI,KIRKYEDI,KIRKSEKIZ,KIRKDOKUZ,ELLI
10005 DATA ELLIBIR,ELLIIKI,ELLIUC,ELLIDORT,ELLIBES,ELLIALTI,ELLIYEDI,ELLISEKIZ,ELLIDOKUZ,ALTMIS
10006 DATA ALTMISBIR,ALTMISIKI,ALTMISUC,ALTMISDORT,ALTMISBES,ALTMISALTI,ALTMISYEDI,ALTMISSEKIZ,ALTMISDOKUZ,YETMIS
10007 DATA YETMISBIR,YETMISIKI,YETMISUC,YETMISDORT,YETMISBES,YETMISALTI,YETMISYEDI,YETMISSEKIZ,YETMISDOKUZ,SEKSEN
10008 DATA SEKSENBIR,SEKSENIKI,SEKSENUC,SEKSENDORT,SEKSENBES,SEKSENALTI,SEKSENYEDI,SEKSENSEKIZ,SEKSENDOKUZ,DOKSAN
10009 DATA DOKSANBIR,DOKSANIKI,DOKSANUC,DOKSANDORT,DOKSANBES,DOKSANALTI,DOKSANYEDI,DOKSANSEKIZ,DOKSANDOKUZ,YUZ
10010 FOR I=1 TO 100
10020 READ SAYI$(I)
10030 NEXT I
10 REM BU PROGRAM OGUZ YOZGATLIOGLU TARAFINDAN HAZIRLANMISTIR.
15 COLOR 7,0:CLS:NNAME$="OGRENCI.DAT"
20 OPEN "R",1,NNAME$,34
30 FIELD 1,10 AS NOTU$,20 AS ISIM$,2 AS VIZE$,2 AS FINAL$
40 UZUN=LOF(1)/35
50 REM ANA MENU
60 CLS:LOCATE 1,30:PRINT "CUMHURIYET UNIVERSITESI":LOCATE 2,30
70 PRINT "BILGI ISLEM DAIRE BASKANLIGI":LOCATE 4,25:PRINT "KAYIT GIRISI"
80 LOCATE 5,25:PRINT "2=EKRANA LISTE":LOCATE 6,25:PRINT "3=YAZICIYA LISTE"
90 LOCATE 7,25:PRINT "ESC-PROGRAM SONU"
100 LOCATE 8,25:PRINT "SECIMINIZ...":SK$=INPUT$(1)
110 IF SK$="1" THEN GOTO 150
120 IF SK$="2" THEN GOTO 270
130 IF SK$="3" THEN GOTO 500
140 IF SK$=CHR$(27) THEN CLOSE #1:CLS:END
145 GOTO 50
150 REM KAYIT GIRISI
151 UZUN=LOF(1)/34:UZUNU=UZUN+1
160 CLS:lOCATE 10,20:INPUT "OGRENCININ NUMARASI=",NOTA$
170 LOCATE 11,20:INPUT "OGRENCINI ADI SOYADI=",ADI$
180 LOCATE 12,20:INPUT "OGRENCININ VIZE ORTALAMASI(VIZE1+VIZE2/2)",VIZ
190 LOCATE 13,20:INPUT "OGRENCININ FINAL NOTU",FIN
200 LOCATE 10,30
210 INPUT "GIRDIGINIZ BILGILER DOGRUMU?LUTFEN KONTROL EDIN(E/H)",KON$
220 IF KON$="H" THEN GOTO 160
230 LSET NOTU$=NOTA$:LSET ISIM$=ADI$:LSET VIZE$=MKI$(VIZ):LSET FINAL$=MKI$(FIN)
240 PUT 1,UZUNU
250 LOCATE 22,20:INPUT "YENI KAYIT (E/ESC)",KAY$
260 IF KAY$=CHR$(27) THEN GOTO 40 ELSE GOTO 150
270 REM EKRANA LISTE
280 CLS:S=1:LOCATE 1,25:PRINT "CUMHURIYET UNIVERSITESI":FINAL=0:VIZE=0:B1=0:B2=0:YAZI=0
290 LOCATE 2,25:PRINT "BILGI ISLEM DAIRE BASKANLIGI"
291 LOCATE 3,2:INPUT "DERSIN ADI=",D$:LOCATE 4,2:INPUT "BOLUMU    =",B$
292 LOCATE 3,55:INPUT "SINAV TARIHI=",TAR$:LOCATE 4,55:INPUT "DERSIN KODU =",KOD$
300 PRINT "�"+STRING$(4,"�")+"�"+STRING$(10,"�")+"�"+STRING$(20,"�")+"�";
310 PRINT STRING$(4,"�")+"�"+STRING$(5,"�")+"�"+STRING$(19,"�")+"�";
320 PRINT STRING$(9,"�")+"�":LOCATE 6,1
330 PRINT "�"+"SIRA"+"�"+"NUMARASI  "+"�"+"ADI SOYADI          "+"�";
340 PRINT "VIZE"+"�"+"....."+"�"+"BASARI NOTU        "+"�"+"SONUC    "+"�"
350 LOCATE 7,1:PRINT "�NO. �          �"+STRING$(20," ")+"�ORT.�.....";
360 PRINT "�������������������Ĵ         �":LOCATE 8,1
370 PRINT "�    �          �"+STRING$(20," ")+"�    �.....�RAK.�YAZIYLA       �         �";
380 LOCATE 9,1:PRINT "�����������������"+STRING$(20,"�")+"������"+STRING$(5,"�");
390 PRINT "�����������������������������Ķ"
400 FOR I=10 TO 24
410 IF S-1=UZUN THEN GOTO 470
415 GET 1,S
420 FINAL=CVI(FINAL$):VIZE=CVI(VIZE$):YAZI=(VIZE*0.3)+(FINAL*0.7)
421 IF FINAL>VIZE AND FINAL>60 THEN YAZI=FINAL:SONUC$="BASARILI"
425 IF YAZI<50 OR FINAL<50 THEN SONUC$="BASARISIZ" ELSE SONUC$="BASARILI"
430 IF FINAL>=60 AND VIZE<=49 THEN YAZI=FINAL:SONUC$="BASARILI"
440 PRINT TAB(1);"�";TAB(2);S;TAB(6);"�";TAB(7);NOTU$;TAB(17);"�";TAB(18);_
ISIM$;TAB(38);"�";TAB(39);VIZE;TAB(43);"�";TAB(44);FINAL;TAB(49);"�";TAB(54);"�";TAB(61);"        �";SONUC$;TAB(79);"�"
460 LOCATE I,50:PRINT USING "###";YAZI: LOCATE I,55: PRINT SAYI$(YAZI+1)
465 IF S>=UZUN THEN GOTO 470 ELSE S=S+1:NEXT I
470 PRINT "�����������������"+STRING$(20,"�")+"����������������������������������������ͼ"
480 PRINT "GERI DONMEK ICIN BIR TUSA BASIN":SS$=INPUT$(1)
490 GOTO 60
500 REM YAZICIYA LISTE
510 CLS:PRINT "YAZICIYI HAZIRLAYIP BIR TUSA BASIN.":SN$=INPUT$(1)
660 S=1:LPRINT TAB(30);"CUMHURIYET UNIVERSITESI":FINAL=0:VIZE=0:B1=0:B2=0:YAZI=0
670 LPRINT TAB(31);"MUHENDISLIK FAKULTESI"
680 LPRINT TAB(3);"DERSIN ADI=................";TAB(50);"SINAV TARIHI=..............."
690 LPRINT TAB(3);"BOLUMU    =................";TAB(50);"DERSIN KODU =..............."
700 LPRINT "�"+STRING$(4,"�")+"�"+STRING$(10,"�")+"�"+STRING$(20,"�")+"�";
710 LPRINT STRING$(4,"�")+"�"+STRING$(5,"�")+"�"+STRING$(19,"�")+"�";
720 LPRINT STRING$(9,"�")+"�"
730 LPRINT "�"+"SIRA"+"�"+"NUMARASI  "+"�"+"ADI SOYADI          "+"�";
740 LPRINT "VIZE"+"�"+"....."+"�"+"BASARI NOTU        "+"�"+"SONUC    "+"�"
750 LPRINT "�NO. �          �"+STRING$(20," ")+"�ORT.�.....";
760 LPRINT "�������������������Ĵ         �":LOCATE 8,1
770 LPRINT "�    �          �"+STRING$(20," ")+"�    �.....�RAK.�YAZIYLA       �         �";
780 LPRINT "�����������������"+STRING$(20,"�")+"������"+STRING$(5,"�");
790 LPRINT "�����������������������������Ķ"
800 FOR I=1 TO 25
810 IF S-1=UZUN THEN GOTO 470
820 GET 1,S
830 FINAL=CVI(FINAL$):VIZE=CVI(VIZE$):YAZI=(VIZE*0.3)+(FINAL*0.7)
840 IF FINAL>VIZE AND FINAL>60 THEN YAZI=FINAL:SONUC$="BASARILI"
850 IF YAZI<50 OR FINAL<50 THEN SONUC$="BASARISIZ" ELSE SONUC$="BASARILI"
860 IF FINAL>=60 AND VIZE<=49 THEN YAZI=FINAL:SONUC$="BASARILI"
870 'X$=LEFT$(STR$(YAZI),5)
880 'NO=INSTR(X$,".")
890 'S1=VAL(LEFT$(X$,NO-1))
900 'N=YAZI-T1
910 'IF S2>=.45 THEN S1=S1+1
920 LPRINT TAB(1);"�";TAB(2);S;TAB(6);"�";TAB(7);NOTU$;TAB(17);"�";TAB(18);_
ISIM$;TAB(38);"�";TAB(39);VIZE;TAB(43);"�";TAB(44);FINAL;TAB(49);"�";:LPRINT TAB(50) USING "###";S1;:LPRINT TAB(54);"�";TAB(55);SAYI$(S1)TAB(70);"�";SONUC$;TAB(79);"�";
930 IF S>=UZUN THEN GOTO 940 ELSE S=S+1:NEXT I
940 LPRINT "�����������������"+STRING$(20,"�")+"����������������������������������������ͼ"
941 LPRINT TAB(5);"NOT...................................................................."
942 LPRINT:LPRINT:LPRINT:LPRINT TAB(5);"�gretim �yesi";TAB(55);"B�l�m Baskani"
950 PRINT "GERI DONMEK ICIN BIR TUSA BASIN":SS$=INPUT$(1)
960 GOTO 60
