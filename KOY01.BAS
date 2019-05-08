$INCLUDE "KUTUP.BAS"
CONST:
  OPTION BASE 1
  DIM D$(25),SAT%(25),SUT%(25),UZ%(25),TIP%(25)

EKRAN.DIZAYNI:
   EKRAN$=FNEKRAN$("KOY01.SCR")

OKUNACAK.SAHALAR:
    RESTORE OKUNACAK.SAHALAR
    DATA   7,20,15 : ' ADI
    DATA   8,20,15 : ' SOYADI
    DATA   9,20,15 : ' BABA ADI
    DATA  10,20,15 : ' ANA ADI
    DATA  11,20,15 : ' DOGUM YERI
    DATA  12,20,08 : ' DOGUM TARIHI
    DATA  13,20,05 : ' MEDENi HALi
    DATA  14,20,10 : ' DiNi
    DATA  15,16,04 : ' SERi NO
    DATA  15,28,07 : ' NO
    DATA  17,20,15 : ' iL
    DATA  18,20,15 : ' iLCE
    DATA  19,20,15 : ' MAHALLE
    DATA  20,20,15 : ' KOY
    DATA  07,56,06 : ' CiLT NO
    DATA  08,56,03 : ' SAYFA NO
    DATA  09,56,03 : ' KUTUK NO
    DATA  10,56,08 : ' VERiLDiGi YER
    DATA  11,56,08 : ' VERiLiS NEDENi
    DATA  12,56,08 : ' VERiLiS TARiHi
    DATA  13,56,08 : ' C ZDAN KAYIT NO
    DATA  14,56,08 : ' ASKERLiK C ZDAN NO
    DATA  15,56,05 : ' CiNSiYET
    DATA  18,40,24 : ' ADRES 1
    DATA  19,40,24 : ' ADRES 2
    FOR I%=1 TO 25
        READ SAT%(I%),SUT%(I%),UZ%(I%)
        NEXT I%

FILE.OPEN:
     OPEN "KOY01.DAT" FOR RANDOM AS #1 LEN=275
     FIELD #1,1 AS DURUM$ ,274 AS RECORD$
     RUZ=275
     OPEN "KOY02.DAT" FOR RANDOM AS #2 LEN=75
     FIELD #2,11 AS SEHIR$,25 AS MUHADR$,25 AS MUHTAR$
     GET #1,1


READING:
    IF DUZELT=0 THEN KNO$=STR$(INT(LOF(1)/RUZ)+1) : NO$=MID$(KNO$,2)
    NO$=FNINPUT$(5,20,5,NO$,1)
    IF TUS=113 THEN GOSUB SABIT.BILGILERIN.OKUNMASI : GOSUB GORDUM : GOTO READING
    IF TUS=59 THEN
       GOSUB ISIMDEN.ARAMA
       IF VAR=1 THEN KOM$="23456"+CHR$(27) : GOTO JOBS ELSE GOTO READING
       END IF
    IF VAL(NO$)>VAL(KNO$) OR VAL(NO$)<=0 THEN GOTO READING

    IF TUS=27 THEN GOTO MENU
    GOSUB KAYIT.KONTROL
    IF VAR=1 THEN KOM$="23456"+CHR$(27) : GOTO JOBS
DUZELT:
      LL%=1  : TUS=0
      WHILE TUS<>27
        D$(LL%)=FNINPUT$(SAT%(LL%),SUT%(LL%),UZ%(LL%),D$(LL%),0)
        IF TUS=72 AND DUZELT=0 AND LL%=1 THEN GOTO READING
        IF TUS=72 THEN IF LL%>1 THEN LL%=LL%-1
        IF TUS=80 OR TUS=13 THEN IF LL%<25 THEN LL%=LL%+1
        WEND
        KOM$="124"+CHR$(27)
        IF DUZELT=1 THEN RETURN
JOBS:
    KB$=FNMESAJ$("1>KAYIT   2>D ZELT  3>SiL   4>G≥RD M   5>N F S S RETi  6>iKAMETGAH iLM HABERi",KOM$)
    SELECT CASE KB$
           CASE "1" : GOSUB KAYIT     : GOSUB GORDUM
           CASE "2" : GOSUB DUZELTME  : GOSUB GORDUM
           CASE "3" : GOSUB SIL       : GOSUB GORDUM
           CASE "4" : GOSUB GORDUM
           CASE "5" : GOSUB NUFUS.SURETI : GOSUB GORDUM
           CASE "6" : GOSUB IKAMETGAH.KAGIDI : GOSUB GORDUM
           CASE CHR$(27) : GOTO MENU
           END SELECT
    GOTO READING


KAYIT:
     KAYIT%=VAL(NO$)
     KAYIT$=""
     FOR I%=1 TO 25 : KAYIT$=KAYIT$+D$(I%) : NEXT I%
     LSET RECORD$=KAYIT$
     LSET DURUM$="1"
     PUT #1,KAYIT%
     RETURN

SiL:
     KB$=FNMESAJ$("Silmekte Eminmisiniz ? (Evet Hayir)","EH")
     IF KB$="H" THEN RETURN
     LSET RECORD$="iPTAL"+STRING$(RUZ," ")
     LSET DURUM$="2"
     KAYIT%=VAL(NO$)
     PUT #1,KAYIT%
     RETURN

DUZELTME:
     DUZELT=1
     GOSUB DUZELT
     KB$=FNMESAJ$("D│zeltmeleri Kayit Edeyimmi ? (Evet / Hayir)","EH")
     IF KB$="H" THEN RETURN
     GOSUB KAYIT
     DUZELT=0
     RETURN


GORDUM:
     EKRAN$=FNEKRAN$("KOY01.SCR")
     ERASE D$
     DIM D$(25)
     DUZELT=0
     RETURN

MENU:
     CLS
     END

KAYIT.KONTROL:
     KAYIT%=VAL(NO$)
     GET #1,KAYIT%
     IF DURUM$<>"1" THEN VAR=0 : RETURN
     VAR=1  : GOSUB EKRAN.YAZ
     RETURN
EKRAN.YAZ:
     KOL%=1
     FOR I%=1 TO 25
        D$(I%)=MID$(RECORD$,KOL%,UZ%(I%))
        KOL%=KOL%+UZ%(I%)
        NEXT I%
     LOCATE 5,20 : PRINT MID$(NO$+"      ",1,5)
     FOR I%=1 TO 14
        LOCATE SAT%(I%),SUT%(I%),0 : PRINT D$(I%)
        NEXT I%
     IF YARIM=1 THEN RETURN
     FOR I%=15 TO 25
        LOCATE SAT%(I%),SUT%(I%),0 : PRINT D$(I%)
        NEXT I%
   RETURN

ISIMDEN.ARAMA:
    ADET%=INT(LOF(1)/RUZ)
    IF ADET%=0 THEN
          KB$=FNMESAJ$("Dosyada Bos..",CHR$(27)+CHR$(13)+CHR$(32))
          RETURN
          END IF
    EKRAN$=FNEKRAN$("KOY02.SCR")   : 'KOY02.SCR Ekranini Yukleniyor.
    MAX.SAYFA%=INT(ADET%/15)+1     : 'Olusacak Maximum Sayfa Adedi . 15 Satirlik
    IF ADET%/15=INT(ADET%/15) THEN MAX.SAYFA=MAX.SAYFA-1
    SAYFA%=1                       : 'Birinci Sayfa
    GOSUB SAYFA.OKU.YAZ            : 'Bir Sayfalik Oku ve Yaz.
    GOSUB YENI.SATIR               : 'ilk Satiri REVERS Yap.
    GOTO SATIR.KONTROL             : 'Satir Kontrollerini Yap.

SAYFA.OKU.YAZ:
    FOR I%=1 TO 15
        LOCATE 4+I%,42
        PRINT SPACE$(30)
        NEXT I%
    FIRST%=(SAYFA%-1)*15+1        : 'Yazilan Sayfadaki ilk Kaydin Gorunur Adresi
    KK%=FIRST% : YENI%=1 : ESKI%=1
    FOR I%=1 TO 15
         GET #1,KK%
         LOCATE 4+I%,42,0 : PRINT MID$(RECORD$,1,30) : MAXSATIR%=I%
         IF ADET%=KK% THEN GOSUB  YENI.SATIR : RETURN
         KK%=KK%+1
         NEXT I%
         GOSUB YENI.SATIR
    RETURN

YENI.SATIR:
    GET #1,FIRST%+ESKI%-1
    COLOR 7,0 : COLOR 10,0
    LOCATE 4+ESKI%,42 : PRINT MID$(RECORD$,1,30)
    GET #1,FIRST%+YENI%-1
    COLOR 0,7
    LOCATE 4+YENI%,42 : PRINT MID$(RECORD$,1,30)
    COLOR 7,0 : COLOR 10,0
    ESKI%=YENI%
    YARIM=1 : NO$=MID$(STR$(FIRST%+YENI%-1),2) :  GOSUB EKRAN.YAZ
    RETURN
SATIR.KONTROL:
    KB$="" : VAR=0
    WHILE KB$="" : KB$=INKEY$ : WEND
    IF LEN(KB$)=1 THEN KB=ASC(KB$) ELSE KB=ASC(RIGHT$(KB$,1))
    IF KB=27 THEN GOSUB GORDUM : RETURN
    IF KB=13 THEN
          GOSUB GORDUM
          YARIM=0
          GOSUB EKRAN.YAZ
          VAR=1 : RETURN
          END IF
    IF KB=72 THEN
       IF YENI%=1 THEN BEEP ELSE YENI%=YENI%-1 : GOSUB YENI.SATIR
       END IF
    IF KB=80 THEN
       IF YENI%=MAXSATIR% THEN BEEP ELSE YENI%=YENI%+1 : GOSUB YENI.SATIR
       END IF
    IF KB=73 THEN
       IF SAYFA%=1 THEN BEEP ELSE SAYFA%=SAYFA%-1 : GOSUB SAYFA.OKU.YAZ
       END IF
    IF KB=81 THEN
       IF SAYFA%=MAX.SAYFA% THEN BEEP ELSE SAYFA%=SAYFA%+1 : GOSUB SAYFA.OKU.YAZ
       END IF
  GOTO SATIR.KONTROL

NUFUS.SURETI:

LPRINT CHR$(27);"W1";TAB(12);"N FUS C ZDAN SURETi";CHR$(27);"W0"
LPRINT "имммммммммммммммкмммммммммммммммммкммммммммммммммммммммммммммммммммм╩"
LPRINT "╨ SERi NO.      ╨ ";D$(9);" - ";D$(10);"  ╨                   +-----------+ ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   |           | ╨"
LPRINT "╨ SOYADI        ╨ ";D$(2);" ╨                   |           | ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   | FOTOGRAF  | ╨"
LPRINT "╨ ADI           ╨ ";D$(1);" ╨                   |           | ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   |           | ╨"
LPRINT "╨ BABA ADI      ╨ ";D$(3);" ╨                   +-----------+ ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддрддддддддддддддддд╤"
LPRINT "╨ ANA ADI       ╨ ";D$(4);" ╨ MEDENi HALi   ╨ ";D$(7);"           ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддвддддддддддддддддд╤"
LPRINT "╨ DOGUM YERi    ╨ ";D$(5);" ╨ DiNi          ╨ ";D$(8);"      ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддвддддддддддддддддд╤"
LPRINT "╨ DOGUM TARiHi  ╨ ";D$(6);"        ╨ CiNSiYETi     ╨ ";D$(23);"           ╨"
LPRINT "гдддддддддддддддпдддддддддддддддддпдддддддддддддддпддддддддддддддддд╤"
LPRINT "╨               N FUS K T G NDE KAYITLI OLDUGU YER                  ╨"
LPRINT "гдддддддддддддддрдддддддддддддддддрдддддддддддддддрддддддддддддддддд╤"
LPRINT "╨ iLi           ╨ ";D$(11);" ╨ CiLT NO       ╨ ";D$(15);"          ╨"
LPRINT "╨ iL─E          ╨ ";D$(12);" ╨ SAYFA NO      ╨ ";D$(16);"             ╨"
LPRINT "╨ K≥Y           ╨ ";D$(13);" ╨ K T K SIRA NO ╨ ";D$(17);"             ╨"
LPRINT "╨ MAHALLE       ╨ ";D$(14);" ╨ ASKER.C Z.NO. ╨ ";D$(22);"        ╨"
LPRINT "гдддддддддддддддпдддддддддддддддддпдддддддддддддддпддддддддддддддддд╤"
LPRINT "╨                      N FUS C ZDANI                                ╨"
LPRINT "гдддддддддддддддрдддддддддддддддддрдддддддддддддддрддддддддддддддддд╤"
LPRINT "╨ VERiLDiGi YER ╨ ";D$(18);"        ╨ VERiLiS NEDENi╨ ";D$(19);"        ╨"
LPRINT "╨ VERiLiS TARiHi╨ ";D$(20);"        ╨ C ZDAN KAY.NO.╨ ";D$(21);"        ╨"
LPRINT "гдддддддддддддддпдддддддддддддддддпдддддддддддддддпддддддддддддддддд╤"
LPRINT "╨                                                                   ╨"
LPRINT "╨      ";MUHADR$;"         ";DATE$;"                 ╨"
LPRINT "╨      ";MUHTAR$;"                                    ╨"
LPRINT "╨                                                                   ╨"
LPRINT "хммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╪"
RETURN

IKAMETGAH.KAGIDI:
    LOCATE 24,1,0 : PRINT "NE AMA─LA VERiLiYOR [";SPACE$(45);"]";CHR$(7);
    AMAC$="iSTEGi  ZERiNE "
    AMAC$=FNINPUT$(24,22,45,AMAC$,0)
    IF TUS=27 THEN RETURN
    UNVAN$=FNRTRIM$(D$(1))+" "+FNRTRIM$(D$(2))+STRING$(36,"_") : UNVAN$=MID$(UNVAN$,1,36)
    ADRES$=FNRTRIM$(D$(24))+" "+FNRTRIM$(D$(25))+STRING$(48,"_") : ADRES$=MID$(ADRES$,1,48)

LPRINT CHR$(27);"W1";TAB(14);"iKAMETGAH KAGIDI";CHR$(27);"W0"
LPRINT "имммммммммммммммкмммммммммммммммммкммммммммммммммммммммммммммммммммм╩"
LPRINT "╨               ╨                 ╨                   +-----------+ ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   |           | ╨"
LPRINT "╨ SOYADI        ╨ ";D$(2);" ╨                   |           | ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   | FOTOGRAF  | ╨"
LPRINT "╨ ADI           ╨ ";D$(1);" ╨                   |           | ╨"
LPRINT "гдддддддддддддддвддддддддддддддддд╤                   |           | ╨"
LPRINT "╨ BABA ADI      ╨ ";D$(3);" ╨                   +-----------+ ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддрддддддддддддддддд╤"
LPRINT "╨ ANA ADI       ╨ ";D$(4);" ╨ MEDENi HALi   ╨ ";D$(7);"           ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддвддддддддддддддддд╤"
LPRINT "╨ DOGUM YERi    ╨ ";D$(5);" ╨ DiNi          ╨ ";D$(8);"      ╨"
LPRINT "гдддддддддддддддвдддддддддддддддддвдддддддддддддддвддддддддддддддддд╤"
LPRINT "╨ DOGUM TARiHi  ╨ ";D$(6);"        ╨ CiNSiYETi     ╨ ";D$(23);"           ╨"
LPRINT "гдддддддддддддддпдддддддддддддддддпдддддддддддддддпддддддддддддддддд╤"
LPRINT "╨               N FUS K T G NDE KAYITLI OLDUGU YER                  ╨"
LPRINT "гдддддддддддддддрдддддддддддддддддрдддддддддддддддрддддддддддддддддд╤"
LPRINT "╨ iLi           ╨ ";D$(11);" ╨ CiLT NO       ╨ ";D$(15);"          ╨"
LPRINT "╨ iL─E          ╨ ";D$(12);" ╨ SAYFA NO      ╨ ";D$(16);"             ╨"
LPRINT "╨ K≥Y           ╨ ";D$(13);" ╨ K T K SIRA NO ╨ ";D$(17);"             ╨"
LPRINT "╨ MAHALLE       ╨ ";D$(14);" ╨ ASKER.C Z.NO. ╨ ";D$(22);"        ╨"
LPRINT "гдддддддддддддддпдддрдддддддддддддпдддддддддддддддпддддддддддддддддд╤"
LPRINT "╨ NE i─iN VERiLDiGi ╨ ";AMAC$;" ╨"
LPRINT "гдддддддддддддддддддпддддддддддддддддддддддддддддддддддддддддддддддд╤"
LPRINT "╨  YUKARIDA FOTOGRAFI YAPISIK  ";UNVAN$;"       ╨"
LPRINT "╨  ";SEHIR$;" iLi ";ADRES$;" ╨"
LPRINT "╨  ADRESiNDE OTURMAKTA OLDUGUNU BiLDiRiR iKAMETGAH KAGIDI VERiLDi.  ╨"
LPRINT "╨                                                                   ╨"
LPRINT "╨                                                                   ╨"
LPRINT "╨      ";MUHADR$;"         ";DATE$;"                 ╨"
LPRINT "╨      ";MUHTAR$;"                                    ╨"
LPRINT "╨                                                                   ╨"
LPRINT "хммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╪"
RETURN

SABIT.BILGILERIN.OKUNMASI:
   CLS
   LOCATE 8, 10,0: PRINT "SABiT BiLGiLERiN GiRiLMESi"
   LOCATE 9, 10,0: PRINT "____________________";STRING$(26,"_")
   LOCATE 10,10,0: PRINT "ViLAYET'in ADI     [";SPACE$(11);"]"
   LOCATE 11,10,0: PRINT "MUHTAR MAHALLE ADI [";SPACE$(25);"]"
   LOCATE 12,10,0: PRINT "MUHTAR ADI SOYADI  [";SPACE$(25);"]"
   GET #2,1
   LOCATE 10,30,0 : PRINT SEHIR$   : A1$=SEHIR$
   LOCATE 11,30,0 : PRINT MUHADR$  : A2$=MUHADR$
   LOCATE 12,30,0 : PRINT MUHTAR$  : A3$=MUHTAR$
A1:
   A1$=FNINPUT$(10,30,11,A1$,0)
   IF TUS=72 THEN GOTO A3
A2:
   A2$=FNINPUT$(11,30,25,A2$,0)
   IF TUS=72 THEN GOTO A1
A3:
   A3$=FNINPUT$(12,30,25,A3$,0)
   IF TUS=72 THEN GOTO A2
   LSET SEHIR$=A1$
   LSET MUHADR$=A2$
   LSET MUHTAR$=A3$
   PUT #2,1
   RETURN
