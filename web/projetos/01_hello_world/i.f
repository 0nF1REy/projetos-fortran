      PROGRAM hello_world
      INTEGER ASCII_CODES(13)
      CHARACTER*1 CHAR_VAL
      CHARACTER*14 HELLO_WORLD_STRING
      INTEGER I
      INTEGER IO_UNIT
      PARAMETER (IO_UNIT = 12)
      CHARACTER*20 DATE_STR
      CHARACTER*20 TIME_STR
      CHARACTER*2 DAY
      CHARACTER*2 MONTH
      CHARACTER*4 YEAR
      CHARACTER*20 FORMATTED_DATE
      CHARACTER*15 EQUAL_LINE
      
      DATA ASCII_CODES / 72, 101, 108, 108, 111, 44,
     &                 32, 87, 111, 114, 108, 100, 33 /
      DATA EQUAL_LINE /'============='/

      HELLO_WORLD_STRING = '              '

      DO 10 I = 1, 13
         CHAR_VAL = CHAR(ASCII_CODES(I))
         HELLO_WORLD_STRING(I:I) = CHAR_VAL
10    CONTINUE

      OPEN (UNIT=IO_UNIT,FILE='data-hora.txt',STATUS='UNKNOWN')

      CALL SYSTEM('date "+%Y-%m-%d %H:%M:%S" > output.txt',I)

      DATE_STR = ' '
      TIME_STR = ' '
        
      OPEN (UNIT=14, FILE='output.txt', STATUS='OLD')
      READ(14,*) DATE_STR, TIME_STR
      CLOSE(UNIT=14)

      CALL SYSTEM('rm output.txt',I)
          
      YEAR = DATE_STR(1:4)
      MONTH = DATE_STR(6:7)
      DAY = DATE_STR(9:10)

      FORMATTED_DATE = DAY // "/" // MONTH // "/" // YEAR
      
      WRITE (IO_UNIT, *) EQUAL_LINE
      WRITE (IO_UNIT, *) 'Data atual: ' , FORMATTED_DATE
      WRITE (IO_UNIT, *) 'Hora atual: ', TIME_STR
      WRITE (IO_UNIT, *) EQUAL_LINE
    
      CLOSE (UNIT=IO_UNIT)
      
      WRITE(*,*) EQUAL_LINE
      WRITE(*,*) HELLO_WORLD_STRING
      WRITE(*,*) EQUAL_LINE
      STOP
      END