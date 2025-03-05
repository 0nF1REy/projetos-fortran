PROGRAM wave_vibrato_generator
    IMPLICIT NONE
    
    INTEGER(KIND=4) :: myByte
    INTEGER :: i, numAmostras, taxaAmostragem, frequencia, tipoOnda
    REAL :: tempo, amplitude, pi, fase
    CHARACTER(LEN=20) :: nomeArquivoPCM, nomeArquivoWAV
    INTEGER :: unidade
    CHARACTER(LEN=200) :: comando
    REAL :: frequenciaVibrato, taxaModulacao

    pi = 3.14159265358979
    taxaAmostragem = 44100
    frequencia = 440
    amplitude = 127
    numAmostras = taxaAmostragem * 5
    tipoOnda = 1
    nomeArquivoPCM = 'advanced_wave.pcm'
    nomeArquivoWAV = 'advanced_wave.wav'

    OPEN(unit=10, file=nomeArquivoPCM, status='replace', form='unformatted')
    DO i = 0, numAmostras-1
        tempo = REAL(i) / REAL(taxaAmostragem)
        
        frequenciaVibrato = frequencia + 5.0 * SIN(2.0 * pi * 5.0 * tempo)

        IF (tipoOnda == 1) THEN
            myByte = INT(amplitude * SIN(2.0 * pi * frequenciaVibrato * tempo) + 128)
        ELSE IF (tipoOnda == 2) THEN
            fase = MOD(tempo * frequenciaVibrato, 1.0)
            IF (fase < 0.5) THEN
                myByte = 255
            ELSE
                myByte = 0
            END IF
        ELSE IF (tipoOnda == 3) THEN
            myByte = INT(amplitude * (2.0 * ABS(2.0 * (tempo * frequenciaVibrato - FLOOR(tempo * frequenciaVibrato + 0.5))) - 1.0) + 128)
        ELSE IF (tipoOnda == 4) THEN
            myByte = INT(amplitude * (2.0 * (tempo * frequenciaVibrato - FLOOR(tempo * frequenciaVibrato))) - 1.0 + 128)
        END IF

        IF (myByte < 0) myByte = 0
        IF (myByte > 255) myByte = 255

        WRITE(10) myByte
    END DO
    CLOSE(10)

    WRITE(comando, '("sox -r 44100 -e unsigned-integer -b 8 -c 1 -t raw ", A, " ", A)') nomeArquivoPCM, nomeArquivoWAV

    PRINT *, "Executando comando: ", TRIM(comando)

    CALL SYSTEM(comando)

    PRINT *, "Arquivo PCM gerado e convertido para WAV com sucesso: ", TRIM(nomeArquivoWAV)

END PROGRAM wave_vibrato_generator