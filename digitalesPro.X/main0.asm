;*******************************************************************************
;                                                                              *
;    Filename: main0                                                           *
;    Date: 06/03/19                                                            *
;    File Version: 1.1.1                                                       *
;    Author: Killtutor                                                         *
;    Company: UCV                                                              *
;    Description: Project for the digitales subject                            *
;                                                                              *
;*******************************************************************************

#include "p16f676.inc"
    
 __CONFIG _FOSC_INTRCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _BOREN_OFF & _CP_OFF & _CPD_OFF  ;Solo uso el oscilador interno sin salida de reloj, las demas caracteristicas apagadas
 
;VARIABLE DEFINITION 
;*******************************************************************************

PWMDATA		EQU	    5FH	    ; DATA QUE MANDA EL USUARIO PWM
T0C		EQU	    5EH	    ; CONTADAOR PARA EL RELOJ 0
DATO		EQU	    5DH	    ; DATO MANDADO
BUFFER		EQU	    5CH	    ;BUFFER REC AND SEND
TEMP		EQU	    5BH	    ;VARIABLE TEMPORAL
COUNT		EQU	    5AH	    ;CONTADOR
W_TEMP		EQU	    59H	    ; w register for context saving (ACCESS)
STATUS_TEMP	EQU	    58H	    ; status used for context saving
BSR_TEMP	EQU	    57H	    ; bank select used for ISR context saving
CUENTA		EQU	    56H	    ;CONTADOR PARA ENVIAR MENSAJE DE BIENVENIDA
SIZE		EQU	    55H	    ;TAMAÑO DE LA CADENA A MANDAR

;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;*******************************************************************************
; INTERRUPT VECTOR
;*******************************************************************************
ISR       CODE    0x0004           ; interrupt vector location
    GOTO    INTERRUPT

;SUBRUTINA PARA ESPERAR 1 BAUD O 1/2 BAUD        
BAUD                           ; A 1200 BAUD EL PERIODO POR BIT ES 833.3 US         
    movlw   89h			; 1 US   (BAUD RATE CONSTANT)  137'DECIMAL'     
    movwf   COUNT   		; 1 US  MUEVO W A COUNT
baud1        
    decfsz  COUNT,1		; 1 US (+ 1 US MORE IF SKIP)   CUENTO HASTA 137     
    goto    baud1	        ; 2 US REGRESO A BAUD1 (CICLO)                               
				; SE ACABA LUEGO DE 1+1+1+1+3x137+1 = 416 US 
HALFBAUD			;	AQUI REPITO TODO OTRA VEZ.. PARA EL MEDIO BAUD FALTANTE
    movlw   89h			; 1 US        ASI LOS PUEDO DIVIDIR EN 1 BAUD Y 1/2 BAUD
    movwf   COUNT		; 1 US
hbaud1        
    decfsz  COUNT,F		; 1 US (+ 1 US MORE IF SKIP)       
    goto    hbaud1		; 2 US        
    RETURN   			; SALGO LUEGO DE 1+1+1+1+3x137+1 = 416 US  (TOTAL=832 US O 416 US)
    
; SUBRUTINA QUE RECIBE SERIAL
RECEIVE
RE0
    BTFSC	PORTA, 2	    ;SALTO EL BIT DE START
    GOTO	RE0		    ;SINO SIGO ESPERANDO EL BIT DE START
    MOVLW	08H		    ; QUIERO RECIBIR 8 BITS
    MOVWF	TEMP		    ;TEMP = 8 Ó 1000
    CLRF	BUFFER	    	    ;LIMPIO EL REGISTRO BUFFER
    CALL	HALFBAUD	    ;ESPERO MEDIO CICLO
    BTFSC	PORTA, 2	    ;SALTO SI SIGO ESTANDO EN EL BIT DE START
    GOTO	RE0		    ;FUE UN SPIKE ASI Q SIGO ESPERANDO EL BIT DE START
RE1
    CALL	BAUD		    ;SUPONIENDO  SIGO N EL BIT START ESPERO UN CICLO PARA LEER EL SIGUINTE BIT
    BCF		STATUS, 0	    ; LIMPIO EL CARRY BIT
    RRF		BUFFER, 1	    ;ROTO MSB LE METO EL CARRY(0) EN EL LSB
    BTFSC	PORTA,2		    ;eL PUERTO ESTA EN 0?
    BSF		BUFFER,7	    ;NO, METO EN EL ULTIMO BIT UN 1 
    DECFSZ	TEMP, 1		    ;YA TENGO LOS 8BITS?
    GOTO	RE1		    ;NO, TODAVI NO LOS TIENES SIGUE RECIBIENDO
    CALL	BAUD		    ;ESPERO UN BAUD ESPERANDO EL BIT DE PARADA	
    MOVF	BUFFER, 0	    ;METO BUFFER EN W
    GOTO	INTLIST		    ;LISTA LA INTERRUPCION YA RECIBÍ LA INSTRUCCION
    
RECEIVED
RE0D
    BTFSC	PORTA, 2	    ;SALTO EL BIT DE START
    GOTO	RE0D		    ;SINO SIGO ESPERANDO EL BIT DE START
    MOVLW	08H		    ; QUIERO RECIBIR 8 BITS
    MOVWF	TEMP		    ;TEMP = 8 Ó 1000
    CLRF	BUFFER	    	    ;LIMPIO EL REGISTRO BUFFER
    CALL	HALFBAUD	    ;ESPERO MEDIO CICLO
    BTFSC	PORTA, 2	    ;SALTO SI SIGO ESTANDO EN EL BIT DE START
    GOTO	RE0D		    ;FUE UN SPIKE ASI Q SIGO ESPERANDO EL BIT DE START
RE1D
    CALL	BAUD		    ;SUPONIENDO  SIGO N EL BIT START ESPERO UN CICLO PARA LEER EL SIGUINTE BIT
    BCF		STATUS, 0	    ; LIMPIO EL CARRY BIT
    RRF		BUFFER, 1	    ;ROTO MSB LE METO EL CARRY(0) EN EL LSB
    BTFSC	PORTA,2		    ;eL PUERTO ESTA EN 0?
    BSF		BUFFER,7	    ;NO, METO EN EL ULTIMO BIT UN 1 
    DECFSZ	TEMP, 1		    ;YA TENGO LOS 8BITS?
    GOTO	RE1D		    ;NO, TODAVI NO LOS TIENES SIGUE RECIBIENDO
    CALL	BAUD		    ;ESPERO UN BAUD ESPERANDO EL BIT DE PARADA	
    MOVF	BUFFER, 0	    ;METO BUFFER EN W
    RETURN
    
;SUBRUTINA QUE ENVIA SERIAL
SEND
    MOVWF	BUFFER		    ;BUFFER=W EN 2 LLEVARE EL CONTADOR
    MOVLW	08H		    ;W=8
    MOVWF	TEMP		    ;TEMP=W
    BCF		PORTA, 1	    ;BIT DE START
    CALL	BAUD		    ;ESPERO 1 BAUD
S1
    RRF		BUFFER, 1	    ;ROTO Y GUARDO EN BUFFER
    BTFSS	STATUS, 0	    ;PRUEBO SI CARRY ES 1
    BCF		PORTA, 1	    ;SI ES CERO MANDO 0
    BTFSC	STATUS, 0	    ;PRUEBO SI ES 0 EL CARRY
    BSF		PORTA, 1	    ;SI ES UNO MANDO 1
    MOVF	PORTA, 0		;W=PÒRTA
    CALL	BAUD		    ;ESPERO 1 BAUD
    DECFSZ	TEMP, 1		    ;VOY DISMINUYENDO TEMP
    GOTO	S1		    ;MIENTRAS NO SEA 0 SIGO ENVIANDO
    ;RRF		BUFFER, 1	    ;DEVUELVO EL BUFFER AL ORIGINAL
    BSF		PORTA, 1	    ;BIT DE STOP
    CALL	BAUD		    ;BAUD 1
    RETLW	0		    ;REGRESO CON W EN CERO 
    
;SUBRUTINA INTERRUPCION TIMER 1 (MASTER PWM) FRECUENCIA SEÑAL PWM 61HZ CADA 16.4MS ENCINDO EL LED
T1INT
    BSF	    PORTA, 4		    ;PRENDO EL LED (PWM)
    BCF	    T1CON,0		    ;PARO TIMER1
    MOVLW   06H			    ;W=6 (TOMANDO EN CUENTA LOS CICLOS Q ESTA SIN CONTAR)
    MOVWF   TMR1L		    ;TMR1L=W=6
    MOVLW   0C0H		    ;W=C0H=01000000
    MOVWF   TMR1H		    ;TMR1H=W=01000000
    BSF	    T1CON, 0		    ;ARRANCO TIMER1 DE NUEVO
    MOVF    PWMDATA, 0		    ;W=PWMDATA PARA METERSELO AL CONTADOR DEL T0
    ADDLW   01H			    ;AÑADO 1 PUESTO Q LO PRIMERO Q HAGO ES RESTAR 
    MOVWF   T0C			    ;METO W AL CONTADOR DE T0
    CLRF    TMR0		    ;LIMPIO TIMER0
    BCF	    PIR1, 0		    ;RESETEO EL BIT DE INTERRUPCION TIMER1
    GOTO    LISTO		    ;REGRESO AL FINAL DE LA INTERRUPCION
    
;SUBRUTINA INTERRUPCION TIMER 0 (SLAVE DUTYCYCLE) SE EJECUTA CADA 64US    
;CADA 64US DISMINUYO T0C <0-255>  Y CUANDO LLEGO A 0 APGO EL LED DELAY DE 2US
T0INT
    BCF	    STATUS, 5		    ;BANK 0
    CLRF    TMR0
    BCF	    INTCON, 2		    ;RESETEO EL BIT DE INTERRUPCION TIMERO
    DECFSZ  T0C, 1		    ;DIMINUYO T0C SALTO SI ES 0 EL RESULTADO 1US +1 SI SALTA
    GOTO    LISTO		    ;yA DECREMENTE T0C AS Q ME VOY AL FINAL DE LA INTERRUPCION 
    BCF	    PORTA, 4		    ;APAGO EL LED (PWM) (MIN=66US)--> ENCINDE POR 66US CON DUTYCYCLE EN 0%
    GOTO    LISTO		    ;REGRESO AL FINAL DE LA INTERRUPCION
    
    
INTERRUPT
    ;GUARDO W Y STATUS PORSIACA
    MOVWF  W_TEMP		    ;copy W to temp register, could be in either bank
    SWAPF  STATUS,0		    ;swap status to be saved into W
    BCF    STATUS,5		    ;change to bank 0 regardless of current bank
    MOVWF  STATUS_TEMP		    ;save status to bank 0 register
    BCF		INTCON,7	    ;DESHABILITO LAS INTERRUPCIONES GENERALES 
    BCF		INTCON,6	    ;DESHABILITO LAS INTERRUPCIONES PERIFERICAS
    BCF		IOCA, 2		    ;DESHABILITO INTERRUPCIONES PUERTO A

    ;COMIENZA LA INTERRUPCIÓN
    BTFSC   PIR1,0		;PRUEBO SI FUE EL TIMER1 EL QUE SE DESBORDO
    GOTO    T1INT		;SI ES TIMER1 ME VOY A LA FUNCION DE TIMER 1
    BTFSC   INTCON,2		;PRUEBO A VER SI FUE TIMER 0 QUE SE DESBORDO
    GOTO    T0INT		;LLAMO A LA RUTINA DE TIMER0
    BTFSC   INTCON, 0		;PRUEBO SI LA INTERRUPCION FUE DEL PORTA
    GOTO    RECEIVE		;LLAMO A LA RUTINA DEL RECEPTOR
    BTFSC   PIR1, 6		;VERIFICO SI FUE EL ADC
    GOTO    FOUR1		;MANEJO LA CONVERSION
    
LISTO
    BSF		INTCON, 7	    ;RESTABLEZCO LAS INTERRUPCIONES GENERALES
    BSF		INTCON, 6	    ;RESTABLEZCO LAS INTERRUPCIONES PERIFERICAS
    BSF		IOCA, 2		    ;RESTABLEZCO INTERRUPCIONES PUERTO A
     ;RESTAURO W Y STATUS
    SWAPF  STATUS_TEMP,0 ;sWap STATUS_TEMP register into W, sets bank to original state
    MOVWF  STATUS	    ;move W into STATUS register
    SWAPF  W_TEMP,1	    ;swap W_TEMP
    SWAPF  W_TEMP,0	    ;swap W_TEMP into W
    RETFIE
 
ONE 
    MOVLW   01H			    ; W = 0000001
    BTFSS   PORTA, 3		    ; Si RA3 es 1 salta (w=0000001) SINO W=0000000
    MOVLW   00H			    ; W = 0000000
    CALL    SEND		    ; ENVIO W A LA CONSOLA      
    GOTO    LISTO
    
TWO
    CALL    RECEIVED		    ; RECIBO EL DATO 
    MOVWF   DATO			; LO que este en w lo meto en dato
    BSF	    PORTA, 5		    ; RA5 LO PONGO EN 1 (PRENDE EL LED)
    BTFSS   DATO, 0		    ; SI lsb of dato es 1 salta sino sigue
    BCF	    PORTA, 5		    ; RA5 LO PONGO EN 0 (APAGO EL LED)
    GOTO    LISTO
    
TREE
    CALL    RECEIVED		    ; RECIBO EL DATO 
    MOVWF   PWMDATA		    ; METO EL DATO Q RECIBI A PWMDATA
    GOTO    LISTO		    ; WAITING INSTRUCTIONS
    
FOUR0
    BSF	    ADCON0, 1		    ; EMPIEZO LA CONVERSION ADC PAPAS Y LISTO!
    GOTO    LISTO		    ;MIENTRAS CONVIERTO RECIBO DATA PORSIACA
FOUR1
    BSF	    STATUS, 5		    ;MUEVO AL BANCO 1
    MOVWF   ADRESL		    ;RESULTADO ADC A W
    CALL    SEND		    ;ENVIO A LA CONSOLA
    BCF	    PIR1, 6		    ;RESETEO LAS INTERRUPCIONES DEL ADC
    GOTO    LISTO
    
    
;SUBRUTINA QUE DECIDE QUE COSA HACER LUEGO DE RECIBIR DATA
INTLIST
    SUBLW   01H			    ;RESTO 1 A W PARA VER SI ES ESTA
    BTFSC   STATUS, 2		    ;ES CERO? Z=1 ?
    GOTO    ONE			    ;SI ES CERO INSTRUCTION 1
    ADDLW   01H			    ; LE DEVUELVO LO Q LE QUITE
    SUBLW   02H			    ; RESTO 2 A W
    BTFSC   STATUS, 2		    ;ES CERO? Z=1 ?
    GOTO    TWO			    ; SI, ES 0 FUNCTION 2
    ADDLW   02H			    ; LE DEVUELVO LO Q LE QUITE
    SUBLW   03H			    ; RESTO 3
    BTFSC   STATUS, 2		    ; ES CERO? Z=1 ?
    GOTO    TREE		    ; SI, ES 0 FUNCTION 2
    ADDLW   03H			    ; LE DEVUELVO LO QUE LE QUITE
    SUBLW   11H			    ; RESTO 11H
    BTFSC   STATUS, 2		    ; ES CERO? Z=1 ?
    GOTO    FOUR0		    ; SI, ES 0 FUNCTION 2
    GOTO    LISTO		    ; CICLO INFINITO

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

;BIENVENIDA OFICIAL
GUERCOME 	addwf PCL,1 		;suma la posición del elemento a la del PC
	retlw 0x42 		;retorna en w el código ASCII de la B
	retlw 0x69 		;retorna en w el código ASCII de la I
	retlw 0x65 		;retorna en w el código ASCII de la E
	retlw 0x6E 		;retorna en w el código ASCII de la N  
	retlw 0x76 		;retorna en w el código ASCII de la V
	retlw 0x65 		;retorna en w el código ASCII de la E
	retlw 0x6E 		;retorna en w el código ASCII de la N
	retlw 0x69 		;retorna en w el código ASCII de la I
	retlw 0x64 		;retorna en w el código ASCII de la D
	retlw 0x6F 		;retorna en w el código ASCII de la O
	retlw 0x73 		;retorna en w el código ASCII de la S
	retlw 0x0D 		;retorna en w el código ASCII de la CR  
;ESPERO INSTRUCCION
ESPERAI 	addwf PCL,1 		;suma la posición del elemento a la del PC
	retlw 0x45 		;retorna en w el código ASCII de la E
	retlw 0x73 		;retorna en w el código ASCII de la S
	retlw 0x70     		;retorna en w el código ASCII de la P
	retlw 0x65 		;retorna en w el código ASCII de la E  
	retlw 0x72 		;retorna en w el código ASCII de la R
	retlw 0x6F 		;retorna en w el código ASCII de la O
	retlw 0x20 		;retorna en w el código ASCII de la SPACE
	retlw 0x49 		;retorna en w el código ASCII de la I
	retlw 0x4E 		;retorna en w el código ASCII de la N
	retlw 0x53 		;retorna en w el código ASCII de la S
	retlw 0x3A 		;retorna en w el código ASCII de la :
	retlw 0x20 		;retorna en w el código ASCII de la CR  
;ESPERO DATO STRING	
ESPERAD 	addwf PCL,1 		;suma la posición del elemento a la del PC
	retlw 0x45 		;retorna en w el código ASCII de la E
	retlw 0x73 		;retorna en w el código ASCII de la S
	retlw 0x70     		;retorna en w el código ASCII de la P
	retlw 0x65 		;retorna en w el código ASCII de la E  
	retlw 0x72 		;retorna en w el código ASCII de la R
	retlw 0x6F 		;retorna en w el código ASCII de la O
	retlw 0x20 		;retorna en w el código ASCII de la SPACE
	retlw 0x44 		;retorna en w el código ASCII de la D
	retlw 0x41 		;retorna en w el código ASCII de la A
	retlw 0x54 		;retorna en w el código ASCII de la T
	retlw 0x4F    		;retorna en w el código ASCII de la :
	retlw 0x3A 		;retorna en w el código ASCII de la CR  
	
START;VERIFICADO TODO.. LISTO PAPU
    ;LIMPIANDO TODO vERIFICADO, FUNCIONA AL PELO
    ;CLRF    STOPT0	
    BCF	    STATUS, 5		    ; BANK0
    CLRF    ADCON0
    CLRF    PORTA		    ;LIMPIO PUERTOA
    CLRF    PORTC		    ;LIMPIO PUERTO C
    BSF	    STATUS, 5		    ;BANK1
    CLRF    TRISA		    ;LIMPIO CONFIG PORTA
    CLRF    TRISC		    ;LIMPIO CONFIG PORTC
    CLRF    ANSEL		    ;LIMPIO ANSEL REGISTRO
    CLRF    IOCA		    ;INTERRUPCIONES EN PUERTO
    BCF	    STATUS, 5		    ;BANK0
    
    ;Configurando ADC  --vERIFICADO, FUNCIONA PEPA
    BSF	    ADCON0, 7		    ; CONFIGURANDO ADC JUSTIFICADO DERECHA
    BSF	    STATUS, 5		    ; BANK1
    BSF	    ADCON1, 6		    ; CONFIGURANDO ADC 4TOSC
    BSF	    ANSEL, 0		    ; CONFIGURANDO PIN ADC A CONVERTIR
    
    
    ;CONFIGURANDO INTERRUPCIONES
    MOVLW   0f8h				    ;ACTIVO TODAS LAS INTERRUPCIONES
    MOVWF   INTCON			    ;CON ESTA CONFIG 11111000
    ;BCF	    INTCON, 5
    BCF	    PIE1,0			    ; ENABLES TIMER1 INTERRUPTS
    MOVLW   B'11000101'
    MOVWF   OPTION_REG
    BCF	    STATUS, 5			    ; BANK0
    MOVLW   00H				    ;W=0
    MOVWF   TMR1L			    ;TMR1L=W=0
    MOVLW   0C0H				    ;W=40H=01000000
    MOVWF   TMR1H			    ;TMR1H=W=01000000
    MOVLW   0DH				    ;00001111 T1CON CONFIG
    MOVWF   T1CON			    ;CONFIGURO T1
    
    
    ;CONFIGURACION DE PUERTOS
    BSF	    STATUS, 5		    ; BANK1
    MOVLW   0dh			    ; Configurando TRISA para O, O, I, I, O, I
    MOVWF   TRISA		    ; Metiendo la configuración del TRISA desde W
    BSF	    IOCA , 2		    ;INTERRUPCIONES EN EL PUERTO A2 MANAO
    BCF	    STATUS, 5			    ; BANK0
    ;LIMPIANDO COSAS
    MOVLW   0FFH		    ; W= 000111B DUTY CYCLE A LA MITAD
    MOVWF   PWMDATA		    ; PWMDATA=W
    BSF	    PORTA, 1		    ; tx a 1
    
;BIENVENIDOS MESSAGE  
    MOVLW   0CH			    ;W=12
    MOVWF   SIZE		    ;SIZE=W
    MOVLW   00H			    ;W=0
    MOVWF   CUENTA		    ;CUENTA=W
BIENVENIDA    
    MOVWF   CUENTA		    ;CUENTA=W
    CALL    GUERCOME		    ;OBTENGO EL CARACTER
    CALL    SEND		    ;LO ENVIO
    MOVF    CUENTA, 0		    ;W=CUENTA
    ADDLW   01H			    ;W=W+1
    DECFSZ  SIZE, 1		    ;SIZE=SIZE-1 SALTA CUANDO LLEGA A 0
    GOTO    BIENVENIDA
;ESPERO INS MESSAGE   
    MOVLW   0BH			    ;W=11
    MOVWF   SIZE		    ;SIZE=W
    MOVLW   00H			    ;W=1
    MOVWF   CUENTA		    ;CUENTA=W
BIENVENIDA2    
    MOVWF   CUENTA		    ;CUENTA=W
    CALL    ESPERAI		    ;OBTENGO EL CARACTER
    CALL    SEND		    ;LO ENVIO
    MOVF    CUENTA, 0		    ;W=CUENTA
    ADDLW   01H			    ;W=W+1    
    DECFSZ  SIZE, 1		    ;SIZE=SIZE-1 SALTA CUANDO LLEGA A 0
    GOTO    BIENVENIDA2

    GOTO    $			    ;LOOP FOREVER lml

    END