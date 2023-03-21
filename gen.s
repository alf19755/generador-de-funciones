;UNIVERSIDAD DEL VALLE DE GUATEMALA
;IE2023 Programación de Microcontroladores
;Autor:		Mónica Alfaro
;Compilador:	pic-as (v2.36), MPLABX (v6.00)
;
;Programa:	PROYECTO (Generador de funciones)		
;
;				
;Dispositivo:	PIC16F887
;Hardware:	LEDs en el puerto D, botones en el puerto B
;
;Creado:	       19 de febrero , 2023
;Última modificación:  19 de febrero , 2023


PROCESSOR 16F887
#include <xc.inc>
;configuration word 1
 CONFIG FOSC=INTRC_NOCLKOUT //OSCILADOR INTERNO SIN SALIDAS
 CONFIG WDTE=OFF //WDT DISEABLED (REINICIO REPETITIVO DEL PIC)
 CONFIG PWRTE=OFF //PWRT ENABLED (ESPERA DE 72ms AL INICIAR)
 CONFIG MCLRE=OFF //EL PIN DE MCLR SE UTILIZA COMO I/0
 CONFIG CP=OFF	//SIN PROTECCIÓN DE CÓDIGO
 CONFIG CPD=OFF	//SIN PROTECCIÓN DE DATOS
 
 CONFIG BOREN=OFF //SIN REINICIO CUÁNDO EL VOLTAJE DE ALIMENTACIÓN BAJA DE 4V
 CONFIG IESO=OFF //REINCIO SIN CAMBIO DE RELOJ DE INTERNO A EXTERNO
 CONFIG FCMEN=OFF //CAMBIO DE RELOJ EXTERNO A INTERNO EN CASO DE FALLO
 CONFIG LVP=OFF //PROGRAMACIÓN EN BAJO VOLTAJE PERMITIDA
 
;configuration word 2
 CONFIG WRT=OFF	//PROTECCIÓN DE AUTOESCRITURA POR EL PROGRAMA DESACTIVADA
 CONFIG BOR4V=BOR40V //REINICIO ABAJO DE 4V, (BOR21V=2.1V)

PSECT udata_bank0	; common memory

    
 PSECT udata
 W_TEMP:
    DS 1
 STATUS_TEMP:
    DS 1

var: DS 1
bandera: DS 1
    
display_variable: DS 2
    
nibble: DS 2

valor_a_convertir: DS 1
MILES: DS 1
CENTENA: DS 1	   
DECENA: DS 1	    
UNIDAD: DS 1
modo: DS 1
vartmr0:DS 1
    
multiplexado: DS 1
    
lastablas: DS 1
    
 valorhz: DS 1
valorkhz: DS 2
    
UNIDAD_V: DS 1
DECENA_V: DS 1
CENTENA_V: DS 1
    

senialcuadrada: DS 1
    
PSECT resVect, class=code, abs, delta=2
;----------------------------------VECTOR RESET---------------------------------
ORG 00h			    ;Posicion 0000h para el vector
resetVec:
    PAGESEL main
    goto main

PSECT code, delta=2, abs
ORG 100h		    ;Posicion para el codigo
 
;--------------------------VECTOR DE INTERRUPCIONES-----------------------------

PSECT code, delta=2, abs
 ORG 0x0004
PUSH:			    ;Parte de el código que menciona el datasheet.
    MOVWF W_TEMP
    SWAPF STATUS, W
    MOVWF STATUS_TEMP
    
    BANKSEL PORTA
    
///////////////////        ÁREA DE INTERRUPCIONES      /////////////////////////
;Interrupción de TMR0

I_T0IF:
    btfss T0IF	    ;Ver si la bandera T0IF es 0, me salto una línea
    goto I_PIR1
    call reiniciar_tmr0  ;Pero si no es 0 (es 1) me voy a la subrutina   
    call salidacuadrada

    
    goto I_PIR1
    
    
    

;Interrupción de TMR1 a 20ms
    
I_PIR1:
    banksel PIR1
    btfss   PIR1, 0	    ;Ver si la bandera T0IF es 0, me salto una línea
    goto    I_RBIF 
    
    call    reiniciar_tmr1
    bcf	    PIR1, 0
    call    multiplexado_display
    
    
    
;Interrupción de PORTB
I_RBIF:
    btfss INTCON, 0	    ; Revisa si la bandera IRBIF está activada
			    ;?antes estaba en Btfss pero aca estaba toda la info
			    ;de incremento y decremento de botones. 
    goto POP
    call botonesB
    
    
    
    
    ;Ingresar estados de Khz y Hz
    
    
POP:
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    RETFIE		    ;REGRESA DE LA INTERRUPCIÓN
    
;*******************************************************************************
;CÓDIGO PRINCIPAL
;*******************************************************************************
PSECT CODE, delta = 2, abs
 ORG 0x100
;-------------Tabla display antes de código-------------------
contadorhexa:		    
			    ;Tablas para cátodo común
   
   clrf PCLATH		    ;Program Counter
   bsf	PCLATH, 0 
   ;--------------Volviendo de 4 bits---------------------------
   andlw 0x0f		    ;Solo puedo llegar a 16 con el and.
   addwf PCL		    ;PC= PCLATH+ PCL +W
   
   retlw 00111111B ; 0	    ;No usamos return normal,ya que esa devuelve una lit
   retlw 00000110B  ;1
   retlw 01011011B  ;2
   retlw 01001111B  ;3
   retlw 01100110B  ;4
   retlw 01101101B  ;5
   retlw 01111101B  ;6
   retlw 00000111B  ;7
   retlw 01111111B  ;8
   retlw 01101111B  ;9
   retlw 01110111B  ;A
   retlw 01111100B  ;b
   retlw 00111001B  ;C
   retlw 01011110B  ;d
   retlw 01111001B  ;E
   retlw 01110001B  ;F
   
   /*A la hora de conectarlo se ve así
   Bit0 del puerto = Línea superior (pin 4 de arriba)
   Bit1 del puerto = Línea inferior derecha (pin 4 de abajo)
   Bit2 del puerto = Línea 
   Bit3 del puerto = Línea 
   Bit4 del puerto = Línea 
   Bit5 del puerto = Línea
   Bit5 del puerto = Línea 


   
   */
   
tablahz:		    
    
    CLRF    PCLATH
    BSF	    PCLATH, 0
    ANDLW   0x0F	    ;limite 16 
    ADDWF   PCL, F
    RETLW   3		    
    RETLW   4		    
    RETLW   05		   
    RETLW   5		   
    RETLW   6		    
    RETLW   7		    
    RETLW   8		    
    RETLW   9		    
    RETLW   10		    
    RETLW   11		    
    RETLW   14		    
    RETLW   19		    
    RETLW   28		    
    RETLW   55		    
    RETLW   976	

tablakhz:		    
    
    CLRF    PCLATH
    BSF	    PCLATH, 0
    ANDLW   0x0F	    ;limite 16 
    ADDWF   PCL, F
    RETLW   261		    
    RETLW   281	    
    RETLW   304		   
    RETLW   332   
    RETLW   365	    
    RETLW   405	    
    RETLW   456	    
    RETLW   520	    
    RETLW   726		    
    RETLW   905		    
    RETLW   1201		    
    RETLW   1785		    
    RETLW   3472	    
    RETLW   6250		    
 
   
main:    
    call configuracion_inicial
    call interrupcion_puertoB
    call configuracion_interrupciones
    call configuracion_clck
    call configuracion_tmr0
    call configuracion_tmr1

    ;Limpieza para iniciar en 00
    ;---> Puertos
    BANKSEL PORTC
    ;contador
    clrf PORTB		    ;BOTONES
    clrf PORTD		    ;DAC
    clrf PORTE		    ;LEDS

    ;display
    clrf PORTA		    ;DISPLAYS
    ;transistores
    clrf PORTC		    ;TRANSISTORES
    
    ;---> Variables
	
   
   clrf multiplexado
   clrf MILES
   clrf CENTENA
   clrf DECENA
   clrf UNIDAD
   clrf modo
   clrf vartmr0
   
   clrf lastablas
   clrf valorhz
    clrf valorkhz
    clrf UNIDAD_V
    clrf DECENA_V
    clrf CENTENA_V
    


 
    
;*******************************************************************************
;LOOP INFINITO
;*******************************************************************************
    
loop: 
   ;señal cuadrada
    ;call reiniciar_tmr0
    ;incf PORTD
    
    selector:
    

    
    btfss   modo, 0	    ;Verificación de modo
    goto    Hz		    ;Modo 1 (Hz)
    goto    Khz		    ;Modo 2 (KHz)
    
    Hz:
    ;Activación de puertos indicadores de estado

    bsf	    PORTE, 0	    ;LED indica estar en modo Hz 
    bcf	    PORTE, 1	 
    
    ;Activación de puertos indicadores de estado

    call    preparacionvalor    ; LLAMAMOS A LA SUBRUTINA PARA HACER EL SWITCH CON LA TABLA DE HZ
    call    descomposicion_varhz	    ; LLAMAMOS PARA HACER LA DIVISION PARA LOS DISPLAYS
    call    transformacion_var	    ; LLAMAMOS A LA SUBRUTINA PARA HACER EL CAMBIO DE VARIABLES
    
    
    goto    loop
    
    Khz:
    ;Activación de puertos indicadores de estado
    bcf	    PORTE, 0	   ;LED indica estar en modo KHz 
    bsf	    PORTE, 1	    
    
    CALL    preparacionvalor2    ; LLAMAMOS A LA SUBRUTINA PARA HACER EL SWITCH CON LA TABLA DE kHZ
    CALL    descomposicion_varkhz	    ; LLAMAMOS PARA HACER LA DIVISION PARA LOS DISPLAYS
    CALL    transformacion_var	    ; LLAMAMOS A LA SUBRUTINA PARA HACER EL CAMBIO DE VARIABLES
    
    
    goto loop
    
;*******************************************************************************
;				CONFIGURACIONES
;*******************************************************************************
  
configuracion_inicial:
    
    banksel ANSEL	    ; Configuración de pines digitales
    clrf ANSEL		  
    clrf ANSELH		   
    
    banksel TRISB
    clrf TRISB
    
    banksel TRISB
    
    ;Salidas
    clrf TRISD		    ;Todo el puerto C como salida  (Contador)
    
    clrf TRISA		    ;Todo el puerto A como salida (Displays)
    
    movlw 0b11110000	    ;4 bits de salida para puerto c(transistores mux)
    movwf TRISC
    
    clrf TRISE		    ;2 bits de salida para LEDS MODO(HZ Y KHZ)
    ;Entradas
    
    movlw 0b00001111	    ;2 bits de entrada para puerto b(Botones Contador)
    movwf TRISB
    
    return

configuracion_clck:
    
    banksel OSCCON	    ;Vamos al banco 1, donde está el OSCCON 
    bcf OSCCON, 4	    ;Los bits 5 y 6 del registro OSCCON son IRCF y 
    bsf OSCCON, 5	    ;la combinación de esos bits determina el FOSC
    bcf OSCCON, 6	    ;IRCF=010  250khZ --> verificar ircf en datasheet 
    bsf OSCCON, 0	    ;Selección de reloj interno con el bit 0 (SCS)
    return
 
//TIMERS//
;TMR0
configuracion_tmr0:
    ;Seleccionando un prescaler de 256

    banksel OPTION_REG	    ;Vamos al banco 1
    bcf OPTION_REG, 5	    ; T0CS RELOJ INTERNO
    
    ;PRESCALER
    bcf OPTION_REG, 3	    ;PSA		    
    
    bsf OPTION_REG, 2	    ;PS2
    bcf OPTION_REG, 1	    ;PS1 --> 256 -> verificar en hoja de datos
    bsf OPTION_REG, 0	    ;PS0 
    call reiniciar_tmr0	    ;optimización del reinicio
    return

reiniciar_tmr0:
    banksel PORTA
    movlw vartmr0                     ;254
    movwf TMR0
    bcf   T0IF
    return
;-------------------RECORDAR FORMULA PARA SABER LA FRECUENCIA DEL TIMER-----------
;   TEMPORIZACIÓN=TOSC*TMR0*PRESCALER
;   FOSC(frecuencia de oscilación)= 250 khz
;   TOSC (periodo de oscilación)= 1/250 000 = 0.000004 O 4 micro segundos
;   TMR0 = 256- n
;   N=VALOR A CARGAR EN TMR0
;   prescaler= 101 equivale a 64 (revisar datasheet)
;Entonces, para una temmporización de 20 ms:
;	N = 254
        
;TMR1
configuracion_tmr1:
    ;Seleccionando un prescaler de 1
    banksel T1CON
    bcf	    T1CON, 6
    bsf	    T1CON, 0	    ;Habilitar TMR1 
    bcf	    T1CON, 1	    ;TMR1CS --> RELOJ INTERNO
    
    ;Valor preescaler bits 4-5
    bcf	    T1CON, 4
    bcf	    T1CON, 5	    ;T1CKPS PRESCALER PARA TMR1 DE 1:1
    
    
    bcf	    T1CON, 3	    ;LP Oscillator Enable Control bit
    
    call    reiniciar_tmr1
    
    banksel PIR1
    bcf	    PIR1, 0	    ;Limpiando bandera 
    
    return  
    
    
reiniciar_tmr1:
    ;Valor de 65535 para tener 0.00001600 segs con un prescaler de 1
    banksel TMR1L
    movlw   255
    movwf   TMR1L	    ;Valor nibble LOW
    movlw   255
    movwf   TMR1H	    ;Valor nibble HIGH
    return
    
    
configuracion_interrupciones:
    
    banksel INTCON
    clrf INTCON
    
  
    bsf INTCON, 6	    ;Habilita las interrupciones Periféricas PEIE
   
    ;Interrupciones Puerto B
    bsf INTCON, 3	    ;Habilita las interrupciones RBIE
    bsf INTCON, 0	    ;Habilita las interrupciones RBIF
    ;Interrupciónes tmr0
    bsf	T0IE
    bcf	T0IF
    ;Interrupciónes tmr1
    bsf	 PIE1,   0	    
    
    bsf INTCON, 7	    ;Habilita las interrupciones Globales GIE
    
    
    
    return

;*******************************************************************************
;				SUBRUTINAS 
;*******************************************************************************
interrupcion_puertoB:
    
    BANKSEL IOCB
    BSF IOCB, 0
    BSF IOCB, 1		    ;Habilita RB0,RB1,RB2 y RB3 para ISR RBIE
    BSF IOCB, 2
    BSF IOCB, 3
    
    
    BANKSEL OPTION_REG
    BCF OPTION_REG, 7	    ;Habilita PULLUPS puerto B
    
    BSF WPUB, 0
    BSF WPUB, 1		    ;Habilita  PULLUPS en RB0 y RB1
    BSF WPUB, 2
    BSF WPUB, 3	
    RETURN
    
botonesB:
    
    
    
    
    
    
    ;Se revisa en qué MODO está (Hz o KHz)
    revisar:
    btfss   modo, 0	    ; X = 1?
    goto    modoHz	    ; X = 0, ESTADO 0 (Hz)
    goto    modoKhz	    ; X = 1, ESTADO 1 (KHz)
    
    
    
    modoHz:
    movlw   0b00000001
    btfss   PORTB, 3	    ;Verificar boton activado
    movwf   modo	    ;Seleccion de modo
    btfss   PORTB, 3	    ;antirrebote
    goto    $-1
    
    ;Parte de contador 
    incrementovar:
	//btfss PORTB, 0	    ;Revisa si el botón está presinoado (llegan 5v)
	//incf PORTD, F	    ;Incrementa el contador en PORTC
	;Antirebote
	//btfss PORTB, 0
	//goto  $-1 
	
	
	incrementoHZ:
	frecuenciahz:
 
	;Seleccionando un prescaler de 64, para rango de frecuencias 3 Hz - 976 Hz
	   banksel OPTION_REG	    ;Vamos al banco 1
	   ;PRESCALER

	   bsf OPTION_REG, 2	    ;PS2
	   bcf OPTION_REG, 1	    ;PS1 --> 64 -> verificar en hoja de datos
	   bsf OPTION_REG, 0	    ;PS0 
	   
	   call reiniciar_tmr0 
	   
	modificarfrecuenciahz:
	call modificacion 

	    
	    
	    bcf INTCON, 0	    ;Limpiar bandera
	    goto POP
	
	
    
    
    modoKhz:
    MOVLW   0b00000000
    BTFSS   PORTB, 2	    ;Verificar boton activado
    MOVWF   modo	    ;Seleccion de modo
    BTFSS   PORTB, 2	    ;antirrebote
    GOTO    $-1	    
    
    decrementovar:		    
	/*btfss PORTB, 1	    ;Revisa si el botón está presinoado (llegan 5v)
	decf PORTD, F	    ;Decrementa el contador en PORTC
	;Antirebote
	btfss PORTB, 1
	goto  $-1
	*/
	
	incrementoKHZ:
	frecuenciakhz:
	;Seleccionando un prescaler de 1, para rango de frecuencias 0.244 khz- 6250khz

	banksel OPTION_REG	    ;Vamos al banco 1
	;PRESCALER

	bcf OPTION_REG, 2	    ;PS2
	bcf OPTION_REG, 1	    ;PS1 --> 1 -> verificar en hoja de datos
	bcf OPTION_REG, 0	    ;PS0 
	call reiniciar_tmr0
	    
	modificarfrecuenciakhz:
	;AUMENTO DE FRECUENCIA
	call modificacion

	
	
    
    
	    bcf  INTCON, 0	    ;Limpiar bandera
	    goto POP
	    
//////////////////////////////MULTIPLEXADO//////////////////////////////////////
    
multiplexado_display:
      
    incf    multiplexado    ;Incrementar var multiplexado		    
    
    display1:
    
    ;Revisando en qué display está
    movf multiplexado, w
    sublw 1
    btfss STATUS, 2	
    
    goto display2
    clrf    PORTC		
    
    ;Ingresando valor al display
    movf    UNIDAD, W
    call    contadorhexa
    movwf   PORTA
    
    ;Activando transistor
    bsf	    PORTC, 0
   
    return
 
 
    display2:
    
    ;Revisando en qué display está
    movf multiplexado, w
    sublw 2
    btfss STATUS, 2	
    
    goto display3
    clrf    PORTC		
    
    ;Ingresando valor al display
    movf    DECENA, W
    call    contadorhexa
    movwf   PORTA
    
    ;Activando transistor
    bsf	    PORTC, 1
   
    return
    
    
    display3:
    
    ;Revisando en qué display está
    movf multiplexado, w
    sublw 3
    btfss STATUS, 2	
    
    goto display4
    clrf    PORTC		
    
    ;Ingresando valor al display
    movf    CENTENA, W
    call    contadorhexa
    movwf   PORTA
    
    ;Activando transistor
    bsf	    PORTC, 2
   
    return
    
    
    
    display4:
    
    ;Revisando en qué display está
    movf multiplexado, w
    sublw 4
    btfss STATUS, 2
    return
    
    clrf    PORTC		
    
    ;Ingresando valor al display
    movf    MILES, W
    call    contadorhexa
    movwf   PORTA
    
    ;Activando transistor
    bsf	    PORTC, 3
    
    clrf multiplexado
   
    return
    
    ///OPERACIONES////
preparacionvalor:
    
    MOVF    lastablas, W
    CALL    tablahz
    MOVWF   valorhz
    
    RETURN
    
transformacion_var:
    ;valores para display
    MOVF    UNIDAD_V, W		
    MOVWF   UNIDAD		;
    MOVF    DECENA_V, W		; 
    MOVWF   DECENA		; 
    MOVF    CENTENA_V, W		; 
    MOVWF   CENTENA		; 
    
    RETURN
    
    
descomposicion_varhz:  
    
    CLRF UNIDAD_V				    ; LIMPIAMOS LAS VARIABLES
    CLRF DECENA_V
    CLRF CENTENA_V
    ;CENTENA
 
    clrf    CENTENA_V
    MOVF    valorhz, W
    MOVWF   valorhz
    INCF    CENTENA_V
    MOVLW   100
    SUBWF   valorhz, F
    BTFSC   CARRY
    GOTO    $-4
    DECF    CENTENA_V
    MOVLW   100
    ADDWF   valorhz, F
    ;DECENA
 
    clrf    DECENA_V
    MOVF    valorhz, W
    MOVWF   UNIDAD_V
    INCF    DECENA_V
    MOVLW   10
    SUBWF   UNIDAD_V, F
    BTFSC   CARRY
    GOTO    $-4
    DECF    DECENA_V
    MOVLW   10
    ADDWF   UNIDAD_V, F
    
    
    RETURN
    
preparacionvalor2:
    
    MOVF    lastablas, W
    CALL    tablakhz
    MOVWF   valorkhz
    
    RETURN
    
descomposicion_varkhz:  

    
    CLRF UNIDAD_V				    ; LIMPIAMOS LAS VARIABLES
    CLRF CENTENA_V
    CLRF DECENA_V
  
    
    
    ;CENTENA
    clrf    CENTENA_V
    MOVF    valorkhz, W
    MOVWF   valorkhz
    INCF    CENTENA_V
    MOVLW   100
    SUBWF   valorkhz, F
    BTFSC   CARRY
    GOTO    $-4
    DECF    CENTENA_V
    MOVLW   100
    ADDWF   valorkhz, F
    
    ;DECENA
 
 
    clrf    DECENA_V
    MOVF    valorkhz, W
    MOVWF   UNIDAD_V
    INCF    DECENA_V
    MOVLW   10
    SUBWF   UNIDAD_V, F
    BTFSC   CARRY
    GOTO    $-4
    DECF    DECENA_V
    MOVLW   10
    ADDWF   UNIDAD_V, F
    
    
    RETURN
    
    


    ///////////////////SUBRUTINAS DE DIFERENTES FRECUENCIAS/////////////////////
    
modificacion:
    
    CHKB1H:
    
    BANKSEL PORTD
    BTFSS   PORTB, 0		    ; B0 = 0?
    CALL    inc17		    ; Incrementar 17
    BTFSS   PORTB, 0		    ; ANTIREBOTE .
    GOTO    $-1
    
    CHKB2H:
    
    BTFSS   PORTB, 1		    ; B1 = 0?
    CALL    dec17		    ; Decrementar 17
    BTFSS   PORTB, 1		    ; ANTIREBOTE
    GOTO    $-1
    
    RETURN
    
inc17:
    
    MOVLW   17			    ; MOVEMOS EL VALOR DE INCREMENTO
    ADDWF   vartmr0	    ; SUMAMOS 17 A LA VARIABLE DEL TMR0
    INCF    lastablas

    RETURN
    
dec17:
    
    MOVLW   17		    ; MOVEMOS EL VALOR DE DECREMENTO
    SUBWF   vartmr0		    ; RESTAMOS 17 A LA VARIABLE DEL TMR0
    DECF    lastablas
    
    RETURN	
    

 salidacuadrada:
    btfss senialcuadrada, 0	  
    goto down	   
    goto up
    
    down:
    BSF	senialcuadrada, 0	    
    BSF	PORTE, 2	    ;Output señal cuadrada (parte baja)
    BCF	T0IF		    ;Limpiando bandera
    
    goto I_PIR1
    
    up:
    BCF	senialcuadrada, 0	   
    BCF	PORTE, 2	    ;Output señal cuadrada (parte alta)
    BCF	T0IF	    
    return
    
			    
END
 
