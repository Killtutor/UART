# UART
Assembler handmade UART and PWM and some features like read a pin and write in another one. 

al mandarle al micro por la consola recibiendo por UART los siguientes codigos.. el mismo tendra diferentes respuestas:

01h  lee lo que esta en el pin RA3  y lo devuelve en binario
02h  prende o apaga el led segun lo que le mandes como segunda instruccion 1 o 0
03h  segun el dato que le mandes despues de esta instruccion el micro variara su salida pwm
11h  agarra el valor de tensión en AN0 y lo devuelve a la consola 
c0h  establece la salida del puerto C segun el codigo mandado por el usuario
