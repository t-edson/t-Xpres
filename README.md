t-Xpres
=======

Sencillo framework para compilador, desarrollado en Lazarus. 
Usa un lenguaje especial, llamado Xpres que está basado en tipos de datos y operadores.
Incluye un ejemplo de intérprete y compilador a código ASM.

Aún está en fase de desarrollo. 

Esta implementación, es una implementación sencilla del lenguaje Xpres que incluye un 
interprete sencillo que solo permite manejar variables y expresiones numéricas.

El intérprete mostrará siempre el valor de la última expresión evaluada.
 
Solo se reconocen los tipos: int, y float.

Un programa sencillo, que se puede ejecutar es este:

program NoName;

var x,y: int;

    z: float;
    
begin
  x := 5;
  
  y := 10;
  
  z:=x+y*2;
  
  z;  //para mostrar el resultado
  
end;
