t-Xpres
=======

Sencillo marco de trabajo (framework) para implementar un compilador o intérprete para un nuevo lenguaje llamado Xpres. Esta desarrollado en Lazarus e incluye una sencilla IDE.

Para el analizador léxico se usa el resaltador de sintaxis SynFacilSyn.

El lenguaje Xpres que está basado en tipos de datos y operadores.
Incluye un ejemplo de intérprete y compilador a código ASM.

Aún está en fase de desarrollo. 

Esta implementación, es una implementación sencilla del lenguaje Xpres que incluye un 
interprete sencillo que solo permite manejar variables, expresiones numéricas y de cadena.

El intérprete mostrará siempre el valor de la última expresión evaluada.
 
Solo se reconocen los tipos: int, float y string.

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
