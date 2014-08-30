t-Xpres
=======

Sencillo marco de trabajo (framework) para implementar compiladores o intérpretes para un lenguajes imperativos y tipados. Como parte del proyecto, se define un nuevo lenguaje, al que también se le llama Xpres.

Esta desarrollado en Lazarus e incluye una sencilla IDE.

Xpres no es un programa, sino una infraestructura que define normas y procedimientos en la creación de intérpretes o compiladores, usando Lazarus. Sin embargo, como ejemplo, se incluyen implementaciones simples de un intérprete y un compilador.

El framework, incluye las siguientes partes:

* Editor de texto. Se usa el control SynEdit que ya viene con Lazarus y se usa la librería de utilidades https://github.com/t-edson/UtilEditSyn para facilitar la implementación del editor.
* Analizador léxico o lexer. Requiere el uso el resaltador de sintaxis https://github.com/t-edson/SynFacilSyn al que usaremos, más como analizador léxico que como resaltador de sintaxis.
* Rutinas básicas del framework. Incluye el manejo de contextos como entrada de datos. Se incluyen en la unidad 'XpresBas.pas', y por lo general no debería modificacrse.
* Rutinas principales del framework. Incluyen el analizador sintáctivo o parser. Se incluyen en la unidad 'XpresParser.pas', e incluye el analizador de expresiones y de las estructuras del lenguaje. No debería cambiar si no cambia el lenguaje.
* Intérpretes o Generadores de código. Se implementan en un archivo separado que será incluido luego como parte de 'XpresParser.pas'.

Xpres está diseñado para trabajar con el paquete SynEdit de Lazarus, porque el analizador léxico trabaja sobre SynEdit. Así se espera crear aplicaciones gráficas más que de consola. Esto implica que para implementar un intérprete/compilador, se debe usa el paquete SynEdit, siempre. La arquitectura planteada no implica el uso de SynEdit como editor, pero si se requiere SynEdit, para el analizador léxico.

Además, al ser el analizador léxico, también un resaltador de sintaxis, se puede resaltar el código fuente con las mismas rutinas del analizador léxico, sin necesidad de implementar algún otro resaltador. De esta forma se garantiza una correspondencia al 100% entre los tokens del analizador léxico, y el coloreado que se puede lograr en pantalla.

Hay que notar que de SynFacilSyn, solo se está usando su capacidad de lexer, más no de manejo de bloques de sintaxis. De momento los bloques de sintaxis, los maneja el analizador sintáctico. He intentado usar las opciones de manejo de bloques de SynFacilSyn como parte del analizador sintáctico, ero de momento no lo he logrado exitósamente.

Los generadores de código pueden desarrollarse para generar código intermedio, como el bytecode de Java, o cualquier otro. Este proyecto no incluye ninguna máquina virtual.

El framework se ha definido para un lenguaje especial al que se le ha llamado también Xpres, pero en teoría podría implementarse cualquier otro similar que se ajuste al modo de trabajo de los analizadores léxico y sintáctico; y de las librerías. 

En los ejemplos se incluye un caso minimalista con intérprete y un ejemplo de compilador para el intel 8086 en 16 bits.

Xpres, aún está en fase de desarrollo. El estado del framework, es todavía incipiente, pero permite realizar implementaciones básicas. 

El ejemplo de compilador incluye también una IDE sencilla para el lenguaje Xpres, que incluye un interprete sencillo que solo permite manejar variables, expresiones numéricas y de cadena.

El intérprete mostrará siempre el valor de la última expresión evaluada.
 
Solo se reconocen los tipos: int, float y string.

Un programa sencillo, que se puede ejecutar es este:
```
program NoName;

var x,y: int;
   
begin
  puts "Hola mundo";
  x := 5;
  y := 10;
  x;  //para mostrar el resultado
end;
```

Lenguaje Xpres
==============

El lenguaje Xpres es un lenguaje simple, tipado e imperativo. En su versión actual, no soporta el manejo de objetos, pero se espera incluirlo en el futuro. Está pensado para poder implementar compiladores de bajo nivel para microcontroladores (de hecho el desarrollo del framework estuvo motivado en el desarrollo de un compilador para microcontroladores PIC). Por ello, se definen tipos de 8 bits, y la arquitectura del generador de código permite optimizar las operaciones.

Xpres, es un lenguaje que se define empezando por los tipos, luego se definen los operadores que se apliquen a esos tipos, y luego las operaciones (sobre que tipo se aplica) que se permiten para cada operador. Por ejemplo:

Si se define el tipo 'int8'. Luego se puede definir el operador '+' para 'int8', y luego se puede definir la operación 'suma de int8', que es la suma de un dato 'int8' con otro dato 'int8'.

En este sentido, no existen operadores como elementos fundamentales del lenguaje, lo que si ocurre en la mayoría de lenguajes (como Pascal o C). Tampoco puede existir la sobrecarga de operadores, porque por definición, los operadores se definen para cada tipo de datos.

Se espera que una implementación del lenguaje incluya ya las definiciones de tipos,  operadores y expresiones, de modo que no tengan que cambiarse luego.

La definición del lenguaje Xpres, es también parte de este proyecto. Sin embargo, no se incluirá una implementación que cumpla con el 100% del lenguaje, al menos a corto plazo. El lenguaje se define, dejando abierta las puertas para implementaciones parciales. Por ejemplo, no se espera poder implementar aritmética en coma flotante de 64 bits, para microcontroladores sencillo de 8 bits. Las rutinas básicas del framework, detectan cuando no se ha implementado una operación u operador, y generan el mensaje de error correspondiente. De esta forma, el tratamiento de error se simplica considerablemente.

