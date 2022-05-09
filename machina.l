%{
#include <stdio.h>
#include "machina.tab.h"
#include "tokens.h" 

int numLineas = 1;

%}

IDENTIFICADOR [a-zA-Z_0-9]+
NUMEROS [0-9]+
OCTAL \\[Oo]([0-7]+)
HEXADECIMAL \\[Hh]([0-9a-fA-F]+)
CONSTANTE_ENTERA ({NUMEROS}|{OCTAL}|{HEXADECIMAL})
NUM_REAL_NORMAL {CONSTANTE_ENTERA}*\.{CONSTANTE_ENTERA} 
MANTISA ({NUM_REAL_NORMAL}|{NUMEROS})
NUM_REAL_CON_MANTISAEXPONENTE ({MANTISA}\^([+-]?){CONSTANTE_ENTERA})
NUM_REAL ({NUM_REAL_NORMAL}|{NUM_REAL_CON_MANTISAEXPONENTE})
CHARACTER_UNICO '[^\n'\\]'
CHARACTER_ESCAPADOS '\\[\\'"abfnrtv]'
CHARACTER_NUM '\\([01]?[0-9][0-9]?|2[0-4][0-9]|25[0-5])'
CHARACTER_HEX '\\[Hh]([0-9a-fA-F]{1,2})'
CHARACTER_OCT '\\[Oo]([012]?[0-9][0-9]?|3[0-6][0-9]|37[0-7])' 
CHARACTER ({CHARACTER_UNICO}|{CHARACTER_ESCAPADOS}|{CHARACTER_NUM}|{CHARACTER_HEX}|{CHARACTER_OCT})

CADENA_UNICA \"[^"\"\n\"]*\"
CADENA_ESCAPADOS \"\\[\\'"abfnrtv]\"
CADENA_NUM \"\\([01]?[0-9][0-9]?|2[0-4][0-9]|25[0-5])\"
CADENA_HEX \"\\[Hh]([0-9a-fA-F]{1,2})\"
CADENA_OCT \"\\[Oo]([012]?[0-9][0-9]?|3[0-6][0-9]|37[0-7])\"
CADENA_ESCAPE \"\\\\n\"
CADENAS ({CADENA_UNICA}|{CADENA_ESCAPADOS}|{CADENA_NUM}|{CADENA_HEX}|{CADENA_OCT}|{CADENA_ESCAPE})

DELIMITADOR "{"|"}"|"("|")"|"["|"]"|"|"|":"|";"|","
DELIMITADOR_FLECHA "->"
DELIMITADOR_DOS_PUNTOS ".."
OP_ARITMETICOS "+"|"-"|"*"|"%"
OP_ARITMETICOS_EXP "**"
OP_LOGICOS "not"|"and"|"or"
OP_ASIGNACION ":="
OP_BITS "@"|"&" 
OP_RELACIONALES "="|"<"|">"
OP_RELACIONAL_DIST "/="
OP_RELACIONAL_MEN "<="
OP_RELACIONAL_MAY ">="
COMENTARIO "--".*$

%x COMENTARIO_MULTILINEA
%x CADENA_MULTILINEA

%%

\n ++numLineas; /* Incrementamos 'numLineas' siempre que nos encontramos un salto de linea */

(?i:abstract)    fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return ABSTRACT;
(?i:array)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return ARRAY;
(?i:boolean)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return BOOLEAN;
(?i:case)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return CASE;
(?i:character)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return CHARACTER;
(?i:class)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return CLASS;
(?i:constant)    fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return CONSTANT;
(?i:constructor) fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return CONSTRUCTOR;
(?i:default)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return DEFAULT;
(?i:destructor)  fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return DESTRUCTOR;
(?i:else)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return ELSE;
(?i:enumeration) fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return ENUMERATION;
(?i:especific)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return ESPECIFIC;
(?i:exception)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return EXCEPTION;
(?i:exit)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return EXIT;
(?i:false)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FALSE;
(?i:final)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FINAL;
(?i:finish)      fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FINISH;
(?i:float)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FLOAT;
(?i:for)         fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FOR;
(?i:foreach)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FOREACH;
(?i:function)    fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return FUNCTION;
(?i:hashtable)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return HASHTABLE;
(?i:if)          fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return IF;
(?i:in)          fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return IN;
(?i:integer)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return INTEGER;
(?i:is)          fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return IS;
(?i:loop)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return LOOP;
(?i:nil)         fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return NIL;
(?i:of)          fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return OF;
(?i:others)      fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return OTHERS;
(?i:out)         fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return OUT;
(?i:private)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return PRIVATE;
(?i:procedure)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return PROCEDURE;
(?i:protected)   fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return PROTECTED;
(?i:public)      fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return PUBLIC;
(?i:raise)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return RAISE;
(?i:record)      fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return RECORD;
(?i:return)      fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return RETURN;
(?i:reverse)     fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return REVERSE;
(?i:then)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return THEN;
(?i:true)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return TRUE;
(?i:try)         fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return TRY;
(?i:type)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return TYPE;
(?i:start)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return START;
(?i:when)        fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return WHEN;
(?i:while)       fprintf(yyout,"Linea %d - Palabra Reservada: %s\n", numLineas, yytext); return WHILE;

 /* Queremos que se identifiquen antes los operadores, podriamos utilizar las macros definidas */
(?i:or)          fprintf(yyout,"Linea %d - Operador Logicos: %s\n",numLineas,yytext); return OR;
(?i:and)         fprintf(yyout,"Linea %d - Operador Logicos: %s\n",numLineas,yytext); return AND;
(?i:not)         fprintf(yyout,"Linea %d - Operador Logicos: %s\n",numLineas,yytext); return NOT;
"."              fprintf(yyout,"Linea %d - Operador de Operador de acceso a memoria: %s\n",numLineas,yytext); return yytext[0];

 /* Podriamos utilizar los tokens definidos con 2 lineas mas */
{OP_BITS}            fprintf(yyout,"Linea %d - Operador de Bits: %s\n",numLineas,yytext); return yytext[0];
{OP_ASIGNACION}      fprintf(yyout,"Linea %d - Operador de Asignacion: %s\n",numLineas,yytext); return ASIG;
{OP_RELACIONALES}    fprintf(yyout,"Linea %d - Operador Relacional: %s\n",numLineas,yytext); return yytext[0];
{OP_RELACIONAL_DIST} fprintf(yyout,"Linea %d - Operador Relacional: %s\n",numLineas,yytext); return DISTINTO;
{OP_RELACIONAL_MEN}  fprintf(yyout,"Linea %d - Operador Relacional: %s\n",numLineas,yytext); return MENOR_IGUAL;
{OP_RELACIONAL_MAY}  fprintf(yyout,"Linea %d - Operador Relacional: %s\n",numLineas,yytext); return MAYOR_IGUAL;
{OP_ARITMETICOS_EXP} fprintf(yyout,"Linea %d - Operador Aritmetico: %s\n",numLineas,yytext); return EXP;

{OP_ARITMETICOS}         fprintf(yyout,"Linea %d - Operador Aritmetico: %s\n",numLineas,yytext); return yytext[0];
{DELIMITADOR}            fprintf(yyout,"Linea %d - Delimitador: %s\n",numLineas,yytext); return yytext[0];
{DELIMITADOR_FLECHA}     fprintf(yyout,"Linea %d - Delimitador: %s\n",numLineas,yytext); return FLECHA;
{DELIMITADOR_DOS_PUNTOS} fprintf(yyout,"Linea %d - Delimitador: %s\n",numLineas,yytext); return DOS_PTOS;


{CONSTANTE_ENTERA} fprintf(yyout,"Linea %d - Constante Entera: %s\n",numLineas,yytext); return CTC_INT;
{NUM_REAL}         fprintf(yyout,"Linea %d - Constante Real: %s\n",numLineas,yytext); return CTC_FLOAT;
{CHARACTER}        fprintf(yyout,"Linea %d - Constante Caracter: %s\n",numLineas,yytext); return CTC_CARACTER;
{CADENAS}          fprintf(yyout,"Linea %d - Constante Cadena: %s\n",numLineas,yytext); return CTC_CADENA;
{IDENTIFICADOR}    fprintf(yyout,"Linea %d - Identificador: %s\n",numLineas,yytext); return IDENTIFICADOR;

{COMENTARIO}       printf("Linea %d - Comentario.\n",numLineas);

"//" BEGIN(COMENTARIO_MULTILINEA); printf("Linea %d - Inicio comentario multilinea.\n",numLineas); 
<COMENTARIO_MULTILINEA>. ;
<COMENTARIO_MULTILINEA>\n ++numLineas;
<COMENTARIO_MULTILINEA>\\\\ printf("Linea %d - Final comentario multilinea.\n",numLineas); BEGIN(INITIAL);

\" BEGIN(CADENA_MULTILINEA); printf("Linea %d - Inicio cadena multilinea.\n",numLineas); 
<CADENA_MULTILINEA>[^\"] ;
<CADENA_MULTILINEA>\n ++numLineas;
<CADENA_MULTILINEA>\" printf("Linea %d - Final cadena multilinea.\n",numLineas); BEGIN(INITIAL);

[ \t\n] ;

%%