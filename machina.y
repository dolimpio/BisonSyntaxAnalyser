%{

  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();

  #define YYDEBUG 1

  int yyerror(char *);

%}

%token ABSTRACT AND ARRAY ASIG BOOLEAN CLASS CONSTRUCTOR CTC_CADENA CTC_CARACTER CTC_FLOAT CTC_INT
%token CASE CHARACTER CONSTANT DEFAULT DESTRUCTOR DISTINTO DOS_PTOS ELSE ENUMERATION ESPECIFIC
%token EXCEPTION EXIT EXP FALSE FINAL FINISH FLECHA FLOAT FOREACH FOR FUNCTION HASHTABLE IDENTIFICADOR
%token IF IN INTEGER IS LOOP MAYOR_IGUAL MENOR_IGUAL MOD NOT NIL OF OR OTHERS OUT PROCEDURE PRIVATE 
%token PROTECTED PUBLIC RAISE RECORD RETURN REVERSE START THEN TRUE TRY TYPE WHEN WHILE

%%

/* -------- DECLARACIONES -------- */

programa_entero : programa
                ;

programa : declaracion  
         | declaracion programa          
         ;

declaracion : declaracion_objeto      { printf("\tdeclaracion -> declaracion_objeto\n"); }
            | declaracion_tipo        { printf("\tdeclaracion -> declaracion_tipo\n"); }
            | declaracion_subprograma { printf("\tdeclaracion -> declaracion_subprograma\n"); }
            ;

declaracion_objeto : identificadores_comas ':' constante_opcional tipo_escalar asignacion_escalar_opcional ';'   { printf("\tdeclaracion_objeto -> identificadores_comas ':' constante_opcional tipo_escalar asignacion_escalar_opcional ';'\n"); }
                   | identificadores_comas ':' constante_opcional tipo_complejo asignacion_complejo_opcional ';' { printf("\tdeclaracion_objeto -> identificadores_comas ':' constante_opcional tipo_complejo asignacion_complejo_opcional ';'\n"); }
                   ;

identificadores_comas : IDENTIFICADOR
                      | IDENTIFICADOR ',' identificadores_comas
                      ;

constante_opcional : /* empty */
                   | CONSTANT
                   ;

asignacion_escalar_opcional : /* empty */  
                            | asignacion_escalar
                            ;

asignacion_complejo_opcional : /* empty */ 
                             | asignacion_compleja
                             ;

tipo_escalar : INTEGER   { printf("\ttipo_escalar -> INTEGER\n"); }
             | FLOAT     { printf("\ttipo_escalar -> FLOAT\n"); }
             | BOOLEAN   { printf("\ttipo_escalar -> BOOLEAN\n"); }
             | CHARACTER { printf("\ttipo_escalar -> CHARACTER\n"); }
             ;

asignacion_escalar : ASIG expresion_coma { printf("\tasignacion_escalar -> ASIG expresion_comas\n"); }
                   ;

tipo_complejo : nombre_de_tipo 
              | tipo_compuesto
              ;

nombre_de_tipo : IDENTIFICADOR { printf("\tnombre_de_tipo -> IDENTIFICADOR\n"); }
               ;

tipo_compuesto : tipo_tablero 
               | tipo_registro 
               | tipo_hashtable 
               | tipo_clase 
               | tipo_enumeracion
               ;

asignacion_compleja : ASIG objeto_complejo { printf("\tasignacion_complejo -> ASIG objeto_complejo\n"); }
                    ;

objeto_complejo : '[' objeto_complejo_comas ']'    { printf("\tobjeto_complejo -> '[' objeto_complejo_comas ']'\n"); }
                | '{' elemento_hashtable_comas '}' { printf("\tobjeto_complejo -> '{' elemento_hashtable_comas '}'\n"); } 
                | '(' elemento_registro_comas ')'  { printf("\tobjeto_complejo -> '(' elemento_registro_comas ')'\n"); }
                | literal                          { printf("\tobjeto_complejo -> literal\n"); }
                ; 

objeto_complejo_comas : objeto_complejo
                      | objeto_complejo ',' objeto_complejo_comas
                      ;
              
elemento_hashtable_comas : elemento_hashtable
                         |  elemento_hashtable ',' elemento_hashtable_comas
                         ;

elemento_registro_comas : elemento_registro
                        | elemento_registro ',' elemento_registro_comas
                        ;

elemento_hashtable : objeto_complejo FLECHA objeto_complejo { printf("\telemento_hashtable -> objeto_complejo FLECHA objeto_complejo\n"); }
                   ;

elemento_registro : IDENTIFICADOR ASIG objeto_complejo { printf("\telemento_registro -> IDENTIFICADOR ASIG objeto_complejo\n"); }
                  ;              




/* -------- TIPOS -------- */

declaracion_tipo : TYPE IDENTIFICADOR IS especificacion_tipo ';' { printf("\tdeclaracion_tipo -> TYPE IDENTIFICADOR IS especificacion_tipo ';'\n"); }
                 | error ';' {yyerrok;}
                 ;

especificacion_tipo : tipo_escalar 
                    | nombre_de_tipo 
                    | tipo_compuesto
                    ;

tipo_tablero : ARRAY '(' expresion DOS_PTOS expresion ')' OF especificacion_tipo { printf("\ttipo_tablero -> ARRAY '(' expresion DOS_PTOS expresion ')' OF especificacion_tipo\n"); }
             ;

tipo_registro : RECORD uno_mas_componentes FINISH RECORD { printf("\ttipo_registro -> RECORD uno_mas_componentes FINISH RECORD\n"); }
              ;

uno_mas_componentes : componente
                    | componente uno_mas_componentes
                    ;

componente : identificadores_comas ':' especificacion_tipo ';' { printf("\tcomponente -> identificadores_comas ':' especificacion_tipo ';'\n"); }
           ;

tipo_hashtable : HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>' { printf("\ttipo_hashtable -> HASHTABLE OF '<' especificacion_tipo ',' especificacion_tipo '>'\n"); }
               ;

tipo_clase : CLASS nombre_de_tipo_opcional uno_mas_componentes_clase FINISH CLASS { printf("\ttipo_clase -> CLASS nombre_de_tipo_opcional uno_mas_componentes FINISH CLASS\n"); }
           ;      

nombre_de_tipo_opcional : /* empty */
                        | '(' nombre_de_tipo ')'
                        ;      

componente_clase : visibilidad_opcional declaracion_componente { printf("\tcomponente_clase -> visibilidad_opcional declaracion_componente\n"); }
                 ;

declaracion_componente : declaracion_objeto 
                       | declaracion_tipo 
                       | declaracion_metodo
                       ;

visibilidad : PUBLIC     { printf("\tvisibilidad -> PUBLIC\n"); }
            | PROTECTED  { printf("\tvisibilidad -> PROTECTED\n"); }
            | PRIVATE    { printf("\tvisibilidad -> PRIVATE\n"); }
            ;

declaracion_metodo : cero_mas_modificadores declaracion_subprograma { printf("\tdeclaracion_metodo -> cero_mas_modificadores declaracion_subprograma\n"); }
                   ;

cero_mas_modificadores : /* empty */ 
                       | modificador cero_mas_modificadores
                       ;

modificador : CONSTRUCTOR  { printf("\tmodificador -> CONSTRUCTOR\n"); }
            | DESTRUCTOR   { printf("\tmodificador -> DESTRUCTOR\n"); }
            | ABSTRACT     { printf("\tmodificador -> ABSTRACT\n"); }
            | ESPECIFIC    { printf("\tmodificador -> ESPECIFIC\n"); }
            | FINAL        { printf("\tmodificador -> FINAL\n"); }
            ;

tipo_enumeracion : ENUMERATION OF tipo_escalar elementos_comas FINISH ENUMERATION { printf("\ttipo_enumeracion -> ENUMERATION OF tipo_escalar elementos_comas FINISH ENUMERATION\n"); }
                 ;

elementos_comas : elemento
                | elemento ',' elementos_comas
                ;

elemento : identificador_flecha_opcional literal { printf("\telemento -> identificador_flecha_opcional literal\n"); }
         ;

uno_mas_componentes_clase : componente_clase // cambie el orden, checkear
                          | componente_clase uno_mas_componentes_clase
                          ;

visibilidad_opcional : /* empty */ 
                     | visibilidad
                     ;

identificador_flecha_opcional : /* empty */
                              | IDENTIFICADOR FLECHA
                              ;



/* -------- SUBPROGRAMAS -------- */

declaracion_subprograma : especificacion_subprograma cuerpo_subprograma_opcional ';' { printf("\tdeclaracion_subprograma -> especificacion_subprograma cuerpo_subprograma_opcional ';'\n"); }
                        ;

especificacion_subprograma : PROCEDURE IDENTIFICADOR parte_formal_opcional { printf("\tespecificacion_subprograma -> PROCEDURE IDENTIFICADOR parte_formal_opcional ';'\n"); }
                           | FUNCTION IDENTIFICADOR parte_formal_opcional RETURN especificacion_tipo { printf("\tespecificacion_subprograma -> FUNCTION IDENTIFICADOR parte_formal_opcional RETURN especificacion_tipo \n"); }
                           ;

parte_formal : declaracion_parametros_opcional { printf("\tparte_formal -> declaracion_parametros_opcional \n"); }
             | error ';' {yyerrok;}
             ;

declaracion_parametros_opcional : /* empty */ 
                                | declaracion_parametros
                                ;

cuerpo_subprograma_opcional : /* empty */  
                            | cuerpo_subprograma
                            ;

parte_formal_opcional : /* empty */ 
                      | '(' parte_formal ')'
                      ;

declaracion_parametros : declaracion_parametro cero_mas_declaracion_parametro { printf("\tdeclaracion_parametros -> declaracion_parametro cero_mas_declaracion_parametro \n"); }
                       ;

cero_mas_declaracion_parametro : /* empty */  
                               | ';' declaracion_parametro cero_mas_declaracion_parametro
                               ;

declaracion_parametro : identificadores_comas ':' modo_opcional especificacion_tipo { printf("\tdeclaracion_parametro -> identificadores_comas ';' modo_opcional especificacion_tipo\n"); }
                      ;

modo_opcional : /* empty */  
              | modo
              ;

modo : IN out_opcional { printf("\tmodo -> IN out_opcional\n"); }
     ;

out_opcional : /* empty */ 
             | OUT
             ;

cuerpo_subprograma : IS cero_mas_declaracion START una_mas_instrucciones FINISH identificador_opcional
                   { printf("\tcuerpo_subprograma -> IS cero_mas_declaracion START una_mas_instrucciones FINISH identificador_opcional \n"); }
                   ;

cero_mas_declaracion : /* empty */ 
                     | declaracion cero_mas_declaracion
                     ;   

una_mas_instrucciones : instruccion
                      | instruccion una_mas_instrucciones
                      ;

identificador_opcional : /* empty */
                       | IDENTIFICADOR { printf("\tidentificador_opcional -> IDENTIFICADOR \n"); }
                       ;
             



/* -------- INSTRUCCIONES -------- */


instruccion : instruccion_vacia
            | instruccion_asignacion
            | instruccion_exit
            | instruccion_return
            | instruccion_if
            | instruccion_case
            | instruccion_loop
            | instruccion_rise 
            | instruccion_try_catch
            | llamada_procedure 
            | error ';' {yyerrok;} 
            ;

instruccion_vacia : NIL ';' { printf("\tinstruccion_vacia -> NIL ';'\n"); }
                  ;

instruccion_asignacion : nombre ASIG expresion ';' { printf("\tinstruccion_asignacion -> nombre ASIG expresion ';'\n"); }
                       ;

instruccion_return : RETURN expresion ';' { printf("\tinstruccion_return -> RETURN expresion ';'\n"); }
                   ;

instruccion_exit : EXIT identificador_opcional when_opcional ';' { printf("\tinstruccion_exit -> EXIT identificador_opcional when_opcional ';'\n"); }
                 ;

when_opcional : /* empty */ 
              | WHEN expresion
              ;  

instruccion_if : IF expresion THEN una_mas_instrucciones else_opcional FINISH IF ';' 
                { printf("\tinstruccion_if -> IF expresion THEN una_mas_instrucciones else_opcional FINISH IF ';' \n"); }
               ;

instruccion_case : CASE expresion IS uno_mas_caso_when FINISH CASE ';' { printf("\tinstruccion_case -> CASE expresion IS uno_mas_caso_when FINISH CASE ';'\n"); }
                 ;

uno_mas_caso_when : caso_when 
                  | caso_when uno_mas_caso_when 
                  ;

caso_when : WHEN entrada cero_mas_entradas FLECHA una_mas_instrucciones { printf("\tcaso_when -> WHEN entrada cero_mas_entradas FLECHA una_mas_instrucciones\n"); }
          ;

entrada : expresion expresion_puntos_opcional { printf("\tentrada -> expresion expresion_puntos_opcional\n"); }
        | OTHERS                              { printf("\tentrada -> OTHERS\n"); }
        ;

expresion_puntos_opcional : /* empty */  
                          | DOS_PTOS expresion
                          ;

instruccion_loop : identificador_dospts_opcional clausula_iteracion bucle_base ';' { printf("\tinstruccion_loop -> identificador_dospts_opcional clausula_iteracion bucle_base ';' \n"); }
                 ;

identificador_dospts_opcional : /* empty */ 
                              | IDENTIFICADOR ':'
                              ;

clausula_iteracion : FOR IDENTIFICADOR IN reverse_opcional expresion DOS_PTOS expresion { printf("\tclausula_iteracion -> FOR IDENTIFICADOR IN reverse_opcional expresion DOS_PTOS expresion\n"); }
                   | FOREACH IDENTIFICADOR IN expresion                                 { printf("\tclausula_iteracion -> FOREACH IDENTIFICADOR IN expresion\n"); }
                   | WHILE expresion                                                    { printf("\tclausula_iteracion -> WHILE expresion\n"); }
                   ;

reverse_opcional : /* empty */ 
                 | REVERSE
                 ;  

bucle_base : LOOP instruccion_opcional FINISH LOOP { printf("\tbucle_base -> LOOP instruccion_opcional FINISH LOOP\n"); }
           ;

instruccion_rise : RAISE IDENTIFICADOR ';' { printf("\tinstruccion_rise -> RAISE IDENTIFICADOR ';'\n"); }
                 ;

instruccion_try_catch : TRY una_mas_instrucciones clausulas_excepcion FINISH TRY { printf("\tinstruccion_try_catch -> TRY una_mas_instrucciones clausulas_excepcion FINISH TRY\n"); }
                      ;

clausulas_excepcion : cero_mas_clausula_especifica clausula_defecto { printf("\tclausulas_excepcion -> cero_mas_clausula_especifica clausula_defecto\n"); }
                    | una_mas_clausula_especifica                   { printf("\tclausulas_excepcion -> una_mas_clausula_especifica\n"); }
                    ;

cero_mas_clausula_especifica : /* empty */
                             | clausula_especifica cero_mas_clausula_especifica 
                             ;

una_mas_clausula_especifica : clausula_especifica 
                            | clausula_especifica una_mas_clausula_especifica
                            ;  

clausula_especifica : EXCEPTION '(' IDENTIFICADOR ')' una_mas_instrucciones { printf("\tclausula_especifica -> EXCEPTION '(' IDENTIFICADOR ')' una_mas_instrucciones\n"); }
                    ;

clausula_defecto : DEFAULT '(' IDENTIFICADOR ')' una_mas_instrucciones { printf("\tclausula_defecto -> DEFAULT '(' IDENTIFICADOR ')' una_mas_instrucciones\n"); }
                 ;

llamada_procedure : llamada_suprograma ';' { printf("\tllamada_procedure -> llamada_suprograma ';'\n"); }
                  ;

llamada_suprograma : IDENTIFICADOR '(' cero_mas_expresiones ')' { printf("\tllamada_suprograma -> IDENTIFICADOR '(' cero_mas_expresiones ')' \n"); }
                   ;
       
expresion_coma : expresion  
               | expresion ',' expresion_coma
;

else_opcional : /* empty */
              | ELSE una_mas_instrucciones
              ;

cero_mas_entradas : /* empty */ 
                  | '|' entrada 
                  ;

instruccion_opcional : instruccion
                     | instruccion instruccion_opcional
                     ; 

/* -------- EXPRESIONES -------- */

primario : literal 
         | nombre 
         | '(' expresion ')' { printf("\tprimario -> '(' expresion ')' \n"); }
         ;

literal : CTC_CADENA    { printf("\tliteral -> CTC_CADENA \n"); }
         | CTC_CARACTER { printf("\tliteral -> CTC_CARACTER \n"); }
         | CTC_FLOAT    { printf("\tliteral -> CTC_FLOAT \n"); }
         | CTC_INT      { printf("\tliteral -> CTC_INT \n"); }
         | TRUE         { printf("\tliteral -> TRUE \n"); }
         | FALSE        { printf("\tliteral -> FALSE \n"); }
         ;

nombre : componente_indexado
       | componente_hash
       | componente_compuesto
       | llamada_suprograma
       | IDENTIFICADOR { printf("\tnombre -> IDENTIFICADOR \n"); }
       ;  

componente_indexado : nombre '[' expresion ']' { printf("\tcomponente_indexado -> nombre '[' expresion ']' \n"); } 
                    ;

componente_hash : nombre '{' expresion '}' { printf("\tcomponente_hash -> nombre '{' expresion '}' \n"); }
                ;

componente_compuesto : nombre '.' IDENTIFICADOR      { printf("\tcomponente_compuesto -> nombre '.' IDENTIFICADOR \n"); }
                     | nombre '.' llamada_suprograma { printf("\tcomponente_compuesto -> nombre '.' llamada_suprograma \n"); }
                     ;

expresion_logica : expresion_logica OR and_logico  { printf("\texpresion_logica -> expresion_logica OR and_logico \n"); }
                 | and_logico                      { printf("\texpresion_logica -> and_logico \n"); }
                 ; 

and_logico : and_logico AND negacion  { printf("\tand_logico -> and_logico AND negacion \n"); }
           | negacion                 { printf("\tand_logico -> negacion \n"); }
           ;

negacion : NOT igual { printf("\tnegacion -> NOT igual \n"); }                         
         | igual     { printf("\tnegacion -> igual \n"); }    
         ;

igual : or_binario '=' or_binario         { printf("\tigual -> or_binario '=' or_binario\n"); }
      | or_binario DISTINTO or_binario    { printf("\tigual -> or_binario DISTINTO or_binario\n"); }
      | or_binario '<' or_binario         { printf("\tigual -> or_binario '<' or_binario\n"); }
      | or_binario '>' or_binario         { printf("\tigual -> or_binario '>' or_binario\n"); }
      | or_binario MENOR_IGUAL or_binario { printf("\tigual -> or_binario MENOR_IGUAL or_binario\n"); }
      | or_binario MAYOR_IGUAL or_binario { printf("\tigual -> or_binario MAYOR_IGUAL or_binario\n"); }
      | or_binario
      ;

or_binario : or_binario '@' and_binario { printf("\tor_binario -> or_binario '@' and_binario \n"); }
           | and_binario                { printf("\tor_binario -> and_binario \n"); }
           ;

and_binario : and_binario '&' resta { printf("\tand_binario -> and_binario '&' resta \n"); }
            | resta                 { printf("\tand_binario -> resta \n"); }
            ;

resta : resta '+' modulo { printf("\tresta -> resta '+' modulo \n"); }
      | resta '-' modulo { printf("\tresta -> resta '-' modulo \n"); }
      | modulo           { printf("\tresta -> modulo \n"); }
      ;

modulo : modulo MOD potencia { printf("\tmodulo -> modulo MOD potencia \n"); }
       | modulo '%' potencia { printf("\tmodulo -> modulo '%' potencia \n"); }
       | modulo '*' potencia { printf("\tmodulo -> modulo '*' potencia \n"); }
       | potencia            { printf("\tmodulo -> potencia \n"); }
       ;

potencia : menos_unario EXP potencia { printf("\tpotencia -> menos_unario EXP potencia  \n"); } // Asociatividad por la derecha
         | menos_unario              { printf("\tpotencia -> menos_unario \n"); }
         ;

menos_unario : '-' primario { printf("\tmenos_unario -> '-' primario  \n"); }
             | primario     { printf("\tmenos_unario -> primario \n"); }
             ;

expresion : expresion_logica if_expresion_opcional { printf("\texpresion -> expresion_logica if_expresion_opcional \n"); }
          ;

if_expresion_opcional : /* empty */ 
                      | IF expresion ELSE expresion { printf("\tif_expresion_opcional -> IF expresion ELSE expresion \n"); }
                      ;

cero_mas_expresiones : /* empty */
                     | expresion_con_comas      
                     ; 

expresion_con_comas : expresion
                | expresion ',' expresion_con_comas
                ;

%%


int yyerror(char *s) {
  fflush(stdout);
  printf("   *****************, %s\n",s);
}

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./machina NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
