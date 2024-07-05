%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               Tablero
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% --------------------------- Ejercicio 1 -------------------------------------
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(F,C,T) :-  length(T,F),  maplist(longColumnas(C),T).

% longColumnas(?C,?Lista)
longColumnas(C,Lista) :- length(Lista,C).

%% --------------------------- Ejercicio 2 -------------------------------------
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

% Instancia Tablero[F][C] = ocupada
ocupar(pos(F,C),T):- nth0(F,T,X), nth0(C,X,ocupada).

%% --------------------------- Ejercicio 3 -------------------------------------
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

vecino(pos(F,C), T, pos(F2,C)) :- filas(T, Filas), F2 is F + 1, F2 < Filas.
vecino(pos(F,C), T, pos(F2,C)) :- F > 0, F2 is F - 1, filas(T, Filas), F2 >= 0, F2 < Filas.
vecino(pos(F,C), T, pos(F,C2)) :- columnas(T, Columnas), C2 is C + 1, C2 < Columnas.
vecino(pos(F,C), _, pos(F,C2)) :-   C > 0, C2 is C - 1, C2 >= 0.

% filas(?T, ?F)
filas(T, F) :- length(T, F).

% columnas(?T, ?C)
columnas([F|_], C) :- length(F, C).
 
%% --------------------------- Ejercicio 4 -------------------------------------
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

vecinoLibre(pos(F, C), T, pos(F2, C2)) :- vecino(pos(F, C), T, pos(F2, C2)), desocupada(pos(F2, C2), T).

% desocupada(+Pos,+T)
% Verifica si Tablero[F][C] == _
desocupada(pos(F, C), T) :- nth0(F, T, Fila), nth0(C, Fila, X), var(X).  % var(X) verifica que X no este instanciada.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% --------------------------- Ejercicio 5 -------------------------------------
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

%% camino(+Inicio, +Fin, +Tablero, -Camino)
camino(I, F, T, C) :- caminoAux(I, F, T, [I], C).

% caminoAux(+Inicio, +Fin, +Tablero, +Visitadas,-Camino)

% Cuando la posición actual es la posición final, el camino es la posición actual
caminoAux(pos(F, C), pos(F, C), _, _, [pos(F, C)]).
% Se mueve a un Vecino libre hasta llegar a Fin sin repetir posiciones Visitadas.
caminoAux(I, F, T, Visitadas, [I | XS]) :- vecinoLibre(I, T, Vecino),
                                           not(member(Vecino, Visitadas)), 
                                           caminoAux(Vecino, F, T, [Vecino | Visitadas], XS).
  
%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace
  /*

  CORREGIR/REVISAR

  Fin sí es reversible ya que, como el tablero es finito, los caminos son finitos y en cada paso siempre se recorre sin repetir posiciones
  visitadas, eventualmente va a probar con todas las posiciones válidas y por cada una devolverá un camino C valido.

  Camino si es reversible.

  */


%% --------------------------- Ejercicio 6 -------------------------------------
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.


camino2(I, F, T, C) :-  % Encuentra todos los caminos posibles entre Inicio y Fin en el tablero T,
                        % junto con la longitud de cada camino, y los guarda en la lista Caminos.
                        findall((C, L), (camino(I, F, T, C), length(C, L)), Caminos),
                        % Ordena la lista de caminos Caminos por la longitud (segundo elemento de cada tupla),
                        % en orden creciente.
                        sort(2, =<, Caminos, CaminosOrdenados),
                        % Extrae un camino C de la lista ordenada de caminos.
                        member((C, _), CaminosOrdenados).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

/*
  CORREGIR/REVISAR

  Inicio no es reversible porque si no esta instanciada, al unificar con el predicado vecinoLibre/3 (que pide que este instanciada)
  en la regla que verifica si una posición es vecina en la misma fila, se suma 1 a la columna actual (C2 is C + 1). 
  Sin embargo, al sumar 1 a una variable no instanciada, Prolog no puede determinar si el  resultado será menor que el número
  de columnas del tablero, ya que no puede realizar operaciones aritméticas con variables no instanciadas. 
  Esto provoca que la regla falle y, en consecuencia, el predicado vecinoLibre/3 también falle, lo que afecta la reversibilidad de camino/4.

  Me suena a que camino no es reversible.

*/

%% --------------------------- Ejercicio 7 -------------------------------------
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

caminoOptimo(I, F, T, C) :- camino(I, F, T, C), length(C, Long),  not(hayMasCorto(I, F, T, Long)).

% hayMasCorto(+Inicio,+Fin,+Tablero,+Long)
hayMasCorto(I, F, T, Long) :- camino(I, F, T, C), length(C, Long2), Long2 < Long.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% --------------------------- Ejercicio 8 -------------------------------------
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.

caminoDual(I, F, T1, T2, C) :- camino(I,F,T1,C), camino(I,F,T2,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tableros de ejemplo:
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
/*

 .  .  .  .  .
 .  X  X  .  .
 .  .  .  .  .
 .  .  .  .  .
 .  .  .  .  .

*/
tablero(ej5x5_2, T) :- tablero(5, 5, T), ocupar(pos(0, 2), T), ocupar(pos(2,1), T), ocupar(pos(3,1), T), ocupar(pos(3,2),T).
/*

 .  .  X  .  .
 .  .  .  .  .
 .  X  .  .  .
 .  X  X  .  .
 .  .  .  .  .

*/

tablero(ej2x2, T) :- tablero(2, 2, T), ocupar(pos(0, 0), T), ocupar(pos(0, 1), T).
/*

 X  X  
 .  .  

*/
tablero(ej3x3_bloqueado, T) :- tablero(3, 3, T), ocupar(pos(1, 0), T), ocupar(pos(1, 1),T), ocupar(pos(1, 2), T).
/*

 .  .  . 
 X  X  X  
 .  .  .  

*/

% Auxiliares:

% prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(X,L) :- append(X,_,L).

% sufijo(?S, +L), donde S es sufijo de la lista L.
sufijo(X,L) :- append(_,X,L).

% sublista(?L1,+L2).
sublista([],_).
sublista([S|SS],L) :- sufijo(X,L), prefijo([S|SS],X).

% ------------------------------------------------------------------

cantidadTestsTablero(6). 
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(3) :- tablero(1,1,[[_]]).
testTablero(4) :- tablero(2,2,[[_,_],[_,_]]).
testTablero(5) :- tablero(4,2,[[_,_],[_,_],[_,_],[_,_]]).
testTablero(6) :- ocupar(pos(1,1),[[_,_],[_,ocupada],[_,_],[_,_]]).
% ------------------------------------------

cantidadTestsVecino(6).
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Vecino derecha
testVecino(2) :- tablero(2, 2, T), vecino(pos(0, 0), T, pos(0, 1)).
% Vecino izquierda
testVecino(3) :- tablero(2, 2, T), vecino(pos(0, 1), T, pos(0, 0)).
% Vecino abajo
testVecino(4) :- tablero(2, 2, T), vecino(pos(0, 0), T, pos(1, 0)).
% Vecino arriba
testVecino(5) :- tablero(2, 2, T), vecino(pos(1, 0), T, pos(0, 0)).
% No hay vecino fuera de los límites
testVecino(6) :- tablero(1, 1, T), not(vecino(pos(0, 0), T, _)).
% ------------------------------------------
cantidadTestsVecinoLibre(3).
% Vecino libre a la derecha
testVecinoLibre(1) :- tablero(2, 2, T), vecinoLibre(pos(0, 0), T, pos(0, 1)).
% Vecino libre arriba
testVecinoLibre(2) :- tablero(2, 2, T), ocupar(pos(0, 1), T), vecinoLibre(pos(0, 0), T, pos(1, 0)).
% No hay vecino libre fuera de los límites del tablero
testVecinoLibre(3) :- tablero(1, 1, T), not(vecinoLibre(pos(0, 0), T, _)).

% ------------------------------------------
cantidadTestsCamino(3).
% Camino sencillo en tablero 2x2
testCamino(1) :- tablero(2, 2, T),
                 camino(pos(0, 0), pos(1, 1), T, C),
                 member(C, [[pos(0, 0), pos(1, 0), pos(1, 1)], [pos(0, 0), pos(0, 1), pos(1, 1)]]).
% Camino más largo en tablero 3x3
testCamino(2) :- tablero(3, 3, T),
                 camino(pos(0, 0), pos(2, 2), T, C),
                 % Verifica que el camino encontrado sea una secuencia válida de movimientos
                 CaminoEsperado = [pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)],
                 sublista(CaminoEsperado, C).

% No hay camino posible debido a ocupaciones
testCamino(3) :- tablero(ej3x3_bloqueado, T), not(camino(pos(0, 0), pos(2, 2), T, _)).

% ------------------------------------------
cantidadTestsCaminoOptimo(3).

% Camino óptimo en tablero 2x2
testCaminoOptimo(1) :-
    tablero(2, 2, T),
    caminoOptimo(pos(0, 0), pos(1, 1), T, C),
    member(C, [[pos(0, 0), pos(1, 0), pos(1, 1)], [pos(0, 0), pos(0, 1), pos(1, 1)]]).

% Camino óptimo en tablero 3x3
testCaminoOptimo(2) :-
    tablero(3, 3, T),
    caminoOptimo(pos(0, 0), pos(2, 2), T, Camino),
    length(Camino, 5).  % El camino óptimo tendrá longitud 5

% No hay camino óptimo posible debido a ocupaciones
testCaminoOptimo(3) :- tablero(ej3x3_bloqueado, T), not(caminoOptimo(pos(0, 0), pos(2, 2), T, _)).

% ------------------------------------------
cantidadTestsCaminoDual(2).

% Camino dual en tableros idénticos 2x2
testCaminoDual(1) :-
    tablero(2, 2, T1),
    tablero(2, 2, T2),
    caminoDual(pos(0, 0), pos(1, 1), T1, T2, [pos(0, 0), pos(0, 1), pos(1, 1)]).

% Camino dual en tableros diferentes
testCaminoDual(2) :-
    tablero(ej5x5, T1),
    tablero(ej5x5_2, T2),
    caminoDual(pos(0, 0), pos(4, 3), T1, T2, Camino),
    Camino = [pos(0, 0), pos(1,0), pos(2,0), pos(3,0), pos(4,0),pos(4,1),pos(4,2),pos(4,3)].




tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(vecinoLibre) :- cantidadTestsVecinoLibre(M), forall(between(1, M, N), testVecinoLibre(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(vecinoLibre),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).
