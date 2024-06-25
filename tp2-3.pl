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

% Tableros de ejemplo:
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej2x2, T) :- tablero(2, 2, T), ocupar(pos(0, 0), T), ocupar(pos(0, 1), T).
tablero(libre20, T) :- tablero(20, 20, T).

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
  Inicio no es reversible porque si no esta instanciada, al unificar con el predicado vecinoLibre/3 (que pide que este instanciada)
  en la regla que verifica si una posición es vecina en la misma fila, se suma 1 a la columna actual (C2 is C + 1). 
  Sin embargo, al sumar 1 a una variable no instanciada, Prolog no puede determinar si el  resultado será menor que el número
  de columnas del tablero, ya que no puede realizar operaciones aritméticas con variables no instanciadas. 
  Esto provoca que la regla falle y, en consecuencia, el predicado vecinoLibre/3 también falle, lo que afecta la reversibilidad de camino/4.

  Fin sí es reversible ya que, como el tablero es finito, los caminos son finitos y en cada paso siempre se recorre sin repetir posiciones
  visitadas, eventualmente va a probar con todas las posiciones válidas y por cada una devolverá un camino C valido.
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
 Inicio debe estar intanciado por el mismo motivo que Inicio en camino/4
 Fin puede no estarlo por el mismo motivo que Fin en camino/4
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

caminoDual(I, F, T1, T2, C) :- camino(I, F, T1, C1), camino(I, F, T2, C2), mismoCamino(C1, C2, C).

% mismoCamino(+Camino1, +Camino2, -Camino)
mismoCamino(C1, C2, C) :- mismoCaminoAux(C1, C2, [], C). % El tercer parametro sera para posiciones visitadas de C1

% mismoCaminoAux(+Camino1, +Camino2, +CaminoParcial, -Camino)
mismoCaminoAux([], _, C, C).      % Cuando termino de recorrer el Camino1 el caminoParcial sera el camino C y coincide con el Camino2
% En CaminoParcial me guardo las posiciones que recorri de Camino1 que coinciden con las de Camino2 hasta llegar al final
mismoCaminoAux([Pos|XS], C2, CaminoParcial, C) :- member(Pos, C2), 
                                                  append(CaminoParcial, [Pos], NuevoCaminoParcial), 
                                                  mismoCaminoAux(XS, C2, NuevoCaminoParcial, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsTablero(6). 
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(3) :- tablero(1,1,[[_]]).
testTablero(4) :- tablero(2,2,[[_,_],[_,_]]).
testTablero(5) :- tablero(4,2,[[_,_],[_,_],[_,_],[_,_]]).
testTablero(6) :- ocupar(pos(1,1),[[_,_],[_,ocupada],[_,_],[_,_]]).


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


cantidadTestsVecinoLibre(3).
% Vecino libre a la derecha
testVecinoLibre(1) :- tablero(2, 2, T), vecinoLibre(pos(0, 0), T, pos(0, 1)).
% Vecino libre arriba
testVecinoLibre(2) :- tablero(2, 2, T), ocupar(pos(0, 1), T), vecinoLibre(pos(0, 0), T, pos(1, 0)).
% No hay vecino libre fuera de los límites
testVecinoLibre(3) :- tablero(1, 1, T), not(vecinoLibre(pos(0, 0), T, _)).



cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests
testCamino(1).

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests
testCaminoOptimo(1).

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests
testCaminoDual(1).

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