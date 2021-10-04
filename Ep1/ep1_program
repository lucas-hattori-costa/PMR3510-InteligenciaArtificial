%%% Definindo predicados basicos

% - node_ref: guarda numero do node, o estado, o custo e a acao anterior para chegar a este node
% - father: guarda o node pai e o respectivo node filho
% - min_cost: guarda o custo minimo quando sao avaliados nodes nas fronteiras
% - best_node: guarda o numero do node com menor custo na fronteira
% - num_nodes: guarda o numero de nodes explorados
% - frontier: guarda lista com numero dos nodes na fronteira
% - action_list: guarda as acoes tomadas para chegar em cada node
% - counter: guarda um valor auxiliar utilizado em outros predicados


node_ref(node_num, state, cost, previous_action). % e.g node_ref(0,[[1,2,3,4,5,6],[6],[6]],0,'A2B')
father(child_node_num, father_node_num).
min_cost(inf).
best_node(0).
num_nodes(0).
frontier([0]).
action_list([]).
counter(0).

:- dynamic(node_ref/4).
:- dynamic(father/2).
:- dynamic(frontier/1).
:- dynamic(min_cost/1).
:- dynamic(best_node/1).
:- dynamic(num_nodes/1).
:- dynamic(counter/1).
:- dynamic(action_list/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Custos Hanoi.
% Calculados manualmente a partir da equacao H(n) = 2^n - 1

hanoi(5,31).
hanoi(4,15).
hanoi(3,7).
hanoi(2,3).
hanoi(1,1).
hanoi(0,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Predicados auxiliares

% is_empty: True se a LIST for vazia
% less_than: Atualiza o predicado counter com o numero de elementos de LIST que sao menores que VALUE
% update_counter: Atualiza o predicado num_nodes com o valor NUM_NODES passado

is_empty(LIST):- not(member(_,LIST)).

less_than([],_).

less_than(LIST, VALUE) :-
    
    [H|T] = LIST,
    (
        H < VALUE -> 
            (
                counter(N),
                retractall(counter(_)),
                AUX is N+1,
                assertz(counter(AUX))
            );true
    ),
    less_than(T,VALUE).

update_counter(NUM_NODES) :-
    % Atualiza o predicado num_nodes
    retractall(num_nodes(_)),
    AUX is NUM_NODES+1,
    assertz(num_nodes(AUX)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Definindo as acoes

move_A2B(PREV_STATE, NEXT_STATE) :-
    [[HA|TA],[HB|TB],[HC|TC]|_] = PREV_STATE,
    NEXT_STATE = [TA,[HA|[HB|TB]],[HC|TC]].

move_B2A(PREV_STATE, NEXT_STATE) :-
    [[HA|TA],[HB|TB],[HC|TC]|_] = PREV_STATE,
    NEXT_STATE = [[HB|[HA|TA]],TB,[HC|TC]].    

move_A2C(PREV_STATE, NEXT_STATE) :-
    [[HA|TA],[HB|TB],[HC|TC]|_] = PREV_STATE,
    NEXT_STATE = [TA,[HB|TB],[HA|[HC|TC]]].

move_C2A(PREV_STATE, NEXT_STATE) :-
    [A,B,[HC|TC]] = PREV_STATE,
    NEXT_STATE = [[HC|A], B, TC].

move_B2C(PREV_STATE, NEXT_STATE) :-
    [A,[HB|TB],C] = PREV_STATE,
    NEXT_STATE = [A, TB, [HB|C]].

move_C2B(PREV_STATE, NEXT_STATE) :-
    [A,B,[HC|TC]] = PREV_STATE,
    NEXT_STATE = [A, [HC|B], TC].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Predicados relativos ao objetivo final e estado inicial. 

% O numero 6 representa nenhum disco.
% reached_goal: True se existe um node_ref no database que corresponde ao estado-meta
% reached_subgoal: True se o STATE passado corresponde a um sub-objetivo, isto e, se 
%                  existe uma sequencia correta em C. 
%                  Exemplo: [[1,2],[3],[4,5]] is subgoal -> [4,5] em C esta correto.
% return_goal: Devolve o numero de nodes explorados, custo otimo e lista de acoes
%              otimas.
% modify_initial_state: cria um node_ref com o estado inicial passado.


reached_goal :-
    node_ref(_, [[6],[6],[1,2,3,4,5,6]], _, _).

reached_subgoal(STATE) :-
    [_,_,C] = STATE,
    length(C, LEN),
    [HC|_] = C,
    (
        HC =:= 7-LEN
    ).

return_goal(EXPLORED_NODES, COST, ACTION_LIST) :- 
    node_ref(GOAL_NODE, [[6],[6],[1,2,3,4,5,6]], COST, _),
    num_nodes(EXPLORED_NODES),
    actions(GOAL_NODE),
    action_list(ACTION_LIST).

modify_initial_state(INITIAL_STATE) :-
    retractall(node_ref(0,_,0,_)),
    assertz(node_ref(0, INITIAL_STATE, 0, 'INIT')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Predicados relativos a busca

% create_father: cria um predicado father atribuindo o node pai de determinado node.
% update_frontier: atualiza o predicado frontier com o node expandido.
% create_children: cria os nodes filhos possiveis seguindo as seguintes restricoes
%                   1 - Respeita as regras do sistema
%                   2 - Nao recria o node pai
%                   3 - Nao desfaz um subgoal (jogada sempre indesejada)
% heuristic: calcula valor de heuristica para determinado node
% evaluate_nodes: avalia nodes da fronteira e estabelece o node de menor custo
% search: realiza a busca A*
% actions: estabelece acoes que devem ser seguidas para chegar na solucao otima, a partir do node final


create_father(NODE) :-
    num_nodes(NUM_NODES),
    assertz(father(NODE, NUM_NODES)).

update_frontier(NODE) :-
    num_nodes(NUM_NODES),
    frontier(FRONTIER_NODES),
    delete(FRONTIER_NODES, NODE, AUX),
    append(AUX, [NUM_NODES], NEW_FRONTIER),
    retractall(frontier(_)),
    assertz(frontier(NEW_FRONTIER)).

create_children :-
    best_node(NODE),
    node_ref(NODE, STATE, FATHER_COST, FATHER_ACTION),
    [[HA|_],[HB|_],[HC|_]|_] = STATE,
    num_nodes(NUM_NODES),
    (
        HA > HB -> 
            (   (FATHER_ACTION \== 'A2B') -> (  N1 is NUM_NODES+1, F1 is FATHER_COST+1,
                                                move_B2A(STATE, NEXT_STATE),
                                                assertz(node_ref(N1, NEXT_STATE, F1, 'B2A')),
                                                update_counter(NUM_NODES),
                                                create_father(NODE),
                                                update_frontier(NODE)
                                            ); true
            );
            (   (HA \== HB),(FATHER_ACTION \== 'B2A') -> (  N1 is NUM_NODES+1, F1 is FATHER_COST+1,
                                                            move_A2B(STATE, NEXT_STATE),
                                                            assertz(node_ref(N1, NEXT_STATE, F1, 'A2B')),
                                                            update_counter(NUM_NODES),
                                                            create_father(NODE),
                                                            update_frontier(NODE)
                                                    ); true
        	)
    ),
    num_nodes(NUM_NODES_2),
    (
        HA > HC -> 
            (   (FATHER_ACTION \== 'A2C'),not(reached_subgoal(STATE)) -> (  N2 is NUM_NODES_2+1, F2 is FATHER_COST+1,
                                                move_C2A(STATE, NEXT_STATE_2),
                                                assertz(node_ref(N2, NEXT_STATE_2, F2, 'C2A')),
                                                update_counter(NUM_NODES_2),
                                                create_father(NODE),
                                                update_frontier(NODE)
                                            ); true
            );
            (   (HA \== HC),(FATHER_ACTION \== 'C2A') -> (  N2 is NUM_NODES_2+1, F2 is FATHER_COST+1,
                                                            move_A2C(STATE, NEXT_STATE_2),
                                                            assertz(node_ref(N2, NEXT_STATE_2, F2, 'A2C')),
                                                            update_counter(NUM_NODES_2),
                                                            create_father(NODE),
                                                            update_frontier(NODE)
                                                    ); true
        	)
    ),
    num_nodes(NUM_NODES_3),
    (
        HB > HC -> 
            (   (FATHER_ACTION \== 'B2C'),not(reached_subgoal(STATE)) -> (  N3 is NUM_NODES_3+1, F3 is FATHER_COST+1,
                                                move_C2B(STATE, NEXT_STATE_3),
                                                assertz(node_ref(N3, NEXT_STATE_3, F3, 'C2B')),
                                                update_counter(NUM_NODES_3),
                                                create_father(NODE),
                                                update_frontier(NODE)
                                            ); true
            );
            (   (HB \== HC),(FATHER_ACTION \== 'C2B') -> (  N3 is NUM_NODES_3+1, F3 is FATHER_COST+1,
                                                            move_B2C(STATE, NEXT_STATE_3),
                                                            assertz(node_ref(N3, NEXT_STATE_3, F3, 'B2C')),
                                                            update_counter(NUM_NODES_3),
                                                            create_father(NODE),
                                                            update_frontier(NODE)
                                                    ); true
        	)
    ).

heuristic(NODE, H) :-
    node_ref(NODE, STATE, _, _),
    [A, B, C] = STATE,

    % acha o maior elemento em A ou B. Chama ele de N.
    delete(A, 6, A_NEW),
    delete(B, 6, B_NEW),
    (   not(is_empty(A_NEW)) -> max_list(A_NEW, A_MAX);A_MAX = 0	),
    (   not(is_empty(B_NEW)) -> max_list(B_NEW, B_MAX);B_MAX = 0	),
    max_list([A_MAX, B_MAX], N),
    
    % conta quantos discos menores que N estao em C.
    retractall(counter(_)),
    assertz(counter(0)),
    less_than(C, N),
    counter(COUNTER),
    hanoi(COUNTER, HANOI_C),

    % quantidade de discos em A e B
    length(A_NEW,A_LEN),
    length(B_NEW,B_LEN),
    
    (
        member(N,A) ->
            (   
                hanoi(A_LEN, HANOI_A),
                AUX is B_LEN + COUNTER,
                hanoi(AUX, HANOI_BC),
                H is HANOI_A + HANOI_C + HANOI_BC
            );
            (
                hanoi(B_LEN, HANOI_B),
                AUX is A_LEN + COUNTER,
                hanoi(AUX, HANOI_AC),
                H is HANOI_B + HANOI_C + HANOI_AC
            )
    ).


evaluate_nodes([]).

evaluate_nodes(FRONTIER) :-
    [NODE|OTHER_NODES] = FRONTIER,
    node_ref(NODE,_,COST_TO_NODE, _),
    heuristic(NODE, H),
    COST is COST_TO_NODE + H,
    min_cost(MIN_COST),
    (
    	COST < MIN_COST -> (   
                           	retractall(min_cost(_)),
                            assertz(min_cost(COST)),
                           	retractall(best_node(_)),
                            assertz(best_node(NODE))
                           );true
    ),
    evaluate_nodes(OTHER_NODES).

search :-
    frontier(FRONTIER),
    retractall(min_cost(_)),
    assertz(min_cost(inf)),
    retractall(best_node(_)),
    evaluate_nodes(FRONTIER),
    (   
    	(reached_goal) ->
    		true;
    		(  
                create_children,
                search
            )
    ).

actions(0).

actions(FINAL_NODE) :-
    node_ref(FINAL_NODE, _, _, ACTION),
    action_list(AUX),
    append([ACTION], AUX, ACTION_LIST),
    retractall(action_list(_)),
    assertz(action_list(ACTION_LIST)),
    father(FATHER, FINAL_NODE),
    actions(FATHER).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(INITIAL_STATE,EXPLORED_NODES,COST,ACTION_LIST) :-
    modify_initial_state(INITIAL_STATE),
    search,
    return_goal(EXPLORED_NODES, COST, ACTION_LIST).