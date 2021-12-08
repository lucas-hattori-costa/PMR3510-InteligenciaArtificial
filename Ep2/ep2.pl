frontier([0]).
node_ref(node_num, state, cost, previous_action).
father(child_node_num, father_node_num).
min_cost(inf).
best_node(0).
num_nodes(0).
action_list([]).

:- dynamic(on/2).
:- dynamic(node_ref/4).
:- dynamic(father/2).
:- dynamic(frontier/1).
:- dynamic(min_cost/1).
:- dynamic(best_node/1).
:- dynamic(num_nodes/1).
:- dynamic(counter/1).
:- dynamic(action_list/1).

free(table).
free(B) :- 
    not(on(_,B)).

on(_,_) :- false.

return_goal(GOALS, EXPLORED_NODES, COST, ACTION_LIST) :- 
    node_ref(GOAL_NODE, GOALS, COST, _),
    num_nodes(EXPLORED_NODES),
    actions(GOAL_NODE),
    action_list(ACTION_LIST).

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

update_counter(NUM_NODES) :-
    % Atualiza o predicado num_nodes
    retractall(num_nodes(_)),
    AUX is NUM_NODES+1,
    assertz(num_nodes(AUX)).



move_preconditions(X,Y):-
    % preconditions
    (X \== table),
    X \== Y,
    free(X),
    free(Y),
    on(X,Z),
    Z \== Y.

record_state(NEXT_STATE3) :-
    on(a,X),
    append([], [on(a,X)], NEXT_STATE),
    on(b,Y),
    append(NEXT_STATE, [on(b,Y)], NEXT_STATE2),
    on(c,Z),
    append(NEXT_STATE2, [on(c,Z)], NEXT_STATE3).

move_effects(X,Y) :-
    % remove effects
    retractall(on(X,_)),
    % add effects
    assertz(on(X,Y)).

move(X,Y):-
    num_nodes(NUM_NODES),
    best_node(NODE),
    node_ref(NODE, STATE, FATHER_COST, _),
    assert_state(STATE),
    (
        move_preconditions(X,Y) ->
            (
                N1 is NUM_NODES+1, F1 is FATHER_COST+1,
                move_effects(X,Y),
                record_state(NEXT_STATE),
                retract_state(NEXT_STATE),
                assertz(node_ref(N1, NEXT_STATE, F1, [X,Y])),
                update_counter(NUM_NODES),
                create_father(NODE),
                update_frontier(NODE)
            );
            (	
            	retract_state(STATE)
            )
    ).

assert_state([]).
assert_state(STATE) :-
    [H|T] = STATE,
    assertz(H),
    assert_state(T).

retract_state([]).
retract_state(STATE) :-
    [H|T] = STATE,
    retractall(H),
    retract_state(T).

check_goals([]).
check_goals(GOALS) :-
    [H|T] = GOALS,
    H,
    check_goals(T).

reached_goals(GOALS) :-
    best_node(NODE),
    node_ref(NODE, STATE, _, _),    
    assert_state(STATE),
    (
        check_goals(GOALS) ->
            (
                retract_state(STATE),
                true
            );
            (
                retract_state(STATE),
                false
            )
    ).

create_children :-
    move(a,b),
    move(a,c),
    move(b,a),
    move(b,c),
    move(c,a),
    move(c,b),
    move(a,table),
    move(b,table),
    move(c,table).

evaluate_nodes([]).
evaluate_nodes(FRONTIER) :-
    [NODE|OTHER_NODES] = FRONTIER,
    node_ref(NODE,_,COST_TO_NODE,_),
    COST is COST_TO_NODE,
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

search(GOALS):-
    frontier(FRONTIER),
    retractall(min_cost(_)),
    assertz(min_cost(inf)),
    retractall(best_node(_)),
    evaluate_nodes(FRONTIER),
    (   
    	(reached_goals(GOALS)) ->
    		true;
    		(  
                create_children,
                search(GOALS)
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

modify_initial_state(INITIAL_STATE) :-
    retractall(node_ref(0,_,0,_)),
    assertz(node_ref(0, INITIAL_STATE, 0, 'INIT')).

main(INITIAL_STATE, GOALS, EXPLORED_NODES, COST, ACTION_LIST) :-
    modify_initial_state(INITIAL_STATE),
    search(GOALS),
    return_goal(GOALS, EXPLORED_NODES, COST, ACTION_LIST).