:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').

% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state([]).

% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile(S, Pos, SNew) :-
    Cel = [Pos | "+"],
    SNew = [Cel | S].

% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(S, Pos, SNew) :-
    Cel = [Pos | " "],
    SNew = [Cel | S].

% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(S, Pos, SNew) :-
    Cel = [Pos | "S"],
    SNew = [Cel | S].

% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(S, Pos, SNew) :-
    Cel = [Pos | "-"],
    SNew = [Cel | S].

% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial(S, Pos, SNew) :-
    CelB = [Pos | "B"],
    CelNormal = [Pos | "+"],
    SNew = [CelB, CelNormal | S].

% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)])
get_b_pos([], _) :- false.
get_b_pos(S, Pos) :-
    get_b_aux(S, SPos1, SRest),
    (
        (\+ get_b_aux(SRest, _, _),
        Pos = SPos1)
    ;
        (get_b_aux(SRest, SPos2, _),
        Pos = [SPos1, SPos2])
    ).

% get_b_aux/3
% get_b_aux(+S, -Pos, -Rest)
% Obține o parte a poziției blocului și întoarce poziția acestuia și restul listei
get_b_aux([], _, _) :- false.
get_b_aux([S | SRest1], Pos, SRest) :-
    (S = [Pos | "B"], SRest = SRest1)
    ;
    (\+ S = [Pos | "B"], get_b_aux(SRest1, Pos, SRest2), SRest = SRest2).

% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds([], 1000, -1000, 1000, -1000).
get_bounds([[(X, Y) | _] | SRest], Xmin, Xmax, Ymin, Ymax) :-
    get_bounds(SRest, XminTemp, XmaxTemp, YminTemp, YmaxTemp),
    (X < XminTemp, Xmin = X; Xmin = XminTemp),
    (X > XmaxTemp, Xmax = X; Xmax = XmaxTemp),
    (Y < YminTemp, Ymin = Y; Ymin = YminTemp),
    (Y > YmaxTemp, Ymax = Y; Ymax = YmaxTemp).

% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell([], _, _) :- false.
get_cell([[SPos | SType] | _], SPos, Type):-
    (SType == "+", Type = tile);
    (SType == "-", Type = fragile);
    (SType == "S", Type = target);
    (SType = (Type, _, _)).
get_cell([_ | SRest], Pos, Type) :-
    get_cell(SRest, Pos, Type).

get_switch_val([], _, _, _) :- false.
get_switch_val([[SPos | (_, Func, Posx)] | _], SPos, Func, Posx).
get_switch_val([_ | SRest], Pos, Func, Posx) :-
    get_switch_val(SRest, Pos, Func, Posx).

% change_cell(S+, Pos+, New_type+, SNew-)
% schimba valoarea unei celule si returneaza noua lista
change_cell([], _, _, _) :- false.
change_cell([[SPos | _] | SRest], SPos, New_type, SNew) :-
    SNew = [[SPos | New_type] | SRest].
change_cell([S | SRest], Pos, New_type, SNew) :-
    change_cell(SRest, Pos, New_type, Rest),
    SNew = [S | Rest].

% delete_cell(S+, Pos+, Type+, SNew-)
delete_cell([], _, _, _) :- false.
delete_cell([[SPos | Type] | SRest], SPos, Type, SNew) :-
    SNew = SRest.
delete_cell([S | SRest], Pos, Type, SNew) :-
    delete_cell(SRest, Pos, Type, SNew1),
    SNew = [S | SNew1].

change_b_pos([], _, _, _) :- false.
change_b_pos(S, PosOld, PosNew, SNew) :-
    ((is_list(PosOld),
        PosOld = [Pos1, Pos2],
        delete_cell(S, Pos1, "B", S1), delete_cell(S1, Pos2, "B", SNewAux))
    ;
    (\+ is_list(PosOld),
        PosOld = Pos,
        delete_cell(S, Pos, "B", SNewAux))),
    ((is_list(PosNew),
        PosNew = [PosNew1, PosNew2],
        SNewAux1 = [PosNew1 | "B"],
        SNewAux2 = [PosNew2 | "B"],
        SNew = [SNewAux1, SNewAux2 | SNewAux])
    ;
    (\+ is_list(PosNew),
        SNewAux1 = [PosNew | "B"],
        SNew = [SNewAux1 | SNewAux])).

check_if_tiles_exist(_, []).
check_if_tiles_exist(S, [Pos | PosRest]) :-
    get_cell(S, Pos, _),
    check_if_tiles_exist(S, PosRest).


add_tiles(_, [], []).
add_tiles(S, [Pos | PosRest], SNew) :-
    add_tiles([], PosRest, SAux),
    ((S == [],
        SNew = [[Pos | "+"] | SAux])
    ;
    SAux = [SAux2],
    SNew = [[Pos | "+"] , SAux2 | S]).

delete_tiles(_,[],[]).
delete_tiles(S, [Pos | PosRest], SNew) :-
    delete_tiles(S, PosRest, SAux),
    delete_cell(SAux, Pos, "+", SNew).

delete_tiles(S, [Pos], SNew) :-
    delete_cell(S, Pos, "+", SNew).

% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move([], _, _) :- false.
move(S, Move, SNext) :-
    get_b_pos(S, Pos),
    is_list(Pos),
    Pos = [(X1, Y1), (X2, Y2)],
        % down -> down
        ((((Move == r, X1 == X2,
            XNew1 is X1 + 1, XNew2 is X1 + 1,
            YNew1 is Y1, YNew2 is Y2)
        ;
        (Move == l, X1 == X2,
            XNew1 is X1 - 1, XNew2 is X1 - 1,
            YNew1 is Y1, YNew2 is Y2)
        ;
        (Move == u, Y1 == Y2,
            YNew1 is Y1 - 1, YNew2 is Y1 -1,
            XNew1 is X1, XNew2 is X2)
        ;
        (Move == d, Y1 == Y2,
            YNew1 is Y1 + 1, YNew2 is Y1 +1,
            XNew1 is X1, XNew2 is X2)),
        (get_cell(S, (XNew1, YNew1), Type1), get_cell(S, (XNew2, YNew2), Type2),
        change_b_pos(S, Pos, [(XNew1, YNew1), (XNew2, YNew2)], S1)),
        TypeAux1 = Type1, XAux1 = XNew1, YAux1 = YNew1,
        TypeAux2 = Type2, XAux2 = XNew2, YAux2 = YNew2)
        ; % down -> up
        (((Move == r, Y1 == Y2,
            XNew is X2 + 1,
            YNew is Y1)
        ;
        (Move == l, Y1 == Y2,
            XNew is X1 - 1,
            YNew is Y1)
        ;
        (Move == u, X1 == X2,
            YNew is Y1 - 1,
            XNew is X1)
        ;
        (Move == d, X1 == X2,
            YNew is Y2 + 1,
            XNew is X1)),
        (get_cell(S, (XNew, YNew), Type), Type \= fragile,
        change_b_pos(S, Pos, (XNew, YNew), S1)),
        TypeAux1 = Type, XAux1 = XNew, YAux1 = YNew, TypeAux2 = null)),
        ((((TypeAux1 == oswitch, XSwitch = XAux1, YSwitch = YAux1) ; (TypeAux2 == oswitch, XSwitch = XAux2, YSwitch = YAux2)),
        get_switch_val(S1, (XSwitch, YSwitch), Func, [Posx]),
        ((Func == uponly,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func = dnonly,
            ((check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func = switch,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            (check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))))))
        ;
        (((TypeAux1 == xswitch, TypeAux2 == null, XSwitch = XAux1, YSwitch = YAux1)),
        get_switch_val(S1, (XSwitch, YSwitch), Func, [Posx]),
        ((Func == uponly,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func = dnonly,
            ((check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func = switch,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            (check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))))))
    ;
    SAux = S1),
    SNext = SAux.

% up -> down
move(S, Move, SNext) :-
    get_b_pos(S, Pos),
    \+ is_list(Pos),
    Pos = (X, Y),
    ((Move == r,
        X1 is X + 1, get_cell(S, (X1, Y), Type1),
        X2 is X + 2, get_cell(S, (X2, Y), Type2),
        Y1 is Y, Y2 is Y)
    ;
    (Move == l,
        X1 is X - 2, get_cell(S, (X1, Y), Type1),
        X2 is X - 1, get_cell(S, (X2, Y), Type2),
        Y1 is Y, Y2 is Y)
    ;
    (Move == u,
        Y1 is Y - 2, get_cell(S, (X, Y1), Type1),
        Y2 is Y - 1, get_cell(S, (X, Y2), Type2),
        X1 is X, X2 is X)
    ;
    (Move == d,
        Y1 is Y + 1, get_cell(S, (X, Y1), Type1),
        Y2 is Y + 2, get_cell(S, (X, Y2), Type2),
        X1 is X, X2 is X)),
    change_b_pos(S, Pos, [(X1, Y1), (X2, Y2)], S1),
    ((((Type1 == oswitch, XSwitch = X1, YSwitch = Y1) ; (Type2 == oswitch, XSwitch = X2, YSwitch = Y2)),
        get_switch_val(S1, (XSwitch, YSwitch), Func, [Posx]),
        ((Func == uponly,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func == dnonly,
            ((check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))
            ;
            SAux = S1))
        ;
        (Func = switch,
            ((\+ check_if_tiles_exist(S1, Posx), add_tiles(S1, Posx, SAux))
            ;
            (check_if_tiles_exist(S1, Posx), delete_tiles(S1, Posx, SAux))))))
    ;
    SAux = S1),
    SNext = SAux.

% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(S) :-
    get_b_pos(S, Pos),
    \+ Pos = [(_, _), (_, _)],
    get_cell(S, Pos, Type),
    Type == target.



%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(S, Pos, Switch, Func, Posx, SNew) :-
    Cel = [Pos | (Switch, Func, [Posx])],
    SNew = [Cel | S].

%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).
solve(_, _) :- false.






