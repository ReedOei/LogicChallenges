#!/usr/bin/env swipl

:- use_module(library(clpfd)).

:- use_module(library(yall)).

:- style_check(-singleton).
:- style_check(-no_effect).
:- style_check(-var_branches).
:- style_check(-discontiguous).
:- style_check(-charset).


map_built_in(_F, empty, empty).
map_built_in(F,cons(H,T),cons(NewH,NewT)) :-
    call(F, H, NewH),
    map_built_in(F, T, NewT).

filter_built_in(_F, empty, empty).
filter_built_in(F, cons(H, T), Out) :-
    call(F, H), filter_built_in(F, T, Temp),
        Out = cons(H, Temp);
    filter_built_in(F, T, Out).

call_built_in(F, X, Res) :- call(F, X, Res).
call_rule_built_in(F, X) :- call(F, X).

disjunction_built_in(A, B, X) :-
    call(A, X);
    call(B, X).

one_of_built_in(empty, _) :- false.
one_of_built_in(cons(P, Rest), X) :-
    call(P, X);
    one_of_built_in(Rest, X).



:- initialization(main, main).

main(Argv) :-
            in_luhn_form_with(cons(7,cons(9,cons(9,cons(2,cons(7,cons(3,cons(9,cons(8,cons(7,cons(1,empty)))))))))),3)
            ,
            display('Success: Yay!'),
            in_luhn_form_with(cons(7,cons(9,cons(9,cons(2,cons(7,cons(3,cons(9,cons(8,cons(7,cons(1,empty)))))))))),CheckDigit)
            ,
            as_text(CheckDigit,Temp211),
            display(Temp211),
            not(in_luhn_form_with(cons(7,cons(9,cons(9,cons(2,cons(7,cons(3,cons(9,cons(8,cons(7,cons(1,empty)))))))))),5))
            ,
            display('Failure :(').
% Unit
enki_true :-
    1 = 1.

% Unit
enki_false :-
    1 = 2.

% FuncType EnkiInt EnkiInt
-(X,Temp0) :-
    Temp0 #= (0 - X).

% EnkiString
display(X) :-
    writeln(X).

% FuncType (Any "T7") EnkiString
as_text(X,Temp2) :-
    term_to_atom(X,Temp2).

% FuncType (FuncType (Any "T11") (Any "T12")) (FuncType (TypeName [Named "list",Any "T11"]) (TypeName [Named "list",Any "T12"]))
map_over(F,Xs,Temp3) :-
    map_built_in(F,Xs,Temp3).

% FuncType (TypeName [Named "list",Any "T16"]) (FuncType (Any "T16") (TypeName [Named "list",Any "T16"]))
filter_with(Xs,F,Temp4) :-
    filter_built_in(F,Xs,Temp4).

% RuleType (Any "T20") (RuleType (Any "T20") (Any "T20"))
either_or_holds_for(P,Q,X) :-
    disjunction_built_in(P,Q,X).

% RuleType (TypeName [Named "list",Any "T23"]) (Any "T23")
one_of_holds_for(Ps,X) :-
    one_of_built_in(Ps,X).

% FuncType EnkiString EnkiInt
number_of_characters_in(T,Temp7) :-
    atom_length(T,Temp7).

% FuncType (TypeName [Named "list",Any "T29"]) (FuncType (Any "T29") (TypeName [Named "list",Any "T29"]))
only_elements_of_satisfying(List,F,Temp8) :-
    filter_with(List,F,Temp8).

% FuncType (FuncType (Any "T33") (Any "T34")) (FuncType (Any "T33") (Any "T34"))
call_on(F,X,Temp9) :-
    call_built_in(F,X,Temp9).

% FuncType (Any "T38") (FuncType (Any "T38") Void)
call_rule_on(R,X,call_rule_built_in(R,X)) :-
    call_rule_built_in(R,X).

% FuncType (Any "T39") (Any "T39")
the(X,X).

% FuncType (Any "T40") (Any "T40")
id(X,X).

% EnkiInt
odd(X) :-
    Temp11 #= (2 * K),
    Temp12 #= (Temp11 + 1),
    X = Temp12.

% EnkiInt
even(X) :-
    Temp13 #= (2 * K),
    X = Temp13.

% FuncType EnkiInt EnkiInt
square_root(X,Root) :-
    Temp14 #= (Root ^ 2),
    Temp14 = X.

% RuleType EnkiInt EnkiInt
divides(A,B) :-
    Temp15 #= (A * N),
    B = Temp15.

% RuleType EnkiInt EnkiInt
pipe_(A,B) :-
    divides(A,B).

% FuncType (Any "T61") (FuncType (TypeName [Named "list",Any "T61"]) (TypeName [Named "list",Any "T61"]))
colon_colon_(X,Xs,cons(X,Xs)).

% FuncType (Any "T65") (FuncType (TypeName [Named "list",Any "T65"]) (TypeName [Named "list",Any "T65"]))
prepend_to(Head,Tail,cons(Head,Tail)).

% FuncType (TypeName [Named "pair",Any "T70",Any "T71"]) (Any "T70")
fst(AUTOGENARG2,A) :-
    AUTOGENARG2 = pair_and(A,B).

% FuncType (TypeName [Named "pair",Any "T78",Any "T79"]) (Any "T79")
snd(AUTOGENARG2,B) :-
    AUTOGENARG2 = pair_and(A,B).

% FuncType EnkiInt (TypeName [Named "list",Any "T88"])
list_of_length(N,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N #=< 0
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
            Temp20 #= (N - 1),
            list_of_length(Temp20,Temp19),
            prepend_to(NewElem,Temp19,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType (TypeName [Named "list",Any "T115"]) EnkiInt
length_of(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T)
            ,
            length_of(T,Temp21),
            Temp22 #= (1 + Temp21),
            AUTOGENERATEDFUNCTIONRESULT = Temp22
        ;
            AUTOGENERATEDFUNCTIONRESULT = 0
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T141"]) (Any "T141"))
th_element_of(N,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T),
            N = 0
            ,
            AUTOGENERATEDFUNCTIONRESULT = H
        ;
                List = cons(H,T),
                N #> 0
                ,
                Temp23 #= (N - 1),
                th_element_of(Temp23,T,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T183"]) (FuncType (Any "T183") (TypeName [Named "list",Any "T183"])))
set_index_of_to(Idx,List,NewVal,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T),
            Idx #=< 0
            ,
            prepend_to(NewVal,T,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                List = cons(H,T),
                Idx #> 0
                ,
                Temp25 #= (Idx - 1),
                set_index_of_to(Temp25,T,NewVal,Temp24),
                prepend_to(H,Temp24,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                prepend_to(NewVal,empty,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        )
    ).

% FuncType (TypeName [Named "list",Any "T207"]) (FuncType (TypeName [Named "list",Any "T201"]) (TypeName [Named "list",Any "T201"]))
concat_with(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(H,T)
            ,
            concat_with(T,B,Temp27),
            prepend_to(H,Temp27,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
            A = empty,
            AUTOGENERATEDFUNCTIONRESULT = B
    ).

% FuncType (TypeName [Named "list",Any "T220"]) (FuncType (TypeName [Named "list",Any "T221"]) (TypeName [Named "list",Any "T221"]))
plus_plus_(A,B,Temp28) :-
    concat_with(A,B,Temp28).

% FuncType (TypeName [Named "list",TypeName [Named "list",Any "T239"]]) (TypeName [Named "list",Any "T234"])
flatten(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T)
            ,
            flatten(T,Temp29),
            concat_with(H,Temp29,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
            AUTOGENERATEDFUNCTIONRESULT = empty
    ).

% FuncType (TypeName [Named "list",Any "T264"]) (FuncType (TypeName [Named "list",Any "T264"]) (TypeName [Named "list",Any "T264"]))
reverseAcc(A,Acc,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(H,T)
            ,
            reverseAcc(T,cons(H,Acc),AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
                A = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = Acc
    ).

% FuncType (TypeName [Named "list",Any "T274"]) (TypeName [Named "list",Any "T274"])
reverse_list(A,Temp31) :-
    reverseAcc(A,empty,Temp31).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
sum_of(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = empty
            ,
            AUTOGENERATEDFUNCTIONRESULT = 0
        ;
                List = cons(H,T)
                ,
                sum_of(T,Temp33),
                Temp34 #= (H + Temp33),
                AUTOGENERATEDFUNCTIONRESULT = Temp34
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",EnkiInt]) (TypeName [Named "list",EnkiInt]))
multiples_of_in(A,List,Temp35) :-
    filter_with(List,{A}/[_0]>>(divides(A,_0)),Temp35).

% FuncType (TypeName [Named "list",Any "T338"]) EnkiString
formatHelper(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,empty)
            ,
            as_text(H,Temp38),
            atom_concat(Temp38,']',AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                List = cons(H,T)
                ,
                as_text(H,Temp40),
                atom_concat(Temp40,',',Temp39),
                formatHelper(T,Temp41),
                atom_concat(Temp39,Temp41,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = ']'
        )
    ).

% FuncType (TypeName [Named "list",Any "T350"]) EnkiString
format_list(List,Temp42) :-
    formatHelper(List,Temp43),
    atom_concat('[',Temp43,Temp42).

% FuncType EnkiInt (FuncType EnkiInt (TypeName [Named "list",EnkiInt]))
range_to(Low,High,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Low #> High
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
                Low #=< High
                ,
                Temp45 #= (Low + 1),
                range_to(Temp45,High,Temp44),
                AUTOGENERATEDFUNCTIONRESULT = cons(Low,Temp44)
    ).

% FuncType EnkiInt (FuncType EnkiInt (TypeName [Named "list",EnkiInt]))
range_from_to(Low,High,Temp46) :-
    range_to(Low,High,Temp46).

% FuncType EnkiInt (FuncType EnkiInt (TypeName [Named "list",EnkiInt]))
integers_from_to(Low,High,Temp47) :-
    range_to(Low,High,Temp47).

% FuncType (TypeName [Named "list",Any "T409"]) (FuncType (TypeName [Named "list",Any "T410"]) (TypeName [Named "list",TypeName [Named "pair",Any "T399",Any "T400"]]))
zip_and(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(HA,TA),
            B = cons(HB,TB)
            ,
            zip_and(TA,TB,Temp49),
            AUTOGENERATEDFUNCTIONRESULT = cons(pair_and(HA,HB),Temp49)
        ;
        (
                A = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = empty
            ;
                    B = empty
                    ,
                    AUTOGENERATEDFUNCTIONRESULT = empty
        )
    ).

% FuncType (FuncType (Any "T473") (FuncType (Any "T474") (FuncType (Any "T474") (Any "T475")))) (FuncType (TypeName [Named "list",Any "T473"]) (FuncType (TypeName [Named "list",Any "T474"]) (TypeName [Named "list",Any "T448"])))
zip_with_and(F,A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(HA,TA),
            B = cons(HB,TB)
            ,
            call_on(F,HA,Temp51),
            call_on(Temp51,HB,Temp50),
            zip_with_and(F,TA,TB,Temp52),
            AUTOGENERATEDFUNCTIONRESULT = cons(Temp50,Temp52)
        ;
        (
                A = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = empty
            ;
                    B = empty
                    ,
                    AUTOGENERATEDFUNCTIONRESULT = empty
        )
    ).

% FuncType (FuncType (Any "T538") (FuncType (Any "T539") (FuncType (Any "T539") (Any "T540")))) (FuncType (TypeName [Named "list",Any "T538"]) (FuncType (TypeName [Named "list",Any "T539"]) (TypeName [Named "list",Any "T513"])))
zip_same_length_and(F,A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(HA,TA),
            B = cons(HB,TB)
            ,
            call_on(F,HA,Temp54),
            call_on(Temp54,HB,Temp53),
            zip_same_length_and(F,TA,TB,Temp55),
            AUTOGENERATEDFUNCTIONRESULT = cons(Temp53,Temp55)
        ;
        (
                A = empty,
                B = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = empty
            ;
                enki_false(),
                AUTOGENERATEDFUNCTIONRESULT = empty
        )
    ).

% FuncType EnkiInt (FuncType EnkiInt EnkiInt)
max_of_and(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A #> B
            ,
            AUTOGENERATEDFUNCTIONRESULT = A
        ;
            AUTOGENERATEDFUNCTIONRESULT = B
    ).

% FuncType EnkiInt (FuncType EnkiInt EnkiInt)
min_of_and(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A #> B
            ,
            AUTOGENERATEDFUNCTIONRESULT = A
        ;
            AUTOGENERATEDFUNCTIONRESULT = B
    ).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
maximum_of(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,empty)
            ,
            AUTOGENERATEDFUNCTIONRESULT = H
        ;
                List = cons(H,T)
                ,
                maximum_of(T,Temp58),
                max_of_and(H,Temp58,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
minimum_of(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,empty)
            ,
            AUTOGENERATEDFUNCTIONRESULT = H
        ;
                List = cons(H,T)
                ,
                minimum_of(T,Temp60),
                min_of_and(H,Temp60,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType EnkiInt (FuncType EnkiInt EnkiInt)
find_factor_of_starting_with(N,X,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            divides(X,N)
            ,
            AUTOGENERATEDFUNCTIONRESULT = X
        ;
            Temp62 #= (X + 1),
            find_factor_of_starting_with(N,Temp62,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType EnkiInt (TypeName [Named "list",EnkiInt])
factors_of(N,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N = 1
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
            find_factor_of_starting_with(N,2,Factor),
            Factor = Factor,
            Temp64 #= (N div Factor),
            factors_of(Temp64,Temp63),
            AUTOGENERATEDFUNCTIONRESULT = cons(Factor,Temp63)
    ).

% FuncType EnkiInt (TypeName [Named "list",EnkiInt])
digits_of(N,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N #< 10
            ,
            AUTOGENERATEDFUNCTIONRESULT = cons(N,empty)
        ;
                N #>= 10
                ,
                Temp66 #= (10 * Rest),
                Temp67 #= (Temp66 + Digit),
                N = Temp67,
                Digit #>= 0,
                Digit #< 10,
                digits_of(Rest,Temp68),
                AUTOGENERATEDFUNCTIONRESULT = cons(Digit,Temp68)
    ).

% TypeName [Named "list",Any "T699"]
palindrome(List) :-
    reverse_list(List,List),
    List = List.

% EnkiInt
palindromic_number(N) :-
    digits_of(N,Temp70),
    palindrome(Temp70).

% FuncType (Any "T726") (FuncType (TypeName [Named "list",Any "T727"]) (TypeName [Named "list",TypeName [Named "pair",Any "T716",Any "T717"]]))
pair_with_each(X,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T)
            ,
            pair_with_each(X,T,Temp72),
            AUTOGENERATEDFUNCTIONRESULT = cons(pair_and(X,H),Temp72)
        ;
            AUTOGENERATEDFUNCTIONRESULT = empty
    ).

% FuncType (TypeName [Named "list",Any "T763"]) (FuncType (TypeName [Named "list",Any "T764"]) (TypeName [Named "list",Any "T747"]))
cartesian_product_of_and(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = cons(H,T)
            ,
            pair_with_each(H,B,Temp73),
            cartesian_product_of_and(T,B,Temp74),
            concat_with(Temp73,Temp74,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
                A = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = empty
    ).

% FuncType (TypeName [Named "list",Any "T778"]) (FuncType (TypeName [Named "list",Any "T779"]) (TypeName [Named "list",Any "T780"]))
pairs_of_and(A,B,Temp75) :-
    cartesian_product_of_and(A,B,Temp75).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T801"]) (TypeName [Named "list",Any "T794"]))
take_from(N,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N #> 0,
            List = cons(H,T)
            ,
            Temp77 #= (N - 1),
            take_from(Temp77,T,Temp76),
            AUTOGENERATEDFUNCTIONRESULT = cons(H,Temp76)
        ;
            AUTOGENERATEDFUNCTIONRESULT = empty
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T823"]) (TypeName [Named "list",Any "T823"]))
drop_from(N,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N #> 0,
            List = cons(H,T)
            ,
            Temp78 #= (N - 1),
            drop_from(Temp78,T,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
            AUTOGENERATEDFUNCTIONRESULT = List
    ).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
product_of(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T)
            ,
            product_of(T,Temp79),
            Temp80 #= (H * Temp79),
            AUTOGENERATEDFUNCTIONRESULT = Temp80
        ;
            AUTOGENERATEDFUNCTIONRESULT = 1
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T862"]) (FuncType (Any "T868") (Any "T868")))
element_of_starting_with(N,List,H,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            N #=< 0
            ,
            AUTOGENERATEDFUNCTIONRESULT = H
        ;
        (
                List = cons(X,Rest)
                ,
                Temp81 #= (N - 1),
                element_of_starting_with(Temp81,Rest,X,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = H
        )
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T904"]) (TypeName [Named "list",TypeName [Named "list",Any "T887"]]))
chunks_of_size_in(L,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = empty
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
            take_from(L,List,Temp82),
            drop_from(L,List,Temp84),
            chunks_of_size_in(L,Temp84,Temp83),
            AUTOGENERATEDFUNCTIONRESULT = cons(Temp82,Temp83)
    ).

% FuncType EnkiInt (FuncType (TypeName [Named "list",Any "T909"]) (TypeName [Named "list",TypeName [Named "list",Any "T910"]]))
chunks_of_length_in(L,List,Temp85) :-
    chunks_of_size_in(L,List,Temp85).

% RuleType (TypeName [Named "list",Any "T921"]) (Any "T921")
contains(List,Element) :-
    (
            List = cons(H,T),
            H = Element
            ,
            1 = 1
        ;
        (
                List = cons(H,T)
                ,
                contains(T,Element)
            ;
                1 = 2
        )
    ).

% FuncType (Any "T948") (FuncType (TypeName [Named "list",Any "T948"]) (TypeName [Named "list",Any "T948"]))
remove_from(Element,List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T),
            H = Element
            ,
            AUTOGENERATEDFUNCTIONRESULT = T
        ;
        (
                List = cons(H,T)
                ,
                remove_from(Element,T,Temp87),
                AUTOGENERATEDFUNCTIONRESULT = cons(H,Temp87)
            ;
                1 = 2,
                AUTOGENERATEDFUNCTIONRESULT = empty
        )
    ).

% FuncType (Any "T960") (FuncType (Any "T961") (TypeName [Named "mapping",Any "T960",Any "T961"]))
maps_to(X,Y,pipe_dash_gt_(X,Y)).

% FuncType EnkiInt (FuncType EnkiInt (Named "ordering"))
compare_to(A,B,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A #> B
            ,
            AUTOGENERATEDFUNCTIONRESULT = greater
        ;
        (
                A #< B
                ,
                AUTOGENERATEDFUNCTIONRESULT = less
            ;
                AUTOGENERATEDFUNCTIONRESULT = equal
        )
    ).

% FuncType (TypeName [Named "mapping",Any "T981",Any "T982"]) (Any "T981")
key_in(AUTOGENARG3,K) :-
    maps_to(K,V,AUTOGENARG3),
    AUTOGENARG3 = AUTOGENARG3.

% FuncType (TypeName [Named "mapping",Any "T989",Any "T990"]) (Any "T990")
value_in(AUTOGENARG3,V) :-
    maps_to(K,V,AUTOGENARG3),
    AUTOGENARG3 = AUTOGENARG3.

% FuncType (TypeName [Named "mapping",EnkiInt,Any "T999"]) (FuncType (TypeName [Named "mapping",EnkiInt,Any "T1006"]) (Named "ordering"))
compare_keys_to(AUTOGENARG3,AUTOGENARG5,Temp89) :-
    AUTOGENARG3 = pipe_dash_gt_(K1,V1),
    AUTOGENARG5 = pipe_dash_gt_(K2,V2),
    compare_to(K1,K2,Temp89).

% FuncType (TypeName [Named "mapping",Any "T1015",EnkiInt]) (FuncType (TypeName [Named "mapping",Any "T1022",EnkiInt]) (Named "ordering"))
compare_values_to(AUTOGENARG3,AUTOGENARG5,Temp90) :-
    AUTOGENARG3 = pipe_dash_gt_(K1,V1),
    AUTOGENARG5 = pipe_dash_gt_(K2,V2),
    compare_to(V1,V2,Temp90).

% FuncType (Any "T1036") (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1036",Any "T1037"]]) (Any "T1037"))
get_key_from(Key,Map,V) :-
    contains(Map,pipe_dash_gt_(Key,V)).

% FuncType (Any "T1047") (FuncType (Any "T1048") (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1047",Any "T1048"]]) (TypeName [Named "list",TypeName [Named "mapping",Any "T1047",Any "T1048"]])))
insert_into(K,V,Map,cons(pipe_dash_gt_(K,V),Map)).

% FuncType (Any "T1058") (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1058",Any "T1059"]]) (TypeName [Named "list",TypeName [Named "mapping",Any "T1058",Any "T1059"]]))
remove_key_from(Key,Map,Temp95) :-
    remove_from(pipe_dash_gt_(Key,V),Map,Temp95).

% FuncType (Any "T1081") (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1081",EnkiInt]]) (TypeName [Named "list",TypeName [Named "mapping",Any "T1081",EnkiInt]]))
increment_key_in(Key,Map,Temp97) :-
    Temp100 #= (V + 1),
    remove_from(pipe_dash_gt_(Key,V),Map,Temp98),
    insert_into(Key,Temp100,Temp98,Temp97).

% FuncType (TypeName [Named "mapping",EnkiInt,Any "T1113"]) (FuncType (TypeName [Named "list",TypeName [Named "mapping",EnkiInt,Any "T1114"]]) (TypeName [Named "mapping",EnkiInt,Any "T1113"]))
max_key_at_least_in(M1,Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M2,Assocs),
            compare_keys_to(M2,M1,Temp101),
            Temp101 = less
            ,
            max_key_at_least_in(M2,Assocs,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                Map = cons(P,Assocs)
                ,
                max_key_at_least_in(M1,Assocs,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = M1
        )
    ).

% FuncType (TypeName [Named "list",TypeName [Named "mapping",EnkiInt,Any "T1136"]]) (TypeName [Named "maybe",TypeName [Named "mapping",EnkiInt,Any "T1136"]])
max_key_in(Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M1,Assocs)
            ,
            max_key_at_least_in(M1,Map,Temp103),
            AUTOGENERATEDFUNCTIONRESULT = just(Temp103)
        ;
            AUTOGENERATEDFUNCTIONRESULT = nothing
    ).

% FuncType (TypeName [Named "mapping",EnkiInt,Any "T1171"]) (FuncType (TypeName [Named "list",TypeName [Named "mapping",EnkiInt,Any "T1172"]]) (TypeName [Named "mapping",EnkiInt,Any "T1171"]))
min_key_no_more_than_in(M1,Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M2,Assocs),
            compare_keys_to(M2,M1,Temp104),
            Temp104 = less
            ,
            min_key_no_more_than_in(M2,Assocs,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                Map = cons(P,Assocs)
                ,
                min_key_no_more_than_in(M1,Assocs,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = M1
        )
    ).

% FuncType (TypeName [Named "list",TypeName [Named "mapping",EnkiInt,Any "T1194"]]) (TypeName [Named "maybe",TypeName [Named "mapping",EnkiInt,Any "T1194"]])
min_key_in(Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M1,Assocs)
            ,
            min_key_no_more_than_in(M1,Map,Temp106),
            AUTOGENERATEDFUNCTIONRESULT = just(Temp106)
        ;
            AUTOGENERATEDFUNCTIONRESULT = nothing
    ).

% FuncType (TypeName [Named "mapping",Any "T1229",EnkiInt]) (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1230",EnkiInt]]) (TypeName [Named "mapping",Any "T1229",EnkiInt]))
max_value_at_least_in(M1,Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M2,Assocs),
            compare_values_to(M2,M1,Temp107),
            Temp107 = less
            ,
            max_value_at_least_in(M2,Assocs,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                Map = cons(P,Assocs)
                ,
                max_value_at_least_in(M1,Assocs,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = M1
        )
    ).

% FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1252",EnkiInt]]) (TypeName [Named "maybe",TypeName [Named "mapping",Any "T1252",EnkiInt]])
max_value_in(Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M1,Assocs)
            ,
            max_value_at_least_in(M1,Map,Temp109),
            AUTOGENERATEDFUNCTIONRESULT = just(Temp109)
        ;
            AUTOGENERATEDFUNCTIONRESULT = nothing
    ).

% FuncType (TypeName [Named "mapping",Any "T1287",EnkiInt]) (FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1288",EnkiInt]]) (TypeName [Named "mapping",Any "T1287",EnkiInt]))
min_value_no_more_than_in(M1,Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M2,Assocs),
            compare_values_to(M2,M1,Temp110),
            Temp110 = less
            ,
            min_value_no_more_than_in(M2,Assocs,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                Map = cons(P,Assocs)
                ,
                min_value_no_more_than_in(M1,Assocs,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                AUTOGENERATEDFUNCTIONRESULT = M1
        )
    ).

% FuncType (TypeName [Named "list",TypeName [Named "mapping",Any "T1310",EnkiInt]]) (TypeName [Named "maybe",TypeName [Named "mapping",Any "T1310",EnkiInt]])
min_value_in(Map,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Map = cons(M1,Assocs)
            ,
            min_value_no_more_than_in(M1,Map,Temp112),
            AUTOGENERATEDFUNCTIONRESULT = just(Temp112)
        ;
                Map = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = nothing
    ).

% FuncType (Any "T1322") (TypeName [Named "list",Any "T1322"])
singleton_list(X,cons(X,empty)).

% FuncType (TypeName [Named "list",Any "T1355"]) (TypeName [Named "list",Any "T1341"])
init(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,empty)
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
        (
                List = cons(H,T)
                ,
                init(T,Temp116),
                AUTOGENERATEDFUNCTIONRESULT = cons(H,Temp116)
            ;
                    List = empty
                    ,
                    AUTOGENERATEDFUNCTIONRESULT = empty
        )
    ).

% FuncType (TypeName [Named "list",Any "T1389"]) (TypeName [Named "maybe",Any "T1389"])
last(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,empty)
            ,
            AUTOGENERATEDFUNCTIONRESULT = just(H)
        ;
        (
                List = cons(H,T)
                ,
                last(T,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                    List = empty
                    ,
                    AUTOGENERATEDFUNCTIONRESULT = nothing
        )
    ).

% TypeName [Named "list",EnkiString]
digit_list(cons('0',cons('1',cons('2',cons('3',cons('4',cons('5',cons('6',cons('7',cons('8',cons('9',empty))))))))))).

% FuncType EnkiString EnkiString
first_character_of(Text,Letter) :-
    atom_concat(Letter,Rest,Text),
    Text = Text,
    number_of_characters_in(Letter,Temp129),
    Temp129 = 1.

% FuncType EnkiString EnkiString
first_letter_of(Text,Temp130) :-
    first_character_of(Text,Temp130).

% RuleType EnkiInt (RuleType EnkiString EnkiString)
first_digit_of_and(Digit,Str,Rest) :-
    (
            atom_concat('0',Rest,Str),
            Str = Str
            ,
            Digit = 0
        ;
        (
                atom_concat('1',Rest,Str),
                Str = Str
                ,
                Digit = 1
            ;
            (
                    atom_concat('2',Rest,Str),
                    Str = Str
                    ,
                    Digit = 2
                ;
                (
                        atom_concat('3',Rest,Str),
                        Str = Str
                        ,
                        Digit = 3
                    ;
                    (
                            atom_concat('4',Rest,Str),
                            Str = Str
                            ,
                            Digit = 4
                        ;
                        (
                                atom_concat('5',Rest,Str),
                                Str = Str
                                ,
                                Digit = 5
                            ;
                            (
                                    atom_concat('6',Rest,Str),
                                    Str = Str
                                    ,
                                    Digit = 6
                                ;
                                (
                                        atom_concat('7',Rest,Str),
                                        Str = Str
                                        ,
                                        Digit = 7
                                    ;
                                    (
                                            atom_concat('8',Rest,Str),
                                            Str = Str
                                            ,
                                            Digit = 8
                                        ;
                                                atom_concat('9',Rest,Str),
                                                Str = Str
                                                ,
                                                Digit = 9
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ).

% FuncType EnkiString (TypeName [Named "pair",TypeName [Named "list",EnkiInt],EnkiString])
digits_from(Str,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            first_digit_of_and(Digit,Str,Rest)
            ,
            digits_from(Rest,Temp133),
            pair_and(Digs,Remaining) = Temp133,
            prepend_to(Digit,Digs,Temp134),
            AUTOGENERATEDFUNCTIONRESULT = pair_and(Temp134,Remaining)
        ;
                not(first_digit_of_and(Digit,Str,Rest))
                ,
                AUTOGENERATEDFUNCTIONRESULT = pair_and(empty,Str)
    ).

% FuncType EnkiInt (FuncType EnkiInt EnkiInt)
mod(A,B,R) :-
    Temp138 #= (Q * B),
    Temp139 #= (Temp138 + R),
    A = Temp139,
    0 #=< R,
    R #< B.

% FuncType EnkiInt (TypeName [Named "list",EnkiInt])
to_reversed_digits(X,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            X #< 10
            ,
            singleton_list(X,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
                X #>= 10
                ,
                mod(X,10,Temp140),
                Temp142 #= (X div 10),
                to_reversed_digits(Temp142,Temp141),
                prepend_to(Temp140,Temp141,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType EnkiInt (TypeName [Named "list",EnkiInt])
to_digits(X,Temp143) :-
    to_reversed_digits(X,Temp144),
    reverse_list(Temp144,Temp143).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
from_reversed_digit_list(List,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            List = cons(H,T)
            ,
            from_reversed_digit_list(T,Temp145),
            Temp146 #= (10 * Temp145),
            Temp147 #= (Temp146 + H),
            AUTOGENERATEDFUNCTIONRESULT = Temp147
        ;
                List = empty
                ,
                AUTOGENERATEDFUNCTIONRESULT = 0
    ).

% FuncType (TypeName [Named "list",EnkiInt]) EnkiInt
from_digit_list(List,Temp148) :-
    reverse_list(List,Temp149),
    from_reversed_digit_list(Temp149,Temp148).

% RuleType EnkiInt (RuleType EnkiString EnkiString)
parse_int_from_and(I,Str,Rest) :-
    digits_from(Str,Temp151),
    pair_and(Digits,Rest) = Temp151,
    from_digit_list(Digits,I),
    I = I.

% FuncType (FuncType EnkiString (TypeName [Named "pair",TypeName [Named "list",Any "T9764"],EnkiString])) (FuncType EnkiString (TypeName [Named "list",Any "T9764"]))
parse_all(F,Str,AUTOGENERATEDFUNCTIONRESULT) :-
    call_on(F,Str,Temp152),
    Temp152 = pair_and(Res,Rest),
    (
            Rest = ''
            ,
            AUTOGENERATEDFUNCTIONRESULT = Res
        ;
                Rest \= ''
                ,
                parse_all(F,Rest,Temp154),
                plus_plus_(Res,Temp154,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType EnkiString EnkiInt
int_from(Str,I) :-
    parse_int_from_and(I,Str,Rest).

% FuncType EnkiString (TypeName [Named "list",EnkiString])
characters_of(Text,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Text = ''
            ,
            AUTOGENERATEDFUNCTIONRESULT = empty
        ;
                Text \= ''
                ,
                atom_concat(Letter,Rest,Text),
                Text = Text,
                number_of_characters_in(Letter,Temp156),
                Temp156 = 1,
                characters_of(Rest,Temp157),
                prepend_to(Letter,Temp157,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
    ).

% FuncType (TypeName [Named "list",EnkiString]) EnkiString
text_from_characters_in(Cs,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            Cs = cons(C,Rest)
            ,
            text_from_characters_in(Rest,Temp158),
            atom_concat(C,Temp158,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
            AUTOGENERATEDFUNCTIONRESULT = ''
    ).

% FuncType (Any "T9831") (FuncType (Any "T9831") (FuncType (Any "T9831") (Any "T9831")))
replace_with_in(X,Y,A,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            A = X
            ,
            AUTOGENERATEDFUNCTIONRESULT = Y
        ;
                A \= X
                ,
                AUTOGENERATEDFUNCTIONRESULT = A
    ).

% FuncType EnkiString (FuncType EnkiString (FuncType EnkiString EnkiString))
text_replace_with_in(X,Y,T,Temp159) :-
    characters_of(T,Temp162),
    map_over({X,Y}/[_1]>>({X,Y,_1}/[Temp161]>>(replace_with_in(X,Y,_1,Temp161))),Temp162,Temp160),
    text_from_characters_in(Temp160,Temp159).

% RuleType (Any "T9872") (TypeName [Named "set",Named "of",Any "T9872"])
element_of(X,S) :-
    (
            S = leftcurly__rightcurly_(Y)
            ,
            X = Y
        ;
                S = u(A,B)
                ,
                either_or_holds_for({A}/[_2]>>(element_of(_2,A)),{B}/[_3]>>(element_of(_3,B)),X)
    ).

% FuncType (TypeName [Named "set",Named "of",Any "T9921"]) (TypeName [Named "set",Named "of",Any "T9910"])
canonize_set(S,AUTOGENERATEDFUNCTIONRESULT) :-
    (
            S = u(empty_set,X)
            ,
            canonize_set(X,AUTOGENERATEDFUNCTIONRESULT),
            AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
        ;
        (
                S = u(X,empty_set)
                ,
                canonize_set(X,AUTOGENERATEDFUNCTIONRESULT),
                AUTOGENERATEDFUNCTIONRESULT = AUTOGENERATEDFUNCTIONRESULT
            ;
                    S = u(X,Y)
                    ,
                    canonize_set(X,Temp168),
                    canonize_set(Y,Temp169),
                    AUTOGENERATEDFUNCTIONRESULT = u(Temp168,Temp169)
        )
    ).

% RuleType (TypeName [Named "list",Any "T9946"]) (RuleType (TypeName [Named "list",Any "T9946"]) (TypeName [Named "list",Any "T9946"]))
separate_into_and(List,L,R) :-
    (
            colon_colon_(Y,Xs,Temp170),
            colon_colon_(X,Temp170,List),
            List = List
            ,
            separate_into_and(Xs,Ls,Rs),
            colon_colon_(X,Ls,L),
            L = L,
            colon_colon_(Y,Rs,R),
            R = R
        ;
        (
                singleton_list(X,List),
                List = List
                ,
                singleton_list(X,L),
                L = L,
                R = empty
            ;
                    List = empty
                    ,
                    L = empty,
                    R = empty
        )
    ).

% FuncType EnkiInt EnkiInt
double(X,Temp172) :-
    Temp172 #= (2 * X).

% FuncType EnkiInt EnkiInt
sum_digits(N,Temp173) :-
    digits_of(N,Temp174),
    sum_of(Temp174,Temp173).

% EnkiInt
isa_digit(D) :-
    D #>= 0,
    D #< 10.

% RuleType (TypeName [Named "list",EnkiInt]) EnkiInt
in_luhn_form_with(List,CheckDigit) :-
    separate_into_and(List,L,R),
    map_over({}/[FAKEARGNAME]>>({FAKEARGNAME}/[Temp178]>>(double(FAKEARGNAME,Temp178))),R,Temp177),
    map_over({}/[_0]>>({_0}/[Temp176]>>(sum_digits(_0,Temp176))),Temp177,NewR),
    NewR = NewR,
    isa_digit(CheckDigit),
    sum_of(L,Temp181),
    sum_of(NewR,Temp182),
    Temp183 #= (Temp181 + Temp182),
    Temp184 #= (Temp183 + CheckDigit),
    divides(10,Temp184).