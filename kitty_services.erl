%%%%% Abstracted version
-module(kitty_services).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, 
        start_deceased_cat_server/0, show_deceased_cat_list/1, register_deceased_cat/5,
        add_cat_with_price_and_age/6, show_statistic_of_all_cat_age/1, show_statistic_of_all_cat_price/1]).
-export([init/1, handle_call/3, handle_cast/2, tukar_kucing/4]).

-record(cat, {name, color=black, description}).
-record(deceased_cat, {name, date, cause}).
-record(cat_with_price_and_age, {name, color=black, description, price=0, age=0}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

start_deceased_cat_server() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

add_cat_with_price_and_age(Pid, Name, Color, Description, Price, Age) ->
    my_server:call(Pid, {add_with_price_and_age, Name, Color, Description, Price, Age}).

show_statistic_of_all_cat_price(Pid) ->
    my_server:call(Pid, {show_statistic_of_all_cat_price}).  

show_statistic_of_all_cat_age(Pid) ->
    my_server:call(Pid, {show_statistic_of_all_cat_age}).  

show_deceased_cat_list(Pid) ->
    my_server:call(Pid, show_deceased).       
    
%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

register_deceased_cat(Pid1, Pid2, Name, Date = {DD, MM, YY}, Cause) ->
    case calendar:valid_date({YY, MM, DD}) of 
        true ->
            my_server:cast(Pid1, {decease, Name}),
            my_server:call(Pid2, {decease, Name, Date, Cause});
        false ->
            my_server:call(Pid1, invalid_date)
    end.

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).

%% Synchronous call untuk fungsi tukar_kucing
tukar_kucing(Pid, Name, Color, Description) ->
    my_server:call(Pid, {tukar, Name, Color, Description}).

%%% Server functions
init([]) -> []. %% no treatment of info here!

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, make_cat(Name, Color, Description)),
        Cats;
       Cats =/= [] ->
        my_server:reply(From, hd(Cats)),
        tl(Cats)
    end;

%%%% Server functions untuk layanan tukar kucing
handle_call({tukar, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, {error, "Tidak ada kucing untuk ditukar."}),
        Cats;
       Cats =/= [] ->
        my_server:reply(From, hd(Cats)),
        [make_cat(Name, Color, Description)] ++ tl(Cats)

    end;

handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats);
    
handle_call(show_deceased, From, Cats) ->
    Total = lists:foldl(fun(_, Sum) -> Sum + 1 end, 0, Cats),
    my_server:reply(From, 
                    {[{Cat#deceased_cat.name, Cat#deceased_cat.date, Cat#deceased_cat.cause} || Cat <- Cats],
                    Total}),
    Cats;

handle_call(invalid_date, From, Cats) ->
    my_server: reply(From, {error, invalid_date}),
    Cats;

handle_call({decease, Name, Date, Cause}, From, DeceasedCats) ->
    case lists:filter(fun(Cat) -> Cat#deceased_cat.name == Name end, DeceasedCats) of
        [] ->
            my_server:reply(From, {ok, noted}),
            DeceasedCats ++ [make_deceased_cat(Name, Date, Cause)];
        _ ->
            my_server:reply(From, {error, "can't die more than once"}),
            DeceasedCats
    end;

handle_call({add_with_price_and_age, Name, Color, Description, Price, Age}, From, Cats) ->
    my_server:reply(From, {ok, cat_with_price_and_age}),
    Cats ++ [make_cat_with_price_and_age(Name, Color, Description, Price, Age)];

handle_call({show_statistic_of_all_cat_age}, From, Cats) ->
    Stat = dict:new(),
    StatWFreq = dict:store(frequency, count_age_freq(Cats), Stat),
    StatWMean = dict:store(mean, count_mean(dict:fetch(frequency, StatWFreq), 0, 0), StatWFreq),
    StatWMode = dict:store(mode, get_mode(dict:fetch(frequency, StatWFreq)), StatWMean),
    my_server:reply(From, {ok, dict:to_list(StatWMode)}),
    Cats;

handle_call({show_statistic_of_all_cat_price}, From, Cats) ->
    Stat = dict:new(),
    StatWFreq = dict:store(frequency, count_price_freq(Cats), Stat),
    StatWMean = dict:store(mean, count_mean(dict:fetch(frequency, StatWFreq), 0, 0), StatWFreq),
    StatWMode = dict:store(mode, get_mode(dict:fetch(frequency, StatWFreq)), StatWMean),
    my_server:reply(From, {ok, dict:to_list(StatWMode)}),
    Cats.

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat|Cats];

handle_cast({decease, Name}, Cats) ->
    lists:filter(fun(Cat) -> Cat#cat.name =/= Name end, Cats).
        
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

make_deceased_cat(Name, Date, Cause) ->
    #deceased_cat{name=Name, date=Date, cause=Cause}.

make_cat_with_price_and_age(Name, Color, Description, Price, Age) ->
    #cat_with_price_and_age{name=Name, color=Color, description=Description, price=Price, age=Age}.

%%% Util functions
count_age_freq(Cats) ->
    lists:foldl(fun (C, D) -> orddict:update_counter(C#cat_with_price_and_age.age, 1, D) end, orddict:new(), Cats).

count_price_freq(Cats) ->
    lists:foldl(fun (C, D) -> orddict:update_counter(C#cat_with_price_and_age.price, 1, D) end, orddict:new(), Cats).

get_mode([]) -> none;
get_mode(L) -> get_mode(L, 0, 0).
get_mode([], Mode, _) -> Mode;
get_mode([{N, F}|T], _, MaxFreq) when F > MaxFreq -> get_mode(T, N, F);
get_mode([{_, F}|T], Mode, MaxFreq) when F =< MaxFreq -> get_mode(T, Mode, MaxFreq).

count_mean([], C, Sum) -> Sum/C;
count_mean([{N, F}|T], C, Sum) -> count_mean(T, C + F, Sum + (N * F)). 

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
