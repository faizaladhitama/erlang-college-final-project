%%%%% Abstracted version
-module(kitty_services).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, 
        start_deceased_cat_server/0, show_deceased_cat_list/1, register_deceased_cat/5,
        assign_work/3, retire/2, add_cat/4, browse_cat_with_job/1]).
-export([init/1, handle_call/3, handle_cast/2, tukar_kucing/4]).

-record(cat, {name, color=black, description, job=unemployed}).
-record(deceased_cat, {name, date, cause}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

start_deceased_cat_server() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).
    
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

%% Synchronous call untuk fungsi assign_work
assign_work(Pid, Name, Job) ->
    my_server:call(Pid, {work, Name, Job}).

%% Synchronous call untuk fungsi retire
retire(Pid, Name) ->
    my_server:call(Pid, {retire, Name}).


%% Synchronous call untuk fungsi browse_cat_with_job
browse_cat_with_job(Pid) ->
    my_server:call(Pid, browse).

%% Synchronous call untuk fungsi add_cat
add_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {add_cat, Name, Color, Description}).

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

%%%% Server functions for assigning work to respective cat
handle_call({work, Name, Job}, From, Cats) ->
    case lists:filter(fun(Cat) -> Cat#cat.name == Name end, Cats) of
        [] ->
            my_server:reply(From, {error, "no cat with respective name"}),
            Cats;
        _ ->
            my_server:reply(From, {ok, assigned}),
            lists:map(
                fun(Cat) ->
                    case Cat#cat.name == Name of
                        true -> make_cat_with_job(Cat#cat.name, Cat#cat.color, Cat#cat.description, Job);
                        false -> Cat
                    end
                end,
                Cats
                )
    end;

%%%% Server functions for retiring a respective cat
handle_call({retire, Name}, From, Cats) ->
    case lists:filter(fun(Cat) -> Cat#cat.name == Name end, Cats) of
        [] ->
            my_server:reply(From, {error, "no cat with respective name"}),
            Cats;
        _ ->
            my_server:reply(From, {ok, retired}),
            lists:map(
                fun(Cat) ->
                    case Cat#cat.name == Name of
                        true -> make_cat_with_job(Cat#cat.name, Cat#cat.color, Cat#cat.description, retirement);
                        false -> Cat
                    end
                end,
                Cats
                )
    end;

%%%% Server functions for showing all cats
handle_call(browse, From, Cats) ->
    my_server:reply(From, {ok, [{Cat#cat.name, Cat#cat.job} || Cat <- Cats]}),
    Cats;

handle_call({add_cat, Name, Color, Description}, From, Cats) ->

    my_server:reply(From, {ok, add_cat}),
    Cats ++ [make_cat(Name, Color, Description)].


handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat|Cats];

handle_cast({decease, Name}, Cats) ->
    lists:filter(fun(Cat) -> Cat#cat.name =/= Name end, Cats).
        
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

make_deceased_cat(Name, Date, Cause) ->
    #deceased_cat{name=Name, date=Date, cause=Cause}.


make_cat_with_job(Name, Col, Desc, Job) ->
    #cat{name=Name, color=Col, description=Desc, job=Job}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
