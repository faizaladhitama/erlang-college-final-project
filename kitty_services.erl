%%%%% Abstracted version
-module(kitty_services).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, 
        start_deceased_cat_server/0, show_deceased_cat_list/1, register_deceased_cat/5]).
-export([init/1, handle_call/3, handle_cast/2, tukar_kucing/4, show_cleaning_service_price/1, cat_cleaning_service/3]).

-record(cat, {name, color=black, description}).
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

show_cleaning_service_price(Pid) ->
	my_server:call(Pid, show_cleaning_price).  

cat_cleaning_service(Pid, Cat = #cat{}, Money) ->
	my_server:call(Pid, {cleaning, Cat, Money}).

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

handle_call({cleaning, Cat = #cat{}, Money}, From, Cats) ->
	Cost = (length(Cats) * 5000) + 20000,
	if 
		Cost > Money ->
			my_server:reply(From, {error, "Uang anda tidak cukup untuk pelayanan pembersihan kucing."}),
			Cats;
		true ->
			Change = Money - Cost,
			Name = Cat#cat.name,
			Message = lists:concat([Name, " bersih kembali, kembalian anda ", Change]),
			my_server:reply(From, {ok, Message}),
			Cats
	end;

handle_call(show_cleaning_price, From, Cats) ->
	Cost = (length(Cats) * 5000) + 20000,
	Message = lists:concat(["Harga jasa pembersihan kucing saat ini adalah ", Cost]),
	my_server:reply(From, {Message}),
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
    
terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
