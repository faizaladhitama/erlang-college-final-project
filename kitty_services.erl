%%%%% Abstracted version
-module(kitty_services).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1,
         start_deceased_cat_server/0, show_deceased_cat_list/1, register_deceased_cat/5,
         assign_work/3, retire/2, add_cat/4, browse_cat_with_job/1,
         show_count_all_cat_with_sum_price/1, add_cat_with_price/5,
         start_feed_server/0, show_feed_queue/1, register_cat_to_feed_queue/2, make_cat_hungry/1, feed_the_queue/1
        ]).
-export([add_cat_with_price_and_age/6,
         show_statistic_of_all_cat_age/1,
         show_statistic_of_all_cat_price/1]).
-export([init/1, handle_call/3, handle_cast/2, tukar_kucing/4]).
-export([cari_kucing/2, search_cat_h/3]).
-export([show_cleaning_service_price/1, cat_cleaning_service/3]).
-export([change_working_hour/3]).

-export([tambah_kucing_warna/4, lihat_warna_kucing/1, show_cat/1, remove_duplicates/1]).


-record(cat, {name, color=black, description, hungry=false, job=unemployed}).
-record(deceased_cat, {name, date, cause}).
-record(cat_with_price, {name, color=black, description, price=0}).
-record(cat_with_price_and_age, {name, color=black, description, price=0, age=0}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

start_deceased_cat_server() -> my_server:start_link(?MODULE, []).

start_feed_server() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).


add_cat_with_price(Pid, Name, Color, Description, Price) ->
    my_server:call(Pid, {add_with_price, Name, Color, Description, Price}).

show_count_all_cat_with_sum_price(Pid) ->
    my_server:call(Pid, show_count_all_cat_with_sum_price).

show_deceased_cat_list(Pid) -> my_server:call(Pid, show_deceased).

show_feed_queue(Pid) -> my_server:call(Pid, show_feed_queue).

make_cat_hungry(Cat = #cat{}) -> #cat{name=Cat#cat.name, color=Cat#cat.color, description=Cat#cat.description, hungry=true}.

feed_the_queue(Pid) -> my_server:call(Pid, feed_queue).


add_cat_with_price_and_age(Pid, Name, Color, Description, Price, Age) ->
    my_server:call(Pid, {add_with_price_and_age, Name, Color, Description, Price, Age}).

show_statistic_of_all_cat_price(Pid) ->
    my_server:call(Pid, {show_statistic_of_all_cat_price}).

show_statistic_of_all_cat_age(Pid) ->
    my_server:call(Pid, {show_statistic_of_all_cat_age}).


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

register_cat_to_feed_queue(Pid, Cat = #cat{}) ->
    case Cat#cat.hungry of
        true ->
            my_server:cast(Pid, {add_feed_queue, Cat}),
            my_server:call(Pid, {add_feed_queue, Cat});
        false ->
            my_server:call(Pid, {cat_not_hungry, Cat})
    end.

%% Call to change server's working hour
change_working_hour(Pid, Start, End) when is_integer(Start), is_integer(End)->
    if End =< Start    ->
            erlang:error("Start begins before End");
	     End > Start     ->
            my_server:change_working_hour(Pid, Start, End)
    end.

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).

%% Synchronous call untuk fungsi tukar_kucing
tukar_kucing(Pid, Name, Color, Description) ->
    my_server:call(Pid, {tukar, Name, Color, Description}).

%% Synchronous call untuk fungsi cari_kucing
%% Format pencarian [{with/without, category, query}, {with/without, category, query}, ... ]
%%
%% Memiliki mode pencarian AND, jadi query1 AND query2 AND query3
%%
%% Sedangkan {with/without, category, query} dianggap satu kueri.
%%		with menandakan bahwa hasil harus sama dengan kueri, without menandakan bahwa hasil harus tidak sama dengan kueri
%% 		category menyatakan kategori pencarian berdasarkan record: name, color, atau description
%%			(If you want to add another category, add method search_cat_h on private method section)
%%		query adalah hal yang ingin dicari
%%
cari_kucing(Pid, Query) ->
	my_server:call(Pid, {search_cat, Query}).

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

show_cleaning_service_price(Pid) ->
	  my_server:call(Pid, show_cleaning_price).

cat_cleaning_service(Pid, Cat = #cat{}, Money) ->
	  my_server:call(Pid, {cleaning, Cat, Money}).

%% Synchronous call untuk menambahkan kucing yang warnanya sama
tambah_kucing_warna(Pid, Name, Color, Description) ->
    my_server:call(Pid, {color_register, Name, Color, Description}).


%% Synchronous call untuk menampilkan seluruh kucing yang ada di server
show_cat(Pid) ->
    my_server:call(Pid, show).

%% Synchronous call untuk menampilkan seluruh warna yang ada di server
lihat_warna_kucing(Pid) ->
    my_server:call(Pid, colors).

%%% Server functions
init([]) -> []. %% no treatment of info here!

remove_duplicates(List) ->
    lists:foldl(fun(Color, Set) ->
            Length = length(lists:filter(fun(Other) -> Other =:= Color end, Set)),
            case Length of
                0 -> Set ++ [Color];
                _ -> Set
            end
        end,
    [hd(List)], [hd(List) | tl(List)]).

handle_call({order, Name, Color, Description}, From, Cats) ->
	Hasil = search_cat_h([{with,name,Name},{with,color,Color},{with,description,Description}], Cats, []),
	if Hasil =:= [] ->
		my_server:reply(From, make_cat(Name, Color, Description)),
		Cats;
	   Hasil =/= [] ->
	    my_server:reply(From, hd(Hasil)),
		[C || C <- Cats, C =/= hd(Hasil)]
	end;
	% This line of code does not do what it needs to do, consider deleting
	% if Cats =:= [] ->
    %     my_server:reply(From, make_cat(Name, Color, Description)),
    %     Cats;
    %    Cats =/= [] ->
    %     my_server:reply(From, hd(Cats)),
    %     tl(Cats)
    % end;

handle_call({add_with_price, Name, Color, Description, Price}, From, Cats) ->
    if Price >= 0 ->
    	my_server:reply(From, {ok, "success add cat"}),
    	Cats ++ [make_cat_with_price(Name, Color, Description, Price)];

	Price < 0 ->
    	my_server:reply(From, {error, "can't negative price"}),
	Cats
   end;

%%%Menampilkan jumlah uang yang harus dibayar untuk membeli semua kucing, beserta jumlah kucingnya
handle_call(show_count_all_cat_with_sum_price, From, Cats) ->
    Total = lists:foldl(fun(_, Sum) -> Sum + 1 end, 0, Cats),
	List_price = [Cat#cat_with_price.price|| Cat <- Cats],
	Sum = lists:sum(List_price),
	my_server:reply(From,{Sum,Total}),
    Cats;

%%%% Server functions untuk layanan tukar kucing
handle_call({tukar, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, {error, "Tidak ada kucing untuk ditukar."}),
        Cats;
       Cats =/= [] ->
        my_server:reply(From, hd(Cats)),
        [make_cat(Name, Color, Description)] ++ tl(Cats)
    end;

%% Handle call untuk cari_kucing
handle_call({search_cat, Query}, From, Cats) ->
	Results = search_cat_h(Query, Cats, []),
	if Results =:= [] ->
		my_server:reply(From, {error, "Tidak ditemukan kucing yang sesuai dengan kueri"}),
		Cats;
	   Results =:= [error] ->
		my_server:reply(From, {error, "Sintaks pencarian salah"}),
		Cats;
	   Results =/= [] ->
		my_server:reply(From, {ok, Results}), Cats
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
    Cats ++ [make_cat(Name, Color, Description)];

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
    Cats;

handle_call({add_feed_queue, Cat = #cat{}}, From, Queue) ->
    my_server:reply(From, {ok, add_feed_queue, {Cat#cat.name, Cat#cat.color, Cat#cat.description, Cat#cat.hungry}, queue_length, length(Queue)}),
    Queue;

handle_call(show_feed_queue, From, Queue) ->
    my_server:reply(From, {[{Cat#cat.name, Cat#cat.color, Cat#cat.description, Cat#cat.hungry} || Cat <- Queue]}),
    Queue;

handle_call({cat_not_hungry, Cat = #cat{}}, From, Queue) ->
    my_server:reply(From, {error, cat_not_hungry, {Cat#cat.name, Cat#cat.color, Cat#cat.description, Cat#cat.hungry}}),
    Queue;

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
    Cats;

handle_call(show, From, Cats) ->
    my_server:reply(From, {ok, [{Cat#cat.name, Cat#cat.color} || Cat <- Cats]}),
    Cats;

handle_call(colors, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, {error, "No cats found"}),
        Cats;
       Cats =/= [] ->
        Colors = lists:map(fun(Cat) -> Cat#cat.color end, Cats),
        my_server:reply(From, {ok, remove_duplicates(Colors)}),
        Cats
    end;

handle_call({color_register, Name, Color, Description}, From, Cats) ->
    if  Cats =:= [] ->
            my_server:reply(From, {ok, "added cat"}),
            Cats ++ [make_cat(Name, Color, Description)];
        Cats =/= [] ->
            AllColor = remove_duplicates([Cat#cat.color || Cat <- Cats]),
            case lists:member(Color, AllColor) of
                true ->
                    my_server:reply(From, {ok, "added cat with the same color"}),
                    Cats ++ [make_cat(Name, Color, Description)];
                false ->
                    my_server:reply(From, {error, "Please add cat with available colors:", AllColor}),
                    Cats
            end
    end;

handle_call(feed_queue, From, []) ->
    my_server:reply(From, {error, empty_feed_queue, all_cat_is_happy}),
    [];

handle_call(feed_queue, From, [Cat|Queue]) ->
    my_server:reply(From, {ok, finish_eat, {Cat#cat.name, Cat#cat.color, Cat#cat.description, Cat#cat.hungry}, happy_cat}),
    Queue.

handle_cast({return, Cat = #cat{}}, Cats) -> [Cat|Cats];

handle_cast({decease, Name}, Cats) ->
    lists:filter(fun(Cat) -> Cat#cat.name =/= Name end, Cats);

handle_cast({add_feed_queue, Cat = #cat{}}, Queue) -> Queue ++ [Cat].

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

make_deceased_cat(Name, Date, Cause) ->
    #deceased_cat{name=Name, date=Date, cause=Cause}.

make_cat_with_price(Name, Col, Desc, Price) ->
    #cat_with_price{name=Name, color=Col, description=Desc, price=Price}.

make_cat_with_job(Name, Col, Desc, Job) ->
    #cat{name=Name, color=Col, description=Desc, job=Job}.
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

%% Private Function for search_cat
search_cat_h([], _, Results) -> Results;

search_cat_h([{with, name, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.name =:= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.name =:= Query]) end;
search_cat_h([{without, name, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.name =/= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.name =/= Query]) end;

search_cat_h([{with, color, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.color =:= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.color =:= Query]) end;
search_cat_h([{without, color, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.color =/= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.color =/= Query]) end;

search_cat_h([{with, description, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.description =:= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.description =:= Query]) end;
search_cat_h([{without, description, Query}|T], Cats, Results) ->
	if Results =:= [] -> search_cat_h(T, Cats, [C || C <- Cats, C#cat.description =/= Query]);
	   Results =/= [] -> search_cat_h(T, Cats, [C || C <- Results, C#cat.description =/= Query]) end;

search_cat_h(_, _, _) -> [error].
