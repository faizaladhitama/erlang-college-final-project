-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).
-export([make_hour/2, make_hour/0, change_working_hour/3]).

-record(work, {starth, endh}).


%%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

change_working_hour(Pid, Start, End) ->
	Pid ! {work_hour, make_hour(Start, End)}.

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState), make_hour()).

loop(Module, State, Work_Hour) ->
    receive
        {async, Msg} ->
        	case is_work_hour(Work_Hour) of
        		true	->
        			loop(Module, Module:handle_cast(Msg, State), Work_Hour);
            	false	->
            		not_work_hour(Work_Hour),
            		loop(Module, State, Work_Hour)
            end;

        {sync, Pid, Ref, Msg} ->
        	case is_work_hour(Work_Hour) of
        		true	->
        			loop(Module, Module:handle_call(Msg, {Pid, Ref}, State), Work_Hour);
        		false	->
					reply({Pid, Ref}, not_work_hour(Work_Hour)),
            		loop(Module, State, Work_Hour)
            end;

        {work_hour, New_Work_Hour} ->
        	loop(Module, State, New_Work_Hour)
    end.

%% Checking whether current time is working hour
is_work_hour(Work_Hour) ->
	{{_,_,_},{Hour,_,_}} = erlang:localtime(),
	Not_early = (Hour >= Work_Hour#work.starth),
	Not_late = Hour < Work_Hour#work.endh,
	Not_early and Not_late.

%% Message output when it's not working hour
not_work_hour(Work_Hour) ->
	io:format("Petugas sedang beristirahat, coba datang lagi pada jam kerja.~nJam kerja: ~p s/d ~p~n",
		[Work_Hour#work.starth, Work_Hour#work.endh]),
	ok.

make_hour() ->
	make_hour(0, 20).

make_hour(Start, End) ->
	if End < Start 	->
		erlang:error("End is Earier than Start")
	;	true		->
		#work{	starth=erlang:max(Start,0), 
				endh=erlang:min(End, 24)}
	end.
