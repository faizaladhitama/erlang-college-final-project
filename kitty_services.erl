%%%%% Abstracted version
-module(kitty_services).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, 
        start_deceased_cat_server/0, show_deceased_cat_list/1, register_deceased_cat/5]).
-export([init/1, handle_call/3, handle_cast/2, tukar_kucing/4]).
-export([start_doctor_server/0,start_clinic_server/0,make_doctor/2,make_clinic/2,new_clinic/3,
        new_doctor/3,register_clinic/2,register_doctor/2,show/1,filter_doctor/2,filter_clinic/2,
        assign_doctor/4,show2/1]).

-record(cat, {name, color=black, description}).
-record(deceased_cat, {name, date, cause}).
-record(doctor, {name, specialization={}}).
-record(clinic, {name, max_doctor=5, doctor_list=[]}).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

start_deceased_cat_server() -> my_server:start_link(?MODULE, []).

start_doctor_server() -> my_server:start_link(?MODULE, []).

start_clinic_server() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).
    
show_deceased_cat_list(Pid) ->
    my_server:call(Pid, show_deceased).

new_clinic(Pid, Name, Max_Doctor)->
    my_server:call(Pid,{new_clinic,Name,Max_Doctor}).

new_doctor(Pid, Name, Specialization)->
    my_server:call(Pid,{new_doctor,Name,Specialization}).   

show(Pid)->
    my_server:call(Pid,{print}).  

show2(Pid)->
    my_server:call(Pid,{print2}).  

%PID 1 = SERVER DOKTER
%PID 2 = SERVER KLINIK
assign_doctor(Pid1,Pid2,SearchedDoctor,SearchedClinic) ->
    ResultDoctor = filter_doctor(Pid1,SearchedDoctor),
    ResultClinic = filter_clinic(Pid2,SearchedClinic),
    if ResultDoctor =/= [] andalso ResultClinic =/= [] ->    
            {Rec1,DoctorName,Specialization} = hd(ResultDoctor),    
            io:format("~p ~n",[DoctorName]),
            {Rec2,ClinicName,Max_Doctor,Doctor_List} = hd(ResultClinic),    
            io:format("~p ~n",[ClinicName]),
            io:format("True ~n"),
            my_server:call(Pid2,{assign,true,ClinicName,hd(ResultDoctor)});
       true ->
            my_server:call(Pid2,{assign,false,"A","B"})
    end.

%% This call is asynchronous
filter_doctor(Pid,Name)->
    my_server:call(Pid,{filter_doctor,Name}). 

filter_clinic(Pid,Name)->
    my_server:call(Pid,{filter_clinic,Name}).

return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

register_clinic(Pid, Clinic = #clinic{}) ->
    my_server:cast(Pid, {register_clinic, Clinic}).

register_doctor(Pid, Doctor = #doctor{}) ->
    my_server:cast(Pid, {register_doctor, Doctor}).

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

handle_call({filter_doctor,Name}, From, Doctors) ->
    io:format("wkwk"),
    Filter = lists:filter(fun(Doctor)-> Doctor#doctor.name == Name end, Doctors),
    my_server:reply(From,Filter),
    Doctors;

handle_call({filter_clinic,Name}, From, Clinics) ->
    io:format("wowowow"),    
    Filter = lists:filter(fun(Clinic)-> Clinic#clinic.name == Name end, Clinics),
    my_server:reply(From,Filter),
    Clinics;

handle_call({new_clinic,Name,Max_Doctor}, From, Clinics) ->
    my_server:reply(From, make_clinic(Name, Max_Doctor)),
    Clinics;    

handle_call({new_doctor,Name,Specialization}, From, Doctors) ->
    my_server:reply(From, make_doctor(Name, Specialization)),
    Doctors;

handle_call({assign,Result,Search,Updated},From,Obj)->
    if Result =:= false ->
        my_server:reply(From, "Neither Doctor nor Clinic not exist ~n"),
        Obj;
       true ->
        Update_Result = update_record(Obj,Search,Updated),
        io:format("~p ",[Update_Result]),
        my_server:reply(From, "Both exists"),     
        Update_Result
    end;

handle_call({print}, From, Obj) ->
    my_server:reply(From, ok),
    print_list(Obj);

handle_call({print2}, From, Obj) ->
    my_server:reply(From, ok),
    print_list2(Obj);

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
    end.

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat|Cats];

handle_cast({register_clinic, Clinic = #clinic{}}, Clinics) ->
    [Clinic|Clinics];

handle_cast({register_doctor, Doctor = #doctor{}}, Doctors) ->
    [Doctor|Doctors];

handle_cast({decease, Name}, Cats) ->
    lists:filter(fun(Cat) -> Cat#cat.name =/= Name end, Cats).
        
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

make_deceased_cat(Name, Date, Cause) ->
    #deceased_cat{name=Name, date=Date, cause=Cause}.
    
make_clinic(Name, Max_Doctor) -> 
    #clinic{name=Name,max_doctor=Max_Doctor,doctor_list=[]}.

make_doctor(Name, Specialization) ->
    #doctor{name=Name,specialization=Specialization}.

print_list(Obj) ->
    io:format("masuk "),
    [io:format("~p was set free.~n",[C#doctor.name]) || C <- Obj],
    Obj.

print_list2(Obj) ->
    io:format("masuk2 "),
    [io:format("~p was set free.~n",[C#clinic.name]) || C <- Obj],
    Obj.

update_record([],Searched,Updated) -> [];
update_record(List,Searched,Updated) -> update_record(List,Searched,Updated,[]).
update_record([],Searched,Updated,Result)->Result;
update_record(List,Searched,Updated,Result)->
    Head = hd(List),
    if Head#clinic.name =:= Searched ->
        HeadList = Head#clinic.doctor_list,
        NewHeadList = HeadList++[Updated],
        NewHead = Head#clinic{doctor_list = NewHeadList},
        update_record(tl(List),Searched,Updated,Result++[NewHead]);
       true->
        update_record(tl(List),Searched,Updated,Result++[Head])
    end.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
