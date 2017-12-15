-module(tes).

-record(cat, {name, color=black, description}).

-export([tes_fun/1]).

tes_fun(Cat = #cat{}) ->
	Name = Cat#cat.name,
	lists:concat([Name, " bersih kembali, kembalian anda ", 5000]).