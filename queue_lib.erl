-module(queue_lib).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-record(queue, {list1 = [] :: list(), list2 = [] :: list()}).

-type queue() :: #queue{}.

-spec new() -> queue().
new() ->
	#queue{}.

-spec is_queue(queue()) -> boolean().
is_queue(MaybeQueue) -> is_record(MaybeQueue, queue).

get_list1(Queue) -> Queue#queue.list1.
set_list1(List, Queue) -> Queue#queue{list1 = List}.

-spec len(queue()) -> integer().
len(Queue) -> length(get_list1(Queue)).

-spec in(term(), queue()) -> queue().
in(Item, Queue) -> 
	List = get_list1(Queue),
	UpdatedList = List ++ [Item], 	
	set_list1(UpdatedList, Queue).

-spec out(queue()) -> {{value, term()}, queue()}.
out(Queue) ->
	[Item|Rest] = get_list1(Queue),
	UpdatedQueue = set_list1(Rest, Queue),
	{{value, Item}, UpdatedQueue}.

is_queue_test() ->
	Queue = new(),
	?assert(is_queue(Queue)),
	?assertNot(is_queue(not_a_queue)).

len_test() ->
	Queue = new(),
	?assertEqual(0, len(Queue)).

in_out_test() ->
	Queue1 = new(),
	Queue2 = in(1, Queue1),
	Queue3 = in(2, Queue2),
	?assertEqual(2, len(Queue3)),
	{{value, 1}, Queue4} = out(Queue3),
	?assertMatch({{value, 2}, _}, out(Queue4)).

