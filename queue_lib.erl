%% Queue implementation using 2 stacks (inbox and outbox).
-module(queue_lib).
-export([new/0, is_queue/1, len/1, in/2, is_empty/1, out/1]).

-include_lib("eunit/include/eunit.hrl").

-record(queue, {inbox = [] :: list(), outbox = [] :: list()}).

-type queue() :: #queue{}.

-spec new() -> queue().
new() ->
	#queue{}.

-spec is_queue(queue()) -> boolean().
is_queue(MaybeQueue) -> is_record(MaybeQueue, queue).

-spec get_inbox(queue()) -> list().
get_inbox(Queue) -> Queue#queue.inbox.

-spec set_inbox(list(), queue()) -> queue(). 
set_inbox(NewInbox, Queue) -> Queue#queue{inbox = NewInbox}.

-spec get_outbox(queue()) -> list().
get_outbox(Queue) -> Queue#queue.outbox.

-spec set_outbox(list(), queue()) -> queue(). 
set_outbox(NewOutbox, Queue) -> Queue#queue{outbox = NewOutbox}.

-spec len_inbox(queue()) -> integer().
len_inbox(Queue) -> length(get_inbox(Queue)).

-spec len_outbox(queue()) -> integer().
len_outbox(Queue) -> length(get_outbox(Queue)).

-spec len(queue()) -> integer().
len(Queue) -> len_inbox(Queue) + len_outbox(Queue).

-spec push_to_inbox(term(), queue()) -> queue().
push_to_inbox(Item, Queue) -> 
	Inbox = get_inbox(Queue),
	UpdatedInbox = [Item | Inbox],
	set_inbox(UpdatedInbox, Queue). 

-spec in(term(), queue()) -> queue().
in(Item, Queue) -> 
	push_to_inbox(Item, Queue).

-spec is_empty(queue()) -> boolean().
is_empty(Queue) -> len(Queue) == 0. 

-spec is_empty_outbox(queue()) -> boolean().
is_empty_outbox(Queue) -> len_outbox(Queue) == 0.

copy_list(List) ->
	copy_list(List, []).

copy_list([], ResultList) ->
	ResultList;
copy_list([Item|SourceListTail], DestinationList) ->
	copy_list(SourceListTail, [Item | DestinationList]). 

set_inbox_and_outbox(Inbox, Outbox, Queue) ->
	UpdatedQueue = set_inbox(Inbox, Queue),
	FinalQueue = set_outbox(Outbox, UpdatedQueue),
	FinalQueue.

move_inbox_to_outbox(Queue) ->
	Inbox = get_inbox(Queue),
	NewOutbox = copy_list(Inbox),
	set_inbox_and_outbox([], NewOutbox, Queue). 

pop_from_outbox(Queue) -> 
	case is_empty_outbox(Queue) of
		true ->
			UpdatedQueue = move_inbox_to_outbox(Queue),
			pop_from_outbox(UpdatedQueue);
		false -> 
			[Item | OutboxTail] = get_outbox(Queue),
			UpdatedQueue = set_outbox(OutboxTail, Queue),
			{Item, UpdatedQueue}
	end.

-spec out(queue()) -> {{value, term()}, queue()}.
out(Queue) ->
	case is_empty(Queue) of
		true -> {empty, Queue};
		false -> {Item, UpdatedQueue} = pop_from_outbox(Queue),
			 {{value, Item}, UpdatedQueue}
	end.

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

