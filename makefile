all:
	./rebar compile

run:
	erl \
		-name jd \
		-pa `pwd` ebin

doc:
	./rebar doc

eunit:
	./rebar eunit

clean:
	./rebar clean

