REBAR = $(CURDIR)/rebar3
install:
	bundle install

eunit:
	PATH=bin/:$(PATH) bundle exec $(REBAR) eunit

travis:
	PATH=bin/:$(PATH) bundle exec $(REBAR) do eunit, dialyzer
	bundle exec rspec
