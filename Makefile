REBAR = $(CURDIR)/rebar3
install:
	bundle install

travis:
	PATH=bin/:$(PATH) bundle exec $(REBAR) do eunit, dialyzer
	bundle exec rspec
