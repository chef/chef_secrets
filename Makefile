REBAR = $(CURDIR)/rebar3

travis:
	bundle install
	PATH=bin/:$(PATH) bundle exec $(REBAR) do eunit, dialyzer
	bundle exec rspec
