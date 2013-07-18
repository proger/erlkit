# erlkit

Random awesomeness for Erlang.

## random hacks

* Loading modules

```sh
cat netstat.erl | tr '\n' ' ' | sed 's#"#\\\"#g' | xclip		# for Linux
cat netstat.erl | tr '\n' ' ' | sed 's#"#\\\"#g' | pbcopy 		# for OSX

# erl:
1> rt_load:load_source("<<PASTE HERE>>").
```
