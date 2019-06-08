PROJECT = firebase_xmpp
PROJECT_DESCRIPTION = Firebase Cloud Messaging With XMPP
PROJECT_VERSION = 0.1.0
LOCAL_DEPS = mnesia

DEPS = cowboy jsx gproc hashids bbmustache
dep_hashids = git https://github.com/snaiper80/hashids-erlang.git 1.0.5
dep_bbmustache = git https://github.com/soranoba/bbmustache.git master
dep_cowboy_commit = 2.6.0

DEP_PLUGINS = cowboy

include erlang.mk
