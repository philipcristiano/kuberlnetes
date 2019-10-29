PROJECT = kuberlnetes
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = swaggerl yamerl

dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git master
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.4.0

include erlang.mk
