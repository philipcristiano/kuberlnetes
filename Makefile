PROJECT = kuberlnetes
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = elvis_mk
LOCAL_DEPS = public_key
DEPS = swaggerl yamerl
TEST_DEPS = meck
TEST_DIR = tests


dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git master
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.4.0

DEP_PLUGINS = elvis_mk

include erlang.mk
