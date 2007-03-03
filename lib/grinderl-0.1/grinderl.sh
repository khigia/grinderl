#!/bin/sh
 

# ==== Configuration ====

HOSTNAME=`hostname`

APP_NAME="Grinderl"
APP_API=grinderl
APP_ARGS=

APP_ROOT=`pwd`
APP_START_NODE=${APP_API}_node
APP_START_MOD=${APP_API}
APP_START_FUN=start
APP_START_ARGS=${APP_ARGS}
APP_STOP_NODE=${APP_START_NODE}_stoppernode
APP_STOP_MOD=${APP_API}
APP_STOP_FUN=stop
APP_STOP_ARGS=${APP_START_NODE}

ERL_CMD=`which erl`
ERL_OPT="+W w"
ERL_START_CFG=`if [ -e ebin/env_prod.config ] ; then echo "-config ebin/env_prod" ; fi`
ERL_DEBUG_CFG=`if [ -e ebin/env_dev.config ] ; then echo "-config ebin/env_dev" ; fi`

export HEART_COMMAND="$APP_ROOT/$0 start"


# ==== Internal implementation ====

case $1 in
  start)
    echo  "Starting ${APP_NAME} on node ${APP_START_NODE}"
    ${ERL_CMD} ${ERL_OPT} ${ERL_START_CFG} \
        -boot start_sasl \
        -sname ${APP_START_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_START_MOD} ${APP_START_FUN} ${APP_START_ARGS} \
        -heart -detached
    ;;
 
  debug)
    echo  "Starting ${APP_NAME} in a shell"
    ${ERL_CMD} ${ERL_OPT} ${ERL_DEBUG_CFG} \
        -boot start_sasl \
        -sname ${APP_START_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_START_MOD} ${APP_START_FUN} ${APP_START_ARGS} \
        -s appmon start
    ;;
 
  stop)
    echo "Stopping ${APP_NAME}"
    ${ERL_CMD} \
        -noshell \
        -sname ${APP_STOP_NODE} \
        -pa ${APP_ROOT}/ebin \
        -s ${APP_STOP_MOD} ${APP_STOP_FUN} ${APP_STOP_ARGS}
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    echo "  start: start a new node and run application as deamon (heart mode)"
    echo "  debug: start a new shell and run application"
    echo "  stop: stop both application and node"
    exit 1
esac
 
exit 0
                    
