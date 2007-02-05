#!/bin/sh

CMD_ERL="erl"
NODE_SNAME=node1
#OPT_OPTIM="-smp enable +S 2 +K true +A 2 "
#OPT_CFG="-config ebin/test"
OPT_DEBUG="-s appmon start"
#OPT_DEBUG="-s appmon start -s debugger start"

$CMD_ERL $OPT_OPTIM -sname $NODE_SNAME -pa ebin -boot start_sasl +W w $OPT_CFG $OPT_DEBUG -eval 'application:start(grinderl).'
