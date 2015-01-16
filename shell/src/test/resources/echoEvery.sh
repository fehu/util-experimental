#!/bin/sh

SLEEP=$1            ##  Time to sleep between echo
TIMES=$2            ##  How many times to echo
MSG=$3              ##  The message to echo
APPEND_ITER=$4      ##  Append iteration number to the message?
ECHO_ERR=$5         ##  Echo in error stream?
ERROR_IN_FIN=$6     ##  error on termination?
ECHO_MSG_FIN=$7     ##  echo on termination?

output() {
    msg="$@"
    case $ECHO_ERR in
        true)   echo $msg >/dev/stderr ;;
        false)  echo $msg ;;
        both)   echo $msg
                echo $msg >/dev/stderr ;;
    esac
}

for ((i=1; i<=$TIMES; i++))
do
    if [ $APPEND_ITER -eq 0 ]; then txt=$MSG
                               else txt=$MSG$i
    fi

    output $txt
    if [ $i -ne $TIMES ]; then sleep $SLEEP
    fi
done

if [ $ECHO_MSG_FIN -ne 0 ]; then
    MSG_END=$8      ##  Message to echo on termination
    output $MSG_END
fi

if [ $ERROR_IN_FIN -ne 0 ]; then ((1/0))    # cause error
fi