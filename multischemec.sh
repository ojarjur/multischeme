#!/bin/bash
COMPILE=$0
OPTIONS="Options:
  -h  Print this message and exit.
  -o  The compiler output file."

usage () {
    echo "Multischeme Compiler."
    echo "Usage: ${COMPILE} <OPTIONS> (- | <SOURCE>)"
    echo "${OPTIONS}"
}

SOURCE="-"
PARSED_ARGS=`getopt ho: -- $@`
if [ $? ]; then
    for ARG in ${PARSED_ARGS}; do
        if [[ -n "${PARSING_DESTINATION}" ]]; then
            DESTINATION="${ARG}"
            PARSING_DESTINATION=''
        elif [[ "${ARG}" == "-o" ]]; then
            PARSING_DESTINATION='Yes'
        elif [[ "${ARG}" == "-h" ]]; then
            usage
            exit 0
        elif [[ "${ARG}" != "--" ]]; then
            SOURCE="${ARG}"
        fi
    done
else
    usage
    exit 1
fi

if [ ! -e 'bin/multischemec' ]; then ./bootstrap.sh; fi
if [[ "${SOURCE}" == "-" ]]; then
    REWRITTEN_CODE=`bin/multischemec < /dev/stdin`
else
    REWRITTEN_CODE=`bin/multischemec < ${SOURCE}`
fi
if [[ -n "${DESTINATION}" ]]; then
    echo ${REWRITTEN_CODE} | csc - -o ${DESTINATION}
else
    echo ${REWRITTEN_CODE} | csc -
fi
