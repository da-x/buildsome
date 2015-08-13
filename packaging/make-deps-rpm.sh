#!/bin/bash

set -u
set -e

NAME=buildsome
DEPS_VERSION=0.3.1
MOCK_TARGET=default

if [[ ! -d /usr/share/${NAME}-deps ]] ; then
    echo /usr/share/${NAME}-deps does not exist
    exit -1
fi

if [[ ! -w /usr/share/${NAME}-deps ]] ; then
    echo /usr/share/${NAME}-deps is not writable exist
    exit -1
fi

nr_files=`ls -l /usr/share/${NAME}-deps | wc -l`
if [[ "${nr_files}" != "1" ]] ; then
    echo /usr/share/${NAME}-deps is not empty
    exit -1
fi

t=`dirname ${BASH_SOURCE}`
packaging_path=`realpath ${t}`
root_src_path=`realpath ${t}/..`

cd /usr/share/${NAME}-deps

sandbox_stage() {
    rm -rf .cabal-sandbox
    cabal sandbox init
    cabal install --only-dependencies ${root_src_path}
}

rpm_stage() {
    TARGET_DIR=$(mktemp --tmpdir -d XXXXXXrpm-packaging)
    cleanups () {
        rm -rf ${TARGET_DIR}
    }
    trap cleanups EXIT

    cat ${packaging_path}/deps.spec \
        | sed s/@@VERSION@@/${DEPS_VERSION}/g \
        | sed s/@@NAME@@/${NAME}/g \
         > ${TARGET_DIR}/${NAME}-deps.spec

    (cd /usr/share &&
    mkdir ${TARGET_DIR}/sources &&
    tar -czf ${TARGET_DIR}/sources/${NAME}-deps.tar.gz ${NAME}-deps)
    ls -lR ${TARGET_DIR}
    mock -r ${MOCK_TARGET} --buildsrpm \
         --spec ${TARGET_DIR}/${NAME}-deps.spec \
         --source ${TARGET_DIR}/sources \
         --resultdir ${TARGET_DIR}/result
    mock -r ${MOCK_TARGET} ${TARGET_DIR}/result/*.src.rpm \
         --resultdir ${TARGET_DIR}/result
    mv ${TARGET_DIR}/result/*.rpm ${root_src_path}
}

sandbox_stage
rpm_stage
