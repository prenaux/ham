#!/bin/bash

toolset_import hadoop
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=HIVE
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=hive
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/hive"

# path setup
case $HAM_OS in
    OSX*)
        export HIVE_HOME="${HAM_TOOLSET_DIR}/jvm/"
        export PATH="${HIVE_HOME}/bin":${PATH}
        if [ ! -e "$HIVE_HOME" ]; then
            toolset_dl hive hive_jvm
            if [ ! -e "$HIVE_HOME" ]; then
                echo "E/jvm folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export PATH="${HAM_TOOLSET_DIR}":${PATH}

export HIVE_DB_DIR="$WORK/Server/hive"

export HIVE_OPTS="-hiveconf
mapred.job.tracker=local
-hiveconf fs.default.name=file:////${HIVE_DB_DIR}/hivelocal/tmp
-hiveconf hive.metastore.warehouse.dir=file:////${HIVE_DB_DIR}/hivelocal/warehouse
-hiveconf javax.jdo.option.ConnectionURL=jdbc:derby:;databaseName=/${HIVE_DB_DIR}/hivelocal/metastore_db;create=true"

cat <<EOF>> $HOME/.hiverc
SET hive.metastore.warehouse.dir=${HIVE_DB_DIR}/hive-warehouse;
EOF

VER="--- hive ---------------------
Hive `basename $(ls $HIVE_HOME/lib/hive-exec*) | sed s~hive-exec-~~ | sed s~.jar~~`"
# Works but is tediously slow: `hive --version | grep Hive`
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
