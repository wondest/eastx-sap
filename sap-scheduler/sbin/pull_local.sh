#!/bin/sh

##############################################################
# Program: pull_local.sh
# Author: Tender Xie
# Description:
#   (1) sync dags from store to master (*.py)
#   (2) sync plugins from store to master (*.py)
#   (3) sync airflow.cfg from store to master
# History:
#   (1) create
##############################################################

# config for store
CFG_STORE_PATH="/mnt/e/GitHub/remote/eastx-sap/sap-scheduler"

# config for master
CFG_MASTER_HOME="/home/ap/airflow"
CFG_MASTER_PATH="/home/ap/airflow/airflow_bin"

# check store directory
[ ! -d "${CFG_STORE_PATH}" ] && echo "[ ! -d ${CFG_STORE_PATH} ],please check!" && exit 1

# rsync dags
echo "1 - rsync dags"
rsync -rvP "${CFG_STORE_PATH}/dags/" "${CFG_MASTER_PATH}/dags" --exclude="operators" --exclude="hooks" --exclude="__init__.py" --include="*.py" --delete

# rsync plugins
echo "2 - rsync plugins"
rsync -rvP "${CFG_STORE_PATH}/dags/operators" "${CFG_MASTER_PATH}/plugins" --exclude="__init__.py" --include="*.py"  --delete
rsync -rvP "${CFG_STORE_PATH}/dags/hooks" "${CFG_MASTER_PATH}/plugins" --exclude="__init__.py" --include="*.py"  --delete

# rsync airflow.cfg
echo "3 - rsync airflow.cfg"
rsync -rvP "${CFG_STORE_PATH}/" "${CFG_MASTER_PATH}" --include="airflow.cfg" --exclude="*" --delete

# rsync config
echo "4 - rsync config"
rsync -rvP "${CFG_STORE_PATH}/config/" "${CFG_MASTER_PATH}/config" --include="*.yaml" --exclude="*" --delete

# rsync sbin
echo "5 - rsync sbin"
rsync -rvP "${CFG_STORE_PATH}/sbin/" "${CFG_MASTER_HOME}/sbin" --include="*.sh" --exclude="*" --delete

exit 0