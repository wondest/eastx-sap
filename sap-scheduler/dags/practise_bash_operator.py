#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

"""Example DAG demonstrating the usage of the BashOperator."""

from datetime import timedelta
from airflow import DAG
from airflow.operators.bash import BashOperator
from airflow.operators.dummy import DummyOperator
from airflow.utils.dates import days_ago

args = {
    'owner': 'airflow',
}

with DAG(
        dag_id='practise_bash_operator',
        default_args=args,
        schedule_interval='0 0 * * *',
        start_date=days_ago(1),
        dagrun_timeout=timedelta(minutes=60),
        tags=['practise'],
        params={"input_key": "input_value"},
) as dag:
    run_head = DummyOperator(
        task_id='run_first',
    )

    # [START howto_operator_bash]
    run_main = BashOperator(
        task_id='run_main',
        bash_command='echo 1',
    )
    # [END howto_operator_bash]

    run_tail = DummyOperator(
        task_id='run_last',
    )

    run_head >> run_main >> run_tail
if __name__ == "__main__":
    dag.cli()
