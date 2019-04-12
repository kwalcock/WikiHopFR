#!/bin/bash

# Config values
KG_TYPES="NamedEntityLink Coocurrence OpenIE"
DOC_UNIVERSE_TYPES="Local Random Related"
AGENT_TYPE="Random Cascade"

# Path definitions
OUTPUT_DIR="automated_output"
CONF_FILE="application_venti.conf"

for KG in $KG_TYPES
do
    for DOC_UNIVERSE in $DOC_UNIVERSE_TYPES
    do
        for AGENT in $AGENT_TYPE
        do
            FILE_NAME="benchmark_${KG}_${DOC_UNIVERSE}_${AGENT}.json"

            if [[ ! -d $OUTPUT_DIR ]]
            then
                mkdir -p $OUTPUT_DIR
            fi

            OUTPUT_PATH="$OUTPUT_DIR/$FILE_NAME"

            CMD="sbt -Dconfig.file=${CONF_FILE} -Denvironment.knowledgeGraphType=${KG} -Denvironment.documentUniverse=${DOC_UNIVERSE} -Dbenchmark.agentType=${AGENT} -Dfiles.benchmarkOutput=${OUTPUT_PATH} \"runMain org.ml4ai.exec.BenchmarkApp\""
            eval $CMD

        done
    done
done