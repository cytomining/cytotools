#!/bin/bash

if [ ! -d software ]; then
  mkdir software
  cd software
  git clone https://github.com/broadinstitute/cytominer_scripts.git
  cd ..
fi

cd software/cytominer_scripts

for plate in ../../backend/batch0/*/; do
  plate="${plate%/*}"                       # strip trailing slash
  plate="${plate##../../backend/batch0/}"   # strip path and leading slash

  ./aggregate.R                                   \
    ../../backend/batch0/${plate}/${plate}.sqlite \
    -o ../../backend/batch0/${plate}/${plate}.csv

  ./annotate.R  \
    -b batch0   \
    -p ${plate}

  ./normalize.R \
    -b batch0   \
    -p ${plate} \
    -c          \
    -s "Metadata_broad_sample_type == '''control'''"

  mv ../../backend/batch0/${plate}/${plate}_normalized.csv \
     ../../backend/batch0/${plate}/${plate}_normalized_single_cell.csv

  ./normalize.R \
    -b batch0   \
    -p ${plate} \
    -s "Metadata_broad_sample_type == '''control'''";
done

mkdir -p ../../parameters/batch0/sample/

./sample.R \
  -b batch0 \
  -f "_normalized.csv$" \
  -n 2 \
  -o ../../parameters/batch0/sample/normalized_sample.csv

./preselect.R           \
  -b batch0             \
  -r variance_threshold \
  -n 2                  \
  -i ../../parameters/batch0/sample/normalized_sample.csv

./preselect.R               \
  -b batch0                 \
  -r correlation_threshold  \
  -n 2                      \
  -i ../../parameters/batch0/sample/normalized_sample.csv

for plate in ../../backend/batch0/*/; do
  plate="${plate%/*}"                       # strip trailing slash
  plate="${plate##../../backend/batch0/}"   # strip path and leading slash

  ./select.R                \
    -b batch0               \
    -p ${plate}             \
    -r correlation_threshold;
done

./collapse.R          \
 -b batch0            \
 -m C-7161-01-LM6-001 \
 -o ../../backend/batch0/collapsed.csv

# # script used to create collect the fixture
# BATCH_ID=2016_04_01_a549_48hr_batch1
# parallel aws s3 cp s3://${IP_S3_BUCKET}/projects/${LINCS_PROJECT_NAME}/workspace/analysis/${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}.csv ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}.csv ::: SQ00015116 SQ00015117 ::: {A..E} ::: `seq -f "%02g" 1 6`  ::: {1..9} ::: Cells Nuclei Cytoplasm Image
# parallel "csvcut -c ImageNumber,ObjectNumber,AreaShape_Area,Intensity_IntegratedIntensity_DNA ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}.csv > ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}-trimmed.csv" ::: SQ00015116 SQ00015117 ::: {A..E} ::: `seq -f "%02g" 1 6`  ::: {1..9} ::: Cells Nuclei Cytoplasm
# parallel "csvcut -c ImageNumber,Metadata_Col,Metadata_plate,Metadata_Row,Metadata_Site,Metadata_Well ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}.csv > ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}-trimmed.csv" ::: SQ00015116 SQ00015117 ::: {A..E} ::: `seq -f "%02g" 1 6`  ::: {1..9} ::: Image
# parallel mv ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}-trimmed.csv ${BATCH_ID}/{1}/analysis/{2}{3}-{4}/{5}.csv ::: SQ00015116 SQ00015117 ::: {A..E} ::: `seq -f "%02g" 1 6`  ::: {1..9} ::: Cells Nuclei Cytoplasm Image
#
# mkdir -p backend/batch0/SQ00015116/SQ00015116
# mkdir -p backend/batch0/SQ00015116/SQ00015117
#
# cytominer-database ingest \
#     --skip-image-prefix   \
#     --no-munge            \
#     -c config.ini         \
#     analysis/SQ00015116   \
#     sqlite:///backend/batch0/SQ00015116/SQ00015116.sqlite
#
# cytominer-database ingest \
#     --skip-image-prefix   \
#     --no-munge            \
#     -c config.ini         \
#     analysis/SQ00015117   \
#     sqlite:///backend/batch0/SQ00015117/SQ00015117.sqlite
#
# # Additionally:
# metadata <- read_tsv('metadata/batch0/platemap/C-7161-01-LM6-001.txt') %>%
#
#   mutate(Metadata_broad_sample_type = broad_sample) %>%
#   replace_na(list(Metadata_broad_sample_type = "control")) %>%
#   mutate(Metadata_broad_sample_type = replace(
#     Metadata_broad_sample_type,
#     str_detect(Metadata_broad_sample_type, "BRD"),
#     "permutation"))
#
# write_tsv(metadata, 'metadata/batch0/platemap/C-7161-01-LM6-001.txt')
