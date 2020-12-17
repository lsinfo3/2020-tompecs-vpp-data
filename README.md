# 2020-tompecs-vpp-data

Measurement data and R-based preprocessing scripts.

* `data`: contains folders with raw measurement data.
  * Folder names indicate configuration. For instance, `exp_maxbatch-64_pktsize-64_queuesize-1024` contains data for experiments using a maximum batch size of 64, a packet size of 64 Bytes, and a queue size of 1024.
  * Two types of files.
    * Batch service times are stored in `clock-*.dat` files. For instance, `clock-mix-4500.dat` contains data for the mixed traffic scenario (alternative: xc for the cross-connect scenario) at a rate of 4500 Mbps.
    * Latency histograms are stored in `histogram-*.csv` files. The same encoding as above applies regarding scenario and rate. 
* `code`: R scripts for preprocessing the raw data.
  * `common.r`: imports and general convenience functions.
  * `001_preproc_service_time_data.r`: calculates batch service times and exports linear fits of the batch service time per configuration into the `stats` subfolder.
  * `002_export_batch_size_distributions.r`: calculates and exports batch size distributions for all configurations.
  * `003_preproc_latency_data.r`: calculates average / median latency per configuration based on histogram data.
