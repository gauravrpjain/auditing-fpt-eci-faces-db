#
# target entry to build program executable from program and mylib
# object files
#
all: setup.R 2_Results_Analysis.R 3_Regression.R 4_additional_plots.R API_response/Facepp_results.Rda API_response/microsoft_result.Rda API_response/facex_output.Rda API_response/amazon_test_results_v20210222.csv Cleaned_data_no_PII.Rda image_df.Rda
	Rscript setup.R

clean:
	rm -r Output/
	rm combined_df.Rda
