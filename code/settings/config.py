###################
# File config
###################
'''What we add new variables to. Only PUMS files right now.'''
base_file = ".\scripts\county_assignment\data\ss13pnh.csv"

'''File with outcomes and conditional expressions to apply to base file. Summary files for county case.'''
model_file = ".\scripts\county_assignment\data\sum_13_files.csv"

'''Declare output file.'''
output_file = r".\data\results\pnh13.csv"

###################
# Debugging config
###################
'''Output to stderr the hierarchy after prediction'''
debug_hierarchy = False

'''Output to stderr how the models were parsed into groups'''
debug_model_parser = False

'''Store extra debugging information in hierachy on predictions that dont fit assignments.
	Takes up a lot of space if working with big models, so set to False if needed there.'''
debug_dumped_predictions = True

###################
# Script config
###################
'''If true it will fill output file with new data. If false, create file with only headers.'''
output_rows = False

'''Whether shovel outputs the people one at a time or as a group.'''
duplicate_rows = False

###################
# Model config
###################
'''Which variables to include in product
    Set features = None to include all variables in final product'''
features = ["PWGTP", "AGEP", "RAC1P", "SCH", "FER", "MAR", "SEX", "SCHL"]

'''Which variables, in reverse order, in their sub-groups to build model off of'''
model = [["RAC1P"], ["PUMA00", "PUMA10"]]

'''Initial people for outcomes. For NH county assignment, these are nh county populations.'''
initial_people =  [60088, 47818, 77117, 33055, 89118, 400721, 146445, 295223, 123143, 43742]
