import csv
import sys

import collections
import itertools
import copy

from random import shuffle, seed
from pprint import pprint

def create_var_dict():
	""" Reads the cleaned_sum_files.csv and returns nicer data structure for hierarchal variable breakdown

	Operations are performed on the column names, or categories, that we defined earlier in clean_summaries()
	Categories with dashes are taken to be range, variables with & are treated together

	Returns:
		A trimmed example of the structure created with categories:
			"SCHL=1-11 & AGEP=25-99", "SCHL=12-15 & AGEP=25-99", "AGEP=0-4", "AGEP=5-9", "SEX=1", "SEX=2"
			at column_index: [3, 4, 12, 13, 25, 26]. We trim off county and state in subsequent work so we subtract by 2

		{"AND": [{"SCHL": {1: [1,..,11],
			               2: [12,..,15],..},

			 	  "static": {"AGEP": [25,..,99],..},..]
		"AGEP": {10: [0,..,4],
		 		 11: [5,..,9],..}
		"SEX": {23: [1],
		 		24: [2]}}
	"""

	def transform_col(stmnt, index):
		var, values = stmnt.split("=")

		if "-" in values:
			start, end = values.split("-")
		else:
			start, end = values, values

		return {var: {index: (int(start), int(end))}}

	def update_var_dict(cur_var_dict, new_var_dict):
		var = list(new_var_dict.keys())[0]

		if var in cur_var_dict:
			cur_var_dict[var].update(new_var_dict[var])
		else:
			cur_var_dict.update(new_var_dict)

	def trim_multi_static(multi_dict):
		trim_vars = []
		for var, values_dict in multi_dict.items():
			prev_values = None

			for index, values in values_dict.items():
				if prev_values and prev_values == values:
					multi_dict["static"].update({var: values})
					trim_vars.append(var)
					break
				prev_values = values

		for var in trim_vars:
			del multi_dict[var]

	var_dict = {"AND": []}

	with open("..\scripts\county_assignment\data\sum_13_files.csv") as sumcsv:  #TODO: make this general
		r = list(csv.reader(sumcsv))		
		columns = {r[0][i] : i - 1 for i in range(1, len(r[0]))}

		visited_stmnts = set()

		for stmnt in columns.keys():
			if "&" in stmnt:
				all_stmnts = [transform_col(sub_stmnt, columns[stmnt])
									 	for sub_stmnt in stmnt.split(" & ")]

				new_var_pair = tuple([list(new_var_dict.keys())[0]  # tuple for hashing to visited_stmnts
						for new_var_dict in all_stmnts])

				if new_var_pair not in visited_stmnts:
					var_dict["AND"].append({"static": {}})

				visited_stmnts.add(new_var_pair)

				for new_var_dict in all_stmnts:
					update_var_dict(var_dict["AND"][-1], new_var_dict)
			else:
				update_var_dict(var_dict, transform_col(stmnt, columns[stmnt]))

	for multi_dict in var_dict["AND"]:
		trim_multi_static(multi_dict)

	return var_dict

def create_prediction_map():
	""" Reads the cleaned_sum_files.csv and returns nicer data structure for value accessing by variable index

	Reads until finds rows for this state. Each row has a county and then their variables.
	We store and return a dictionary of the county with its variables.

	Args:
		state: state fips code

	Returns:
		Dictionary with county names as keys with values a list containing its proportions in order

		{"Belknap County": ['550', '6124', ..],
	 	 "Coos County":    ['431', '5312', ..],..}
	"""
	prediction_map = collections.OrderedDict()
	
	with open("..\scripts\county_assignment\data\sum_13_files.csv") as sum_csv:
 		sum_file = csv.reader(sum_csv)

 		for row in list(sum_file)[1:]:
 			prediction, persons = row[0], row[1:]
 			prediction_map[prediction] = [float(people) for people in persons]

	return prediction_map	

def create_hierarchy(var_dict, model):
	""" Transforms the data structure from summary files into a variable proportion hierarchy for application to each row

	Starts with an empty list and recursively constructs the hierarchy
	Goes through each not conjoined variable and adds to current list
	Then inside each new dict entry created, creates an empty list and repeats for next variable
	Then handles non-conjoined variables where compatible with the levels preceding it using multi_check
	Once in final format, can be applied to row easily by diving down hierarchy where row_from_pums_file[var] is in cur_hierarchy[values]

	Args:
		var_dict: Summary file variables after being prepped by create_var_dict()

	Returns:
		Returns a list of dictionaries. 
		Each dictionary has keys - 'var': pums var name, 'index': index in prop_map/puma_dict[puma][county][var_props]
								'values': if row's value matches one of these values, 'next': another list of dictionaries for rest of vars

		[{'var': 'AGEP'
		 'index': 12,
		 'values': [25, 26, 27, 28, 29, 30, 31, 32, 33, 34],
  		 'next': [{'index': 20,
		  		   'values': [1],
		           'var': 'SEX',
		           'next': [{'index': 0,
		                     'next': [],
		                     'values': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
		                     'var': 'SCHL'},
	"""

	def create_level(var, index, values, num_joined, no_matches=False):
		if no_matches:
			num_people = no_matches  # I'm saying no_match is now a list of integers to replace with
		else:
			if index is not None:
				num_people = [predictions[index] for var, predictions in prediction_map]
			else:
				num_people = [0 for var, predictions in prediction_map] # this is for num_joined dummy elements

		nonempty_predictions = [prediction_names[i] for i, prediction in enumerate(num_people) if prediction > 0]

		if nonempty_predictions:
			dump_predictions_iter = itertools.cycle(nonempty_predictions)
		else:
			dump_predictions_iter = itertools.cycle(prediction_names)

		return {"var": var, "index": index, "values": values, "next": list(), "num_joined": num_joined, 
				"num_people": num_people, "dumped": [], "dump_iter": dump_predictions_iter}

	def update_path(path, new_entry):
		new_path = path.copy()
		if new_entry["var"] == "no_matches":
			return new_path

		new_path.update({new_entry["var"]: new_entry["values"]})
		return new_path

	def check_consistent(var_items, path):
		for var_item in var_items:
			var, values = var_item

			if not var in path.keys():
				continue

			lower_bound, upper_bound = path[var]

			if type(values) is list:
				# for i in reversed(list(range(len(values)))):  # iterate through index pairs backwards by enumeration so we can pop safely
				# 	min_var, max_var = values[i]

				# 	if not (lower_bound >= min_var and upper_bound <= max_var): 
				# 		values.pop()  # if a pair of values for dynamic variable is inconsistent, then remove that pair from consideration

				for min_var, max_var in values[::-1]:
					if not (lower_bound >= min_var and upper_bound <= max_var): 
						values.pop()

				return False if not values

				# if not values:
				# 	return False  # if every pair of values for the dynamic variable is inconsistent then whole set is inconsistent
			else:
				min_var, max_var = values

				if not (lower_bound >= min_var and upper_bound <= max_var): 
					return False  # if there is a static check and it fails then the whole set is inconsistent

		return True

	def overflow_strategy(max_num_people, all_num_people):
		return [int((num_people / sum(all_num_people)) * max_num_people) for num_people in all_num_people]  # make new dist of num_peoples match old.
		
		# given some real population values, trim from each county/prediction in num_people to have a certain underflow/overflow. 
		# choose different strategy for how deep the hierarchy is.
		#   this lets us solve rac1p, agep pair race as can do it different on final level to match much more slots.

		# the simpler answer is to say try to predict/provide the predicted people that are in puma10 and rac1p
		# puma + race + (puma, race) vs puma + race
		# (?? puma + race + sex + (puma, race) + (puma, sex) vs puma + race + sex ??)
		#   where the (puma, race) is provided implicitly in pop vals vs in prediction model

	def initialize_var_items(model):
		var_items = []

		# if var_dict["AND"]:
		# 	for multi_dict in var_dict["AND"]:
		# 		if multi_dict["static"]:
		# 			dynamic_var = [v for v in multi_dict.keys() if v != "static"][0]
		# 			multi_vars = [(static_var, values) for static_var, values in multi_dict["static"].items()]
		# 			multi_vars += [(dynamic_var, list(multi_dict[dynamic_var].items()))]

		# 			var_items.append(multi_vars)

		var_items += [[(var, list(var_dict[var].items())) for var in group] 
														for group in model]

		return var_items

	def new_traverse(cur_list, path, var_items, parent_people):
		if not var_items:
			return

		variable_pack = copy.deepcopy(var_items)
		new_vars = variable_pack.pop()

		while variable_pack and not check_consistent(new_vars, path):
			new_vars = variable_pack.pop()

		for var_item in new_vars:
			var_to_add, values = var_item

			num_joined = 0 # TODO SETTing num_joined to 0 always for now

			if type(values) is list:  # if it is a dynamic variable. Always succeeds at this check first so head will have the index, static wont
				cur_list += [create_level(var_to_add, index, value, num_joined) for index, value in values]
			else:
				cur_list.append((create_level(var_to_add, None, values, None)))
		# wait static is easy as everything in here (including all the dynamic variables since im going to keep it simple for now) takes the dummy as next
		underflow =  [parent_people[prediction] - sum([new_entry["num_people"][prediction] for new_entry in cur_list])
																						for prediction in range(len(parent_people))]
		overflow = [sum([new_entry["num_people"][prediction] for new_entry in cur_list]) - parent_people[prediction] 
																						for prediction in range(len(parent_people))]

		underflow, overflow = [[people if people > 0 else 0 for people in flow] 
															for flow in (underflow, overflow)]

		subtract_people = [overflow_strategy(overflow[prediction], 
											[new_entry["num_people"][prediction] for new_entry in cur_list])
								for prediction in range(len(parent_people))]  
			
		for category, new_entry in enumerate(cur_list):		# this is so awesome, uses double level enumerate perfectly
			for prediction, coefficients in enumerate(subtract_people):  # [[cat1, cat2,..], prediction2, ...]
				new_entry["num_people"][prediction] -= coefficients[category]

		cur_list.append((create_level("no_matches", None, None, num_joined, no_matches=underflow)))	

		for new_entry in cur_list:	# now it wont have the var_item for this pack
			new_path = update_path(path, new_entry)		#TODO: make sure this path is ok with new method
			new_traverse(new_entry["next"], new_path, variable_pack, new_entry["num_people"])

	prediction_map = create_prediction_map().items()
	prediction_names = list(create_prediction_map().keys())

	final_list = []
	initial_var_items = initialize_var_items(model)
	nh_county_pops = [60088, 47818, 77117, 33055, 89118, 400721, 146445, 295223, 123143, 43742]

	new_traverse(final_list, dict(), initial_var_items, nh_county_pops)

	return final_list

def generate_output(state, state_fips, year, model, features=None):
	""" Performs duplication of pums data and assigns counties

	Args:
		state: state abbrevation
		state_fips: state fips code
		year: last two digits of year
	"""

	def mk_int(s):
	    s = s.strip()
	    return int(s) if s else None  #TODO: Think about 0 vs None - do we cast NA to zero in some cases?

	def is_match(middle, lower_bound, upper_bound):
		if middle is not None and middle >= lower_bound and middle <= upper_bound:
			return True
		return False

	def trim_hierarchy(row, var_data):
		# possible_predictions arg missing at end
		num_rows = mk_int(row[col_names["PWGTP"]])

		# predicted_people = list(enumerate(var_data["num_people"]))
		# predicted_people = {prediction: people for prediction, people in predicted_people}

		# for prediction in possible_predictions:
		# 	free_num_people = predicted_people[prediction]

		# 	if num_rows <= free_num_people:
		# 		var_data["num_people"][prediction] -= num_rows

		# 		return prediction_names[prediction]

		predicted_people = list(enumerate(var_data["num_people"]))
		predicted_people.sort(key = lambda x: x[1], reverse=True)

		for prediction, free_num_people in predicted_people:
			if num_rows <= free_num_people:
				var_data["num_people"][prediction] -= num_rows

				return prediction_names[prediction]

		# rac1 = [0,1000,2000, ..] in puma100, rac1 = [1000, 0, 500] in puma200...
		# want to make overflow for this one only go into the 2000 slot in puma100
		return None

	def get_different_paths(cur_level, var, values):
		if not cur_level[0]["next"]:
			return list()

		if cur_level[0]["next"][0]["var"] == var:
			# pprint([sub_entry for new_entry in cur_level for sub_entry in new_entry["next"] if sub_entry["values"] == values])
			# print(var, values)
			return [sub_entry for new_entry in cur_level for sub_entry in new_entry["next"] if sub_entry["values"] == values]

		return get_different_paths(cur_level[0]["next"], var, values)

	def predict_row(row, sub_hierarchy):
		""" Recurses through hierarchy based on matching row attribute values

		Args:
			row: row in pums file
			sub_hierarchy: current variable hierarchy
			indices: set of indices where row's attr matches sub_hierarchy levels

		Returns:
			indices: indices of matching proportions
		"""

		for var_data in sub_hierarchy:  #TODO: because ignoring last index here, when using static vars still need to add no_match or account for
			if not var_data["values"] and not var_data["next"]:  # values == None means it is a no_matches. not var_data["next"] means end of file.
				row_prediction = trim_hierarchy(row, var_data)
				if row_prediction: # if this one works then does current level first. otherwise searches all parents branches. 
					return row_prediction	# we can skip because it will try it again and fail in loop.

				# var_data["dumped"] += int(row[col_names["PWGTP"]])
				t = next(var_data["dump_iter"])
				var_data["dumped"] += [{"num_rows": int(row[col_names["PWGTP"]]), "race": row[col_names["RAC1P"]], 
										"puma00": row[col_names["PUMA00"]], "county": t}]
				return t

			row_val = mk_int(row[col_names[var_data["var"]]])
			lower_bound, upper_bound = var_data["values"]

			if not is_match(row_val, lower_bound, upper_bound):	 # skip non-matches
				continue

			if not var_data["next"]:  # we have hit the bottom so do assignment (on just the consistent ones that have matching values to row)
				row_prediction = trim_hierarchy(row, var_data)
				if row_prediction: # if this one works then does current level first. otherwise searches all parents branches. 
					return row_prediction	# we can skip because it will try it again and fail in loop.

				for entry in get_different_paths(hierarchy, var_data["var"], var_data["values"]):
					row_prediction = trim_hierarchy(row, entry)

					if row_prediction:
						return row_prediction

				continue

			return predict_row(row, var_data["next"])

	# def predict_row(row, sub_hierarchy, prev_people):
	# 	""" Recurses through hierarchy based on matching row attribute values

	# 	Args:
	# 		row: row in pums file
	# 		sub_hierarchy: current variable hierarchy
	# 		indices: set of indices where row's attr matches sub_hierarchy levels

	# 	Returns:
	# 		indices: indices of matching proportions
	# 	"""

	# 	for var_data in sub_hierarchy:  #TODO: because ignoring last index here, when using static vars still need to add no_match or account for
	# 		if not var_data["values"] and not var_data["next"]:  # values == None means it is a no_matches. not var_data["next"] means end of file.
	# 			possible_predictions = list(range(len(var_data["num_people"])))

	# 			row_prediction = trim_hierarchy(row, var_data, possible_predictions)
	# 			if row_prediction: # if this one works then does current level first. otherwise searches all parents branches. 
	# 				return row_prediction	# we can skip because it will try it again and fail in loop.

	# 			var_data["dumped"] += int(row[col_names["PWGTP"]])
	# 			return next(var_data["dump_iter"])

	# 		row_val = mk_int(row[col_names[var_data["var"]]])
	# 		lower_bound, upper_bound = var_data["values"]

	# 		if not is_match(row_val, lower_bound, upper_bound):	 # skip non-matches
	# 			continue

	# 		if not var_data["next"]:  # we have hit the bottom so do assignment (on just the consistent ones that have matching values to row)
	# 			possible_predictions = list(range(len(var_data["num_people"])))

	# 			row_prediction = trim_hierarchy(row, var_data, possible_predictions)
	# 			if row_prediction: # if this one works then does current level first. otherwise searches all parents branches. 
	# 				return row_prediction	# we can skip because it will try it again and fail in loop.

	# 			# possible_predictions = [i for i in range(len(var_data["num_people"])) if var_data["num_people"][i] > 0]
	# 			possible_predictions = [i for i in range(len(prev_people)) if prev_people[i] > 0]
	# 			# this right here needs to be determined on parent, not itselves num_people

	# 			# this works since this will return empty list on first level. so can pass all zero's to start off prev_people chain.
	# 			for entry in get_different_paths(hierarchy, var_data["var"], var_data["values"]):
	# 				# have to use possibly predictions in the trim_hierarchy
	# 				row_prediction = trim_hierarchy(row, entry, possible_predictions)

	# 				if row_prediction:
	# 					return row_prediction

	# 			continue

	# 		return predict_row(row, var_data["next"], var_data["num_people"])

			# if var_data["num_joined"] is None:  # if no num_joined then it is a static variable so we have already checked and use head's info
			# 	return predict_row(row, var_data["next"])

			# if var_data["num_joined"]:  # this will be run extra times as we have num_joined decreasing down chain but still call it in successive nodes
			# 	consistent = True
			# 	cur_var_data = var_data

			# 	for i in range(var_data["num_joined"]):
			# 		cur_var_data = cur_var_data["next"][0]  # since its conjoined variable, has only one element in subhierarchy

			# 		row_val = mk_int(row[col_names[cur_var_data["var"]]])
			# 		lower_bound, upper_bound = cur_var_data["values"]

			# 		if not (row_val is not None and row_val >= lower_bound and row_val <= upper_bound):
			# 			consistent = False
			# 			break

			# 	if not consistent:  # this activates only if isn't compatable on some static variable in chain
			# 		continue  # we don't add this variable and jump to next chain

			# CURRENT SITUATION
			# need it to only cycle through to sub_entries with the right parent_people for the predictor we are looking at

		# pprint(sub_hierarchy)
		# the 10 is for number_predictions/counties
		# return predict_row(row, sub_hierarchy[-1]["next"], sub_hierarchy[-1]["no_matches"])  # Could be matches further down so select the no_matches entry

	def get_total_left_to_assign(sub_hierarchy):
		if not sub_hierarchy[0]["next"]:
			dumped = sub_hierarchy[-1]["dumped"]
			return sum([dump["num_rows"] for dump in dumped])

			# return sum([sum(var_data["num_people"]) for var_data in sub_hierarchy])

		return sum([get_total_left_to_assign(var_data["next"]) for var_data in sub_hierarchy])

	seed(12345)

	acs_file = "..\scripts\county_assignment\data\ss13pnh.csv"
	output_file = "..\scripts\county_assignment\data\pnh13.csv"

	hierarchy = create_hierarchy(create_var_dict(), model)
	prediction_names = list(create_prediction_map().keys())
	dump_predictions = itertools.cycle(prediction_names)

	# pprint(hierarchy)
	print(get_total_left_to_assign(hierarchy))

	with open(acs_file) as acs_csv, \
		 open(output_file, "w", newline = "") as w_csv:
		r, output = list(csv.reader(acs_csv)), csv.writer(w_csv)

		if not features:
			features = r[0][:-80]

		col_names = {r[0][i] : i for i in range(len(r[0][:-80]))}
		# output.writerow(["COUNTY_NAME"] + features)
		output.writerow(["PARENT_ROW_ID", "COUNTY_NAME"])

		for row_id, row in enumerate(reversed(r[1:])):  # thid can be done better with a splice
			# row_prediction = predict_row(row, hierarchy, [0 for i in range(10)])
			row_prediction = predict_row(row, hierarchy)
			output.writerow([row_id, row_prediction])

			# for i in range(int(row[col_names["PWGTP"]])):
			# 	output.writerow([row_prediction] + [row[col_names[feat]] for feat in features])

	pprint(hierarchy)
	print(get_total_left_to_assign(hierarchy))
	
	return


# For MAJID
# we merge by parent_row_id == person.parent_row_id (from parent_row_id, county model file versus base(or merged) pums file)

# file1: apply_model.py: prints out county + parent_row_id (which is just row number)
# file2: duplicate_pums.py: prints out person_id, person_code, state_code, state_abbr, parent_row


# authors <- data.frame(
#     surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
#     nationality = c("US", "Australia", "US", "UK", "Australia"),
#     deceased = c("yes", rep("no", 4)))
# books <- data.frame(
#     name = I(c("Tukey", "Venables", "Tierney",
#              "Ripley", "Ripley", "McNeil", "R Core")),
#     title = c("Exploratory Data Analysis",
#               "Modern Applied Statistics ...",
#               "LISP-STAT",
#               "Spatial Statistics", "Stochastic Simulation",
#               "Interactive Data Analysis",
#               "An Introduction to R"),
#     other.author = c(NA, "Ripley", NA, NA, NA, NA,
#                      "Venables & Smith"))

# (m1 <- merge(authors, books, by.x = "surname", by.y = "name"))


state, state_fips, year = "NH", 33, 13
features = ["PWGTP", "AGEP", "RAC1P", "SCH", "FER", "MAR", "SEX", "SCHL"]

# model = [["AGEP"], ["RAC1P"], ["PUMA00", "PUMA10"]]
model = [["RAC1P"], ["PUMA00", "PUMA10"]]

generate_output(state, state_fips, year, model, features=features)

# IDEA:
# take total per race slots left to fill. when we distribute a no_match with rac=3, fill from its total across levels for each county. 
#   if a county for race1 totals can be filled, check that it is compatible with parent puma assignment




# instead of dumping, jump up to parent and look at another branch of that variable/level. 
# if it would fill that spot up, push it over there.




# shuffling at runtime isnt good enough as for some breakdowns there can still be one or two or more categories that didn't get trimmed

# MAYBE

# we do a second pass after assignment. this time we do adjustment off of how the levels ended up getting filled. 
# we assign all dumped_iter assignments a flag if it happened like that so we know they dont contribute to any num_peoples

# go through on each row check if assigning it to a different county would make it overall less or more accurate.
# this way if we have tons of asians in one puma vs another (the pumas are similar in size) that contributes to a county, 
#    then we can bump the asians into the right slots.
# (we are comparing accuracy here to provided summary_files)
# what if we based assignment also off of benchmarking files??

### PARADIGM SHIFT

# all levels in the hierarchy start off with their from file population values

# all levels in the hierarchy start off with their from file population values
# when we go by rows, add num_rows to each level going down that matches
# also have master max_num_rows for each level that is each row from summary file for that variable/column

# when master max_num_rows fills up, we start pushing elsewhere.

# after finishing we go to each level (starting at bottom) and redistribute if it makes it more accurate

# so all previous work sets up the hierarchy for the second pass, although just puma_codes are used in the initial go-through

#### NEW
 
# First pass, dont even assign anything. Just fill up the hierarchy including no_matches. 
# (or more likely we start it off with puma county assignments and apply iteravely on each category we have)
# After we fill up hierarchy, then go back through rows again. At each row:
# 	Go to matching slot in hierarchy.
# 	identify if anywhere else is yields a better fit and overall assignment accuracy
# 	Replace better fits and assign there instead
# 	Modify our tracked totals appropriately to account for changes

# (maybe match accuracy and adjust assignment on top/largest level first and go down?)

# if its puma -> race breakdowns, we are distributing puma=100, race=1 then draw from puma=200,300,.., race=1 on county level
# by design we can narrow down which pumas to look at for a county by checking its num_people[county]. if it isnt empty then we will use inside it for redistributing
# wait also remember that this remaining large number is there since it came last in shuffle
# (if its shuffling error comes from within own level. if its deviation of attr in parent error, comes from parent level.)





           # develops assignments so that model is represented well in statistical breakdowns by those vars
           # YEP - way better as cless with get() methods instead of remembering keys like no_matches, "var", "num_people", "next", ..
           # OR RATHER - those will be attributes of the class (wherever we call the key explicitly). inner methods do all the work