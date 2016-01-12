import csv
import sys

import collections
import itertools
import copy

from random import shuffle, seed
from pprint import pprint

from .settings import config as config

def parse_model_statements():
	""" Reads the cleaned_sum_files.csv and returns nicer data structure for hierarchal variable breakdown

	Operations are performed on the column names, or categories, that we defined earlier in clean_summaries()
	Categories with dashes are taken to be range, variables with & are treated together

	Returns:
		A trimmed example of the structure created with categories:
			"SCHL=1-11 & AGEP=25-99", "SCHL=12-15 & AGEP=25-99", 
			"AGEP=0-4", "AGEP=5-9", "SEX=1", "SEX=2"

		{"AND": [{"SCHL": {1: [1,..,11],
			               2: [12,..,15],..},
			 	  "static": {"AGEP": [25,..,99],..},
			 	  ..]
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

	with open(config.model_file) as sumcsv:
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

	if config.debug_model_parser:
		pprint(var_dict)

	return var_dict

def create_prediction_map():
	""" Reads model files people in

	Returns:
		Ordered dictionary with outcomes as keys
			values are lists containing the outcomes people in order

		{"Belknap County": ['550', '6124', ..],
	 	 "Coos County":    ['431', '5312', ..],..}
	"""
	prediction_map = collections.OrderedDict()
	
	with open(config.model_file) as sum_csv:
 		sum_file = csv.reader(sum_csv)

 		for row in list(sum_file)[1:]:
 			prediction, persons = row[0], row[1:]
 			prediction_map[prediction] = [float(people) for people in persons]

	return prediction_map	

def create_hierarchy(parsed_expressions, model, initial_people):
	""" Transforms the parsed model file into a hierarchy for by row application to base_file.

	Algorithms and ideas associated with this hierarchy are in the technical documentation.
	Additional complexity is due to handling of conjoined and overlapping statements.

	Args:
		parsed_expressions: Summary file variables after being prepped by create_ar_dict()

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

	def create_level(var, index, values, num_joined, replace_num_people_with=False):
		if replace_num_people_with:
			num_people = replace_num_people_with 
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

	def check_consistent(expression_items, path):
		for expression in expression_items:
			var, values = expression

			if not var in path.keys():
				continue

			lower_bound, upper_bound = path[var]

			# iterate through index pairs backwards by enumeration so we can pop safely
			# if a pair of values for dynamic variable is inconsistent, then remove just that pair
			# if all the pairs are inconsistent, then whole group is inconsistent
			# however if a static variable is inconsistent, then whole group is inconsistent.

			if type(values) is list:
				for i in reversed(list(range(len(values)))):
					min_var, max_var = values[i]

					if not (lower_bound >= min_var and upper_bound <= max_var): 
						values.pop() 

				if not values:
					return False 
			else:
				min_var, max_var = values

				if not (lower_bound >= min_var and upper_bound <= max_var): 
					return False

		return True

	def overflow_strategy(max_num_people, all_num_people):
		return [int((num_people / sum(all_num_people)) * max_num_people) for num_people in all_num_people] 

	def initialize_expression_items(model):
		return [[(var, list(parsed_expressions[var].items())) for var in group] for group in model]

	def new_traverse(cur_list, path, expression_items, parent_people):
		# Conjoined variables are not considered in current version
		if not expression_items:
			return

		expression_packs = copy.deepcopy(expression_items)
		expression_pack = expression_packs.pop()

		while expression_packs and not check_consistent(expression_pack, path):
			expression_pack = expression_packs.pop()

		for expression in expression_pack:
			var_to_add, values = expression

			num_joined = 0

			if type(values) is list: 
				cur_list += [create_level(var_to_add, index, value, num_joined) for index, value in values]
			else:
				cur_list.append((create_level(var_to_add, None, values, None)))

		underflow =  [parent_people[prediction] - sum([new_entry["num_people"][prediction] for new_entry in cur_list])
																						for prediction in range(len(parent_people))]
		overflow = [sum([new_entry["num_people"][prediction] for new_entry in cur_list]) - parent_people[prediction] 
																						for prediction in range(len(parent_people))]

		underflow, overflow = [[people if people > 0 else 0 for people in flow] 
															for flow in (underflow, overflow)]

		subtract_people = [overflow_strategy(overflow[prediction], 
											[new_entry["num_people"][prediction] for new_entry in cur_list])
								for prediction in range(len(parent_people))]  
			
		for category, new_entry in enumerate(cur_list):	 # clever double level enumerate
			for prediction, coefficients in enumerate(subtract_people):  # [[cat1, cat2,..], prediction2, ...]
				new_entry["num_people"][prediction] -= coefficients[category]

		cur_list.append((create_level("no_matches", None, None, num_joined, replace_num_people_with=underflow)))	

		for new_entry in cur_list:
			new_path = update_path(path, new_entry)	
			new_traverse(new_entry["next"], new_path, expression_packs, new_entry["num_people"])

	prediction_map = create_prediction_map().items()
	prediction_names = list(create_prediction_map().keys())

	final_list = []
	initial_expression_items = initialize_expression_items(model)

	new_traverse(final_list, dict(), initial_expression_items, initial_people)

	return final_list

def generate_assignments(model, initial_people, features=None):
	""" Performs duplication of pums data and assigns counties

	Args:
		state: state abbrevation
		state_fips: state fips code
		year: last two digits of year
	"""

	def mk_int(s):
	    s = s.strip()
	    return int(s) if s else None  #TODO: Accepting NA/0 for value.

	def is_match(middle, lower_bound, upper_bound):
		if middle is not None and middle >= lower_bound and middle <= upper_bound:
			return True
		return False

	def trim_hierarchy(row, var_data):
		num_rows = mk_int(row[col_names["PWGTP"]])

		predicted_people = list(enumerate(var_data["num_people"]))
		predicted_people.sort(key = lambda x: x[1], reverse=True)

		for prediction, free_num_people in predicted_people:
			if num_rows <= free_num_people:
				var_data["num_people"][prediction] -= num_rows

				return prediction_names[prediction]

		return None

	def get_different_paths(cur_level, var, values):
		if not cur_level[0]["next"]:
			return list()

		if cur_level[0]["next"][0]["var"] == var:
			return [sub_entry for new_entry in cur_level 
							  for sub_entry in new_entry["next"] if sub_entry["values"] == values]

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

		for var_data in sub_hierarchy:  
			if not var_data["values"] and not var_data["next"]: 
				row_prediction = trim_hierarchy(row, var_data)
				if row_prediction:  
					return row_prediction

				row_prediction = next(var_data["dump_iter"])
				if config.debug_dumped_predictions:
					var_data["dumped"] += [{"num_rows": int(row[col_names["PWGTP"]]), "race": row[col_names["RAC1P"]], 
											"puma00": row[col_names["PUMA00"]], "county": row_prediction}]
				return row_prediction

			row_val = mk_int(row[col_names[var_data["var"]]])
			lower_bound, upper_bound = var_data["values"]

			if not is_match(row_val, lower_bound, upper_bound):
				continue

			if not var_data["next"]: 
				row_prediction = trim_hierarchy(row, var_data)
				if row_prediction:
					return row_prediction

				for entry in get_different_paths(hierarchy, var_data["var"], var_data["values"]):
					row_prediction = trim_hierarchy(row, entry)

					if row_prediction:
						return row_prediction

				continue

			return predict_row(row, var_data["next"])

	def get_total_left_to_assign(sub_hierarchy):
		if not sub_hierarchy[0]["next"]:
			return sum([dump["num_rows"] for dump in sub_hierarchy[-1]["dumped"]])

		return sum([get_total_left_to_assign(var_data["next"]) for var_data in sub_hierarchy])

	seed(12345)

	expressions = parse_model_statements()
	hierarchy = create_hierarchy(expressions, model, initial_people)
	prediction_names = list(create_prediction_map().keys()) # used inside trim_hierarchy, inside predict_rows

	with open(config.base_file) as acs_csv, \
		 open(config.output_file, "w", newline = "") as w_csv:
		r, output = list(csv.reader(acs_csv)), csv.writer(w_csv)

		if not features:
			features = r[0][:-80]

		col_names = {r[0][i] : i for i in range(len(r[0][:-80]))}
		output.writerow(["COUNTY_NAME"] + features)

		for row in r[1:]: 
			row_prediction = predict_row(row, hierarchy)
			if not config.output_rows:
				continue

			if not config.duplicate_rows:
				output.writerow([row_prediction] + [row[col_names[feat]] for feat in features])
				# output.writerow([row_id, row_prediction])
			else:
				for i in range(int(row[col_names["PWGTP"]])):
					output.writerow([row_prediction] + [row[col_names[feat]] for feat in features])

	if config.debug_hierarchy:
		pprint(hierarchy)
		print(get_total_left_to_assign(hierarchy))

	return