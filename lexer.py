import re
import random as rand
from numpy.random import choice as np_choice
from myParser import parse
from myParser import getInfo

n = 5		#arbitrary upper bound of repeats

def corRecur(data):
	if data == []:
		return []
	
	#Star (zero or more instances) — *
	if len(data) > 1 and data[1] == "*":
		new_list = []
		for i in range(rand.randrange(n)):
			new_list += corRecur([data[0]])
		return new_list + corRecur(data[2:])
	
	#Grouping — ()	
	if isinstance(data[0], list):
		return corRecur(data[0]) + corRecur(data[1:])
	
	#OR operator — []		
	if data[0] == "[":
		if not "]" in data:
			raise ValueError("Error: Unbalanced brackets")
		end = data.index("]")	#get 1st instance of "["
		end2 = end + 1
		
		#Star (zero or more instances) — *
		if end + 1 < len(data) and data[end+1] == "*":
			chosen = []
			for i in range(rand.randrange(n)):
				chosen += [rand.choice(data[1:end])]
			end2 += 1
		else:
			chosen = [rand.choice(data[1:end])]
			
		return chosen + corRecur(data[(end2):])
		
	#Rest aka just loose chars
	return [data[0]] + corRecur(data[1:])

def correct(data):
	return("" . join(corRecur(data)))


	
def wrongRand(info, ogchar):
	minus = info.replace(ogchar, "")
	az09 = "abcdefghijklmnopqrstuvwxyz0123456789"
	
	choice1 = rand.choice(info)
	choice2 = rand.choice(minus)
	choice3 = rand.choice(az09)
	
	#return rand.choice([choice1,choice2,choice3]) #this gives equal probabilities
	return np_choice([choice1, choice2, choice3], p=[0.95, 0.025, 0.025])	
	
def wroRecur(data, info):
	if data == []:
		return [wrongRand(info, "")]
	
	#Star (zero or more instances) — *
	if len(data) > 1 and data[1] == "*":
		new_list = []
		for i in range(rand.randrange(n)):
			new_list += wroRecur([data[0]], info)
		return new_list + wroRecur(data[2:], info)
	
	#Grouping — ()	
	if isinstance(data[0], list):
		return wroRecur(data[0], info) + wroRecur(data[1:], info)
	
	#OR operator — []		
	if data[0] == "[":
		if not "]" in data:
			raise ValueError("Error: Unbalanced brackets")
		end = data.index("]")	#get 1st instance of "["
		end2 = end + 1
		
		#Star (zero or more instances) — *
		if end + 1 < len(data) and data[end+1] == "*":
			chosen = []
			for i in range(rand.randrange(n)):
				chosen += [rand.choice(data[1:end])]
			end2 += 1
		else:
			chosen = [rand.choice(data[1:end])]
			
		return chosen + wroRecur(data[(end2):], info)
		
	#Rest aka just loose chars
	return [wrongRand(info, data[0])] + wroRecur(data[1:], info)



def rightORwrong(data, info):
	#EXTRA STAGE:
	#70% probability of the 'wrong' being a 'correct' missing 1-2 chars
	if rand.randrange(10) < 7:
		right = corRecur(data)
		for i in range(rand.randrange(1,3)):
			#check if you can remove
			m = len(right)
			if m < 1:
				break
			r = rand.randrange(m)
			right.pop(r)
		return "" . join(right)
	else:
		return "" . join(wroRecur(data, info))
		
def wrong(orig, data, info):
	res = rightORwrong(data, info)
	
	#check result
	while re.match(r"^"+orig+"$", res):
		res = rightORwrong(data, info)
	
	return res


	
def test(orig):
	print("Testing:", orig)
	data = parse(orig)
	print("Some correct inputs:")
	for i in range(10):
		print(correct(data))
	info = getInfo(orig)
	print("Some wrong inputs:")
	for i in range(10):
		print(wrong(orig, data, info))
	print()

while 1:	
	test(input("Give your reg expr: "))
	
#test inputs:
#01*
#[abc]*
#ab(01)*[23456]
