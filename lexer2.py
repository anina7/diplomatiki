import re
import hypothesis.strategies as st
from hypothesis import given

from myParser import parse
from myParser import getInfo

@st.composite
def chooseChar(draw, list):
	return draw(st.characters(whitelist_categories=[], whitelist_characters=list))

@st.composite
def makeCorrect(draw, data):
	if data == []:
		return []
	
	#Star (zero or more instances) — *
	elif len(data) > 1 and data[1] == "*":
		l = []
		for i in range(5):		#to be fixed
			l += draw(makeCorrect([data[0]]))
		return l + draw(makeCorrect(data[2:]))
	
	#Grouping — ()
	elif isinstance(data[0], list):
		return draw(makeCorrect(data[0]))
		
	#OR operator — []		
	elif data[0] == "[":
		if not "]" in data:
			raise ValueError("Error: Unbalanced brackets")
		end = data.index("]")	#get 1st instance of "["
		end2 = end + 1
		
		#Star (zero or more instances) — *
		if end + 1 < len(data) and data[end+1] == "*":
			chosen = []
			for i in range(5):	#to be fixed
				chosen += [draw(chooseChar(data[1:end]))]
			end2 += 1
		else:
			chosen = [draw(chooseChar(data[1:end]))]
			
		return chosen + draw(makeCorrect(data[(end2):]))
		
	#Rest aka just loose chars
	else:
		return [data[0]] + draw(makeCorrect(data[1:]))
		
@given(f=makeCorrect(['a','0','1','2','*']))
def correct(f):
	print("".join(f))

correct()