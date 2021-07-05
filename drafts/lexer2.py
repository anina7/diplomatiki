#!/usr/bin/env python3

import re
import hypothesis.strategies as st
from hypothesis import given

from myParser import parse, getInfo

@st.composite
def chooseChar(draw, list):
	return draw(st.characters(whitelist_categories=[], whitelist_characters=list))

@st.composite
def randInt(draw):
	return draw(st.integers(min_value=0, max_value=5))

@st.composite
def makeCorrect(draw, data):
	if data == []:
		return []

	#Star (zero or more instances) — *
	if len(data) > 1 and data[1] == "*":
		l = []
		for i in range(draw(randInt())):
			l += draw(makeCorrect([data[0]]))
		return l + draw(makeCorrect(data[2:]))

	#Grouping — ()
	if isinstance(data[0], list):
		return draw(makeCorrect(data[0])) + draw(makeCorrect(data[1:]))

	#OR operator — []
	if data[0] == "[":
		if not "]" in data:
			raise ValueError("Error: Unbalanced brackets")
		end = data.index("]")	#get 1st instance of "["
		end2 = end + 1

		#Star (zero or more instances) — *
		if end + 1 < len(data) and data[end+1] == "*":
			chosen = []
			for i in range(draw(randInt())):
				chosen += [draw(chooseChar(data[1:end]))]
			end2 += 1
		else:
			chosen = [draw(chooseChar(data[1:end]))]

		return chosen + draw(makeCorrect(data[(end2):]))

	#Rest aka just loose chars
	return [data[0]] + draw(makeCorrect(data[1:]))

@st.composite
def correct(draw, data):
	cor = draw(makeCorrect(data))
	return("".join(cor))

if __name__ == "__main__":
	while (1):
		orig = input("Give your reg expr: ")
		print("Testing:", orig)
		data = parse(orig)
		print("Some correct inputs:")

		@given(t=correct(data))
		def runCor(t):
			#check if t matches the regexpr
			assert re.match(r"^"+orig+"$", t), ("Generated string \"" + t + "\" does not match regexpr \"" + orig + "\"")
			print(t)

		runCor()

		'''
		info = getInfo(orig)
		print("Some wrong inputs:")

		@given(t=wrong(data, info))
		def runWro(t):
			#check if t doesn't match the regexpr
			assert (not re.match(r"^"+orig+"$", t)), ("Generated string \"" + t + "\" matches regexpr \"" + orig + "\"")
			print(t)

		runWro()
		'''

		print()
